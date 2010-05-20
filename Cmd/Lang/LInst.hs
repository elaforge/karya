-- | REPL Cmds dealing with instruments and MIDI config.
module Cmd.Lang.LInst where
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Pretty as Pretty

import qualified Midi.Midi as Midi

import Ui
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Info as Info

import qualified Derive.Schema as Schema
import qualified Derive.Schema.Default as Default
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument

import qualified Instrument.MidiDb as MidiDb


-- | Called from the browser.
lookup_instrument :: String -> Cmd.CmdL (Maybe Instrument.Instrument)
lookup_instrument inst_name = do
    lookup_inst <- Cmd.get_lookup_midi_instrument
    return $ lookup_inst Score.no_attrs (Score.Instrument inst_name)

inst_info :: String -> Cmd.CmdL String
inst_info inst_name = Info.inst_info (Score.Instrument inst_name)

all_inst_info :: Cmd.CmdL String
all_inst_info = do
    config <- State.get_midi_config
    info <- mapM Info.inst_info (Map.keys (Instrument.config_alloc config))
    return $ show (length info) ++ " instruments:\n" ++ Seq.join "\n\n" info

track_info :: BlockId -> TrackNum
    -> Cmd.CmdL (Schema.TrackType, Maybe Score.Instrument, Pitch.ScaleId)
track_info block_id tracknum = do
    track_tree <- State.get_track_tree block_id
    proj_scale <- State.get_project_scale
    case Schema.get_track_info proj_scale track_tree (Just tracknum) of
        (Nothing, _, _) -> Cmd.throw $ "can't get track type for "
            ++ show block_id ++ " at " ++ show tracknum
        (Just typ, inst, scale) -> return (typ, inst, scale)

-- | Steps to load a new instrument.  All of them are optional, depending on
-- the circumstances.
--
-- - Deallocate address asignments for the old instrument, if one is being
-- replaced.
--
-- - Allocate addresses for the new instrument.
--
-- - Title track with new instrument.
--
-- - Send midi init.
--
-- For example, typing a new instrument in a track title should only complain
-- if there is no allocation, but not necessarily deallocate the replaced
-- instrument or send midi init.
load_instrument :: String -> Cmd.CmdL ()
load_instrument inst_name = do
    let inst = Score.Instrument inst_name
    block_id <- Cmd.get_focused_block
    tracknum <- Cmd.require =<< Cmd.get_insert_tracknum
    track_id <- Cmd.require =<< State.event_track_at block_id tracknum
    old_inst <- Cmd.require =<< fmap inst_type (track_info block_id tracknum)

    dealloc_instrument old_inst
    dev <- Cmd.require_msg ("no device for " ++ show inst)  =<< device_of inst
    chan <- find_chan_for dev
    alloc_instrument inst [(dev, chan)]

    State.set_track_title track_id (Default.instrument_to_title inst)
    send_instrument_init inst chan
    Log.notice $ "deallocating " ++ show old_inst ++ ", allocating "
        ++ show (dev, chan) ++ " to " ++ show inst
    where
    inst_type (Schema.NoteTrack _, inst, _) = inst
        -- maybe also accept control if there is just one inst
        -- but then I'd need some way to know the track_id
    inst_type _ = Nothing

find_chan_for :: Midi.WriteDevice -> Cmd.CmdL Midi.Channel
find_chan_for dev = do
    alloc <- fmap Instrument.config_alloc State.get_midi_config
    let addrs = map ((,) dev) [0..15]
        taken = concat (Map.elems alloc)
    let match = fmap snd $ List.find (not . (`elem` taken)) addrs
    Cmd.require_msg ("couldn't find free channel for " ++ show dev) match

send_instrument_init :: Score.Instrument -> Midi.Channel -> Cmd.CmdL ()
send_instrument_init inst chan = do
    info <- Cmd.require_msg ("inst not found: " ++ show inst)
        =<< Cmd.lookup_instrument_info inst
    let init = Instrument.patch_initialize (MidiDb.info_patch info)
        dev = Instrument.synth_device (MidiDb.info_synth info)
    send_initialization init inst dev chan

-- | This feels like it should go in another module... Cmd.Instrument?
-- I have too many things called Instrument!
send_initialization :: Instrument.InitializePatch
    -> Score.Instrument -> Midi.WriteDevice -> Midi.Channel -> Cmd.CmdL ()
send_initialization init inst dev chan = case init of
    Instrument.InitializeMidi msgs -> do
        Log.notice $ "sending midi init: " ++ Pretty.pretty msgs
        mapM_ ((Cmd.midi dev) . Midi.set_channel chan) msgs
    Instrument.InitializeMessage msg ->
        -- TODO warn doesn't seem quite right for this...
        Log.warn $ "initialize instrument " ++ show inst ++ ": " ++ msg
    Instrument.NoInitialization -> return ()

alloc_instrument :: Score.Instrument -> [Instrument.Addr] -> Cmd.CmdL ()
alloc_instrument inst addrs = do
    config <- State.get_midi_config
    let alloc = Instrument.config_alloc config
    State.set_midi_config $ config
        { Instrument.config_alloc = Map.insert inst addrs alloc }

dealloc_instrument :: Score.Instrument -> Cmd.CmdL ()
dealloc_instrument inst = do
    config <- State.get_midi_config
    let alloc = Instrument.config_alloc config
    State.set_midi_config $ config
        { Instrument.config_alloc = Map.delete inst alloc }

-- | Deallocate the old allocation, and set it to the new one.  Meant for
-- interactive use.
realloc_instrument :: String -> String -> [Midi.Channel] -> Cmd.CmdL ()
realloc_instrument inst_name wdev chans = do
    let inst = Score.Instrument inst_name
    dealloc_instrument inst
    alloc_instrument inst [(Midi.WriteDevice wdev, c) | c <- chans]

schema_instruments :: BlockId -> Cmd.CmdL [Score.Instrument]
schema_instruments block_id = do
    titles <- fmap (map State.track_title) (State.get_track_info block_id)
    return $ Seq.map_maybe Default.title_to_instrument titles

-- | Try to automatically create an instrument config based on the instruments
-- found in the given block.  It simply gives each instrument on a device a
-- single channel increasing from 0.
--
-- Example: @auto_config (bid \"b0\") >>= State.set_midi_config@
--
-- TODO: won't work if there are >1 block, need a merge config
-- TODO: same inst with different keyswitches should get the same addrs
auto_config :: BlockId -> Cmd.CmdL Instrument.Config
auto_config block_id = do
    insts <- schema_instruments block_id
    devs <- mapM device_of insts
    let no_dev = [inst | (inst, Nothing) <- zip insts devs]
        inst_devs = [(inst, dev) | (inst, Just dev) <- zip insts devs]
        allocs = [(inst, [(dev, fromIntegral i)])
            | (dev, by_dev) <- Seq.keyed_group_on snd inst_devs
            , (i, (inst, _dev)) <- Seq.enumerate by_dev]
    unless (null no_dev) $
        Log.warn $ "no synth found for instruments: " ++ show insts
    return $ Instrument.config allocs

device_of :: Score.Instrument -> Cmd.CmdL (Maybe Midi.WriteDevice)
device_of inst = do
    maybe_info <- Cmd.lookup_instrument_info inst
    return $ fmap (Instrument.synth_device . MidiDb.info_synth) maybe_info

controls_of :: Score.Instrument -> [Control.Control]
controls_of _inst = undefined -- TODO

-- | Send AllNotesOff msgs to all inst addr.
all_notes_off :: Cmd.CmdL ()
all_notes_off = do
    config <- State.get_midi_config
    let addrs = Seq.unique $ concat $
            Map.elems (Instrument.config_alloc config)
    let notes_off chan = Midi.ChannelMessage chan Midi.AllNotesOff
    sequence_ [Cmd.midi dev (notes_off chan) | (dev, chan) <- addrs]
