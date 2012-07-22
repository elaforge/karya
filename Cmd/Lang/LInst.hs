-- | REPL Cmds dealing with instruments and MIDI config.
module Cmd.Lang.LInst where
import Prelude hiding (lookup)
import qualified Control.Monad.Trans as Trans
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Info as Info
import qualified Derive.Score as Score
import qualified Derive.TrackInfo as TrackInfo
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb
import Types


-- * instrument config

-- | Send AllNotesOff msgs to all inst addr.
all_notes_off :: Cmd.CmdL ()
all_notes_off = do
    alloc <- State.get_midi_alloc
    let addrs = Seq.unique $ concat $ Map.elems alloc
    let notes_off chan = Midi.ChannelMessage chan Midi.AllNotesOff
    sequence_ [Cmd.midi dev (notes_off chan) | (dev, chan) <- addrs]

lookup :: String -> Cmd.CmdL (Maybe Cmd.MidiInfo)
lookup = Cmd.lookup_instrument_info . Score.Instrument

-- | Deallocate the old allocation, and set it to the new one.  Meant for
-- interactive use.
realloc :: String -> String -> [Midi.Channel] -> Cmd.CmdL ()
realloc inst_name wdev chans = do
    let inst = Score.Instrument inst_name
    dealloc_instrument inst
    alloc_instrument inst [(Midi.write_device wdev, c) | c <- chans]

dealloc :: String -> Cmd.CmdL ()
dealloc = dealloc_instrument . Score.Instrument

alloc :: String -> [Midi.Channel] -> Cmd.CmdL ()
alloc inst_name chans = do
    let inst = Score.Instrument inst_name
    wdev <- maybe (Cmd.throw $ "inst not in db: " ++ Pretty.pretty inst) return
        =<< device_of inst
    alloc_instrument inst [(wdev, c) | c <- chans]

inst_info :: String -> Cmd.CmdL String
inst_info inst_name = Info.inst_info (Score.Instrument inst_name)

all_inst_info :: Cmd.CmdL String
all_inst_info = do
    alloc <- State.get_midi_alloc
    info <- mapM Info.inst_info (Map.keys alloc)
    return $ show (length info) ++ " instruments:\n" ++ Seq.join "\n\n" info

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
load :: String -> Cmd.CmdL ()
load inst_name = do
    let inst = Score.Instrument inst_name
    block_id <- Cmd.get_focused_block
    tracknum <- Cmd.require =<< Cmd.get_insert_tracknum
    track_id <- Cmd.require =<< State.event_track_at block_id tracknum

    -- TODO fix this, parse the title to look for an inst
    -- old_inst <- Cmd.require =<< fmap inst_type (LTrack.info block_id tracknum)
    -- dealloc_instrument old_inst
    dev <- Cmd.require_msg ("no device for " ++ show inst)  =<< device_of inst
    chan <- find_chan_for dev
    alloc_instrument inst [(dev, chan)]

    State.set_track_title track_id (TrackInfo.instrument_to_title inst)
    initialize inst chan
    Log.notice $ "allocating " ++ show (dev, chan) ++ " to " ++ show inst
    -- Log.notice $ "deallocating " ++ show old_inst ++ ", allocating "
    --     ++ show (dev, chan) ++ " to " ++ show inst


-- ** implementation

find_chan_for :: Midi.WriteDevice -> Cmd.CmdL Midi.Channel
find_chan_for dev = do
    alloc <- State.get_midi_alloc
    let addrs = map ((,) dev) [0..15]
        taken = concat (Map.elems alloc)
    let match = fmap snd $ List.find (not . (`elem` taken)) addrs
    Cmd.require_msg ("couldn't find free channel for " ++ show dev) match

initialize :: Score.Instrument -> Midi.Channel -> Cmd.CmdL ()
initialize inst chan = do
    info <- Cmd.require_msg ("inst not found: " ++ show inst)
        =<< Cmd.lookup_instrument_info inst
    let init = Instrument.patch_initialize (MidiDb.info_patch info)
    let dev = Instrument.synth_device (MidiDb.info_synth info)
    send_initialization init inst dev chan

send_initialization :: Instrument.InitializePatch
    -> Score.Instrument -> Midi.WriteDevice -> Midi.Channel -> Cmd.CmdL ()
send_initialization init inst dev chan = case init of
    Instrument.InitializeMidi msgs -> do
        Log.notice $ "sending midi init: " ++ Pretty.pretty msgs
        mapM_ (Cmd.midi dev . Midi.set_channel chan) msgs
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

block_instruments :: BlockId -> Cmd.CmdL [Score.Instrument]
block_instruments block_id = do
    titles <- fmap (map State.track_title) (State.tracks_of block_id)
    return $ Maybe.mapMaybe TrackInfo.title_to_instrument titles

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
    insts <- block_instruments block_id
    devs <- mapM device_of insts
    let no_dev = [inst | (inst, Nothing) <- zip insts devs]
        inst_devs = [(inst, dev) | (inst, Just dev) <- zip insts devs]
        allocs = [(inst, [(dev, fromIntegral i)])
            | (dev, by_dev) <- Seq.keyed_group_on snd inst_devs
            , (i, (inst, _dev)) <- Seq.enumerate by_dev]
    unless (null no_dev) $
        Log.warn $ "no synth or midi device found for instruments: "
            ++ show no_dev
    return $ Instrument.config allocs

device_of :: Score.Instrument -> Cmd.CmdL (Maybe Midi.WriteDevice)
device_of inst = do
    maybe_info <- Cmd.lookup_instrument_info inst
    return $ Instrument.synth_device . MidiDb.info_synth <$> maybe_info


-- * midi interface

read_devices :: Cmd.CmdL [Midi.ReadDevice]
read_devices = run_interface Interface.read_devices

write_devices :: Cmd.CmdL [Midi.WriteDevice]
write_devices = run_interface Interface.write_devices

connect_read_device :: Midi.ReadDevice -> Cmd.CmdL Bool
connect_read_device rdev =
    run_interface (flip Interface.connect_read_device rdev)

disconnect_read_device :: Midi.ReadDevice -> Cmd.CmdL Bool
disconnect_read_device rdev =
    run_interface (flip Interface.disconnect_read_device rdev)

run_interface :: (Interface.Interface -> IO a) -> Cmd.CmdL a
run_interface op = do
    interface <- Cmd.gets Cmd.state_midi_interface
    Trans.liftIO (op interface)
