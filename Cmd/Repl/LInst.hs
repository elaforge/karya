{-# LANGUAGE NoMonomorphismRestriction #-}
-- | REPL Cmds dealing with instruments and MIDI config.
module Cmd.Repl.LInst where
import Prelude hiding (lookup)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Lens as Lens
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Ui.TrackTree as TrackTree
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Info as Info
import qualified Derive.Score as Score
import qualified Derive.TrackInfo as TrackInfo
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb
import Types


-- * instrument info

lookup :: Text -> Cmd.CmdL (Maybe Cmd.MidiInfo)
lookup = Cmd.lookup_instrument . Score.Instrument

info :: Text -> Cmd.CmdL Text
info inst_name = Info.inst_info (Score.Instrument inst_name)

info_all :: Cmd.CmdL Text
info_all = do
    config <- State.get_midi_config
    info <- mapM Info.inst_info (Map.keys config)
    return $ showt (length info) <> " instruments:\n"
        <> Text.intercalate "\n" info

-- * config

configs :: (State.M m) => m Instrument.Configs
configs = State.get_midi_config

aliases :: Cmd.CmdL (Map.Map Score.Instrument Score.Instrument)
aliases = State.config#State.aliases <#> State.get

-- | Add a new instrument, copied from an existing one.
add_alias :: Text -> Text -> Cmd.CmdL ()
add_alias new inst = State.modify $
    State.config#State.aliases %= Map.insert (Score.Instrument new)
        (Score.Instrument inst)

remove_alias :: Text -> Cmd.CmdL ()
remove_alias inst = State.modify $
    State.config#State.aliases %= Map.delete (Score.Instrument inst)

toggle_mute :: (State.M m) => Text -> m Bool
toggle_mute inst = modify_config (Score.Instrument inst) $ \config ->
    let mute = not $ Instrument.config_mute config
    in (config { Instrument.config_mute = mute }, mute)

toggle_solo :: (State.M m) => Text -> m Bool
toggle_solo inst = modify_config (Score.Instrument inst) $ \config ->
    let solo = not $ Instrument.config_solo config
    in (config { Instrument.config_solo = solo }, solo)

modify_config :: (State.M m) => Score.Instrument
    -> (Instrument.Config -> (Instrument.Config, a)) -> m a
modify_config inst modify = do
    config <- State.require ("no config for " <> Pretty.pretty inst)
        . Map.lookup inst =<< configs
    let (new, result) = modify config
    State.modify $ State.config # State.midi # Lens.map inst #= Just new
    return result


-- * allocate a device and channels

-- | Deallocate the old allocation, and set it to the new one.  Meant for
-- interactive use.
alloc :: Text -> Text -> [Midi.Channel] -> Cmd.CmdL ()
alloc inst_name wdev chans = do
    let inst = Score.Instrument inst_name
    dealloc_instrument inst
    alloc_instrument inst [(Midi.write_device wdev, c) | c <- chans]

dealloc :: Text -> Cmd.CmdL ()
dealloc = dealloc_instrument . Score.Instrument

-- | Allocate the given channels for the instrument using its default device.
alloc_default :: Text -> [Midi.Channel] -> Cmd.CmdL ()
alloc_default inst_name chans = do
    let inst = Score.Instrument inst_name
    wdev <- maybe (Cmd.throw $ "inst not in db: " ++ Pretty.pretty inst) return
        =<< device_of inst
    alloc_instrument inst [(wdev, c) | c <- chans]

-- | Merge the given configs into the existing one.
merge :: Instrument.Configs -> Cmd.CmdL ()
merge config = State.modify $ State.config # State.midi %= (config<>)

-- * rest

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
load :: Text -> Cmd.CmdL ()
load inst_name = do
    let inst = Score.Instrument inst_name
    block_id <- Cmd.get_focused_block
    tracknum <- Cmd.require =<< Cmd.get_insert_tracknum
    track_id <- Cmd.require =<< State.event_track_at block_id tracknum

    -- Deallocate the old instrument.
    title <- State.get_track_title
        =<< State.get_event_track_at block_id tracknum
    when_just (TrackInfo.title_to_instrument title) dealloc_instrument

    dev <- Cmd.require_msg ("no device for " ++ show inst) =<< device_of inst
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
    config <- State.get_midi_config
    let addrs = map ((,) dev) [0..15]
        taken = concatMap Instrument.config_addrs (Map.elems config)
    let match = fmap snd $ List.find (not . (`elem` taken)) addrs
    Cmd.require_msg ("couldn't find free channel for " ++ show dev) match

initialize :: Score.Instrument -> Midi.Channel -> Cmd.CmdL ()
initialize inst chan = do
    info <- Cmd.require_msg ("inst not found: " ++ show inst)
        =<< Cmd.lookup_instrument inst
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
alloc_instrument inst addrs = State.modify $
    State.config # State.midi # Lens.map inst #= Just (Instrument.config addrs)

dealloc_instrument :: Score.Instrument -> Cmd.CmdL ()
dealloc_instrument inst = State.modify $
    State.config # State.midi # Lens.map inst #= Nothing

block_instruments :: BlockId -> Cmd.CmdL [Score.Instrument]
block_instruments block_id = do
    titles <- fmap (map State.track_title) (TrackTree.tracks_of block_id)
    return $ mapMaybe TrackInfo.title_to_instrument titles

-- | Try to automatically create an instrument config based on the instruments
-- found in the given block.  It simply gives each instrument on a device a
-- single channel increasing from 0.
--
-- Example: @auto_config (bid \"b0\") >>= State.set_midi_config@
--
-- TODO: won't work if there are >1 block, need a merge config
-- TODO: same inst with different keyswitches should get the same addrs
auto_config :: BlockId -> Cmd.CmdL Instrument.Configs
auto_config block_id = do
    insts <- block_instruments block_id
    devs <- mapM device_of insts
    let no_dev = [inst | (inst, Nothing) <- zip insts devs]
        inst_devs = [(inst, dev) | (inst, Just dev) <- zip insts devs]
        addrs =
            [ (inst, [(dev, fromIntegral i)])
            | (dev, by_dev) <- Seq.keyed_group_on snd inst_devs
            , (i, (inst, _dev)) <- Seq.enumerate by_dev
            ]
    unless (null no_dev) $
        Log.warn $ "no synth or midi device found for instruments: "
            ++ show no_dev
    return $ Instrument.configs addrs

device_of :: Score.Instrument -> Cmd.CmdL (Maybe Midi.WriteDevice)
device_of inst = do
    maybe_info <- Cmd.lookup_instrument inst
    return $ Instrument.synth_device . MidiDb.info_synth <$> maybe_info


-- * midi interface

-- | Every read device on the system, along with any aliases it may have.
read_devices :: Cmd.CmdL [(Midi.ReadDevice, [Midi.ReadDevice])]
read_devices = run_interface Interface.read_devices

-- | Every write device on the system, along with any aliases it may have.
write_devices :: Cmd.CmdL [(Midi.WriteDevice, [Midi.WriteDevice])]
write_devices = run_interface Interface.write_devices

connect_read_device :: Midi.ReadDevice -> Cmd.CmdL Bool
connect_read_device rdev =
    run_interface (flip Interface.connect_read_device rdev)

disconnect_read_device :: Midi.ReadDevice -> Cmd.CmdL Bool
disconnect_read_device rdev =
    run_interface (flip Interface.disconnect_read_device rdev)

run_interface :: (Interface.Interface -> IO a) -> Cmd.CmdL a
run_interface op = do
    interface <- Cmd.gets (Cmd.state_midi_interface . Cmd.state_config)
    liftIO (op interface)


-- * misc

-- | Send a CC MIDI message on the given device.  This is for synths that use
-- MIDI learn.
teach :: Text -> Midi.Channel -> Midi.Control -> Cmd.CmdL ()
teach dev chan cc = Cmd.midi (Midi.write_device dev) $
    Midi.ChannelMessage chan (Midi.ControlChange cc 1)
