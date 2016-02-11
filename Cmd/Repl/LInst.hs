-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
-- | REPL Cmds dealing with instruments and MIDI config.
module Cmd.Repl.LInst where
import Prelude hiding (lookup)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lens as Lens
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Ui.TrackTree as TrackTree
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Info as Info
import qualified Cmd.Instrument.MidiConfig as MidiConfig
import qualified Cmd.Repl.Util as Util
import qualified Cmd.Save as Save

import qualified Derive.Env as Env
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified Instrument.InstTypes as InstTypes
import Global
import Types


-- * instrument info

lookup :: Instrument -> Cmd.CmdL (Maybe Cmd.Inst)
lookup = Cmd.lookup_instrument . Util.instrument

info :: Instrument -> Cmd.CmdL Text
info = Info.instrument_info . Util.instrument

info_all :: Cmd.CmdL Text
info_all = do
    config <- State.get_midi_config
    info <- mapM Info.instrument_info (Map.keys config)
    return $ showt (length info) <> " instruments:\n"
        <> Text.intercalate "\n" info

-- * config

-- | Print out instrument configs all purty-like.
list :: State.M m => m Text
list = list_like ""

-- | Pretty print matching instruments:
--
-- > >pno - pianoteq/ loop1 [0..15]
-- > >syn - sampler/inst
list_like :: State.M m => Text -> m Text
list_like pattern = do
    configs <- State.get_midi_config
    alias_map <- aliases
    return $ Text.intercalate "\n" $
        map (show_inst configs) $ filter (matches . fst) $
        Map.toAscList alias_map
    where
    matches inst = pattern `Text.isInfixOf` Score.instrument_name inst
    show_inst configs (inst, qualified) = ShowVal.show_val inst <> " - "
        <> InstTypes.show_qualified qualified
        <> maybe "" show_config (Map.lookup inst configs)
    show_config config = mconcat
        [ Info.show_addrs (map fst (Patch.config_addrs config))
        , show_environ (Patch.config_restricted_environ config)
        , show_controls "" (Patch.config_controls config)
        , show_flags config
        , show_controls "defaults:" (Patch.config_control_defaults config)
        ]
    show_controls msg controls
        | Map.null controls = ""
        | otherwise = " " <> msg <> pretty controls
    show_environ environ
        | environ == mempty = ""
        | otherwise = " " <> pretty environ
    show_flags config
        | null flags = ""
        | otherwise = " {" <> Text.intercalate ", " flags <> "}"
        where
        flags = ["mute" | Patch.config_mute config]
            ++ ["solo" | Patch.config_solo config]

-- | The not-so-purty version.
midi_config :: State.M m => m Patch.Configs
midi_config = State.get_midi_config

-- | Alias map.  It maps from alias to underlying instrument.
aliases :: State.M m => m (Map.Map Score.Instrument InstTypes.Qualified)
aliases = State.config#State.aliases <#> State.get

-- | Rename an instrument, in both aliases and allocations.
rename :: State.M m => Instrument -> Instrument -> m ()
rename from_ to_ =
    State.modify $ (State.config#State.midi %= rename_alloc)
        . (State.config#State.aliases %= rename_alias)
    where
    rename_alloc configs = case Map.lookup from configs of
        Nothing -> configs
        Just config -> Map.insert to config $ Map.delete from configs
    rename_alias aliases = case Map.lookup from aliases of
        Just qualified -> Map.insert to qualified $ Map.delete from aliases
        Nothing -> aliases
    from = Util.instrument from_
    to = Util.instrument to_

-- | Allocate a new instrument and create an alias for it.  For instance:
--
-- > LInst.add \"m\" \"kontakt/mridangam-g\" \"loop1\" [0]
--
-- This will create an instance of the @kontakt/mridangam@ instrument aliased
-- to @>m@, and assign it to the MIDI WriteDevice @loop1@, with a single MIDI
-- channel 0 allocated.
add :: Instrument -> Qualified -> Text -> [Midi.Channel] -> Cmd.CmdL ()
add name qualified wdev chans = do
    alloc name wdev chans
    add_alias name qualified

add_im :: Instrument -> Qualified -> Cmd.CmdL ()
add_im = add_alias

save :: FilePath -> Cmd.CmdL ()
save = Save.save_midi_config

load :: FilePath -> Cmd.CmdL ()
load = Save.load_midi_config

-- | Create an instrument with no channel allocation.  This is used for
-- instruments which are expected to be converted into other instruments during
-- derivation.  For instance, pasang instruments are stand-ins for polos
-- sangsih pairs.
add_empty :: Instrument -> Instrument -> Cmd.CmdL ()
add_empty name qualified = add name qualified "empty" []

-- | Remove both an alias and its allocation.
remove :: Instrument -> Cmd.CmdL ()
remove inst = do
    remove_alias inst
    dealloc inst

remove_im :: Instrument -> Cmd.CmdL ()
remove_im = remove_alias

-- | Add a new instrument, copied from an existing one.  The argument order is
-- the same as used by 'add'.
add_alias :: Instrument -> Qualified -> Cmd.CmdL ()
add_alias inst qualified = do
    qualified <- parse_qualified qualified
    State.modify $
        State.config#State.aliases %= Map.insert (Util.instrument inst)
            qualified

remove_alias :: Instrument -> Cmd.CmdL ()
remove_alias inst = State.modify $
    State.config#State.aliases %= Map.delete (Util.instrument inst)

-- | Toggle and return the new value.
toggle_mute :: State.M m => Instrument -> m Bool
toggle_mute inst = modify_config inst $ \config ->
    let mute = not $ Patch.config_mute config
    in (config { Patch.config_mute = mute }, mute)

-- | Toggle and return the new value.
toggle_solo :: State.M m => Instrument -> m Bool
toggle_solo inst = modify_config inst $ \config ->
    let solo = not $ Patch.config_solo config
    in (config { Patch.config_solo = solo }, solo)

-- | Add an environ val to the instrument config.
add_environ :: (RestrictedEnviron.ToVal a, State.M m) => Instrument -> Env.Key
    -> a -> m ()
add_environ inst name val = modify_config_ inst $
    Patch.cenviron %= (RestrictedEnviron.make [(name, v)] <>)
    where v = RestrictedEnviron.to_val val

-- | Clear the instrument config's environ.  The instrument's built-in environ
-- from 'Patch.patch_environ' is still present.
clear_environ :: State.M m => Instrument -> m ()
clear_environ inst = modify_config_ inst $ Patch.cenviron #= mempty

set_controls :: State.M m => Instrument -> [(Score.Control, Signal.Y)] -> m ()
set_controls inst controls = modify_config_ inst $
    Patch.controls #= Map.fromList controls

set_scale :: State.M m => Instrument -> Patch.Scale -> m ()
set_scale inst scale = modify_config_ inst $ Patch.cscale #= Just scale

set_control_defaults :: State.M m => Instrument -> [(Score.Control, Signal.Y)]
    -> m ()
set_control_defaults inst controls = modify_config_ inst $
    Patch.control_defaults #= Map.fromList controls

get_config :: State.M m => Score.Instrument -> m Patch.Config
get_config inst = State.require ("no config for " <> pretty inst)
    . Map.lookup inst =<< State.get_midi_config

modify_config :: State.M m => Instrument -> (Patch.Config -> (Patch.Config, a))
    -> m a
modify_config inst_ modify = do
    let inst = Util.instrument inst_
    config <- get_config inst
    let (new, result) = modify config
    State.modify $ State.config # State.midi # Lens.map inst #= Just new
    return result

modify_config_ :: State.M m => Instrument -> (Patch.Config -> Patch.Config)
    -> m ()
modify_config_ inst modify = modify_config inst (\c -> (modify c, ()))


-- * allocate a device and channels

-- | Deallocate the old allocation, and set it to the new one.  Meant for
-- interactive use.
alloc :: Instrument -> Text -> [Midi.Channel] -> Cmd.CmdL ()
alloc inst wdev chans = alloc_voices inst wdev (map (, Nothing) chans)

-- | Like 'alloc', but you can also give maximum voices per channel.
alloc_voices :: Instrument -> Text -> [(Midi.Channel, Maybe Patch.Voices)]
    -> Cmd.CmdL ()
alloc_voices inst_ wdev chan_voices = do
    let inst = Util.instrument inst_
    dealloc_instrument inst
    let dev = Midi.write_device wdev
    alloc_instrument inst [((dev, c), v) | (c, v) <- chan_voices]

dealloc :: Instrument -> Cmd.CmdL ()
dealloc = dealloc_instrument . Util.instrument

-- | Allocate the given channels for the instrument using its default device.
alloc_default :: Instrument -> [(Midi.Channel, Maybe Patch.Voices)]
    -> Cmd.CmdL ()
alloc_default inst_ chans = do
    let inst = Util.instrument inst_
    wdev <- device_of inst
    alloc_instrument inst [((wdev, c), v) | (c, v) <- chans]

-- | Merge the given configs into the existing one.
merge_config :: State.M m => Patch.Configs -> m ()
merge_config config = State.modify $ State.config#State.midi %= (config<>)

-- | Merge or replace both 'Patch.Configs' and aliases.
merge, replace :: State.M m => MidiConfig.Config -> m ()
merge = MidiConfig.merge
replace = MidiConfig.replace

-- * rest

{- | Steps to load a new instrument.  All of them are optional, depending on
    the circumstances.

    - Deallocate address asignments for the old instrument, if one is being
    replaced.

    - Allocate addresses for the new instrument.

    - Title track with new instrument.

    - Send midi init.

    For example, typing a new instrument in a track title should only complain
    if there is no allocation, but not necessarily deallocate the replaced
    instrument or send midi init.
-}
set :: Instrument -> Cmd.CmdL ()
set inst_ = do
    let inst = Util.instrument inst_
    block_id <- Cmd.get_focused_block
    tracknum <- Cmd.abort_unless =<< Cmd.get_insert_tracknum
    track_id <- Cmd.abort_unless =<< State.event_track_at block_id tracknum

    -- Deallocate the old instrument.
    title <- State.get_track_title
        =<< State.get_event_track_at block_id tracknum
    whenJust (ParseTitle.title_to_instrument title) dealloc_instrument

    dev <- device_of inst
    chan <- find_chan_for dev
    alloc_instrument inst [((dev, chan), Nothing)]

    State.set_track_title track_id (ParseTitle.instrument_to_title inst)
    initialize inst dev chan
    Log.notice $ "allocating " <> showt (dev, chan) <> " to " <> showt inst


-- ** implementation

-- | Find an unallocated channel on the given device.
find_chan_for :: Midi.WriteDevice -> Cmd.CmdL Midi.Channel
find_chan_for dev = do
    config <- State.get_midi_config
    let addrs = map ((,) dev) [0..15]
        taken = concatMap (map fst . Patch.config_addrs) (Map.elems config)
    let match = fmap snd $ List.find (not . (`elem` taken)) addrs
    Cmd.require ("couldn't find free channel for " <> showt dev) match

initialize :: Score.Instrument -> Midi.WriteDevice -> Midi.Channel
    -> Cmd.CmdL ()
initialize inst wdev chan = do
    patch <- Cmd.get_midi_patch inst
    send_initialization (Patch.patch_initialize patch) inst wdev chan

send_initialization :: Patch.InitializePatch
    -> Score.Instrument -> Midi.WriteDevice -> Midi.Channel -> Cmd.CmdL ()
send_initialization init inst dev chan = case init of
    Patch.InitializeMidi msgs -> do
        Log.notice $ "sending midi init: " <> pretty msgs
        mapM_ (Cmd.midi dev . Midi.set_channel chan) msgs
    Patch.InitializeMessage msg ->
        -- TODO warn doesn't seem quite right for this...
        Log.warn $ "initialize instrument " <> showt inst <> ": " <> msg
    Patch.NoInitialization -> return ()

alloc_instrument :: Score.Instrument -> [(Patch.Addr, Maybe Patch.Voices)]
    -> Cmd.CmdL ()
alloc_instrument inst addrs = State.modify $
    State.config#State.midi#Lens.map inst #= Just (Patch.voice_config addrs)

dealloc_instrument :: Score.Instrument -> Cmd.CmdL ()
dealloc_instrument inst = State.modify $
    State.config#State.midi#Lens.map inst #= Nothing

block_instruments :: BlockId -> Cmd.CmdL [Score.Instrument]
block_instruments block_id = do
    titles <- fmap (map State.track_title) (TrackTree.tracks_of block_id)
    return $ mapMaybe ParseTitle.title_to_instrument titles

-- | Try to automatically create an instrument config based on the instruments
-- found in the given block.  It simply gives each instrument on a device a
-- single channel increasing from 0.
--
-- Example: @auto_config (bid \"b0\") >>= State.set_midi_config@
--
-- TODO: won't work if there are >1 block, need a merge config
-- TODO: same inst with different keyswitches should get the same addrs
auto_config :: BlockId -> Cmd.CmdL Patch.Configs
auto_config block_id = do
    insts <- block_instruments block_id
    devs <- mapM device_of insts
    let addrs =
            [ (inst, [(dev, fromIntegral i)])
            | (dev, by_dev) <- Seq.keyed_group_sort snd (zip insts devs)
            , (i, (inst, _dev)) <- Seq.enumerate by_dev
            ]
    return $ Patch.configs addrs

-- | Synths default to writing to a device with their name.  You'll have to
-- map it to a real hardware WriteDevice in the 'Cmd.Cmd.write_device_map'.
device_of :: Score.Instrument -> Cmd.CmdL Midi.WriteDevice
device_of inst = do
    InstTypes.Qualified synth _ <-
        Cmd.require ("no instrument: " <> pretty inst)
            =<< Cmd.lookup_qualified inst
    return $ Midi.write_device synth


-- * tuning

-- | Set the instrument's Scale to the given scale and send a MIDI tuning
-- message to retune the synth.  Obviously this only works for synths that
-- support it.
retune :: Cmd.M m => Instrument -> Patch.Scale -> m ()
retune inst scale = do
    let msg = Midi.realtime_tuning $ map (second Pitch.nn_to_double) $
            Patch.scale_keys scale
    set_scale inst scale
    devs <- map (fst . fst) . Patch.config_addrs <$>
        get_config (Util.instrument inst)
    mapM_ (flip Cmd.midi msg) (Seq.unique devs)

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
    interface <- Cmd.gets (Cmd.config_midi_interface . Cmd.state_config)
    liftIO (op interface)


-- * misc

-- | Send a CC MIDI message on the given device.  This is for synths that use
-- MIDI learn.
teach :: Text -> Midi.Channel -> Midi.Control -> Cmd.CmdL ()
teach dev chan cc = Cmd.midi (Midi.write_device dev) $
    Midi.ChannelMessage chan (Midi.ControlChange cc 1)

type Instrument = Text
-- | This is parsed into a 'Inst.Qualified'.
type Qualified = Text

parse_qualified :: Cmd.M m => Qualified -> m InstTypes.Qualified
parse_qualified text
    | "/" `Text.isInfixOf` text = return $ InstTypes.parse_qualified text
    | otherwise =
        Cmd.throw $ "qualified inst name lacks a /: " <> showt text
