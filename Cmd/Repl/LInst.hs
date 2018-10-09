-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | REPL Cmds dealing with instruments and MIDI config.
module Cmd.Repl.LInst where
import Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Lens as Lens
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Info as Info
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Repl.Util as Util
import Cmd.Repl.Util (Instrument)
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection

import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Parse as Parse
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Im.Play as Im.Play
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes

import Global
import Types


-- * get

lookup :: Instrument -> Cmd.CmdL (Maybe Cmd.ResolvedInstrument)
lookup = Cmd.lookup_instrument . Util.instrument

lookup_allocation :: Ui.M m => Instrument -> m (Maybe UiConfig.Allocation)
lookup_allocation inst = Ui.allocation (Util.instrument inst) <#> Ui.get

get_allocation :: Ui.M m => Instrument -> m UiConfig.Allocation
get_allocation = get_instrument_allocation . Util.instrument

-- | List all allocated instruments.
allocated :: Ui.M m => m [Score.Instrument]
allocated = Ui.get_config $ Map.keys . (UiConfig.allocations_map #$)

-- | List all allocated instrument configs all purty-like.
list :: Cmd.M m => m Text
list = list_like ""

-- | Pretty print matching instruments:
--
-- > >pno - pianoteq/ loop1 [0..15]
-- > >syn - sampler/inst 音
list_like :: Cmd.M m => Text -> m Text
list_like pattern = do
    alloc_map <- Ui.config#Ui.allocations_map <#> Ui.get
    let (names, allocs) = unzip $ Map.toAscList alloc_map
    patches <- map (fmap fst . (Cmd.midi_instrument =<<)) <$>
        mapM Cmd.lookup_instrument names
    return $ Text.unlines $ TextUtil.formatColumns 1
        [ pretty_alloc maybe_patch name alloc
        | (name, alloc, maybe_patch) <- zip3 names allocs patches
        , matches name
        ]
    where
    matches inst = pattern `Text.isInfixOf` Score.instrument_name inst

pretty_alloc :: Maybe Patch.Patch -> Score.Instrument -> UiConfig.Allocation
    -> [Text]
pretty_alloc maybe_patch inst alloc =
    [ ShowVal.show_val inst
    , InstTypes.show_qualified (UiConfig.alloc_qualified alloc)
    , case UiConfig.alloc_backend alloc of
        UiConfig.Midi config -> Info.show_addrs (Patch.config_addrs config)
        UiConfig.Im -> "音"
        UiConfig.Dummy -> "(dummy)"
    -- Put flags in their own column to make them obvious.
    , show_flags (UiConfig.alloc_config alloc)
    , join
        [ show_common_config (UiConfig.alloc_config alloc)
        , case UiConfig.alloc_backend alloc of
            UiConfig.Midi config -> show_midi_config config
            _ -> ""
        ]
    ]
    where
    show_common_config config = join
        [ show_environ (Common.config_environ config)
        , show_controls "" (Common.config_controls config)
        ]
    show_environ environ
        | environ == mempty = ""
        | otherwise = pretty environ
    show_flags config
        | null flags = ""
        | otherwise = "{" <> Text.intercalate ", " flags <> "}"
        where
        flags = ["mute" | Common.config_mute config]
            ++ ["solo" | Common.config_solo config]
    show_midi_config config = join
        [ show_controls "defaults:" (Patch.config_control_defaults config)
        , pretty_settings (Patch.patch_defaults <$> maybe_patch)
            (Patch.config_settings config)
        ]
    show_controls msg controls
        | Map.null controls = ""
        | otherwise = msg <> pretty controls
    join = Text.unwords . filter (not . Text.null)

pretty_settings :: Maybe Patch.Settings -> Patch.Settings -> Text
pretty_settings maybe_defaults settings =
    Text.unwords $ filter (not . Text.null)
        [ if_changed Patch.config_flags pretty
        , if_changed Patch.config_scale $
            maybe "" (("("<>) . (<>")") . show_scale)
        , if_changed Patch.config_decay $ ("decay="<>) . pretty
        , if_changed Patch.config_pitch_bend_range $ ("pb="<>) . pretty
        ]
    where
    if_changed get fmt
        | Just defaults <- maybe_defaults, get defaults == get settings = ""
        | otherwise = fmt (get settings)

show_scale :: Patch.Scale -> Text
show_scale scale = "scale " <> Patch.scale_name scale <> " "
    <> showt (length (Patch.scale_nns Nothing scale)) <> " keys"

-- | Instrument allocations.
allocations :: Ui.M m => m UiConfig.Allocations
allocations = Ui.config#Ui.allocations <#> Ui.get

-- * add and remove

-- | Allocate a new MIDI instrument.  For instance:
--
-- > LInst.add "m" "kontakt/mridangam-g" "loop1" [0]
--
-- This will create an instance of the @kontakt/mridangam@ instrument named
-- @>m@, and assign it to the MIDI WriteDevice @loop1@, with a single MIDI
-- channel 0 allocated.
add :: Instrument -> Qualified -> Text -> [Midi.Channel] -> Cmd.CmdL ()
add inst qualified wdev chans =
    add_config inst qualified [((dev, chan), Nothing) | chan <- chans]
    where dev = Midi.write_device wdev

-- | Allocate the given channels for the instrument using its default device.
add_default :: Instrument -> Qualified -> [Midi.Channel] -> Cmd.CmdL ()
add_default inst qualified chans = do
    dev <- device_of (Util.instrument inst)
    add_config inst qualified [((dev, chan), Nothing) | chan <- chans]

add_config :: Instrument -> Qualified -> [(Patch.Addr, Maybe Patch.Voices)]
    -> Cmd.CmdL ()
add_config inst qualified allocs = do
    qualified <- parse_qualified qualified
    patch <- Cmd.require ("not a midi instrument: " <> pretty qualified)
        . Inst.inst_midi =<< Cmd.get_qualified qualified
    let config = Patch.patch_to_config patch allocs
    allocate (Util.instrument inst) $
        UiConfig.allocation qualified (UiConfig.Midi config)

-- | Allocate a new Im instrument.
add_im :: Instrument -> Qualified -> Cmd.CmdL ()
add_im inst qualified = do
    qualified <- parse_qualified qualified
    allocate (Util.instrument inst) $
        UiConfig.allocation qualified UiConfig.Im

-- | Add the play-cache instrument.  This is a dummy instrument used to
-- trigger the play-cache vst.  It's emitted automatically if there are im
-- instruments, but needs a channel allocation.
add_play_cache :: Text -> Midi.Channel -> Cmd.CmdL ()
add_play_cache wdev chan =
    allocate (Util.instrument "play-cache") $
        UiConfig.allocation Im.Play.qualified (UiConfig.Midi config)
    where
    config = Patch.config (Patch.make_settings (0, 0))
        [((Midi.write_device wdev, chan), Nothing)]

-- | Create a dummy instrument.  This is used for instruments which are
-- expected to be converted into other instruments during derivation.  For
-- instance, pasang instruments are stand-ins for polos sangsih pairs.
--
-- The qualified name still has to name a valid patch.  Its common config
-- will be used for the allocation, and MIDI-specific fields discarded.
add_dummy :: Instrument -> Instrument -> Cmd.CmdL ()
add_dummy inst qualified = do
    qualified <- parse_qualified qualified
    allocate (Util.instrument inst) $
        UiConfig.allocation qualified UiConfig.Dummy

-- | All allocations should go through this to verify their validity, unless
-- it's modifying an existing allocation and not changing the Qualified name.
allocate :: Cmd.M m => Score.Instrument -> UiConfig.Allocation -> m ()
allocate score_inst alloc = do
    inst <- Cmd.get_qualified (UiConfig.alloc_qualified alloc)
    allocs <- Ui.config#Ui.allocations <#> Ui.get
    allocs <- Cmd.require_right id $
        UiConfig.allocate (Inst.inst_backend inst) score_inst alloc allocs
    Ui.modify_config $ Ui.allocations #= allocs

-- | Remove an instrument allocation.
remove :: Instrument -> Cmd.CmdL ()
remove = deallocate . Util.instrument

deallocate :: Cmd.M m => Score.Instrument -> m ()
deallocate inst = Ui.modify_config $ Ui.allocations_map %= Map.delete inst

-- | Merge the given configs into the existing ones.  This also merges
-- 'Patch.config_defaults' into 'Patch.config_settings'.  This way functions
-- that create Allocations don't have to find the relevant Patch.
merge :: Cmd.M m => UiConfig.Allocations -> m ()
merge (UiConfig.Allocations alloc_map) = do
    let (names, allocs) = unzip (Map.toList alloc_map)
    insts <- mapM (Cmd.get_qualified . UiConfig.alloc_qualified) allocs
    merged <- Cmd.require_right id $
        mapM (uncurry MidiInst.merge_defaults) (zip insts allocs)
    existing <- Ui.get_config (Ui.allocations #$)
    let errors = mapMaybe (verify existing) (zip3 names merged insts)
    unless (null errors) $
        Cmd.throw $ "merged allocations: " <> Text.intercalate "\n" errors
    Ui.modify_config $ Ui.allocations
        %= (UiConfig.Allocations (Map.fromList (zip names merged)) <>)
    where
    verify allocs (name, alloc, inst) =
        UiConfig.verify_allocation allocs (Inst.inst_backend inst) name alloc

replace :: Cmd.M m => UiConfig.Allocations -> m ()
replace allocs = do
    Ui.modify_config $ Ui.allocations #= mempty
    merge allocs

-- * modify

-- | Point an instrument at a different Qualified.
rename_qualified :: Cmd.M m => Instrument -> Qualified -> m ()
rename_qualified inst qualified = do
    qualified <- parse_qualified qualified
    Cmd.get_qualified qualified
    Ui.modify_allocation (Util.instrument inst) $ \alloc ->
        alloc { UiConfig.alloc_qualified = qualified }

-- | Rename an instrument.
rename :: Ui.M m => Instrument -> Instrument -> m ()
rename from to = modify_allocations from $ \alloc ->
    Map.insert (Util.instrument to) alloc
    . Map.delete (Util.instrument from)

copy :: Ui.M m => Instrument -> Instrument -> m ()
copy from to = modify_allocations from $ Map.insert (Util.instrument to)

modify_allocations :: Ui.M m => Instrument
    -> (UiConfig.Allocation -> Map Score.Instrument UiConfig.Allocation
        -> Map Score.Instrument UiConfig.Allocation)
    -> m ()
modify_allocations inst modify = do
    alloc <- get_allocation inst
    Ui.modify_config $ UiConfig.allocations_map %= modify alloc

-- ** Common.Config

-- | Toggle and return the new value.
mute :: Ui.M m => Instrument -> m Bool
mute = modify_common_config $ \config ->
    let mute = not $ Common.config_mute config
    in (config { Common.config_mute = mute }, mute)

-- | Toggle and return the new value.
solo :: Ui.M m => Instrument -> m Bool
solo = modify_common_config $ \config ->
    let solo = not $ Common.config_solo config
    in (config { Common.config_solo = solo }, solo)

-- | Add an environ val to the instrument config.
add_environ :: (RestrictedEnviron.ToVal a, Ui.M m) =>
    Env.Key -> a -> Instrument -> m ()
add_environ name val = modify_common_config_ $ Common.add_environ name val

-- | Clear the instrument config's environ.  The instrument's built-in environ
-- from 'Patch.patch_environ' is still present.
clear_environ :: Ui.M m => Instrument -> m ()
clear_environ = modify_common_config_ $ Common.cenviron #= mempty

-- ** Midi.Patch.Config

set_addr :: Ui.M m => Text -> [Midi.Channel] -> Instrument -> m ()
set_addr wdev chans = modify_midi_config $
    Patch.allocation #= [((dev, chan), Nothing) | chan <- chans]
    where dev = Midi.write_device wdev

set_controls :: Ui.M m => [(Score.Control, Signal.Y)] -> Instrument -> m ()
set_controls controls = modify_common_config_ $
    Common.controls #= Map.fromList controls

set_control :: Ui.M m => Score.Control -> Maybe Signal.Y -> Instrument -> m ()
set_control control val = modify_common_config_ $
    Common.controls # Lens.map control #= val

set_tuning_scale :: Ui.M m => Text -> Patch.Scale -> Instrument -> m ()
set_tuning_scale tuning scale inst = do
    set_scale scale inst
    add_environ EnvKey.tuning tuning inst

set_control_defaults :: Ui.M m => [(Score.Control, Signal.Y)] -> Instrument
    -> m ()
set_control_defaults controls = modify_midi_config $
    Patch.control_defaults #= Map.fromList controls

-- ** Midi.Patch.Config settings

get_scale :: Cmd.M m => Score.Instrument -> m (Maybe Patch.Scale)
get_scale inst =
    (Patch.settings#Patch.scale #$) . snd <$> Cmd.get_midi_instrument inst

set_scale :: Ui.M m => Patch.Scale -> Instrument -> m ()
set_scale scale = modify_midi_config $ Patch.settings#Patch.scale #= Just scale

copy_scale :: Cmd.M m => Instrument -> Instrument -> m ()
copy_scale from to = do
    scale <- Cmd.require "no scale" =<< get_scale (Util.instrument from)
    set_scale scale to

add_flag :: Ui.M m => Patch.Flag -> Instrument -> m ()
add_flag flag = modify_midi_config $
    Patch.settings#Patch.flags %= Patch.add_flag flag

remove_flag :: Ui.M m => Patch.Flag -> Instrument -> m ()
remove_flag flag = modify_midi_config $
    Patch.settings#Patch.flags %= Patch.remove_flag flag

set_decay :: Ui.M m => Maybe RealTime -> Instrument -> m ()
set_decay decay = modify_midi_config $ Patch.settings#Patch.decay #= decay

-- * util

get_midi_config :: Ui.M m => Score.Instrument
    -> m (InstTypes.Qualified, Common.Config, Patch.Config)
get_midi_config inst =
    Ui.require ("not a midi instrument: " <> pretty inst) =<<
        lookup_midi_config inst

lookup_midi_config :: Ui.M m => Score.Instrument
    -> m (Maybe (InstTypes.Qualified, Common.Config, Patch.Config))
lookup_midi_config inst = do
    UiConfig.Allocation qualified config backend
        <- get_instrument_allocation inst
    return $ case backend of
        UiConfig.Midi midi_config -> Just (qualified, config, midi_config)
        _ -> Nothing

modify_config :: Ui.M m =>
    (Common.Config -> Patch.Config -> ((Common.Config, Patch.Config), a))
    -> Instrument -> m a
modify_config modify inst_ = do
    let inst = Util.instrument inst_
    (qualified, common, midi) <- get_midi_config inst
    let ((new_common, new_midi), result) = modify common midi
        new = UiConfig.Allocation qualified new_common (UiConfig.Midi new_midi)
    Ui.modify_config $ Ui.allocations_map %= Map.insert inst new
    return result

modify_midi_config :: Ui.M m => (Patch.Config -> Patch.Config) -> Instrument
    -> m ()
modify_midi_config modify =
    modify_config $ \common midi -> ((common, modify midi), ())

modify_common_config :: Ui.M m => (Common.Config -> (Common.Config, a))
    -> Instrument -> m a
modify_common_config modify inst_ = do
    let inst = Util.instrument inst_
    alloc <- get_instrument_allocation inst
    let (config, result) = modify (UiConfig.alloc_config alloc)
        new = alloc { UiConfig.alloc_config = config }
    Ui.modify_config $ Ui.allocations_map %= Map.insert inst new
    return result

modify_common_config_ :: Ui.M m => (Common.Config -> Common.Config)
    -> Instrument -> m ()
modify_common_config_ modify =
    modify_common_config $ \config -> (modify config, ())

get_instrument_allocation :: Ui.M m => Score.Instrument -> m UiConfig.Allocation
get_instrument_allocation inst =
    Ui.require ("no allocation for " <> pretty inst)
        =<< Ui.allocation inst <#> Ui.get


-- * Cmd.EditState

set_attrs :: Cmd.M m => Text -> Instrument -> m ()
set_attrs attrs inst_ = do
    let inst = Util.instrument inst_
    Cmd.get_instrument inst -- ensure that it exists
    val <- Cmd.require_right ("parsing attrs: " <>) $
        Parse.parse_val ("+" <> attrs)
    attrs <- Cmd.require_right id $ Typecheck.typecheck_simple val
    Cmd.set_instrument_attributes inst attrs


-- * change_instrument

-- | Replace the instrument in the current track with the given one, and
-- 'initialize' it.  This is intended for hardware synths which need a program
-- change or sysex, and can be invoked via "Instrument.Browser".
change_instrument :: Qualified -> Cmd.CmdL ()
change_instrument new_qualified = do
    new_qualified <- parse_qualified new_qualified
    let new_inst = case new_qualified of
            InstTypes.Qualified _ name -> Score.Instrument name
    track_id <- snd <$> Selection.event_track
    old_inst <- Cmd.require "must select a note track"
        =<< ParseTitle.title_to_instrument <$> Ui.get_track_title track_id
    (_, common_config, midi_config) <- get_midi_config old_inst
    -- Replace the old instrument and reuse its addr.
    deallocate old_inst
    allocate new_inst $ UiConfig.Allocation new_qualified common_config
        (UiConfig.Midi midi_config)
    addr <- Cmd.require ("inst has no addr allocation: " <> pretty old_inst) $
        Seq.head $ Patch.config_addrs midi_config
    Ui.set_track_title track_id (ParseTitle.instrument_to_title new_inst)
    initialize_midi new_inst addr
    return ()

block_instruments :: BlockId -> Cmd.CmdL [Score.Instrument]
block_instruments block_id = do
    titles <- fmap (map Ui.track_title) (TrackTree.tracks_of block_id)
    return $ mapMaybe ParseTitle.title_to_instrument titles

-- | Synths default to writing to a device with their name.  You'll have to
-- map it to a real hardware WriteDevice in the 'Cmd.Cmd.write_device_map'.
device_of :: Score.Instrument -> Cmd.CmdL Midi.WriteDevice
device_of inst = do
    InstTypes.Qualified synth _ <-
        Cmd.inst_qualified <$> Cmd.get_instrument inst
    return $ Midi.write_device synth


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

save :: FilePath -> Cmd.CmdL ()
save = Save.save_allocations

load :: FilePath -> Cmd.CmdL ()
load = Save.load_allocations

-- | Send a CC MIDI message on the given device.  This is for synths that use
-- MIDI learn.
teach :: Text -> Midi.Channel -> Midi.Control -> Cmd.CmdL ()
teach dev chan cc = Cmd.midi (Midi.write_device dev) $
    Midi.ChannelMessage chan (Midi.ControlChange cc 1)

-- | This is parsed into a 'Inst.Qualified'.
type Qualified = Text

parse_qualified :: Cmd.M m => Qualified -> m InstTypes.Qualified
parse_qualified text
    | "/" `Text.isInfixOf` text = return $ InstTypes.parse_qualified text
    | otherwise =
        Cmd.throw $ "qualified inst name lacks a /: " <> showt text


-- * initialize

-- | Initialize all instruments that need it.
initialize_all :: Cmd.M m => m ()
initialize_all = mapM_ initialize_inst =<< allocated

-- | List allocated instruments that need initialization.
need_initialization :: Ui.M m => m Text
need_initialization = fmap Text.unlines . mapMaybeM show1 =<< allocated
    where
    show1 inst = justm (lookup_midi_config inst) $ \(_, _, config) -> do
        let inits = Patch.config_initialization config
        return $ if null inits then Nothing
            else Just $ pretty inst <> ": " <> pretty inits

-- | Initialize an instrument according to its 'Patch.config_initialization'.
initialize_inst :: Cmd.M m => Score.Instrument -> m ()
initialize_inst inst =
    whenJustM (lookup_midi_config inst) $ \(_, _, config) -> do
        let inits = Patch.config_initialization config
        when (Set.member Patch.Tuning inits) $
            initialize_realtime_tuning inst
        when (Set.member Patch.NrpnTuning inits) $
            initialize_nrpn_tuning inst
        when (Set.member Patch.Midi inits) $
            forM_ (Patch.config_addrs config) $ initialize_midi inst

-- | Send a MIDI tuning message to retune the synth to its 'Patch.Scale'.  Very
-- few synths support this, I only know of pianoteq.
initialize_realtime_tuning :: Cmd.M m => Score.Instrument -> m ()
initialize_realtime_tuning inst = do
    keys <- get_tuning_map inst
    (_, _, config) <- get_midi_config inst
    let msg = Midi.realtime_tuning keys
    mapM_ (flip Cmd.midi msg) (Seq.unique (map fst (Patch.config_addrs config)))

-- | Like 'initialize_realtime_tuning', except use 'Midi.nrpn_tuning'.
initialize_nrpn_tuning :: Cmd.M m => Score.Instrument -> m ()
initialize_nrpn_tuning inst = do
    keys <- get_tuning_map inst
    (_, _, config) <- get_midi_config inst
    forM_ (Seq.unique (Patch.config_addrs config)) $ \(dev, chan) ->
        mapM_ (Cmd.midi dev . Midi.ChannelMessage chan) (Midi.nrpn_tuning keys)

get_tuning_map :: Cmd.M m => Score.Instrument -> m [(Midi.Key, Midi.NoteNumber)]
get_tuning_map inst = get_scale inst >>= \case
    Nothing -> return []
    Just scale -> do
        attr_map <- Patch.patch_attribute_map . fst <$>
            Cmd.get_midi_instrument inst
        return $ map (second Pitch.nn_to_double) $
            Patch.scale_nns (Just attr_map) scale

initialize_midi :: Cmd.M m => Score.Instrument -> Patch.Addr -> m ()
initialize_midi inst addr = do
    (patch, _) <- Cmd.get_midi_instrument inst
    send_initialize (Patch.patch_initialize patch) inst addr

send_initialize :: Cmd.M m => Patch.InitializePatch -> Score.Instrument
    -> Patch.Addr -> m ()
send_initialize init inst (dev, chan) = case init of
    Patch.InitializeMidi msgs -> do
        Log.notice $ "sending midi init: " <> pretty msgs
        mapM_ (Cmd.midi dev . Midi.set_channel chan) msgs
    Patch.InitializeMessage msg ->
        -- Warn doesn't seem quite right for this, but the whole point is to
        -- show this message, so it should be emphasized.
        Log.warn $ "initialize instrument " <> pretty inst <> ": " <> msg
    Patch.NoInitialization -> return ()
