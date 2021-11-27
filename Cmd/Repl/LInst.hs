-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | REPL Cmds dealing with instruments and MIDI config.
module Cmd.Repl.LInst where
import           Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Lens as Lens
import qualified Util.Log as Log
import qualified Util.Maps as Maps
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Texts as Texts

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Info as Info
import qualified Cmd.Repl.Util as Util
import           Cmd.Repl.Util (Instrument)
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection

import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Parse as Parse
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.REnv as REnv
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Typecheck as Typecheck

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.Sc.Patch as Sc.Patch
import qualified Perform.Sc.Play as Sc.Play
import qualified Perform.Signal as Signal

import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- * get

lookup :: Instrument -> Cmd.CmdL (Maybe Cmd.ResolvedInstrument)
lookup = Cmd.lookup_instrument . Util.instrument

lookup_allocation :: Ui.M m => Instrument -> m (Maybe UiConfig.Allocation)
lookup_allocation inst = Ui.allocation (Util.instrument inst) <#> Ui.get

get_allocation :: Ui.M m => Instrument -> m UiConfig.Allocation
get_allocation = get_instrument_allocation . Util.instrument

-- | List all allocated instruments.
allocated :: Ui.M m => m [ScoreT.Instrument]
allocated = Ui.get_config $ Map.keys . (UiConfig.allocations_map #$)

-- | List all allocated instrument configs all purty-like.
list :: Cmd.M m => m Text
list = list_like ""

list_midi :: Cmd.M m => m [Instrument]
list_midi = do
    alloc_map <- Ui.config#UiConfig.allocations_map <#> Ui.get
    return
        [ ScoreT.instrument_name inst
        | (inst, alloc) <- Map.toAscList alloc_map
        , UiConfig.is_midi_allocation alloc
        ]


-- | Pretty print matching instruments:
--
-- > >pno - pianoteq/ loop1 [1..16]
-- > >syn - sampler/inst 音
list_like :: Cmd.M m => Text -> m Text
list_like pattern = do
    alloc_map <- Ui.config#UiConfig.allocations_map <#> Ui.get
    db <- Cmd.gets $ Cmd.config_instrument_db . Cmd.state_config
    let (names, allocs) = unzip $ Map.toAscList alloc_map
    return $ Text.unlines $ Texts.columns 1
        [ pretty_alloc name (inst_environ db alloc) alloc
        | (name, alloc) <- zip names allocs
        , matches name
        ]
    where
    matches inst = pattern `Text.isInfixOf` ScoreT.instrument_name inst
    inst_environ db =
        maybe mempty (Common.common_environ . Inst.inst_common)
        . flip Inst.lookup db . UiConfig.alloc_qualified

-- | On the environ, - means it was inherited from the Inst, + is from the
-- Allocation, and * means both had it and the Allocation overrode it.
pretty_alloc :: ScoreT.Instrument -> REnv.Environ
    -> UiConfig.Allocation -> [Text]
pretty_alloc inst inst_environ alloc =
    [ ShowVal.show_val inst
    , InstT.show_qualified (UiConfig.alloc_qualified alloc)
    , case UiConfig.alloc_backend alloc of
        UiConfig.Midi config -> Info.show_addrs (Patch.config_addrs config)
        UiConfig.Im -> "音"
        UiConfig.Sc -> "sc"
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
        | REnv.null environ && REnv.null inst_environ = ""
        | otherwise = pretty $ Pretty.formatMap $
            map (bimap Pretty.text Pretty.format . fmt) $
            Maps.pairs (REnv.to_map inst_environ) (REnv.to_map environ)
        where
        fmt (k, v) = case v of
            Seq.First v -> ("-" <> k, v)
            Seq.Second v -> ("+" <> k, v)
            Seq.Both _ v -> ("*" <> k, v)
    show_flags config
        | null flags = ""
        | otherwise = "{" <> Text.intercalate ", " flags <> "}"
        where
        flags = ["mute" | Common.config_mute config]
            ++ ["solo" | Common.config_solo config]
    show_midi_config = pretty_settings . Patch.config_settings
    show_controls msg controls
        | Map.null controls = ""
        | otherwise = msg <> pretty controls
    join = Text.unwords . filter (not . Text.null)

pretty_settings :: Patch.Settings -> Text
pretty_settings settings =
    Text.unwords $ filter (not . Text.null)
        [ if_changed Patch.config_flags pretty
        , if_changed Patch.config_scale $ (("("<>) . (<>")") . show_scale)
        , if_changed Patch.config_decay $ ("decay="<>) . pretty
        , if_changed Patch.config_pitch_bend_range $ ("pb="<>) . pretty
        , if_changed Patch.config_control_defaults $ ("controls="<>) . pretty
        ]
    where
    if_changed get fmt = maybe "" fmt (get settings)

show_scale :: Patch.Scale -> Text
show_scale scale = "scale " <> Patch.scale_name scale <> " "
    <> showt (length (Patch.scale_nns Nothing scale)) <> " keys"

-- | Instrument allocations.
allocations :: Ui.M m => m UiConfig.Allocations
allocations = Ui.config#UiConfig.allocations <#> Ui.get

-- * add and remove

-- | Midi.Channel is 0-based, but DAWs are 1-based, so so use 1-based for UI.
-- 'list' and ultimately 'Info.show_addrs' also display 1-based.
newtype Channel1 = Channel1 Int
    deriving (Eq, Show, Num)

to_chan :: Channel1 -> Midi.Channel
to_chan (Channel1 c)
    | 1 <= c && c <= 16 = fromIntegral (c - 1)
    | otherwise = error $ "MIDI channel out of range: " <> show c

-- | Allocate a new MIDI instrument.  For instance:
--
-- > LInst.add "m" "kontakt/mridangam-g" "loop1" [1]
--
-- This will create an instance of the @kontakt/mridangam@ instrument named
-- @>m@, and assign it to the MIDI WriteDevice @loop1@, with a single MIDI
-- channel 0 allocated.
add :: Instrument -> Qualified -> Text -> [Channel1] -> Cmd.CmdL ()
add inst qualified wdev chans =
    add_config inst qualified [((dev, to_chan chan), Nothing) | chan <- chans]
    where dev = Midi.write_device wdev

-- | Allocate the given channels for the instrument using its default device.
add_default :: Instrument -> Qualified -> [Channel1] -> Cmd.CmdL ()
add_default inst qualified chans = do
    dev <- device_of (Util.instrument inst)
    add_config inst qualified [((dev, to_chan chan), Nothing) | chan <- chans]

add_config :: Instrument -> Qualified -> [(Patch.Addr, Maybe Patch.Voices)]
    -> Cmd.CmdL ()
add_config inst qualified allocs = do
    qualified <- parse_qualified qualified
    allocate (Util.instrument inst) $ UiConfig.allocation qualified $
        UiConfig.Midi $ Patch.config allocs

-- | Allocate a new Im instrument.
add_im :: Instrument -> Qualified -> Cmd.CmdL ()
add_im inst qualified = do
    qualified <- parse_qualified qualified
    allocate (Util.instrument inst) $
        UiConfig.allocation qualified UiConfig.Im

add_sc :: Instrument -> Text -> Cmd.CmdT IO ()
add_sc inst patch = allocate (Util.instrument inst) $
    UiConfig.allocation (InstT.Qualified "sc" patch) UiConfig.Sc

-- | Add the play-cache instrument.  This is a dummy instrument used to
-- trigger the play-cache vst.  It's emitted automatically if there are im
-- instruments, but needs a channel allocation.
add_play_cache :: Text -> Channel1 -> Cmd.CmdL ()
add_play_cache wdev chan =
    allocate (Util.instrument "play-cache") $
        UiConfig.allocation UiConfig.play_cache (UiConfig.Midi config)
    where
    config = Patch.config [((Midi.write_device wdev, to_chan chan), Nothing)]

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
allocate :: Cmd.M m => ScoreT.Instrument -> UiConfig.Allocation -> m ()
allocate score_inst alloc = do
    inst <- Cmd.get_alloc_qualified alloc
    allocs <- Ui.config#UiConfig.allocations <#> Ui.get
    allocs <- Cmd.require_right id $
        UiConfig.allocate (Inst.inst_backend inst) score_inst alloc allocs
    Ui.modify_config $ UiConfig.allocations #= allocs

-- | Remove an instrument allocation.
remove :: Instrument -> Cmd.CmdL ()
remove = deallocate . Util.instrument

deallocate :: Cmd.M m => ScoreT.Instrument -> m ()
deallocate inst = Ui.modify_config $ UiConfig.allocations_map %= Map.delete inst

-- | Merge the given configs into the existing ones.  This also merges
-- 'Patch.patch_defaults' into 'Patch.config_settings'.  This way functions
-- that create Allocations don't have to find the relevant Patch.
merge :: Cmd.M m => Bool -> UiConfig.Allocations -> m ()
merge override (UiConfig.Allocations alloc_map) = do
    let (names, allocs) = unzip (Map.toList alloc_map)
    insts <- mapM Cmd.get_alloc_qualified allocs
    existing <- Ui.get_config (UiConfig.allocations #$)
    let errors = mapMaybe (verify existing) (zip3 names allocs insts)
    unless (null errors) $
        Cmd.throw $ "merged allocations: " <> Text.intercalate "\n" errors
    let new_allocs = UiConfig.Allocations (Map.fromList (zip names allocs))
    Ui.modify_config $ UiConfig.allocations
        %= if override then (new_allocs<>) else (<>new_allocs)
    where
    verify allocs (name, alloc, inst) =
        UiConfig.verify_allocation allocs (Inst.inst_backend inst) name alloc

replace :: Cmd.M m => UiConfig.Allocations -> m ()
replace allocs = do
    Ui.modify_config $ UiConfig.allocations #= mempty
    merge True allocs

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
    -> (UiConfig.Allocation -> Map ScoreT.Instrument UiConfig.Allocation
        -> Map ScoreT.Instrument UiConfig.Allocation)
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
add_environ :: (REnv.ToVal a, Ui.M m) => Env.Key -> a -> Instrument -> m ()
add_environ name val = modify_common_config_ $ Common.add_cenviron name val

-- | Clear the instrument config's environ.  The instrument's built-in environ
-- from 'Patch.patch_environ' is still present.
clear_environ :: Ui.M m => Instrument -> m ()
clear_environ = modify_common_config_ $ Common.cenviron #= mempty

-- ** Midi.Patch.Config

set_addr :: Ui.M m => Text -> [Channel1] -> Instrument -> m ()
set_addr wdev chans = modify_midi_config_ $
    Patch.allocation #= [((dev, to_chan chan), Nothing) | chan <- chans]
    where dev = Midi.write_device wdev

set_chans :: Ui.M m => [Channel1] -> Instrument -> m ()
set_chans chans = modify_midi_config_ $ \config ->
    case Patch.config_allocation config of
        ((dev, _), _) : _ -> Patch.allocation
            #= [((dev, to_chan chan), Nothing) | chan <- chans] $
                config
        [] -> config

set_controls :: Ui.M m => [(ScoreT.Control, Signal.Y)] -> Instrument -> m ()
set_controls controls = modify_common_config_ $
    Common.controls #= Map.fromList controls

set_control :: Ui.M m => ScoreT.Control -> Maybe Signal.Y -> Instrument -> m ()
set_control control val = modify_common_config_ $
    Common.controls # Lens.map control #= val

set_tuning_scale :: Ui.M m => Text -> Patch.Scale -> Instrument -> m ()
set_tuning_scale tuning scale inst = do
    set_scale scale inst
    add_environ EnvKey.tuning tuning inst

set_control_defaults :: Ui.M m => [(ScoreT.Control, Signal.Y)] -> Instrument
    -> m ()
set_control_defaults controls = modify_midi_config_ $
    Patch.settings#Patch.control_defaults #= Just (Map.fromList controls)

-- ** Midi.Patch.Config settings

get_scale :: Cmd.M m => ScoreT.Instrument -> m (Maybe Patch.Scale)
get_scale inst =
    (Patch.settings#Patch.scale #$) . snd <$> Cmd.get_midi_instrument inst

set_scale :: Ui.M m => Patch.Scale -> Instrument -> m ()
set_scale scale = modify_midi_config_ $ Patch.settings#Patch.scale #= Just scale

copy_scale :: Cmd.M m => Instrument -> Instrument -> m ()
copy_scale from to = do
    scale <- Cmd.require "no scale" =<< get_scale (Util.instrument from)
    set_scale scale to

pressure :: Ui.M m => Instrument -> m ()
pressure = add_flag Patch.Pressure

add_flag :: Ui.M m => Patch.Flag -> Instrument -> m ()
add_flag flag = modify_midi_config_ $
    Patch.settings#Patch.flags %= Just . Patch.add_flag flag . fromMaybe mempty

remove_flag :: Ui.M m => Patch.Flag -> Instrument -> m ()
remove_flag flag = modify_midi_config_ $
    Patch.settings#Patch.flags
        %= Just . Patch.remove_flag flag . fromMaybe mempty

-- | Reset all settings back to instrument defaults.
reset_settings :: Ui.M m => Instrument -> m ()
reset_settings = modify_midi_config_ $ Patch.settings #= mempty

reset_flags :: Ui.M m => Instrument -> m ()
reset_flags = modify_midi_config_ $
    Patch.settings#Patch.flags #= Nothing

set_decay :: Ui.M m => Maybe RealTime -> Instrument -> m ()
set_decay decay = modify_midi_config_ $ Patch.settings#Patch.decay #= decay

set_pb_range :: Ui.M m => Maybe Patch.PbRange -> Instrument -> m ()
set_pb_range range =
    modify_midi_config_ $ Patch.settings#Patch.pitch_bend_range #= range

-- * util

get_midi_config :: Ui.M m => ScoreT.Instrument
    -> m (InstT.Qualified, Common.Config, Patch.Config)
get_midi_config inst =
    Ui.require ("not a midi instrument: " <> pretty inst)
        =<< lookup_midi_config inst

lookup_midi_config :: Ui.M m => ScoreT.Instrument
    -> m (Maybe (InstT.Qualified, Common.Config, Patch.Config))
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
    Ui.modify_config $ UiConfig.allocations_map %= Map.insert inst new
    return result

modify_midi_config_ :: Ui.M m => (Patch.Config -> Patch.Config) -> Instrument
    -> m ()
modify_midi_config_ modify =
    modify_config $ \common midi -> ((common, modify midi), ())

modify_common_config :: Ui.M m => (Common.Config -> (Common.Config, a))
    -> Instrument -> m a
modify_common_config modify inst_ = do
    let inst = Util.instrument inst_
    alloc <- get_instrument_allocation inst
    let (config, result) = modify (UiConfig.alloc_config alloc)
        new = alloc { UiConfig.alloc_config = config }
    Ui.modify_config $ UiConfig.allocations_map %= Map.insert inst new
    return result

modify_common_config_ :: Ui.M m => (Common.Config -> Common.Config)
    -> Instrument -> m ()
modify_common_config_ modify =
    modify_common_config $ \config -> (modify config, ())

get_instrument_allocation :: Ui.M m => ScoreT.Instrument
    -> m UiConfig.Allocation
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

-- * find

find :: Cmd.M m => Text -> m [Text]
find substr = do
    db <- Cmd.gets $ Cmd.config_instrument_db . Cmd.state_config
    return $ filter (substr `Text.isInfixOf`)
        [ InstT.show_qualified $ InstT.Qualified synth inst
        | (synth, s) <- Inst.synths db
        , inst <- Map.keys $ Inst.synth_insts s
        ]


-- * change_instrument

-- | Replace the instrument in the current track with the given one, and
-- 'initialize_midi' it.  This is intended for hardware synths which need a
-- program change or sysex.  It's called by "Instrument.Browser".
set_instrument :: Cmd.M m => Qualified -> m ()
set_instrument new_qualified = do
    new_qualified <- parse_qualified new_qualified
    track_id <- snd <$> Selection.event_track
    inst <- Cmd.require "must select a note track"
        =<< ParseTitle.title_to_instrument <$> Ui.get_track_title track_id
    (_, common_config, midi_config) <- get_midi_config inst
    -- Replace the old instrument and reuse its addr.
    deallocate inst
    allocate inst $ UiConfig.Allocation new_qualified common_config
        (UiConfig.Midi midi_config)
    initialize_inst inst

block_instruments :: BlockId -> Cmd.CmdL [ScoreT.Instrument]
block_instruments block_id = do
    titles <- fmap (map Ui.track_title) (TrackTree.tracks_of block_id)
    return $ mapMaybe ParseTitle.title_to_instrument titles

-- | Synths default to writing to a device with their name.  You'll have to
-- map it to a real hardware WriteDevice in the 'Cmd.Cmd.write_device_map'.
device_of :: ScoreT.Instrument -> Cmd.CmdL Midi.WriteDevice
device_of inst = do
    InstT.Qualified synth _ <- Cmd.inst_qualified <$> Cmd.get_instrument inst
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
load fname = do
    allocs <- Save.load_allocations fname
    Ui.modify_config $ UiConfig.allocations #= allocs

-- | Load and merge instruments.  If there are name collisions, the
-- already-allocated instrument wins.
load_merge :: FilePath -> Cmd.CmdL ()
load_merge fname = merge False =<< Save.load_allocations fname

-- | Send a CC MIDI message on the given device and channel.  This is for
-- synths that use MIDI learn.
teach :: Text -> Channel1 -> Midi.Control -> Cmd.CmdL ()
teach dev chan cc = Cmd.midi (Midi.write_device dev) $
    Midi.ChannelMessage (to_chan chan) (Midi.ControlChange cc 1)

-- | This is parsed into a 'Inst.Qualified'.
type Qualified = Text

parse_qualified :: Cmd.M m => Qualified -> m InstT.Qualified
parse_qualified text
    | "/" `Text.isInfixOf` text = return $ InstT.parse_qualified text
    | otherwise =
        Cmd.throw $ "qualified inst name lacks a /: " <> showt text


-- * initialize

-- | Initialize all instruments that need it.
initialize_all :: Cmd.CmdT IO ()
initialize_all = do
    mapM_ initialize_inst =<< allocated
    sc_initialize

-- | List allocated instruments that need initialization.
need_initialization :: Ui.M m => m Text
need_initialization = fmap Text.unlines . mapMaybeM show1 =<< allocated
    where
    show1 inst = do
        inits <- init_flags inst
        return $ if null inits then Nothing
            else Just $ pretty inst <> ": " <> pretty inits

init_flags :: Ui.M m => ScoreT.Instrument -> m (Set Patch.Initialization)
init_flags inst = lookup_midi_config inst >>= return . \case
    Nothing -> mempty
    Just (_, _, config) -> Patch.config_initialization config

-- | Initialize an instrument according to its 'Patch.config_initialization'.
initialize_inst :: Cmd.M m => ScoreT.Instrument -> m ()
initialize_inst inst =
    whenJustM (lookup_midi_config inst) $ \(_, _, config) -> do
        let inits = Patch.config_initialization config
        when (Set.member Patch.Tuning inits) $
            initialize_realtime_tuning inst
        when (Set.member Patch.NrpnTuning inits) $
            initialize_nrpn_tuning inst
        (patch, _) <- Cmd.get_midi_instrument inst
        forM_ (Patch.config_addrs config) $ \addr ->
            send_midi_initialize inst addr (Patch.patch_initialize patch)

-- | Send a MIDI tuning message to retune the synth to its 'Patch.Scale'.  Very
-- few synths support this, I only know of pianoteq.
initialize_realtime_tuning :: Cmd.M m => ScoreT.Instrument -> m ()
initialize_realtime_tuning inst = do
    keys <- get_tuning_map inst
    (_, _, config) <- get_midi_config inst
    let msg = Midi.realtime_tuning keys
    mapM_ (flip Cmd.midi msg) (Seq.unique (map fst (Patch.config_addrs config)))

-- | Like 'initialize_realtime_tuning', except use 'Midi.nrpn_tuning'.
initialize_nrpn_tuning :: Cmd.M m => ScoreT.Instrument -> m ()
initialize_nrpn_tuning inst = do
    keys <- get_tuning_map inst
    (_, _, config) <- get_midi_config inst
    forM_ (Seq.unique (Patch.config_addrs config)) $ \(dev, chan) ->
        mapM_ (Cmd.midi dev . Midi.ChannelMessage chan) (Midi.nrpn_tuning keys)

get_tuning_map :: Cmd.M m => ScoreT.Instrument
    -> m [(Midi.Key, Midi.NoteNumber)]
get_tuning_map inst = get_scale inst >>= \case
    Nothing -> return []
    Just scale -> do
        attr_map <- Patch.patch_attribute_map . fst <$>
            Cmd.get_midi_instrument inst
        return $ map (second Pitch.nn_to_double) $
            Patch.scale_nns (Just attr_map) scale

initialize_midi :: Cmd.M m => ScoreT.Instrument -> Patch.Addr -> m ()
initialize_midi inst addr = do
    (patch, _) <- Cmd.get_midi_instrument inst
    send_midi_initialize inst addr (Patch.patch_initialize patch)

send_midi_initialize :: Cmd.M m => ScoreT.Instrument -> Patch.Addr
    -> Patch.InitializePatch -> m ()
send_midi_initialize inst (dev, chan) = \case
    Patch.InitializeMidi msgs -> do
        Log.notice $ "sending midi init: " <> pretty msgs
        mapM_ (Cmd.midi dev . Midi.set_channel chan) msgs
    Patch.InitializeMessage msg ->
        -- Warn doesn't seem quite right for this, but the whole point is to
        -- show this message, so it should be emphasized.
        Log.warn $ "initialize instrument " <> pretty inst <> ": " <> msg
    Patch.NoInitialization -> return ()

sc_initialize :: Cmd.CmdT IO ()
sc_initialize = do
    insts <- Ui.get_config $ Map.keys . UiConfig.unallocations
        . UiConfig.config_allocations
    insts <- mapM Cmd.get_instrument insts
    sc_initialize_patches $ mapMaybe Cmd.sc_patch insts

sc_initialize_patches :: [Sc.Patch.Patch] -> Cmd.CmdT IO ()
sc_initialize_patches [] = return ()
sc_initialize_patches patches = do
    -- scsynth has a /d_free, but seems to have no way to query what is
    -- actually loaded, so it's sort of useless, because you must control the
    -- scsynth lifecycle to manually track what it loaded, and at that point
    -- you may as well restart it.
    -- TODO if configured with the path to scsynth, I could start it
    -- automatically.  In that case, I may want to send a /quit on exit.
    msg <- Cmd.require_right ("can't initialize sc patches: "<>)
        =<< liftIO Sc.Play.version
    -- TODO: when called from REPL, log msgs are collected together, so
    -- "waiting for" is not very interesting.
    Log.notice $ "found scsynth: " <> msg
    Log.notice $ "loading patches: "
        <> Text.unwords (map (Texts.toText . Sc.Patch.name) patches)
    liftIO $ mapM_ Sc.Play.initialize_patch patches
    liftIO Sc.Play.sync
    Log.notice "waiting for sync"
