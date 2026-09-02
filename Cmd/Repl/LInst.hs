-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | REPL Cmds dealing with instruments and MIDI config.
module Cmd.Repl.LInst where
import           Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Maps as Maps
import qualified Util.Pretty as Pretty
import qualified Util.Texts as Texts

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Info as Info
import qualified Cmd.Perf as Perf
import qualified Cmd.Repl.Util as Util
import           Cmd.Repl.Util (Instrument)
import qualified Cmd.Selection as Selection

import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Parse as Parse
import qualified Derive.Parse.Instruments as Instruments
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.REnv as REnv
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Typecheck as Typecheck

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstDoc as InstDoc
import qualified Instrument.InstT as InstT

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.Sc.Patch as Sc.Patch
import qualified Perform.Sc.Play as Sc.Play

import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global


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
        UiConfig.Dummy {} -> "(dummy)"
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
            Lists.First v -> ("-" <> k, v)
            Lists.Second v -> ("+" <> k, v)
            Lists.Both _ v -> ("*" <> k, v)
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

doc :: Cmd.M m => m Text
doc = instrument_doc =<< Cmd.require "no instrument" =<< sel_instrument

sel_instrument :: Cmd.M m => m (Maybe ScoreT.Instrument)
sel_instrument = justm environ $ \env ->
    pure $ Env.maybe_val EnvKey.instrument env

environ :: Cmd.M m => m (Maybe Env.Environ)
environ = Perf.lookup_environ =<< Selection.track

instrument_doc :: Cmd.M m => ScoreT.Instrument -> m Text
instrument_doc inst_name = do
    alloc <- get_instrument_allocation inst_name
    let qualified = UiConfig.alloc_qualified alloc
    inst <- Cmd.get_qualified qualified
    pure $ InstDoc.info_of qualified synth_doc inst tags
    where
    tags = []
    synth_doc = ""

-- * add and remove

-- | Midi.Channel is 0-based, but DAWs are 1-based, so so use 1-based for UI.
-- 'list' and ultimately 'Info.show_addrs' also display 1-based.
newtype Channel1 = Channel1 Int
    deriving (Eq, Show, Num, Enum)

to_chan :: Channel1 -> Midi.Channel
to_chan (Channel1 c)
    | 1 <= c && c <= 16 = fromIntegral (c - 1)
    | otherwise = error $ "MIDI channel out of range: " <> show c

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

-- | Look up the patch and send MIDI patch initialization, inferring
-- channels from selected note track.  This is intended for hardware synths
-- which need a program change or sysex.  It's called by "Instrument.Browser",
-- and makes it into a kind of sysex librarian.
set_instrument :: Cmd.M m => Qualified -> m Text
set_instrument qualified = do
    qualified <- parse_qualified qualified
    inst <- Cmd.get_qualified qualified
    patch <- Cmd.require "only for midi" $ case Inst.inst_backend inst of
        Inst.Midi patch -> Just patch
        _ -> Nothing
    -- I need a MIDI addr to reinit, infer it from the selected track.
    track_id <- snd <$> Selection.event_track
    inst <- Cmd.require "must select a note track"
        =<< ParseTitle.title_to_instrument <$> Ui.get_track_title track_id
    (_, _, config) <- get_midi_config inst
    backend <- Cmd.require_right id $ Instruments.to_backend config
    -- Previously I would reallocate the new one to replace, but since
    -- switching to ky config, the most I can do is print it out for
    -- copy paste.
    let alloc = Instruments.un_alloc_line $ Instruments.Allocation
            { alloc_name = "name"
            , alloc_qualified = qualified
            , alloc_config = Instruments.empty_config
            , alloc_backend = backend
            }
    Text.unlines . (++alloc) <$> mapMaybeM
        (\addr -> send_midi_initialize addr (Patch.patch_initialize patch))
        (Patch.config_addrs config)

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
initialize_all :: Cmd.CmdT IO Text
initialize_all = do
    msgs <- concatMapM initialize_inst =<< allocated
    sc_initialize
    pure $ Text.unlines msgs

-- | List allocated instruments that need initialization.
need_initialization :: Ui.M m => m Text
need_initialization = fmap Text.unlines . mapMaybeM show1 =<< allocated
    where
    show1 inst = fmap (\init -> pretty inst <> ": " <> pretty init) <$>
        inst_initialization inst

inst_initialization :: Ui.M m => ScoreT.Instrument
    -> m (Maybe Patch.Initialization)
inst_initialization inst = lookup_midi_config inst >>= return . \case
    Nothing -> Nothing
    Just (_, _, config) -> Patch.config_initialization config

-- | Initialize an instrument according to its 'Patch.config_initialization'.
initialize_inst :: Cmd.M m => ScoreT.Instrument -> m [Text]
initialize_inst inst = lookup_midi_config inst >>= \case
    Nothing -> pure []
    Just (_, _, config) -> do
        whenJust (Patch.config_initialization config) $ \case
            Patch.Tuning -> initialize_realtime_tuning inst
            Patch.NrpnTuning -> initialize_nrpn_tuning inst
        (patch, _) <- Cmd.get_midi_instrument inst
        mapMaybeM
            (\addr -> send_midi_initialize addr (Patch.patch_initialize patch))
            (Patch.config_addrs config)

-- | Send a MIDI tuning message to retune the synth to its 'Patch.Scale'.  Very
-- few synths support this, I only know of pianoteq.
initialize_realtime_tuning :: Cmd.M m => ScoreT.Instrument -> m ()
initialize_realtime_tuning inst = do
    keys <- get_tuning_map inst
    (_, _, config) <- get_midi_config inst
    let msg = Midi.realtime_tuning keys
    mapM_ (flip Cmd.midi msg)
        (Lists.unique (map fst (Patch.config_addrs config)))

-- | Like 'initialize_realtime_tuning', except use 'Midi.nrpn_tuning'.
initialize_nrpn_tuning :: Cmd.M m => ScoreT.Instrument -> m ()
initialize_nrpn_tuning inst = do
    keys <- get_tuning_map inst
    (_, _, config) <- get_midi_config inst
    forM_ (Lists.unique (Patch.config_addrs config)) $ \(dev, chan) ->
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

get_scale :: Cmd.M m => ScoreT.Instrument -> m (Maybe Patch.Scale)
get_scale inst =
    (Patch.settings#Patch.scale #$) . snd <$> Cmd.get_midi_instrument inst

initialize_midi :: Cmd.M m => ScoreT.Instrument -> Patch.Addr -> m (Maybe Text)
initialize_midi inst addr = do
    (patch, _) <- Cmd.get_midi_instrument inst
    send_midi_initialize addr (Patch.patch_initialize patch)

send_midi_initialize :: Cmd.M m => Patch.Addr -> Patch.InitializePatch
    -> m (Maybe Text)
send_midi_initialize (dev, chan) = \case
    Patch.InitializeMidi msgs -> do
        mapM_ (Cmd.midi dev . Midi.set_channel chan) msgs
        pure $ Just $ "sending midi init: " <> pretty msgs
    Patch.InitializeMessage msg -> pure $ Just $ "initialize: " <> msg
    Patch.NoInitialization -> pure Nothing

sc_initialize :: Cmd.CmdT IO ()
sc_initialize = do
    insts <- Ui.get_config $ Map.keys . UiConfig.unallocations
        . UiConfig.config_allocations
    insts <- mapM Cmd.get_instrument insts
    sc_initialize_patches $ mapMaybe Cmd.sc_patch insts

sc_initialize_patches :: [Sc.Patch.Patch] -> Cmd.CmdT IO ()
sc_initialize_patches [] = pure ()
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
