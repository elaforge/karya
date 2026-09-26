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
lookup = Cmd.lookupInstrument . Util.instrument

lookupAllocation :: Ui.M m => Instrument -> m (Maybe UiConfig.Allocation)
lookupAllocation inst = Ui.allocation (Util.instrument inst) <#> Ui.get

getAllocation :: Ui.M m => Instrument -> m UiConfig.Allocation
getAllocation = getInstrumentAllocation . Util.instrument

-- | List all allocated instruments.
allocated :: Ui.M m => m [ScoreT.Instrument]
allocated = Ui.getConfig $ Map.keys . (UiConfig.allocationsMap #$)

-- | List all allocated instrument configs all purty-like.
list :: Cmd.M m => m Text
list = listLike ""

listMidi :: Cmd.M m => m [Instrument]
listMidi = do
    allocMap <- Ui.config#UiConfig.allocationsMap <#> Ui.get
    pure
        [ ScoreT.instrumentName inst
        | (inst, alloc) <- Map.toAscList allocMap
        , UiConfig.isMidiAllocation alloc
        ]


-- | Pretty print matching instruments:
--
-- > >pno - pianoteq/ loop1 [1..16]
-- > >syn - sampler/inst 音
listLike :: Cmd.M m => Text -> m Text
listLike pattern = do
    allocMap <- Ui.config#UiConfig.allocationsMap <#> Ui.get
    lookupInst <- Cmd.getLookupInst <$> Cmd.get
    let instEnv = maybe mempty (Common.commonEnviron . Inst.instCommon)
            . lookupInst . UiConfig.allocQualified
    -- TODO show inherited Midi.Patch and common_flags?
    let (names, allocs) = unzip $ Map.toAscList allocMap
    pure $ Text.unlines $ Texts.columns 1
        [ prettyAlloc name (instEnv alloc) alloc
        | (name, alloc) <- zip names allocs
        , matches name
        ]
    where
    matches inst = pattern `Text.isInfixOf` ScoreT.instrumentName inst

-- | On the environ, - means it was inherited from the Inst, + is from the
-- Allocation, and * means both had it and the Allocation overrode it.
prettyAlloc :: ScoreT.Instrument -> REnv.Environ
    -> UiConfig.Allocation -> [Text]
prettyAlloc inst instEnviron alloc =
    [ ShowVal.showVal inst
    , InstT.showQualified (UiConfig.allocQualified alloc)
    , case UiConfig.allocBackend alloc of
        UiConfig.Midi config -> Info.showAddrs (Patch.configAddrs config)
        UiConfig.Im -> "音"
        UiConfig.Sc -> "sc"
        UiConfig.Dummy {} -> "(dummy)"
    , showCommonConfig (UiConfig.allocConfig alloc)
    ]
    where
    showCommonConfig config = join
        [ showEnviron (Common.configEnviron config)
        , showControls "" (Common.configControls config)
        ]
    showEnviron environ
        | REnv.null environ && REnv.null instEnviron = ""
        | otherwise = pretty $ Pretty.formatMap $
            map (bimap Pretty.text Pretty.format . fmt) $
            Maps.pairs (REnv.toMap instEnviron) (REnv.toMap environ)
        where
        fmt (k, v) = case v of
            Lists.First v -> ("-" <> k, v)
            Lists.Second v -> ("+" <> k, v)
            Lists.Both _ v -> ("*" <> k, v)
    showControls msg controls
        | Map.null controls = ""
        | otherwise = msg <> pretty controls
    join = Text.unwords . filter (not . Text.null)

-- | Instrument allocations.
allocations :: Ui.M m => m UiConfig.Allocations
allocations = Ui.config#UiConfig.allocations <#> Ui.get

doc :: Cmd.M m => m Text
doc = instrumentDoc =<< Cmd.require "no instrument" =<< selInstrument

selInstrument :: Cmd.M m => m (Maybe ScoreT.Instrument)
selInstrument = justm environ $ \env ->
    pure $ Env.maybeVal EnvKey.instrument env

environ :: Cmd.M m => m (Maybe Env.Environ)
environ = Perf.lookupEnviron =<< Selection.track

instrumentDoc :: Cmd.M m => ScoreT.Instrument -> m Text
instrumentDoc instName = do
    alloc <- getInstrumentAllocation instName
    let qualified = UiConfig.allocQualified alloc
    inst <- Cmd.getQualified qualified
    pure $ InstDoc.infoOf qualified synthDoc inst tags
    where
    tags = []
    synthDoc = ""

-- * add and remove

-- | Midi.Channel is 0-based, but DAWs are 1-based, so so use 1-based for UI.
-- 'list' and ultimately 'Info.show_addrs' also display 1-based.
newtype Channel1 = Channel1 Int
    deriving (Eq, Show, Num, Enum)

toChan :: Channel1 -> Midi.Channel
toChan (Channel1 c)
    | 1 <= c && c <= 16 = fromIntegral (c - 1)
    | otherwise = error $ "MIDI channel out of range: " <> show c

-- * util

getMidiConfig :: Ui.M m => ScoreT.Instrument
    -> m (InstT.Qualified, Common.Config, Patch.Config)
getMidiConfig inst =
    Ui.require ("not a midi instrument: " <> pretty inst)
        =<< lookupMidiConfig inst

lookupMidiConfig :: Ui.M m => ScoreT.Instrument
    -> m (Maybe (InstT.Qualified, Common.Config, Patch.Config))
lookupMidiConfig inst = do
    UiConfig.Allocation qualified config backend
        <- getInstrumentAllocation inst
    pure $ case backend of
        UiConfig.Midi midiConfig -> Just (qualified, config, midiConfig)
        _ -> Nothing

getInstrumentAllocation :: Ui.M m => ScoreT.Instrument
    -> m UiConfig.Allocation
getInstrumentAllocation inst =
    Ui.require ("no allocation for " <> pretty inst)
        =<< Ui.allocation inst <#> Ui.get

-- * Cmd.EditState

setAttrs :: Cmd.M m => Text -> Instrument -> m ()
setAttrs attrs inst_ = do
    let inst = Util.instrument inst_
    Cmd.getInstrument inst -- ensure that it exists
    val <- Cmd.requireRight ("parsing attrs: " <>) $
        Parse.parseVal ("+" <> attrs)
    attrs <- Cmd.requireRight id $ Typecheck.typecheckSimple val
    Cmd.setInstrumentAttributes inst attrs

-- * find

find :: Cmd.M m => Text -> m [Text]
find substr = do
    db <- Cmd.gets $ Cmd.configInstrumentDb . Cmd.stateConfig
    pure $ filter (substr `Text.isInfixOf`)
        [ InstT.showQualified $ InstT.Qualified synth inst
        | (synth, s) <- Inst.synths db
        , inst <- Map.keys $ Inst.synthInsts s
        ]

-- * change_instrument

-- | Look up the patch and send MIDI patch initialization, inferring
-- channels from selected note track.  This is intended for hardware synths
-- which need a program change or sysex.  It's called by "Instrument.Browser",
-- and makes it into a kind of sysex librarian.
setInstrument :: Cmd.M m => Qualified -> m Text
setInstrument qualified = do
    qualified <- parseQualified qualified
    inst <- Cmd.getQualified qualified
    patch <- Cmd.require "only for midi" $ case Inst.instBackend inst of
        Inst.Midi patch -> Just patch
        _ -> Nothing
    -- I need a MIDI addr to reinit, infer it from the selected track.
    trackId <- snd <$> Selection.eventTrack
    inst <- Cmd.require "must select a note track"
        =<< ParseTitle.titleToInstrument <$> Ui.getTrackTitle trackId
    (_, _, config) <- getMidiConfig inst
    backend <- Cmd.requireRight id $ Instruments.toBackend config
    -- Previously I would reallocate the new one to replace, but since
    -- switching to ky config, the most I can do is print it out for
    -- copy paste.
    let alloc = Instruments.unAllocLine $ Instruments.Allocation
            { allocName = "name"
            , allocQualified = qualified
            , allocConfig = Instruments.emptyConfig
            , allocBackend = backend
            }
    Text.unlines . (++alloc) <$> mapMaybeM
        (\addr -> sendMidiInitialize addr (Patch.patchInitialize patch))
        (Patch.configAddrs config)

-- * midi interface

-- | Every read device on the system, along with any aliases it may have.
readDevices :: Cmd.CmdL [(Midi.ReadDevice, [Midi.ReadDevice])]
readDevices = runInterface Interface.readDevices

-- | Every write device on the system, along with any aliases it may have.
writeDevices :: Cmd.CmdL [(Midi.WriteDevice, [Midi.WriteDevice])]
writeDevices = runInterface Interface.writeDevices

connectReadDevice :: Midi.ReadDevice -> Cmd.CmdL Bool
connectReadDevice rdev =
    runInterface (flip Interface.connectReadDevice rdev)

disconnectReadDevice :: Midi.ReadDevice -> Cmd.CmdL Bool
disconnectReadDevice rdev =
    runInterface (flip Interface.disconnectReadDevice rdev)

runInterface :: (Interface.Interface -> IO a) -> Cmd.CmdL a
runInterface op = do
    interface <- Cmd.gets (Cmd.configMidiInterface . Cmd.stateConfig)
    liftIO (op interface)

-- * misc

-- | Send a CC MIDI message on the given device and channel.  This is for
-- synths that use MIDI learn.
teach :: Text -> Channel1 -> Midi.Control -> Cmd.CmdL ()
teach dev chan cc = Cmd.midi (Midi.writeDevice dev) $
    Midi.ChannelMessage (toChan chan) (Midi.ControlChange cc 1)

-- | This is parsed into a 'Inst.Qualified'.
type Qualified = Text

parseQualified :: Cmd.M m => Qualified -> m InstT.Qualified
parseQualified text
    | "/" `Text.isInfixOf` text = pure $ InstT.parseQualified text
    | otherwise =
        Cmd.throw $ "qualified inst name lacks a /: " <> showt text

-- * initialize

-- | Initialize all instruments that need it.
initializeAll :: Cmd.CmdT IO Text
initializeAll = do
    msgs <- concatMapM initializeInst =<< allocated
    scInitialize
    pure $ Text.unlines msgs

-- | List allocated instruments that need initialization.
needInitialization :: Ui.M m => m Text
needInitialization = fmap Text.unlines . mapMaybeM show1 =<< allocated
    where
    show1 inst = fmap (\init -> pretty inst <> ": " <> pretty init) <$>
        instInitialization inst

instInitialization :: Ui.M m => ScoreT.Instrument
    -> m (Maybe Patch.Initialization)
instInitialization inst = lookupMidiConfig inst >>= pure . \case
    Nothing -> Nothing
    Just (_, _, config) -> Patch.configInitialization config

-- | Initialize an instrument according to its 'Patch.config_initialization'.
initializeInst :: Cmd.M m => ScoreT.Instrument -> m [Text]
initializeInst inst = lookupMidiConfig inst >>= \case
    Nothing -> pure []
    Just (_, _, config) -> do
        whenJust (Patch.configInitialization config) $ \case
            Patch.Tuning -> initializeRealtimeTuning inst
            Patch.NrpnTuning -> initializeNrpnTuning inst
        (patch, _) <- Cmd.getMidiInstrument inst
        mapMaybeM
            (\addr -> sendMidiInitialize addr (Patch.patchInitialize patch))
            (Patch.configAddrs config)

-- | Send a MIDI tuning message to retune the synth to its 'Patch.Scale'.  Very
-- few synths support this, I only know of pianoteq.
initializeRealtimeTuning :: Cmd.M m => ScoreT.Instrument -> m ()
initializeRealtimeTuning inst = do
    keys <- getTuningMap inst
    (_, _, config) <- getMidiConfig inst
    let msg = Midi.realtimeTuning keys
    mapM_ (flip Cmd.midi msg)
        (Lists.unique (map fst (Patch.configAddrs config)))

-- | Like 'initialize_realtime_tuning', except use 'Midi.nrpn_tuning'.
initializeNrpnTuning :: Cmd.M m => ScoreT.Instrument -> m ()
initializeNrpnTuning inst = do
    keys <- getTuningMap inst
    (_, _, config) <- getMidiConfig inst
    forM_ (Lists.unique (Patch.configAddrs config)) $ \(dev, chan) ->
        mapM_ (Cmd.midi dev . Midi.ChannelMessage chan) (Midi.nrpnTuning keys)

getTuningMap :: Cmd.M m => ScoreT.Instrument
    -> m [(Midi.Key, Midi.NoteNumber)]
getTuningMap inst = getScale inst >>= \case
    Nothing -> pure []
    Just scale -> do
        attrMap <- Patch.patchAttributeMap . fst <$>
            Cmd.getMidiInstrument inst
        pure $ map (second Pitch.nnToDouble) $
            Patch.scaleNns (Just attrMap) scale

getScale :: Cmd.M m => ScoreT.Instrument -> m (Maybe Patch.Scale)
getScale inst =
    (Patch.settings#Patch.scale #$) . snd <$> Cmd.getMidiInstrument inst

initializeMidi :: Cmd.M m => ScoreT.Instrument -> Patch.Addr -> m (Maybe Text)
initializeMidi inst addr = do
    (patch, _) <- Cmd.getMidiInstrument inst
    sendMidiInitialize addr (Patch.patchInitialize patch)

sendMidiInitialize :: Cmd.M m => Patch.Addr -> Patch.InitializePatch
    -> m (Maybe Text)
sendMidiInitialize (dev, chan) = \case
    Patch.InitializeMidi msgs -> do
        mapM_ (Cmd.midi dev . Midi.setChannel chan) msgs
        pure $ Just $ "sending midi init: " <> pretty msgs
    Patch.InitializeMessage msg -> pure $ Just $ "initialize: " <> msg
    Patch.NoInitialization -> pure Nothing

scInitialize :: Cmd.CmdT IO ()
scInitialize = do
    insts <- Ui.getConfig $ Map.keys . UiConfig.unallocations
        . UiConfig.configAllocations
    insts <- mapM Cmd.getInstrument insts
    scInitializePatches $ mapMaybe Cmd.scPatch insts

scInitializePatches :: [Sc.Patch.Patch] -> Cmd.CmdT IO ()
scInitializePatches [] = pure ()
scInitializePatches patches = do
    -- scsynth has a /d_free, but seems to have no way to query what is
    -- actually loaded, so it's sort of useless, because you must control the
    -- scsynth lifecycle to manually track what it loaded, and at that point
    -- you may as well restart it.
    -- TODO if configured with the path to scsynth, I could start it
    -- automatically.  In that case, I may want to send a /quit on exit.
    msg <- Cmd.requireRight ("can't initialize sc patches: "<>)
        =<< liftIO Sc.Play.version
    -- TODO: when called from REPL, log msgs are collected together, so
    -- "waiting for" is not very interesting.
    Log.notice $ "found scsynth: " <> msg
    Log.notice $ "loading patches: "
        <> Text.unwords (map (Texts.toText . Sc.Patch.name) patches)
    liftIO $ mapM_ Sc.Play.initializePatch patches
    liftIO Sc.Play.sync
    Log.notice "waiting for sync"
