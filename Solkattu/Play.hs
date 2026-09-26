-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Convert solkattu to audio via karya score, and play it.
module Solkattu.Play (
    playM, printM
#ifdef TESTING
    , module Solkattu.Play
#endif
) where
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Control as Control
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Processes as Processes

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Performance as Performance
import qualified Cmd.Simple as Simple

import qualified Derive.Controls as Controls
import qualified Derive.DeriveSaved as DeriveSaved
import qualified Derive.Expr as Expr
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Solkattu.Instrument.ToScore as ToScore
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tala as Tala
import qualified Solkattu.Talas as Talas

import qualified Synth.Sampler.PatchDb as Sampler.PatchDb
import qualified Synth.Shared.Config as Config
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.GenId as GenId
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- | Play mridangam realization for the korvai.
playM :: RealTime -- ^ akshara dur
    -> Korvai.Korvai -> IO Bool
playM = playInstrument Korvai.IMridangam
    (InstT.Qualified "sampler" "mridangam-d")
    "# = (natural) | dyn = .75"

-- | Show the generated UI state.
printM :: RealTime -> Korvai.Korvai -> Either Text Simple.State
printM aksharaDur korvai =
    merge $ dump <$> toState Korvai.IMridangam
        (InstT.Qualified "sampler" "mridangam-d") "" aksharaDur korvai
    where
    dump state = Ui.eval state Simple.dumpState
    merge = \case
        Left s -> Left s
        Right (Left e) -> Left $ pretty e
        Right (Right x) -> Right x

playInstrument
    :: ( Solkattu.Notation stroke, Expr.ToExpr (Realize.Stroke stroke)
       , Ord stroke
       )
    => Korvai.Instrument stroke
    -> InstT.Qualified -> Text -> RealTime -> Korvai.Korvai -> IO Bool
playInstrument instrument imInstrument transform aksharaDur korvai = do
    state <- either errorIO pure $
        toState instrument imInstrument transform aksharaDur korvai
    (procs, outputDir) <- deriveToDisk "solkattu" state
    Log.debug $ "procs: " <> showt procs <> " out_dir: " <> txt outputDir
    playProcs procs outputDir

playProcs :: [Performance.Process] -> FilePath -> IO Bool
playProcs [] _ = pure True -- I think this shouldn't happen?
playProcs procs outputDir = do
    ready <- MVar.newEmptyMVar
    Log.debug $ "start render: " <> pretty procs
    rendering <- Async.async $
        Performance.waitForSubprocesses (MVar.putMVar ready ())
            (Set.fromList [instName, metronomeName]) (Set.fromList procs)
    Log.debug "wait for ready"
    MVar.takeMVar ready
    Log.debug $ Text.unwords ["%", "build/opt/stream_audio", txt outputDir]
    Processes.call "build/opt/stream_audio" [outputDir]
    Async.wait rendering

-- | Derive the Ui.State and write the im parts to disk.
deriveToDisk :: FilePath -> Ui.State -> IO ([Performance.Process], FilePath)
deriveToDisk scorePath uiState = do
    cmdState <- loadCmdState
    blockId <- either (errorIO . pretty) pure $
        Ui.eval uiState Ui.getRootId
    let (events, logs) = derive cmdState uiState blockId
    mapM_ Log.write $ filter ((>Log.Debug) . Log.msgPriority) logs
    putStrLn $ "events: " <> show (length events)
    let imConfig = Cmd.configIm (Cmd.stateConfig cmdState)
        lookupInst = either (const Nothing) Just
            . Cmd.stateLookupInstrument uiState cmdState
    (procs, nonIm) <- Performance.evaluateIm imConfig lookupInst scorePath
        0 1 blockId events
    unless (null nonIm) $
        Log.warn $ "non-im events: " <> pretty nonIm
    config <- Config.getConfig
    pure
        ( procs
        , Config.outputDirectory (Config.imDir config) scorePath blockId
        )

derive :: Cmd.State -> Ui.State -> BlockId
    -> (Vector.Vector Score.Event, [Log.Msg])
derive cmdState uiState blockId =
    (Msg.perfEvents perf, logs ++ Msg.perfLogs perf)
    where (perf, logs) = Performance.derive uiState cmdState blockId

loadCmdState :: IO Cmd.State
loadCmdState = Cmd.initialState <$> DeriveSaved.cmdConfig db
    where db = fst $ Inst.db [Sampler.PatchDb.synth]


-- * to_state

-- | Realize and convert to Ui.State.
toState :: (Solkattu.Notation stroke, Expr.ToExpr (Realize.Stroke stroke),
        Ord stroke)
    => Korvai.Instrument stroke
    -> InstT.Qualified -> Text -> RealTime -> Korvai.Korvai
    -> Either Text Ui.State
toState instrument imInstrument transform aksharaDur_ korvai = do
    results <- sequence $ Korvai.realize instrument korvai
    let (strokes, _warnings) = first concat $ unzip results
    -- Leave the warnings for the realize call.
    let notes = toNoteTrack (Korvai.instToScore instrument) aksharaDur
            strokes
    let end = case notes of
            NoteTrack _ events _ -> Events.timeEnd events
    first pretty $ makeState imInstrument transform $ map makeTracks
        [ notes
        , makeTrack metronomeName $
            talaMetronome (Korvai.korvaiTala korvai) aksharaDur end
        ]
    where
    aksharaDur = RealTime.toScore aksharaDur_

makeState :: InstT.Qualified -> Text -> [[Track.Track]]
    -> Either Ui.Error Ui.State
makeState instrument transform trackGroups = Ui.exec Ui.empty $ do
    bid <- GenId.blockId Nothing
    let title = Text.intercalate " | " $ filter (not . Text.null)
            [ "inst = " <> ShowVal.showVal instName
            , transform
            , "scale = just-r"
            , "key = d-maj"
            -- , "just-base = (hz (<-#))"
            -- Mridangam claims (natural) is 62.1, but it sounds more like 62.5
            -- TODO fix this
            , "just-base = (hz 62.5)"
            ]
    bid <- Ui.createBlock bid title []
    Ui.setRootId bid
    tids <- forM (concat trackGroups) $ \t -> do
        tid <- GenId.trackId bid
        Ui.createTrack tid t
    Ui.insertTrack bid 0 $ Block.track (Block.RId Ui.noRuler) 40
    mapM_ (Ui.insertTrack bid 999 . blockTrack) tids
    -- Ui.set_skeleton bid $ Skeleton.make $ note_track_edges track_groups
    allocate instName instrument
    allocate metronomeName (InstT.Qualified "sampler" "metronome")
    where
    blockTrack tid = Block.track (Block.TId tid Ui.noRuler) 40

-- -- | TODO this is much like the one in Cmd.Load.Midi, and the one in
-- -- Cmd.Load.Mod.  I should have a common track creator.
-- note_track_edges :: [[a]] -> [Skeleton.Edge]
-- note_track_edges = concat . snd . List.mapAccumL edges 1
--     where
--     edges n tracks = (end, zip ns (drop 1 ns))
--         where
--         end = n + length tracks
--         ns = [n .. end-1]

metronomeName :: ScoreT.Instrument
metronomeName = "metronome"

instName :: ScoreT.Instrument
instName = "instrument"

allocate :: Ui.M m => ScoreT.Instrument -> InstT.Qualified -> m ()
allocate inst qualified = do
    let alloc = UiConfig.allocation qualified UiConfig.Im
    -- I just trust that this is an im synth and it exists.
    -- Otherwise I need the inst db from Cmd.
    Ui.modifyConfig $ UiConfig.allocationsMap %= Map.insert inst alloc

-- * make_tracks

-- | This is the same as 'ModifyNotes.NoteTrack', so I can convert to one of
-- those, but I don't want to incur the dependency for just that type.
data NoteTrack = NoteTrack !ScoreT.Instrument !Events.Events !Controls
    deriving (Eq, Show)
type Controls = Map Control Events.Events
data Control = Pitch Pitch.ScaleId | Control ScoreT.Control
    deriving (Eq, Ord, Show)

makeTracks :: NoteTrack -> [Track.Track]
makeTracks (NoteTrack inst events controls) =
    Track.track (ParseTitle.instrumentToTitle inst) events
        : map control (Map.toAscList controls)
    where control (c, events) = Track.track (controlToTitle c) events

controlToTitle :: Control -> Text
controlToTitle control = case control of
    Control c -> ParseTitle.controlToTitle $ ScoreT.untyped c
    Pitch scaleId -> ParseTitle.scaleToTitle scaleId

toNoteTrack :: ToScore.ToScore stroke -> TrackTime
    -> [S.Flat g (Realize.Note stroke)] -> NoteTrack
toNoteTrack toScore aksharaDur strokes =
    NoteTrack ScoreT.emptyInstrument (mkEvents notes) controlTracks
    where
    controls :: [(Text, [ToScore.Event])]
    (notes, controls) = ToScore.fromStrokes toScore strokes
    pitches = fromMaybe [] $ lookup "*" controls
    pitchTrack
        | null pitches = Nothing
        | otherwise = Just (Pitch Pitch.emptyScale, mkEvents pitches)
    controlTracks = Map.fromList $ maybe id (:) pitchTrack $
        [ (Control (ScoreT.Control control), mkEvents events)
        | (control, events) <- controls
        , control /= "*"
        ]
    mkEvents = Events.fromList . map mkEvent
    mkEvent (start, dur, text) =
        Event.event (realToFrac start * aksharaDur)
            (realToFrac dur * aksharaDur) text

-- | This needs tala, akshara_dur, base pitch
--
-- I can either hardcode patterns for each tala, or write a generator.
--
-- TODO this is just [(start, dur, pitch, dyn)] -> [Track], which I should have
-- already.
talaMetronome :: Talas.Tala -> TrackTime -> TrackTime
    -> [(TrackTime, TrackTime, Text, Double)]
talaMetronome (Talas.Hindustani _) _ _ = error "hindustani not implemented"
talaMetronome (Talas.Carnatic tala) aksharaDur end =
        takeWhile (\(s, _, _, _) -> s < end)
    [ (s, 0, pitch, dyn)
    | (s, Just (pitch, dyn)) <- zip (Lists.range_ 0 aksharaDur) (cycle pattern)
    ]
    where
    pattern = concatMap make (Tala._angas tala)
    make = \case
        Tala.Clap n -> Just (clap, 1) : replicate (n-1) Nothing
        Tala.Wave n -> Just (wave, 0.75) : replicate (n-1) Nothing
        Tala.I -> map Just $
            (beat, 1) : replicate (Tala._jati tala - 1) (beat, 0.85)
        Tala.O -> map Just [(clap, 1), (wave, 1)]
        Tala.U -> map Just [(clap, 1)]
    clap = "3p"
    wave = "4p"
    beat = "4s"

makeTrack :: ScoreT.Instrument -> [(TrackTime, TrackTime, Text, Double)]
    -> NoteTrack
makeTrack inst notes =
    NoteTrack inst (mkEvents events) (Map.fromList [pitchTrack, dynTrack])
    where
    pitchTrack = (Pitch Pitch.emptyScale, mkEvents pitches)
    dynTrack = (Control Controls.dynamic , mkEvents dyns)
    events = [(s, d, "") | (s, d, _, _) <- notes]
    pitches = [(s, 0, pitch) | (s, _, pitch, _) <- notes]
    dyns = [(s, 0, ShowVal.showVal dyn) | (s, _, _, dyn) <- notes]
    mkEvents = Events.fromList . map (Control.uncurry3 Event.event)
