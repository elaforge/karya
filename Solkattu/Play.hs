-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Convert solkattu to audio via karya score, and play it.
module Solkattu.Play (
    play_m
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

import           System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Util.Processes as Processes
import qualified Util.Seq as Seq

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Performance as Performance

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
play_m :: RealTime -> Korvai.Korvai -> IO Bool
play_m = play_instrument Korvai.IMridangam
    (InstT.Qualified "sampler" "mridangam-d")
    "# = (natural) | %dyn = .75"

play_instrument :: (Solkattu.Notation stroke,
        Expr.ToExpr (Realize.Stroke stroke), Ord stroke)
    => Korvai.Instrument stroke
    -> InstT.Qualified -> Text -> RealTime -> Korvai.Korvai -> IO Bool
play_instrument instrument im_instrument transform akshara_dur korvai = do
    state <- either errorIO return $
        to_state instrument im_instrument transform akshara_dur korvai
    (procs, output_dirs) <- derive_to_disk "solkattu" state
    play_procs procs output_dirs

play_procs :: [Performance.Process] -> [FilePath] -> IO Bool
play_procs [] _ = return True -- I think this shouldn't happen?
play_procs procs output_dirs = do
    ready <- MVar.newEmptyMVar
    Log.debug $ "start render: " <> pretty procs
    rendering <- Async.async $
        Performance.wait_for_subprocesses (MVar.putMVar ready ())
            (Set.singleton inst_name)
            (Set.fromList procs)
    Log.debug "wait for ready"
    MVar.takeMVar ready
    Log.debug $ Text.unwords $
        "%" : "build/opt/stream_audio" : map txt output_dirs
    Processes.call "build/opt/stream_audio" output_dirs
    Async.wait rendering

-- | Derive the Ui.State and write the im parts to disk.
derive_to_disk :: FilePath -> Ui.State -> IO ([Performance.Process], [FilePath])
derive_to_disk score_path ui_state = do
    cmd_state <- load_cmd_state
    block_id <- either (errorIO . pretty) return $
        Ui.eval ui_state Ui.get_root_id
    let (events, logs) = derive cmd_state ui_state block_id
    mapM_ Log.write $ filter ((>Log.Debug) . Log.msg_priority) logs
    let im_config = Cmd.config_im (Cmd.state_config cmd_state)
        lookup_inst = either (const Nothing) Just
            . Cmd.state_lookup_instrument ui_state cmd_state
    (procs, non_im) <- Performance.evaluate_im im_config lookup_inst score_path
        0 1 block_id events
    unless (null non_im) $
        Log.warn $ "non-im events: " <> pretty non_im
    config <- Config.getConfig
    let out_dir inst = Config.outputDirectory (Config.imDir config) score_path
            block_id </> untxt (ScoreT.instrument_name inst)
    return
        ( procs
        , [out_dir inst_name, out_dir metronome_name]
        )

derive :: Cmd.State -> Ui.State -> BlockId
    -> (Vector.Vector Score.Event, [Log.Msg])
derive cmd_state ui_state block_id =
    (Msg.perf_events perf, logs ++ Msg.perf_logs perf)
    where (perf, logs) = Performance.derive ui_state cmd_state block_id

load_cmd_state :: IO Cmd.State
load_cmd_state = Cmd.initial_state <$> DeriveSaved.cmd_config db
    where db = fst $ Inst.db [Sampler.PatchDb.synth]


-- * to_state

-- | Realize and convert to Ui.State.
to_state :: (Solkattu.Notation stroke, Expr.ToExpr (Realize.Stroke stroke),
        Ord stroke)
    => Korvai.Instrument stroke
    -> InstT.Qualified -> Text -> RealTime -> Korvai.Korvai
    -> Either Text Ui.State
to_state instrument im_instrument transform akshara_dur_ korvai = do
    results <- sequence $ Korvai.realize instrument korvai
    let (strokes, _warnings) = first concat $ unzip results
    -- Leave the warnings for the realize call.
    let notes = to_note_track (Korvai.instToScore instrument) akshara_dur
            strokes
    let end = case notes of
            NoteTrack _ events _ -> Events.time_end events
    first pretty $ make_state im_instrument transform $ map make_tracks
        [ notes
        , make_track metronome_name $
            tala_metronome (Korvai.korvaiTala korvai) akshara_dur end
        ]
    where
    akshara_dur = RealTime.to_score akshara_dur_

make_state :: InstT.Qualified -> Text -> [[Track.Track]]
    -> Either Ui.Error Ui.State
make_state instrument transform track_groups = Ui.exec Ui.empty $ do
    bid <- GenId.block_id Nothing
    let title = Text.intercalate " | " $ filter (not . Text.null)
            [ "inst = " <> ShowVal.show_val inst_name
            , transform
            , "scale = just-r"
            , "key = d-maj"
            -- , "%just-base = (hz (<-#))"
            -- Mridangam claims (natural) is 62.1, but it sounds more like 62.5
            -- TODO fix this
            , "%just-base = (hz 62.5)"
            ]
    bid <- Ui.create_block bid title []
    Ui.set_root_id bid
    tids <- forM (concat track_groups) $ \t -> do
        tid <- GenId.track_id bid
        Ui.create_track tid t
    Ui.insert_track bid 0 $ Block.track (Block.RId Ui.no_ruler) 40
    mapM_ (Ui.insert_track bid 999 . block_track) tids
    -- Ui.set_skeleton bid $ Skeleton.make $ note_track_edges track_groups
    allocate inst_name instrument
    allocate metronome_name (InstT.Qualified "sampler" "metronome")
    where
    block_track tid = Block.track (Block.TId tid Ui.no_ruler) 40

-- -- | TODO this is much like the one in Cmd.Load.Midi, and the one in
-- -- Cmd.Load.Mod.  I should have a common track creator.
-- note_track_edges :: [[a]] -> [Skeleton.Edge]
-- note_track_edges = concat . snd . List.mapAccumL edges 1
--     where
--     edges n tracks = (end, zip ns (drop 1 ns))
--         where
--         end = n + length tracks
--         ns = [n .. end-1]

metronome_name :: ScoreT.Instrument
metronome_name = "metronome"

inst_name :: ScoreT.Instrument
inst_name = "instrument"

allocate :: Ui.M m => ScoreT.Instrument -> InstT.Qualified -> m ()
allocate inst qualified = do
    let alloc = UiConfig.allocation qualified UiConfig.Im
    -- I just trust that this is an im synth and it exists.
    -- Otherwise I need the inst db from Cmd.
    Ui.modify_config $ UiConfig.allocations_map %= Map.insert inst alloc

-- * make_tracks

-- | This is the same as 'ModifyNotes.NoteTrack', so I can convert to one of
-- those, but I don't want to incur the dependency for just that type.
data NoteTrack = NoteTrack !ScoreT.Instrument !Events.Events !Controls
    deriving (Eq, Show)
type Controls = Map Control Events.Events
data Control = Pitch Pitch.ScaleId | Control ScoreT.Control
    deriving (Eq, Ord, Show)

make_tracks :: NoteTrack -> [Track.Track]
make_tracks (NoteTrack inst events controls) =
    Track.track (ParseTitle.instrument_to_title inst) events
        : map control (Map.toAscList controls)
    where control (c, events) = Track.track (control_to_title c) events

control_to_title :: Control -> Text
control_to_title control = case control of
    Control c -> ParseTitle.control_to_title $ ScoreT.untyped c
    Pitch scale_id -> ParseTitle.scale_to_title scale_id

to_note_track :: ToScore.ToScore stroke -> TrackTime
    -> [S.Flat g (Realize.Note stroke)] -> NoteTrack
to_note_track to_score akshara_dur strokes =
    NoteTrack ScoreT.empty_instrument (mk_events notes) control_tracks
    where
    controls :: [(Text, [ToScore.Event])]
    (notes, controls) = ToScore.fromStrokes to_score strokes
    pitches = fromMaybe [] $ lookup "*" controls
    pitch_track
        | null pitches = Nothing
        | otherwise = Just (Pitch Pitch.empty_scale, mk_events pitches)
    control_tracks = Map.fromList $ maybe id (:) pitch_track $
        [ (Control (ScoreT.Control control), mk_events events)
        | (control, events) <- controls
        , control /= "*"
        ]
    mk_events = Events.from_list . map mk_event
    mk_event (start, dur, text) =
        Event.event (realToFrac start * akshara_dur)
            (realToFrac dur * akshara_dur) text

-- | This needs tala, akshara_dur, base pitch
--
-- I can either hardcode patterns for each tala, or write a generator.
--
-- TODO this is just [(start, dur, pitch, dyn)] -> [Track], which I should have
-- already.
tala_metronome :: Tala.Tala -> TrackTime -> TrackTime
    -> [(TrackTime, TrackTime, Text, Double)]
tala_metronome tala akshara_dur end = takeWhile (\(s, _, _, _) -> s < end)
    [ (s, 0, pitch, dyn)
    | (s, Just (pitch, dyn)) <- zip (Seq.range_ 0 akshara_dur) (cycle pattern)
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

make_track :: ScoreT.Instrument -> [(TrackTime, TrackTime, Text, Double)]
    -> NoteTrack
make_track inst notes =
    NoteTrack inst (mk_events events) (Map.fromList [pitch_track, dyn_track])
    where
    pitch_track = (Pitch Pitch.empty_scale, mk_events pitches)
    dyn_track = (Control Controls.dynamic , mk_events dyns)
    events = [(s, d, "") | (s, d, _, _) <- notes]
    pitches = [(s, 0, pitch) | (s, _, pitch, _) <- notes]
    dyns = [(s, 0, ShowVal.show_val dyn) | (s, _, _, dyn) <- notes]
    mk_events = Events.from_list . map (uncurry3 Event.event)

    uncurry3 f (a, b, c) = f a b c
