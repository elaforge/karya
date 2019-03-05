-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Convert solkattu to audio via karya score.
module Solkattu.Play where
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector

import System.FilePath ((</>))
import qualified System.IO as IO

import qualified Util.Control as Control
import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.Process as Process
import qualified Util.Process
import qualified Util.TextUtil as TextUtil

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Performance as Performance

import qualified Derive.DeriveSaved as DeriveSaved
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Solkattu.Db as Db
import qualified Solkattu.Instrument.ToScore as ToScore
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu

import qualified Synth.Sampler.PatchDb as Sampler.PatchDb
import qualified Synth.Shared.Config as Config
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.GenId as GenId
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import Global
import Types


test :: IO ()
test = play_m "#=(natural)" 1 (Db.korvais!!30)

play_m :: Text -> RealTime -> Korvai.Korvai -> IO ()
play_m = play_instrument Korvai.mridangam
    (InstTypes.Qualified "sampler" "mridangam-d")

play_instrument :: Solkattu.Notation stroke => Korvai.Instrument stroke
    -> InstTypes.Qualified -> Text -> RealTime -> Korvai.Korvai -> IO ()
play_instrument instrument im_instrument transform akshara_dur korvai = do
    state <- either errorIO return $
        to_state instrument im_instrument transform akshara_dur korvai
    (procs, output_dir) <- derive_to_disk "solkattu" state
    play_procs procs output_dir

play_procs :: [Performance.Process] -> FilePath -> IO ()
play_procs [] output_dir = do
    outputs <- File.list output_dir
    Process.call "bin/play" (filter (".wav" `List.isSuffixOf`) outputs)
play_procs procs output_dir = do
    ready <- MVar.newEmptyMVar
    putStrLn $ "start render: " <> prettys procs
    rendering <- Async.async $ watch_subprocesses ready (Set.fromList procs)
    putStrLn "wait for ready"
    MVar.takeMVar ready
    outputs <- File.list output_dir
    Process.call "bin/play" (filter (".wav" `List.isSuffixOf`) outputs)
    Async.wait rendering

-- | This is analogous to 'Performance.watch_subprocesses', but notifies as
-- soon as all processes have rendered output.
watch_subprocesses :: MVar.MVar () -> Set Performance.Process -> IO ()
watch_subprocesses ready all_procs =
    Util.Process.multipleOutput (Set.toList all_procs) $ \chan ->
        Control.loop1 (all_procs, Set.empty) $ \loop (procs, started) -> if
            | Set.null procs -> MVar.tryPutMVar ready () >> return ()
            | otherwise -> do
                ((cmd, args), out) <- Chan.readChan chan
                loop =<< process procs started (cmd, args) out
    where
    process procs started (cmd, args) = \case
        Util.Process.Stderr line -> put line >> return (procs, started)
        Util.Process.Stdout line -> case Config.parseMessage line of
            Just (Config.Message
                    { Config._payload = Config.RenderingRange start _ })
                | start > 0 -> do
                    -- I can start playing when I see the first progress for
                    -- each process, and for each instrument.  Since I only
                    -- have one instrument the first suffices.
                    let started2 = Set.insert (cmd, args) started
                    when (started2 == all_procs) $
                        void $ MVar.tryPutMVar ready ()
                    return (procs, started2)
            _ -> put ("?: " <> line) >> return (procs, started)
        Util.Process.Exit code -> do
            when (code /= Util.Process.ExitCode 0) $
                Log.warn $ "subprocess " <> txt cmd <> " "
                    <> showt args <> " returned " <> showt code
            return (Set.delete (cmd, args) procs, started)
    -- These get called concurrently, so avoid jumbled output.
    put line = Log.with_stdio_lock $ Text.IO.hPutStrLn IO.stdout line

-- | Derive the Ui.State and write the im parts to disk.
derive_to_disk :: FilePath -> Ui.State -> IO ([Performance.Process], FilePath)
derive_to_disk score_path ui_state = do
    cmd_state <- load_cmd_state
    block_id <- either (errorIO . pretty) return $
        Ui.eval ui_state Ui.get_root_id
    let (events, logs) = derive cmd_state ui_state block_id
    mapM_ Log.write logs
    let im_config = Cmd.config_im (Cmd.state_config cmd_state)
        lookup_inst = either (const Nothing) Just
            . Cmd.state_resolve_instrument ui_state cmd_state
    (procs, non_im) <- Performance.evaluate_im im_config lookup_inst score_path
        1 block_id events
    unless (null non_im) $
        Log.warn $ "non-im events: " <> pretty non_im
    config <- Config.getConfig
    return
        ( procs
        , Config.outputDirectory (Config.imDir config) score_path block_id
            </> "instrument"
        )

instrument_name :: ScoreT.Instrument
instrument_name = "instrument"

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
to_state :: Solkattu.Notation stroke => Korvai.Instrument stroke
    -> InstTypes.Qualified -> Text -> RealTime -> Korvai.Korvai
    -> Either Text Ui.State
to_state instrument im_instrument transform akshara_dur korvai = do
    results <- sequence $ Korvai.realize instrument korvai
    let strokes = concatMap fst results
    first pretty $ make_state im_instrument transform $ make_tracks $
        to_note_track (Korvai.instToScore instrument)
            (RealTime.to_score akshara_dur) 0 strokes

make_state :: InstTypes.Qualified -> Text -> [Track.Track]
    -> Either Ui.Error Ui.State
make_state instrument transform tracks = Ui.exec Ui.empty $ do
    bid <- GenId.block_id Nothing
    bid <- Ui.create_block bid
        (TextUtil.joinWith " | " ("inst=" <> ShowVal.show_val instrument_name)
            transform)
        []
    Ui.set_root_id bid
    tids <- forM tracks $ \t -> do
        tid <- GenId.track_id bid
        Ui.create_track tid t
    mapM_ (Ui.insert_track bid 999 . block_track) tids
    allocate instrument_name instrument
    -- TODO this isn't having an effect, why not?
    Ui.modify_config $ UiConfig.ky #=
        "note transformer:\n\
        \    GLOBAL = inst=" <> ShowVal.show_val instrument_name <> "\n"
    where
    block_track tid = Block.track (Block.TId tid Ui.no_ruler) 40

allocate :: Ui.M m => ScoreT.Instrument -> InstTypes.Qualified -> m ()
allocate inst qualified = do
    let alloc = UiConfig.allocation qualified UiConfig.Im
    -- I just trust that this is an im synth and it exists.
    -- Otherwise I need the inst db from Cmd.
    Ui.modify_config $ UiConfig.allocations_map %= Map.insert inst alloc

-- * make_tracks

-- | This is the same as 'ModifyNotes.NoteTrack', so I can convert to one of
-- those, but I don't want to incur the dependency for just that type.
data NoteTrack = NoteTrack Events.Events Controls
    deriving (Eq, Show)
type Controls = Map Control Events.Events
data Control = Pitch Pitch.ScaleId | Control ScoreT.Control
    deriving (Eq, Ord, Show)

make_tracks :: NoteTrack -> [Track.Track]
make_tracks (NoteTrack events controls) =
    Track.track ">" events : map control (Map.toAscList controls)
    where control (c, events) = Track.track (control_to_title c) events

control_to_title :: Control -> Text
control_to_title control = case control of
    Control c -> ParseTitle.control_to_title $ ScoreT.untyped c
    Pitch scale_id -> ParseTitle.scale_to_title scale_id

to_note_track :: ToScore.ToScore stroke -> TrackTime -> TrackTime
    -> [S.Flat g (Realize.Note stroke)] -> NoteTrack
to_note_track to_score stretch shift strokes =
    NoteTrack (mk_events notes) control_tracks
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
    mk_event (start, dur, text) = place shift stretch $
        Event.event (realToFrac start) (realToFrac dur) text

place :: TrackTime -> TrackTime -> Event.Event -> Event.Event
place shift stretch = (Event.duration_ %= (*stretch))
    . (Event.start_ %= ((+shift) . (*stretch)))
