-- | Cmd-level support for the lilypond backend.
module Cmd.Lilypond where
import qualified Control.Monad.Trans as Trans
import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.Process as Process

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Thread as Thread

import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.SaveGit as SaveGit

import qualified Derive.LEvent as LEvent
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Pitch as Pitch

import Types


-- | Wait this many seconds before kicking off a compile.  This is on top
-- of the usual derive delay.
compile_delay :: Thread.Seconds
compile_delay = 3

cmd_compile :: Msg.Msg -> Cmd.CmdIO
cmd_compile (Msg.DeriveStatus block_id (Msg.DeriveComplete perf)) = do
    compile block_id perf
    return Cmd.Continue
cmd_compile _ = return Cmd.Continue

compile :: BlockId -> Cmd.Performance -> Cmd.CmdT IO ()
compile block_id perf = do
    meta <- Block.block_meta <$> State.get_block block_id
    case Lilypond.meta_to_score (Just (lookup_key perf)) meta of
        Nothing -> return ()
        Just (Left err) -> Log.warn $ "can't convert to lilypond: " ++ err
        Just (Right score) -> run score
    where
    run score = do
        old_compiles <- Cmd.gets
            (Cmd.state_lilypond_compiles . Cmd.state_play)
        -- Cancel the last one, if any.
        case Map.lookup block_id old_compiles of
            Just (Cmd.CancelLilypond var) ->
                Trans.liftIO $ IORef.writeIORef var True
            _ -> return ()
        var <- Trans.liftIO $ IORef.newIORef False
        Cmd.modify_play_state $ \st -> st { Cmd.state_lilypond_compiles =
            Map.insert block_id (Cmd.CancelLilypond var)
                (Cmd.state_lilypond_compiles st) }
        dir <- ly_dir
        Trans.liftIO $ void $ Thread.start $ do
            Thread.delay compile_delay
            cancelled <- IORef.readIORef var
            unless cancelled $
                compile_ly dir block_id config score
                    (LEvent.events_of (Cmd.perf_events perf))
    -- TODO if I still want to do automatic lilypond derivation, I'll have to
    -- stick this in Block.Meta, but that means I should probably use
    -- Data.Dynamic instead of strings.
    config = TimeConfig 1 Lilypond.D64

data TimeConfig = TimeConfig
    { time_quarter :: RealTime
    , time_quantize :: Lilypond.Duration
    }

ly_dir :: (State.M m) => m FilePath
ly_dir = do
    save_file <- SaveGit.save_file False <$> State.get
    return $ save_file ++ "_ly"

lookup_key :: Cmd.Performance -> Pitch.Key
lookup_key perf = fromMaybe Twelve.default_key $ msum $
        map lookup $ Map.elems (Msg.perf_track_environ perf)
    where
    lookup environ = case TrackLang.lookup_val TrackLang.v_key environ of
        Right key -> Just (Pitch.Key key)
        Left _ -> Nothing

compile_ly :: FilePath -> BlockId -> TimeConfig -> Lilypond.Score
    -> [Score.Event] -> IO ()
compile_ly dir block_id config score events = do
    let (ly, logs) = make_ly config score events
    mapM_ Log.write logs
    Directory.createDirectoryIfMissing True dir
    let fname = dir </> Id.ident_name block_id
    writeFile (fname ++ ".ly") (Pretty.formatted ly)
    void $ Process.rawSystem "lilypond" ["-o", fname, fname ++ ".ly"]

make_ly :: TimeConfig -> Lilypond.Score -> [Score.Event]
    -> (Pretty.Doc, [Log.Msg])
make_ly (TimeConfig quarter quantize_dur) score score_events =
    (Lilypond.make_ly Lilypond.default_config score
        (postproc quantize_dur events), logs)
    where
    (events, logs) = LEvent.partition $
        Convert.convert quarter (map LEvent.Event score_events)
        -- Filter out existing logs because those will be reported by normal
        -- performance.

-- * postproc

postproc :: Lilypond.Duration -> [Lilypond.Event] -> [Lilypond.Event]
postproc quantize_dur = quantize quantize_dur . normalize

quantize :: Lilypond.Duration -> [Lilypond.Event] -> [Lilypond.Event]
quantize dur = map $ \e -> e
    { Lilypond.event_start = q (Lilypond.event_start e)
    , Lilypond.event_duration = q (Lilypond.event_duration e)
    }
    where q = quantize_time (Lilypond.dur_to_time dur)

quantize_time :: Lilypond.Time -> Lilypond.Time -> Lilypond.Time
quantize_time time t =
    round (fromIntegral t / fromIntegral time :: Double) * time

normalize :: [Lilypond.Event] -> [Lilypond.Event]
normalize [] = []
normalize events@(e:_)
    | Lilypond.event_start e == 0 = events
    | otherwise = map (move (Lilypond.event_start e)) events
    where move n e = e { Lilypond.event_start = Lilypond.event_start e - n }
