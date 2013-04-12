-- | Cmd-level support for the lilypond backend.
module Cmd.Lilypond where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text.IO as Text.IO

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Process as Process

import Util.Control
import qualified Util.Log as Log
import qualified Util.Process

import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.PlayUtil as PlayUtil

import qualified Derive.Call.Articulation as Articulation
import qualified Derive.Call.Block as Call.Block
import qualified Derive.Call.Note as Note
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Scope as Scope
import qualified Derive.LEvent as LEvent
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Pitch as Pitch

import Types


block_id_filename :: (Cmd.M m) => BlockId -> m FilePath
block_id_filename = ly_filename . Id.ident_name

ly_filename :: (Cmd.M m) => String -> m FilePath
ly_filename name = do
    dir <- Cmd.require_msg "ly_filename: no save dir"
        =<< Cmd.gets Cmd.state_save_dir
    return $ dir </> "ly" </> clean name ++ ".ly"
    where
    clean = map replace . map Char.toLower
    replace '/' = '-'
    replace ' ' = '-'
    replace c = c

lookup_key :: Cmd.Performance -> Pitch.Key
lookup_key perf =
    fromMaybe Twelve.default_key $ msum $ map (lookup . Derive.state_environ) $
        Map.elems (Msg.perf_track_dynamic perf)
    where
    lookup environ = case TrackLang.get_val TrackLang.v_key environ of
        Right key -> Just (Pitch.Key key)
        Left _ -> Nothing

derive_block :: (Cmd.M m) => BlockId -> m Derive.Result
derive_block block_id = do
    global_transform <- State.config#State.global_transform <#> State.get
    -- Make sure a bad block id will fail right away.
    _ <- State.get_block block_id
    derive $ Call.Block.eval_root_block global_transform block_id

-- | Run a derivation in lilypond context, which will cause certain calls to
-- behave differently.
derive :: (Cmd.M m) => Derive.EventDeriver -> m Derive.Result
derive deriver = do
    config <- State.config#State.lilypond <#> State.get
    state <- (State.config#State.default_#State.tempo #= 1) <$> State.get
    scope <- Cmd.gets (Cmd.state_global_scope . Cmd.state_config)
    constant <- PlayUtil.make_constant state mempty mempty
    env <- PlayUtil.make_environ
    return $ Derive.extract_result $ Derive.derive
        (constant { Derive.state_lilypond = Just config })
        (lilypond_scope scope) env deriver

lilypond_scope :: Derive.Scope -> Derive.Scope
lilypond_scope = Scope.add_override_note_lookup lookup
    where
    lookup = Derive.map_lookup $ Derive.make_calls
        [ ("", note), ("n", note)
        , ("(", Articulation.c_ly_slur)
        , ("^(", Articulation.c_ly_slur_up)
        , ("_(", Articulation.c_ly_slur_down)
        ]
    -- Turn off the behaviour where staccato shortens the note, since that's
    -- already implicit when you see the dot.
    note = Note.note_call "" "" (Note.default_note Note.no_duration_attributes)

compile_lys :: FilePath -> Lilypond.Config -> Lilypond.Title
    -> [(String, [Score.Event])] -> IO (Either String Cmd.StackMap, [Log.Msg])
compile_lys filename config title movements = do
    let (result, logs) = make_lys config title movements
    (flip (,) logs) <$> case result of
        Left err -> return $ Left err
        Right (ly, stack_map) -> do
            Directory.createDirectoryIfMissing True
                (FilePath.takeDirectory filename)
            IO.withFile filename IO.WriteMode $ \hdl ->
                mapM_ (Text.IO.hPutStr hdl) ly
            Util.Process.logged $ Process.proc "lilypond"
                ["-o", FilePath.dropExtension filename, filename]
            return $ Right stack_map

make_lys :: Lilypond.Config -> Lilypond.Title -> [(String, [Score.Event])]
    -> (Either String ([Text], Cmd.StackMap), [Log.Msg])
make_lys config title movements = (text, concat logs)
    where
    text = Lilypond.make_lys config title $
        zip (map fst movements) movement_events
    (movement_events, logs) = unzip (map (convert . snd) movements)
    convert score_events =
        (Convert.quantize (Lilypond.config_quantize config) events, logs)
        where
        (events, logs) = LEvent.partition $
            Convert.convert (Lilypond.config_quarter_duration config)
                (map LEvent.Event score_events)
