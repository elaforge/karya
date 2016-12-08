-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Load ky files, which are separate files containing call definitions.
-- The syntax is defined by 'Parse.parse_ky'.
module Cmd.Ky (
    update_cache
    , load
    , compile_library
) where
import qualified Control.Monad.Except as Except
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Time as Time

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

import qualified Util.Doc as Doc
import qualified Util.File as File
import qualified Util.Log as Log

import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.Macro as Macro
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Library as Library
import qualified Derive.Parse as Parse
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig

import Global


-- | Update the definition cache by reading the per-score definition file.
update_cache :: State.State -> Cmd.State -> IO Cmd.State
update_cache ui_state cmd_state = case ky_file of
    Nothing
        | Maybe.isNothing $ Cmd.state_ky_cache cmd_state ->
            return cmd_state
        | otherwise -> return $ cmd_state { Cmd.state_ky_cache = Nothing }
    Just fname -> cached_load cmd_state fname >>= \x -> return $ case x of
        Nothing -> cmd_state
        Just (lib, timestamps) -> cmd_state
            { Cmd.state_ky_cache = Just $ Cmd.KyCache lib timestamps
            , Cmd.state_play = (Cmd.state_play cmd_state)
                { Cmd.state_performance = mempty
                , Cmd.state_current_performance = mempty
                , Cmd.state_performance_threads = mempty
                }
            }
    where ky_file = State.config#State.ky_file #$ ui_state

-- | Load a definition file if the cache is out of date.  Nothing if the cache
-- is up to date.
cached_load :: Cmd.State -> FilePath
    -> IO (Maybe (Either Text Derive.Library, Map.Map FilePath Time.UTCTime))
cached_load state fname = run $ case Cmd.state_ky_cache state of
    Just (Cmd.PermanentKy _) -> return Nothing
    _ -> do
        dir <- require ("need a SaveFile to find " <> showt fname) $
            Cmd.state_save_dir state
        let paths = dir : Cmd.config_ky_paths (Cmd.state_config state)
        current_timestamps <- require_right
            =<< liftIO (get_timestamps (Map.keys cached_timestamps))
        let fresh = not (Map.null cached_timestamps)
                && current_timestamps == cached_timestamps
        if fresh then return Nothing else do
            (lib, timestamps) <- require_right =<< liftIO (load paths fname)
            return $ Just (Right lib, timestamps)
    where
    run = fmap map_error . Except.runExceptT
    map_error (Left msg) = case Cmd.state_ky_cache state of
        -- If it failed last time then don't replace the error.  Otherwise,
        -- 'update_cache' will clear the performance and I'll get an endless
        -- loop.
        Just (Cmd.KyCache (Left _) _) -> Nothing
        _ -> Just (Left msg, mempty)
    map_error (Right val) = val
    require msg = maybe (Except.throwError msg) return
    require_right = either Except.throwError return
    cached_timestamps = case Cmd.state_ky_cache state of
        Just (Cmd.KyCache _ timestamps) -> timestamps
        _ -> mempty

get_timestamps :: [FilePath] -> IO (Either Text (Map.Map FilePath Time.UTCTime))
get_timestamps fns = fmap map_error . File.tryIO $ do
    mtimes <- mapM
        (liftIO . File.ignoreEnoent . Directory.getModificationTime) fns
    return $ Map.fromList [(fn, mtime) | (fn, Just mtime) <- zip fns mtimes]
    where
    map_error = first (("get_timestamps: "<>) . showt)

load :: [FilePath] -> FilePath
    -> IO (Either Text (Derive.Library, Map.Map FilePath Time.UTCTime))
load paths fname = Parse.load_ky paths fname >>= \result -> case result of
    Left err -> return $ Left err
    Right (defs, imported) -> do
        Log.notice $ "imported definitions from "
            <> Text.intercalate ", "
                (map (txt . FilePath.takeFileName . fst) imported)
        let lib = compile_library defs
        forM_ (Library.shadowed lib) $ \((name, _), calls) ->
            Log.warn $ "definitions in " <> showt fname
                <> " " <> name <> " shadowed: " <> pretty calls
        return $ Right (lib, Map.fromList imported)

compile_library :: Parse.Definitions -> Derive.Library
compile_library (Parse.Definitions note control pitch val aliases) =
    Derive.Library
        { lib_note = call_maps note
        , lib_control = call_maps control
        , lib_pitch = call_maps pitch
        , lib_val = Derive.call_map $ compile make_val_call val
        , lib_instrument_aliases = Map.fromList aliases
        }
    where
    call_maps (gen, trans) = Derive.call_maps
        (compile make_generator gen) (compile make_transformer trans)
    compile make = map $ \(fname, (call_id, expr)) ->
        (call_id, make fname (sym_to_name call_id) expr)
    sym_to_name (BaseTypes.Symbol name) = Derive.CallName name

make_generator :: Derive.Callable d => FilePath -> Derive.CallName
    -> Parse.Expr -> Derive.Generator d
make_generator fname name var_expr
    | Just expr <- no_free_vars var_expr = simple_generator fname name expr
    | otherwise = Macro.generator Module.local name mempty
        (Doc.Doc $ "Defined in " <> txt fname <> ".") var_expr

make_transformer :: Derive.Callable d => FilePath -> Derive.CallName
    -> Parse.Expr -> Derive.Transformer d
make_transformer fname name var_expr
    | Just expr <- no_free_vars var_expr = simple_transformer fname name expr
    | otherwise = Macro.transformer Module.local name mempty
        (Doc.Doc $ "Defined in " <> txt fname <> ".") var_expr

make_val_call :: FilePath -> Derive.CallName -> Parse.Expr -> Derive.ValCall
make_val_call fname name var_expr
    | Just expr <- no_free_vars var_expr = case expr of
        call_expr :| [] -> simple_val_call fname name call_expr
        _ -> broken
    | otherwise = case var_expr of
        Parse.Expr (call_expr :| []) -> Macro.val_call Module.local name mempty
            (Doc.Doc $ "Defined in " <> txt fname <> ".") call_expr
        _ -> broken
    where
    broken = broken_val_call name $
        "Broken val call defined in " <> txt fname
        <> ": val calls don't support pipeline syntax: "
        <> ShowVal.show_val var_expr

simple_generator :: Derive.Callable d => FilePath -> Derive.CallName
    -> BaseTypes.Expr -> Derive.Generator d
simple_generator fname name expr =
    Derive.generator Module.local name mempty (make_doc fname name expr) $
    case assign_symbol expr of
        Nothing -> Sig.call0 generator
        Just call_id ->
            Sig.call (Sig.many_vals "arg" "Args parsed by reapplied call.") $
                \_vals args -> Eval.reapply_generator args call_id
    where generator args = Eval.eval_toplevel (Derive.passed_ctx args) expr

simple_transformer :: Derive.Callable d => FilePath -> Derive.CallName
    -> BaseTypes.Expr -> Derive.Transformer d
simple_transformer fname name expr =
    Derive.transformer Module.local name mempty (make_doc fname name expr) $
    case assign_symbol expr of
        Nothing -> Sig.call0t transformer
        Just call_id ->
            Sig.callt (Sig.many_vals "arg" "Args parsed by reapplied call.") $
                \_vals -> reapply call_id
    where
    transformer args deriver =
        Eval.eval_transformers (Derive.passed_ctx args)
            (NonEmpty.toList expr) deriver
    reapply call_id args deriver = do
        call <- Eval.get_transformer call_id
        Eval.apply_transformer (Derive.passed_ctx args) call
            (Derive.passed_vals args) deriver

simple_val_call :: FilePath -> Derive.CallName -> BaseTypes.Call
    -> Derive.ValCall
simple_val_call fname name call_expr =
    Derive.val_call Module.local name mempty (make_doc fname name expr) $
    case assign_symbol expr of
        Nothing -> Sig.call0 $ \args ->
            Eval.eval (Derive.passed_ctx args) (BaseTypes.ValCall call_expr)
        Just call_id ->
            Sig.call (Sig.many_vals "arg" "Args parsed by reapplied call.") $
                \_vals -> call_args call_id
    where
    expr = call_expr :| []
    call_args call_id args = do
        call <- Eval.get_val_call call_id
        Derive.vcall_call call $ args
            { Derive.passed_call_name = Derive.vcall_name call }

broken_val_call :: Derive.CallName -> Text -> Derive.ValCall
broken_val_call name msg = Derive.make_val_call Module.local name mempty
    (Doc.Doc msg)
    (Sig.call (Sig.many_vals "arg" "broken") $ \_ _ -> Derive.throw msg)

-- | If the Parse.Expr has no 'Parse.VarTerm's, it doesn't need to be a macro.
no_free_vars :: Parse.Expr -> Maybe BaseTypes.Expr
no_free_vars (Parse.Expr expr) = traverse convent_call expr
    where
    convent_call (Parse.Call call_id terms) =
        BaseTypes.Call call_id <$> traverse convert_term terms
    convert_term (Parse.VarTerm _) = Nothing
    convert_term (Parse.ValCall call) = BaseTypes.ValCall <$> convent_call call
    convert_term (Parse.Literal val) = Just $ BaseTypes.Literal val

make_doc :: FilePath -> Derive.CallName -> BaseTypes.Expr -> Doc.Doc
make_doc fname name expr = Doc.Doc $
    pretty name <> " defined in " <> txt fname <> ": " <> ShowVal.show_val expr

-- | If there are arguments in the definition, then don't accept any in the
-- score.  I could do partial application, but it seems confusing, so
-- I won't add it unless I need it.
assign_symbol :: BaseTypes.Expr -> Maybe BaseTypes.CallId
assign_symbol (BaseTypes.Call call_id [] :| []) = Just call_id
assign_symbol _ = Nothing
