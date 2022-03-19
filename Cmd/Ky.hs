-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Load ky files, which are separate files containing call definitions.
-- The syntax is defined by 'Parse.parse_ky'.
module Cmd.Ky (
    update_cache
    , load
    , compile_definitions
#ifdef TESTING
    , module Cmd.Ky
#endif
) where
import qualified Control.Monad.Except as Except
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified System.FilePath as FilePath

import qualified Util.Doc as Doc
import qualified Util.Log as Log
import qualified Util.ParseText as ParseText

import qualified Cmd.Cmd as Cmd
import qualified Derive.Call.Macro as Macro
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.Parse as Parse
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig

import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global


-- | Check if ky files have changed, and if they have, update
-- 'Cmd.state_ky_cache' and clear the performances.
update_cache :: Ui.State -> Cmd.State -> IO Cmd.State
update_cache ui_state cmd_state = do
    cache <- check_cache ui_state cmd_state
    return $ case cache of
        Nothing -> cmd_state
        Just ky_cache -> cmd_state
            { Cmd.state_ky_cache = Just ky_cache
            , Cmd.state_play = (Cmd.state_play cmd_state)
                { Cmd.state_performance = mempty
                , Cmd.state_current_performance = mempty
                , Cmd.state_performance_threads = mempty
                }
            }

-- | Reload the ky files if they're out of date, Nothing if no reload is
-- needed.
check_cache :: Ui.State -> Cmd.State -> IO (Maybe Cmd.KyCache)
check_cache ui_state cmd_state = run $ do
    when is_permanent abort
    Parse.Ky defs imported <- tryRight . first (Just . ParseText.show_error)
        =<< liftIO (Parse.load_ky (state_ky_paths cmd_state)
            (Ui.config#UiConfig.ky #$ ui_state))
    -- This uses the contents of all the files for the fingerprint, which
    -- means it has to read and parse them on each respond cycle.  If this
    -- turns out to be too expensive, I can go back to the modification time
    -- like I had before.
    let fingerprint = Cmd.fingerprint imported
    when (fingerprint == old_fingerprint) abort
    builtins <- compile_library (loaded_fnames imported) $
        compile_definitions defs
    return (builtins, Map.fromList (Parse.def_aliases defs), fingerprint)
    where
    is_permanent = case Cmd.state_ky_cache cmd_state of
        Just (Cmd.PermanentKy {}) -> True
        _ -> False
    old_fingerprint = case Cmd.state_ky_cache cmd_state of
        Just (Cmd.KyCache _ fprint) -> fprint
        _ -> mempty
    -- If it failed last time then don't replace the error.  Otherwise, I'll
    -- continually clear the performance and get an endless loop.
    failed_previously err = case Cmd.state_ky_cache cmd_state of
        Just (Cmd.KyCache (Left old_err) _) -> err == old_err
        _ -> False

    abort = Except.throwError Nothing
    run = fmap apply . Except.runExceptT
    apply (Left Nothing) = Nothing
    apply (Left (Just err))
        | failed_previously err = Nothing
        | otherwise = Just $ Cmd.KyCache (Left err) mempty
    apply (Right (builtins, aliases, fingerprint)) =
        Just $ Cmd.KyCache (Right (builtins, aliases)) fingerprint

load :: [FilePath] -> Ui.State
    -> IO (Either Text (Derive.Builtins, Derive.InstrumentAliases))
load paths =
    fmap (first ParseText.show_error) . traverse compile
        <=< Parse.load_ky paths . (Ui.config#UiConfig.ky #$)
    where
    compile (Parse.Ky defs imported) = do
        builtins <- compile_library (loaded_fnames imported) $
            compile_definitions defs
        return (builtins, Map.fromList (Parse.def_aliases defs))

loaded_fnames :: [Parse.Loaded] -> [FilePath]
loaded_fnames loads = [fname | Parse.Loaded fname _ <- loads]

state_ky_paths :: Cmd.State -> [FilePath]
state_ky_paths cmd_state = maybe id (:) (Cmd.state_save_dir cmd_state)
    (Cmd.config_ky_paths (Cmd.state_config cmd_state))

compile_library :: Log.LogMonad m => [FilePath] -> Library.Library
    -> m Derive.Builtins
compile_library imports lib = do
    Log.notice $
        "reloaded ky: [" <> Text.unwords (map show_import imports) <> "]"
    Library.compile_log lib
    where
    show_import "" = "<expr>"
    show_import fname = txt (FilePath.takeFileName fname)

compile_definitions :: Parse.Definitions -> Library.Library
compile_definitions (Parse.Definitions (gnote, tnote) (gcontrol, tcontrol)
        (gpitch, tpitch) val _aliases) =
    Derive.Scopes
        { scopes_generator = Derive.Scope
            { scope_note = map (compile make_generator) gnote
            , scope_control = map (compile make_generator) gcontrol
            , scope_pitch = map (compile make_generator) gpitch
            }
        , scopes_transformer = Derive.Scope
            { scope_note = map (compile make_transformer) tnote
            , scope_control = map (compile make_transformer) tcontrol
            , scope_pitch = map (compile make_transformer) tpitch
            }
        , scopes_track = mempty
        , scopes_val = map (compile make_val_call) val
        }
    where
    compile make (fname, (sym, expr)) =
        Library.Single sym (make fname (sym_to_name sym) expr)
    sym_to_name (Expr.Symbol name) = Derive.CallName name

make_generator :: Derive.CallableExpr d => FilePath -> Derive.CallName
    -> Parse.Expr -> Derive.Generator d
make_generator fname name var_expr
    | Just expr <- no_free_vars var_expr = simple_generator fname name expr
    | otherwise = Macro.generator Module.local name mempty
        (Doc.Doc $ "Defined in " <> txt fname <> ".") var_expr

make_transformer :: Derive.CallableExpr d => FilePath -> Derive.CallName
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

simple_generator :: Derive.CallableExpr d => FilePath -> Derive.CallName
    -> DeriveT.Expr -> Derive.Generator d
simple_generator fname name expr =
    Derive.generator Module.local name mempty (make_doc fname name expr) $
    case assign_symbol expr of
        Nothing -> Sig.call0 generator
        Just sym ->
            Sig.call (Sig.many_vals "arg" "Args parsed by reapplied call.") $
                \_vals args -> Eval.reapply_generator args sym
    where generator args = Eval.eval_toplevel (Derive.passed_ctx args) expr

simple_transformer :: Derive.CallableExpr d => FilePath -> Derive.CallName
    -> DeriveT.Expr -> Derive.Transformer d
simple_transformer fname name expr =
    Derive.transformer Module.local name mempty (make_doc fname name expr) $
    case assign_symbol expr of
        Nothing -> Sig.call0t transformer
        Just sym ->
            Sig.callt (Sig.many_vals "arg" "Args parsed by reapplied call.") $
                \_vals -> reapply sym
    where
    transformer args deriver =
        Eval.eval_transformers (Derive.passed_ctx args)
            (NonEmpty.toList expr) deriver
    reapply sym args deriver = do
        call <- Eval.get_transformer sym
        Eval.apply_transformer (Derive.passed_ctx args) call
            (Derive.passed_vals args) deriver

simple_val_call :: FilePath -> Derive.CallName -> DeriveT.Call
    -> Derive.ValCall
simple_val_call fname name call_expr =
    Derive.val_call Module.local name mempty (make_doc fname name expr) $
    case assign_symbol expr of
        Nothing -> Sig.call0 $ \args ->
            Eval.eval (Derive.passed_ctx args) (Expr.ValCall call_expr)
        Just sym ->
            Sig.call (Sig.many_vals "arg" "Args parsed by reapplied call.") $
                \_vals -> call_args sym
    where
    expr = call_expr :| []
    call_args sym args = do
        call <- Eval.get_val_call sym
        Derive.vcall_call call $ args
            { Derive.passed_call_name = Derive.vcall_name call }

broken_val_call :: Derive.CallName -> Text -> Derive.ValCall
broken_val_call name msg = Derive.make_val_call Module.local name mempty
    (Doc.Doc msg)
    (Sig.call (Sig.many_vals "arg" "broken") $ \_ _ -> Derive.throw msg)

-- | If the Parse.Expr has no 'Parse.VarTerm's, it doesn't need to be a macro.
no_free_vars :: Parse.Expr -> Maybe DeriveT.Expr
no_free_vars (Parse.Expr expr) = traverse convent_call expr
    where
    convent_call (Parse.Call sym terms) =
        Expr.Call sym <$> traverse convert_term terms
    convert_term (Parse.VarTerm _) = Nothing
    convert_term (Parse.ValCall call) = Expr.ValCall <$> convent_call call
    convert_term (Parse.Literal val) = Just $ Expr.Literal val

make_doc :: FilePath -> Derive.CallName -> DeriveT.Expr -> Doc.Doc
make_doc fname name expr = Doc.Doc $
    pretty name <> " defined in " <> txt fname <> ": " <> ShowVal.show_val expr

-- | If there are arguments in the definition, then don't accept any in the
-- score.  I could do partial application, but it seems confusing, so
-- I won't add it unless I need it.
assign_symbol :: Expr.Expr a -> Maybe Expr.Symbol
assign_symbol (Expr.Call sym [] :| []) = Just sym
assign_symbol _ = Nothing
