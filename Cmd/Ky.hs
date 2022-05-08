-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Load ky files, which are separate files containing call definitions.
-- The syntax is defined by 'Ky.parse_ky'.
module Cmd.Ky (
    update
    , set
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
import qualified Derive.Parse.Instruments as Instruments
import qualified Derive.Parse.Ky as Ky
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig

import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global


-- | Check if ky files have changed, and if they have, update
-- 'Cmd.state_ky_cache' and clear the performances.
update :: Ui.State -> Cmd.State -> Text
    -> IO (Maybe (Ui.State, Cmd.State, [Log.Msg]))
update ui_state cmd_state ky_text =
    justm (check_cache lookup_backend cache allocs paths ky_text) $
    \((ky_cache, allocs), logs) -> do
        return $ Just
            ( Ui.config#UiConfig.allocations #= allocs $
                -- Should already be set when Responder calls me, but not
                -- when 'set' calls me.
                Ui.config#UiConfig.ky #= ky_text $ ui_state
            , cmd_state
                { Cmd.state_ky_cache = Just ky_cache
                , Cmd.state_play = (Cmd.state_play cmd_state)
                    -- TODO should I kill threads?
                    { Cmd.state_performance = mempty
                    , Cmd.state_current_performance = mempty
                    , Cmd.state_performance_threads = mempty
                    }
                }
            , logs
            )
    where
    lookup_backend = Cmd.get_lookup_backend cmd_state
    allocs = Ui.config#UiConfig.allocations #$ ui_state
    cache = Cmd.state_ky_cache cmd_state
    paths = state_ky_paths cmd_state

set :: Text -> Cmd.CmdT IO Text
set ky_text = do
    cmd_state <- Cmd.get
    ui_state <- Ui.get
    liftIO (update ui_state cmd_state ky_text) >>= \case
        Nothing -> return ""
        -- Logs are just boring stuff about reloaded files.
        Just (ui_state, cmd_state, _logs) ->
            case Cmd.state_ky_cache cmd_state of
                Just (Cmd.KyCache (Left err) _) -> return err
                _ -> do
                    Cmd.put cmd_state
                    Ui.unsafe_put ui_state
                    return ""

-- | Reload the ky files if they're out of date, Nothing if no reload is
-- needed.
check_cache :: (InstT.Qualified -> Maybe Inst.Backend) -> Maybe Cmd.KyCache
    -> UiConfig.Allocations -> [FilePath] -> Text
    -> IO (Maybe ((Cmd.KyCache, UiConfig.Allocations), [Log.Msg]))
check_cache lookup_backend prev_cache old_allocs paths ky_text = run $ do
    when is_permanent abort
    Ky.Ky defs imported mb_allocs <- try . first ParseText.show_error
        =<< liftIO (Ky.load_ky paths ky_text)
    -- This uses the contents of all the files for the fingerprint, which
    -- means it has to read and parse them on each respond cycle.  If this
    -- turns out to be too expensive, I can go back to the modification time
    -- like I had before.
    let fingerprint = Cmd.fingerprint imported
    when (old_fingerprint == fingerprint) abort
    let (builtins, logs) = compile_library (loaded_fnames imported) $
            compile_definitions defs
    allocs <- case mb_allocs of
        Nothing -> return old_allocs
        Just allocs -> try $
            Instruments.update_ui lookup_backend allocs old_allocs
    return
        ( (builtins, Map.fromList (Ky.def_aliases defs), fingerprint, allocs)
        , logs
        )
    where
    run = fmap apply . Except.runExceptT
    apply (Left Nothing) = Nothing
    apply (Left (Just err))
        | failed_previously err = Nothing
        | otherwise = Just ((Cmd.KyCache (Left err) mempty, old_allocs), [])
    apply (Right ((builtins, aliases, fingerprint, allocs), logs)) = Just
        ((Cmd.KyCache (Right (builtins, aliases)) fingerprint, allocs), logs)
    try = tryRight . first Just
    abort = Except.throwError Nothing
    old_fingerprint = case prev_cache of
        Just (Cmd.KyCache _ fprint) -> fprint
        _ -> mempty
    is_permanent = case prev_cache of
        Just (Cmd.PermanentKy {}) -> True
        _ -> False
    -- If it failed last time then don't replace the error.  Otherwise, I'll
    -- continually clear the performance and get an endless loop.
    failed_previously err = case prev_cache of
        Just (Cmd.KyCache (Left old_err) _) -> err == old_err
        _ -> False

-- | Like 'check_cache', but assuming no existing cmd or ui state.
load :: [FilePath] -> Text
    -> IO (Either Text (Derive.Builtins, Derive.InstrumentAliases))
load paths ky_text =
    bimap ParseText.show_error compile <$> liftIO (Ky.load_ky paths ky_text)
    where
    -- Instrument allocations are stored in the score state, and if there is
    -- anything in the ky text it should be the same as in the score state.
    -- If I ever move the the state entirely to ky then this will need to
    -- parse and return them, but meanwhile existing scores don't have allocs
    -- in the ky.
    compile (Ky.Ky defs imported _allocs) =
        (builtins, Map.fromList (Ky.def_aliases defs))
        where
        -- Logs are boring, just loaded this or that.
        (builtins, _logs) = compile_library (loaded_fnames imported) $
            compile_definitions defs

loaded_fnames :: [Ky.Loaded] -> [FilePath]
loaded_fnames loads = [fname | Ky.Loaded fname _ <- loads]

state_ky_paths :: Cmd.State -> [FilePath]
state_ky_paths cmd_state = maybe id (:) (Cmd.state_save_dir cmd_state)
    (Cmd.config_ky_paths (Cmd.state_config cmd_state))

compile_library :: [FilePath] -> Library.Library -> (Derive.Builtins, [Log.Msg])
compile_library imports lib = Log.run_id $ do
    Log.notice $
        "reloaded ky: [" <> Text.unwords (map show_import imports) <> "]"
    Library.compile_log lib

show_import "" = "<score>"
show_import fname = txt (FilePath.takeFileName fname)

compile_definitions :: Ky.Definitions -> Library.Library
compile_definitions (Ky.Definitions (gnote, tnote) (gcontrol, tcontrol)
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
    -> Ky.Expr -> Derive.Generator d
make_generator fname name var_expr
    | Just expr <- no_free_vars var_expr = simple_generator fname name expr
    | otherwise = Macro.generator Module.local name mempty
        (Doc.Doc $ "Defined in " <> txt fname <> ".") var_expr

make_transformer :: Derive.CallableExpr d => FilePath -> Derive.CallName
    -> Ky.Expr -> Derive.Transformer d
make_transformer fname name var_expr
    | Just expr <- no_free_vars var_expr = simple_transformer fname name expr
    | otherwise = Macro.transformer Module.local name mempty
        (Doc.Doc $ "Defined in " <> txt fname <> ".") var_expr

make_val_call :: FilePath -> Derive.CallName -> Ky.Expr -> Derive.ValCall
make_val_call fname name var_expr
    | Just expr <- no_free_vars var_expr = case expr of
        call_expr :| [] -> simple_val_call fname name call_expr
        _ -> broken
    | otherwise = case var_expr of
        Ky.Expr (call_expr :| []) -> Macro.val_call Module.local name mempty
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

-- | If the Ky.Expr has no 'Ky.VarTerm's, it doesn't need to be a macro.
no_free_vars :: Ky.Expr -> Maybe DeriveT.Expr
no_free_vars (Ky.Expr expr) = traverse convent_call expr
    where
    convent_call (Ky.Call sym terms) =
        Expr.Call sym <$> traverse convert_term terms
    convert_term (Ky.VarTerm _) = Nothing
    convert_term (Ky.ValCall call) = Expr.ValCall <$> convent_call call
    convert_term (Ky.Literal val) = Just $ Expr.Literal val

make_doc :: FilePath -> Derive.CallName -> DeriveT.Expr -> Doc.Doc
make_doc fname name expr = Doc.Doc $
    pretty name <> " defined in " <> txt fname <> ": " <> ShowVal.show_val expr

-- | If there are arguments in the definition, then don't accept any in the
-- score.  I could do partial application, but it seems confusing, so
-- I won't add it unless I need it.
assign_symbol :: Expr.Expr a -> Maybe Expr.Symbol
assign_symbol (Expr.Call sym [] :| []) = Just sym
assign_symbol _ = Nothing
