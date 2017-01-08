-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
{- | Functions to help define call signatures.

    This module, along with the 'Typecheck.Typecheck' class, define a little
    DSL to express function signatures.  Check existing calls for examples.

    Argument passing, in an effort to be flexible, got a bit complicated.  Each
    'Arg' has a name and a possible default.  So right off there are three ways
    to provide an argument:

    1. Pass it explicitly.

    2. If it is omitted, or @_@ is passed explicitly, it will be sought in the
    dynamic environ, under the name @\<call_name>-\<arg_name>@.  E.g. given
    a call @generator \"name\" $ \\args -> call (required \"arg1\") ...@ then
    @name-arg1 = 42 | call _@ will get 42.  Note that it uses the call name,
    and not the symbol it happens to bound to in this scope.  This is because,
    while you may bind different kinds of trills to @tr@ depending on the needs
    of the score, the two kinds of trills may have different arguments with
    different meanings.

    3. If it's omitted, and not in the dynamic environ, the default will be
    used, provided there is one.

    In addition, an arg may be a 'BaseTypes.VPControlRef' or
    'BaseTypes.ControlRef', which introduces yet another way to provide the
    value.  An argument @required_control \"c\"@ will pass
    a 'BaseTypes.LiteralControl'.  Technically it's then up to the call to
    decide what to do with it, but it will likely look it up at its chosen
    point in time, which means you can provide the value by providing a @c@
    control track or binding it explicitly e.g. @%c = .5 | call@.

    - To further complicate the matter, the control arg may itself have a
    default, to relieve the caller from always having to provide that control.
    So an argument @control \"c\" 0.5@ or an explicitly provided control val
    @call %c,.5@ will default to 0.5 if the @c@ control is not in scope.

    Since the arg defaulting and control defaulting are orthogonal, they can be
    combined:

    1. Pass it explicitly with a default: @call %c,.5@.  This is either the
    value of @%c@ or 0.5.

    2. Pass it via the dynamic environ: @call-arg1 = %c,.5 | call@.  This is
    the same as the above, only the argument is provided implicitly.

    3. Fall back on the built-in default: @control \"c\" 0.5@ and then just
    @call@.

    I originally envisioned the dynamic environ passing scheme to be a way to
    default certain arguments within the context of a track, to be used in
    a relatively ad-hoc way in specific parts of the score (e.g. all trills
    within this section of melody default to minor thirds), is not limited to
    numeric types, and is constant in time.  A control, however, is intended to
    capture musical parameters that change in time over the course of the
    piece, and is numeric or a pitch.  So while dynamic environ args are forced
    to be specific to a certain call by prepending the call's name, control
    names should generally have more general and abstract names.

    On the subject of controls, controls (and numeric vals in general) have
    another layer of complexity since they carry types.  For example, here's
    a gloriously complicated argument: @defaulted \"speed\" (typed_control
    \"tremolo-speed\" 10 Score.Real)@.  This argument defaults to
    @%tremolo-speed,10s@.  If it's not given, it will have the value @10s@.  If
    the @%tremolo-speed@ control is in scope but untyped, its values will be
    interpreted as RealTime.  If it's in scope and typed (e.g. with
    a @tremolo-speed:t@ track), then its values will be interpreted as
    ScoreTime.

    Another wrinkle in argument passing is that, in addition to being
    'required', which has no default, or being 'defaulted', which has
    a default, they can be 'defaulted' with a default of Nothing.  This passes
    the argument as a @Maybe a@ instead of @a@ and lets the call distinguish
    whether an argument was provided or not.  This is for arguments which are
    defaulted but need a more complicated defaulting strategy than simply
    a constant.
-}
module Derive.Sig (
    Parser, Generator, Transformer
    , check, parse_or_throw, require_right, parse, parse_vals
    -- * parsers
    , no_args
    , required, required_env, defaulted, defaulted_env, defaulted_env_quoted
    , environ, environ_quoted, required_environ
    , optional, optional_env, many, many_vals, many1, many_pairs, many1_pairs
    , required_vals
    -- ** defaults
    , EnvironDefault(..)
    , control, typed_control, required_control, pitch
    , prefixed_environ, environ_keys
    -- * call
    , call, call0, callt, call0t
) where
import qualified Control.Applicative as Applicative
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Ui.Event as Event
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Derive as Derive
import Derive.Derive (EnvironDefault(..), ArgName, CallName)
import qualified Derive.Env as Env
import qualified Derive.Eval as Eval
import qualified Derive.ScoreTypes as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Typecheck as Typecheck
import qualified Derive.ValType as ValType

import qualified Perform.Signal as Signal
import Global


type Error = Derive.CallError
type Docs = [Derive.ArgDoc]

data Parser a = Parser {
    parser_docs :: !Docs
    , parser_parser :: !(State -> Either Error (State, a))
    }

parser :: Derive.ArgDoc -> (State -> Either Error (State, a)) -> Parser a
parser arg_doc = Parser [arg_doc]

-- | Keep track of state when parsing arguments.
data State = State {
    state_vals :: ![BaseTypes.Val]
    -- | This has to be incremented every time a Val is taken.  Pairing argnums
    -- in state_vals doesn't work because when I run out I don't know where
    -- I am.
    , state_argnum :: !Int
    , state_call_name :: !CallName
    , state_derive :: !Derive.State
    , state_context :: !(Derive.Context Derive.Tagged)
    }

run_parser :: Parser a -> State -> Either Error a
run_parser parser state = case parser_parser parser state of
    Right (state, a) -> case state_vals state of
        [] -> Right a
        vals -> Left $ Derive.ArgError $ "too many arguments: "
            <> Text.intercalate ", " (map ShowVal.show_val vals)
    Left err -> Left err

instance Functor Parser where
    fmap f parser =
        parser { parser_parser = fmap (fmap f) . parser_parser parser }

instance Applicative.Applicative Parser where
    pure a = Parser mempty (\state -> Right (state, a))
    Parser doc1 parse1 <*> Parser doc2 parse2 =
        Parser (doc1 <> doc2) $ \state -> do
            (state, f) <- parse1 state
            (state, a) <- parse2 state
            Right (state, f a)

-- | Annotate a parser with a check on its value.
check :: (a -> Maybe Text) -- ^ return Just error if there's a problem
    -> Parser a -> Parser a
check validate (Parser docs parse) = Parser docs $ \state -> case parse state of
    Left err -> Left err
    Right (state2, val) -> case validate val of
        Just err -> Left $ Derive.ArgError err
        Nothing -> Right (state2, val)

parse_or_throw :: Derive.Taggable d => Parser a -> Derive.PassedArgs d
    -> Derive.Deriver a
parse_or_throw parser args = require_right =<< parse parser args

require_right :: Either Error a -> Derive.Deriver a
require_right = either (Derive.throw_error . Derive.CallError) return

-- | Run a parser against the current derive state.
parse :: Derive.Taggable d => Parser a -> Derive.PassedArgs d
    -> Derive.Deriver (Either Error a)
parse parser args = parse_vals parser
    (Derive.tag_context (Derive.passed_ctx args))
    (Derive.passed_call_name args)
    (Derive.passed_vals args)

parse_vals :: Parser a -> Derive.Context Derive.Tagged -> CallName
    -> [BaseTypes.Val] -> Derive.Deriver (Either Error a)
parse_vals parser ctx name vals =
    run_parser parser . make_state <$> Derive.get
    where
    make_state state = State
        { state_vals = vals
        , state_argnum = 0
        , state_call_name = name
        , state_derive = state
        , state_context = ctx
        }

-- * parsers

-- | Parser for nullary calls.  Either use this with 'call' and 'callt', or use
-- 'call0' and 'call0t' as a shortcut.
no_args :: Parser ()
no_args = pure ()

-- | The argument is required to be present, and have the right type.
required :: forall a. Typecheck.Typecheck a => ArgName -> Doc.Doc -> Parser a
required name = required_env name Derive.Prefixed

required_env :: forall a. Typecheck.Typecheck a => ArgName
    -> Derive.EnvironDefault -> Doc.Doc -> Parser a
required_env name env_default doc = parser arg_doc $ \state1 ->
    case get_val env_default state1 name of
        Nothing -> Left $ Derive.ArgError $
            "expected another argument at argument " <> pretty name
        Just (state, val) -> (,) state <$>
            check_arg state arg_doc (argnum_error state1) name val
    where
    expected = Typecheck.to_type (Proxy :: Proxy a)
    arg_doc = Derive.ArgDoc
        { arg_name = name
        , arg_type = expected
        , arg_parser = Derive.Required
        , arg_environ_default = env_default
        , arg_doc = doc
        }


-- | The argument is not required to be present, but if it is, it has to have
-- either the right type or be VNotGiven.
defaulted :: forall a. (Typecheck.Typecheck a, ShowVal.ShowVal a) =>
    ArgName -> a -> Doc.Doc -> Parser a
defaulted name = defaulted_env name Derive.Prefixed

defaulted_env :: forall a. (Typecheck.Typecheck a, ShowVal.ShowVal a) => ArgName
    -> Derive.EnvironDefault -> a -> Doc.Doc -> Parser a
defaulted_env name env_default deflt =
    defaulted_env_ name env_default (Left deflt)

-- | The defaulted value can be a 'BaseTypes.Quoted', which will be evaluated
-- if needed.
defaulted_env_quoted :: forall a. (Typecheck.Typecheck a, ShowVal.ShowVal a) =>
    ArgName -> Derive.EnvironDefault -> BaseTypes.Quoted -> Doc.Doc -> Parser a
defaulted_env_quoted name env_default quoted =
    defaulted_env_ name env_default (Right quoted)

defaulted_env_ :: forall a. (Typecheck.Typecheck a, ShowVal.ShowVal a) =>
    ArgName -> Derive.EnvironDefault -> Either a BaseTypes.Quoted -> Doc.Doc
    -> Parser a
defaulted_env_ name env_default quoted doc = parser arg_doc $ \state1 ->
    case get_val env_default state1 name of
        Nothing -> deflt state1
        Just (state, BaseTypes.VNotGiven) -> deflt state
        Just (state, val) -> (,) state <$>
            check_arg state arg_doc (argnum_error state1) name val
    where
    deflt state = eval_default arg_doc (argnum_error state) name state quoted
    expected = Typecheck.to_type (Proxy :: Proxy a)
    arg_doc = Derive.ArgDoc
        { arg_name = name
        , arg_type = expected
        , arg_parser = Derive.Defaulted $
            either ShowVal.show_val ShowVal.show_val quoted
        , arg_environ_default = env_default
        , arg_doc = doc
        }

-- | Eval a Quoted default value.
eval_default :: forall a. Typecheck.Typecheck a => Derive.ArgDoc
    -> Derive.ErrorPlace -> ArgName -> State -> Either a BaseTypes.Quoted
    -> Either Error (State, a)
eval_default _ _ _ state (Left a) = return (state, a)
eval_default arg_doc place name state (Right quoted) =
    case eval_quoted state quoted of
        -- Left err -> Left $ Derive.EvalError place quoted name err
        Left err -> Left $ Derive.TypeError place (Derive.Quoted quoted) name
            expected_type Nothing (Just err)
        Right val -> (,) state <$> check_arg state arg_doc place name val
    where
    expected_type = Typecheck.to_type (Proxy :: Proxy a)

-- | This is an argument which is not actually parsed from the argument list.
-- Instead it's looked up it the environ according to the normal defaulting
-- rules.
--
-- Of course, the call could just look in the environ itself, but this way it's
-- uniform and automatically documented.
environ :: forall a. (Typecheck.Typecheck a, ShowVal.ShowVal a) =>
    ArgName -> Derive.EnvironDefault
    -- ^ None doesn't make any sense, but, well, don't pass that then.
    -> a -> Doc.Doc -> Parser a
environ name env_default = environ_ name env_default . Left

-- | This is like 'environ', but the default is a 'BaseTypes.Quoted', which
-- will be evaluated if needed.
environ_quoted :: forall a. (Typecheck.Typecheck a, ShowVal.ShowVal a) =>
    ArgName -> Derive.EnvironDefault -> BaseTypes.Quoted -> Doc.Doc -> Parser a
environ_quoted name env_default = environ_ name env_default . Right

-- Internal function that handles both quoted and unquoted default.
environ_ :: forall a. (Typecheck.Typecheck a, ShowVal.ShowVal a) =>
    ArgName -> Derive.EnvironDefault
    -- ^ None doesn't make any sense, but, well, don't pass that then.
    -> Either a BaseTypes.Quoted -> Doc.Doc -> Parser a
environ_ name env_default quoted doc = parser arg_doc $ \state ->
    case lookup_default env_default state name of
        Nothing -> deflt state
        Just val -> (,) state <$>
            check_arg state arg_doc (environ_error state name) name val
    where
    deflt state = eval_default arg_doc (argnum_error state) name state quoted
    expected = Typecheck.to_type (Proxy :: Proxy a)
    arg_doc = Derive.ArgDoc
        { arg_name = name
        , arg_type = expected
        , arg_parser = Derive.Environ $ Just $
            either ShowVal.show_val ShowVal.show_val quoted
        , arg_environ_default = env_default
        , arg_doc = doc
        }

-- | This is like 'environ', but without a default.
required_environ :: forall a. Typecheck.Typecheck a =>
    ArgName -> Derive.EnvironDefault -> Doc.Doc -> Parser a
required_environ name env_default doc = parser arg_doc $ \state ->
    case lookup_default env_default state name of
        Nothing -> Left $ Derive.TypeError (environ_error state name)
            Derive.Literal name expected Nothing Nothing
        Just val -> (,) state <$>
            check_arg state arg_doc (environ_error state name) name val
    where
    expected = Typecheck.to_type (Proxy :: Proxy a)
    arg_doc = Derive.ArgDoc
        { arg_name = name
        , Derive.arg_type = expected
        , Derive.arg_parser = Derive.Environ Nothing
        , Derive.arg_environ_default = env_default
        , arg_doc = doc
        }

-- | This is like 'defaulted', but if the argument is the wrong type return
-- the default instead of failing.  It's mostly useful with 'many' or 'many1',
-- where you can distinguish the arguments by type.
optional :: forall a. (Typecheck.Typecheck a, ShowVal.ShowVal a) => ArgName -> a
    -> Doc.Doc -> Parser a
optional name = optional_env name Derive.Prefixed

optional_env :: forall a. (Typecheck.Typecheck a, ShowVal.ShowVal a) =>
    ArgName -> Derive.EnvironDefault -> a -> Doc.Doc -> Parser a
optional_env name env_default deflt doc = parser arg_doc $ \state1 ->
    case get_val env_default state1 name of
        Nothing -> Right (state1, deflt)
        Just (state, BaseTypes.VNotGiven) -> Right (state, deflt)
        Just (state, val) ->
            case check_arg state arg_doc (argnum_error state1) name val of
                Right a -> Right (state, a)
                Left (Derive.TypeError {}) ->
                    case lookup_default env_default state name of
                        Nothing -> Right (state, deflt)
                        Just val -> (,) state <$>
                            check_arg state arg_doc (argnum_error state1)
                                name val
                Left err -> Left err
    where
    expected = Typecheck.to_type (Proxy :: Proxy a)
    arg_doc = Derive.ArgDoc
        { arg_name = name
        , arg_type = expected
        , arg_parser = Derive.Optional (ShowVal.show_val deflt)
        , arg_environ_default = env_default
        , arg_doc = doc
        }

-- | Collect the rest of the arguments.
many :: forall a. Typecheck.Typecheck a => ArgName -> Doc.Doc -> Parser [a]
many name doc = parser arg_doc $ \state -> do
    vals <- mapM (typecheck state)
        (zip [state_argnum state ..] (state_vals state))
    let state2 = increment_argnum (length (state_vals state)) $
            state { state_vals = []}
    Right (state2, vals)
    where
    typecheck state (argnum, val) =
        check_arg state arg_doc (Derive.TypeErrorArg argnum) name val
    expected = Typecheck.to_type (Proxy :: Proxy a)
    arg_doc = Derive.ArgDoc
        { arg_name = name
        , arg_type = expected
        , arg_parser = Derive.Many
        , arg_environ_default = Derive.None
        , arg_doc = doc
        }

-- | 'many' specialized to Vals, to avoid a type annotation.
many_vals :: ArgName -> Doc.Doc -> Parser [BaseTypes.Val]
many_vals name doc = many name doc

-- | Collect the rest of the arguments, but there must be at least one.
many1 :: forall a. Typecheck.Typecheck a => ArgName -> Doc.Doc
    -> Parser (NonEmpty a)
many1 name doc = non_empty name $ many name doc

-- | Collect the rest of the arguments, but expect a even number of them and
-- pair them up.
many_pairs :: forall a b. (Typecheck.Typecheck a,  Typecheck.Typecheck b) =>
    ArgName -> Doc.Doc -> Parser [(a, b)]
many_pairs name doc = parser (arg_doc expected) $ \state -> do
    let vals = state_vals state
    when (odd (length vals)) $
        Left $ Derive.ArgError $ "many_pairs requires an even argument length: "
            <> showt (length vals)
    vals <- mapM (typecheck state) $ zip [state_argnum state ..] (pairs vals)
    let state2 = increment_argnum (length vals) $ state { state_vals = [] }
    Right (state2, vals)
    where
    typecheck state (argnum, (val1, val2)) = (,)
        <$> check_arg state (arg_doc (Typecheck.to_type (Proxy :: Proxy a)))
            (Derive.TypeErrorArg (argnum * 2)) name val1
        <*> check_arg state (arg_doc (Typecheck.to_type (Proxy :: Proxy b)))
            (Derive.TypeErrorArg (argnum * 2 + 1)) name val2
    expected = ValType.TPair (Typecheck.to_type (Proxy :: Proxy a))
        (Typecheck.to_type (Proxy :: Proxy b))
    arg_doc expected = Derive.ArgDoc
        { arg_name = name
        , arg_type = expected
        , arg_parser = Derive.Many
        , arg_environ_default = Derive.None
        , arg_doc = doc
        }
    pairs (a : b : xs) = (a, b) : pairs xs
    pairs _ = []

-- | Like 'many_pairs', but require at least one pair.
many1_pairs :: forall a b. (Typecheck.Typecheck a,  Typecheck.Typecheck b) =>
    ArgName -> Doc.Doc -> Parser (NonEmpty (a, b))
many1_pairs name doc = non_empty name $ many_pairs name doc

-- | Modify a 'many' parser to require at least one thing.
non_empty :: ArgName -> Parser [a] -> Parser (NonEmpty a)
non_empty name (Parser docs p) =
    Parser (map (\d -> d { Derive.arg_parser = Derive.Many1 }) docs) convert
    where
    convert state = case p state of
        Left err -> Left err
        Right (_, []) -> Left $ Derive.ArgError $
            "arg requires at least one value: " <> pretty name
        Right (state, x : xs) -> Right (state, x :| xs)

-- | Require one Val for each ArgDoc given, but otherwise do no typechecking.
required_vals :: [Derive.ArgDoc] -> Parser [BaseTypes.Val]
required_vals docs = Parser docs parser
    where
    -- I don't check the number of arguments because this is used for call
    -- macros, and I need to give the sub-call a chance to default its args.
    parser state = Right (state2, vals)
        where
        (vals, rest) = splitAt (length docs) (state_vals state)
        state2 = increment_argnum (length vals) $ state { state_vals = rest}

argnum_error :: State -> Derive.ErrorPlace
argnum_error = Derive.TypeErrorArg . state_argnum

environ_error :: State -> ArgName -> Derive.ErrorPlace
environ_error state name =
    Derive.TypeErrorEnviron (prefixed_environ (state_call_name state) name)

increment_argnum :: Int -> State -> State
increment_argnum n state = state { state_argnum = n + state_argnum state }

-- ** defaults

-- | The argument's value is taken from the given signal, with the given
-- default.  If the value isn't given, the default is Untyped.
control :: Score.Control -> Signal.Y -> BaseTypes.ControlRef
control name deflt = typed_control name deflt Score.Untyped

-- | Like 'control', but the default can have a type.
typed_control :: Score.Control -> Signal.Y -> Score.Type -> BaseTypes.ControlRef
typed_control name deflt typ =
    BaseTypes.DefaultedControl name (Score.Typed typ (Signal.constant deflt))

required_control :: Score.Control -> BaseTypes.ControlRef
required_control = BaseTypes.LiteralControl

-- | Pitch signal.  There's no default because that would depend on the scale.
pitch :: Score.PControl -> BaseTypes.PControlRef
pitch = BaseTypes.LiteralControl

-- ** util

get_val :: Derive.EnvironDefault -> State -> ArgName
    -> Maybe (State, BaseTypes.Val)
get_val env_default state name = case state_vals state of
    [] -> (,) next <$> lookup_default env_default state name
    v : vs -> Just (next { state_vals = vs }, case v of
        BaseTypes.VNotGiven -> fromMaybe BaseTypes.VNotGiven $
            lookup_default env_default state name
        _ -> v)
    where next = increment_argnum 1 state

check_arg :: forall a. Typecheck.Typecheck a => State -> Derive.ArgDoc
    -> Derive.ErrorPlace -> ArgName -> BaseTypes.Val -> Either Error a
check_arg state arg_doc place name val =
    maybe from_quoted Right =<<
        promote_error Derive.Literal val (from_val state val)
    where
    -- 'val' failed to typecheck, so try to coerce a Quoted to a new qval and
    -- typecheck that.
    from_quoted = case val of
        BaseTypes.VQuoted quoted -> do
            let source = Derive.Quoted quoted
            qval <- promote_error source val $ eval_quoted state quoted
            maybe_a <- promote_error source qval $ from_val state qval
            case maybe_a of
                Nothing -> Left $ type_error source qval Nothing
                Just a -> Right a
        _ -> Left $ type_error Derive.Literal val Nothing
    promote_error source val = first (type_error source val . Just)
    type_error source val = Derive.TypeError place source name
        (Derive.arg_type arg_doc) (Just val)

-- | Typecheck a Val, evaluating if necessary.
from_val :: Typecheck.Typecheck a => State -> BaseTypes.Val
    -> Either Derive.Error (Maybe a)
from_val state val = run state $ Typecheck.from_val_eval start val
    where start = Event.start $ Derive.ctx_event $ state_context state

run :: State -> Derive.Deriver a -> Either Derive.Error a
run state deriver = result
    where
    (result, _state, _logs) = Derive.run (state_derive state) deriver

eval_quoted :: State -> BaseTypes.Quoted -> Either Derive.Error BaseTypes.Val
eval_quoted state (BaseTypes.Quoted expr) = result
    where
    (result, _state, _logs) = Derive.run (state_derive state) $ do
        call <- case expr of
            call :| [] -> return call
            _ -> Derive.throw "expected a val call, but got a full expression"
        Eval.eval (state_context state) (BaseTypes.ValCall call)

lookup_default :: Derive.EnvironDefault -> State -> ArgName
    -> Maybe BaseTypes.Val
lookup_default env_default state name =
    msum $ map lookup $ environ_keys (state_call_name state) name env_default
    where lookup key = Env.lookup key (state_environ state)

environ_keys :: CallName -> ArgName -> Derive.EnvironDefault -> [Env.Key]
environ_keys call_name arg_name env_default = case env_default of
    Derive.None -> []
    Derive.Prefixed -> [prefixed]
    Derive.Unprefixed -> [unprefixed]
    Derive.Both -> [unprefixed, prefixed]
    where
    prefixed = prefixed_environ call_name arg_name
    unprefixed = BaseTypes.Symbol ((\(Derive.ArgName n) -> n) arg_name)

prefixed_environ :: CallName -> ArgName -> Env.Key
prefixed_environ (Derive.CallName call_name) (Derive.ArgName arg_name) =
    BaseTypes.Symbol $ call_name <> "-" <> arg_name


-- * call

-- | Similar to 'Derive.GeneratorF', but leaves the PassedArgs prev val
-- type free.  This is important for val calls, which used Tagged.
type Generator y d = Derive.PassedArgs y -> Derive.Deriver d
type Transformer y d =
    Derive.PassedArgs y -> Derive.Deriver d -> Derive.Deriver d

call :: Derive.Taggable y => Parser a -> (a -> Generator y d)
    -> Derive.WithArgDoc (Generator y d)
call parser f = (go, parser_docs parser)
    where go args = parse parser args >>= require_right >>= \a -> f a args

-- | Specialization of 'call' for 0 arguments.
call0 :: Derive.Taggable y => Generator y d -> Derive.WithArgDoc (Generator y d)
call0 f = (go, [])
    where go args = parse no_args args >>= require_right >>= \() -> f args

callt :: Derive.Taggable y => Parser a -> (a -> Transformer y d)
    -> Derive.WithArgDoc (Transformer y d)
callt parser f = (go, parser_docs parser)
    where
    go args deriver = parse parser args >>= require_right
        >>= \a -> f a args deriver

-- | Specialization of 'callt' for 0 arguments.
call0t :: Derive.Taggable y => Transformer y d
    -> Derive.WithArgDoc (Transformer y d)
call0t f = (go, [])
    where
    go args deriver = parse (pure ()) args >>= require_right
        >>= \() -> f args deriver

state_environ :: State -> BaseTypes.Environ
state_environ = Derive.state_environ . Derive.state_dynamic . state_derive
