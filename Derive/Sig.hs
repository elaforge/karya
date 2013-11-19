-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
{- | Functions to help define call signatures.

    This module, along with the 'TrackLang.Typecheck' class, define a little
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

    In addition, an arg may be a 'TrackLang.PitchControl' or
    'TrackLang.ValControl', which introduces yet another way to provide the
    value.  An argument @required_control \"c\"@ will pass
    a 'TrackLang.LiteralControl'.  Technically it's then up to the call to
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

    This module is split off from "Derive.TrackLang" because it needs
    'Derive.PassedArgs' and Derive already imports TrackLang.  It could be in
    "Derive.Call" but Call is already big so let's leave it separate for now.
-}
module Derive.Sig (
    Parser, Generator, Transformer
    -- * parsers
    , parsed_manually, no_args
    , required, required_env, defaulted, defaulted_env
    , environ, required_environ
    , optional, optional_env, many, many1
    -- ** defaults
    , EnvironDefault(..)
    , control, typed_control, required_control, pitch
    , prefixed_environ, environ_keys
    -- * call
    , call, call0, callt, call0t
    -- * misc
    , cast, modify_vcall
) where
import qualified Control.Applicative as Applicative
import qualified Data.Text as Text

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Derive.Derive as Derive
import Derive.Derive (EnvironDefault(..))
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal


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
    -- | Pairs of the arg number, starting at 0, and the argument.
    state_vals :: ![TrackLang.Val]
    -- | This has to be incremented every time a Val is taken.  Pairing argnums
    -- in state_vals doesn't work because when I run out I don't know where
    -- I am.
    , state_argnum :: !Int
    , state_environ :: !TrackLang.Environ
    , state_call_name :: !Text
    } deriving (Show)

run :: Parser a -> State -> Either Error a
run parser state = case parser_parser parser state of
    Right (state, a) -> case state_vals state of
        [] -> Right a
        vals -> Left $ Derive.ArgError $ "too many arguments: "
            <> Text.intercalate ", " (map ShowVal.show_val vals)
    Left err -> Left err

instance Functor Parser where
    fmap f parser =
        parser { parser_parser = fmap (fmap f) . parser_parser parser }

instance Applicative.Applicative Parser where
    pure a = Parser mempty (\vals -> Right (vals, a))
    Parser doc1 parse1 <*> Parser doc2 parse2 =
        Parser (doc1 <> doc2) $ \vals -> do
            (vals, f) <- parse1 vals
            (vals, a) <- parse2 vals
            Right (vals, f a)

-- * parsers

-- | Just pass the arguments through, and let the call process them.
-- This is for calls whose args are more complicated than the Parser can
-- handle.
parsed_manually :: Text -> a -> Derive.WithArgDoc a
parsed_manually doc f = (f, Derive.ArgsParsedSpecially doc)

-- | Parser for nullary calls.  Either use this with 'call' and 'callt', or use
-- 'call0' and 'call0t' as a shortcut.
no_args :: Parser ()
no_args = Applicative.pure ()

-- | The argument is required to be present, and have the right type.
required :: forall a. (TrackLang.Typecheck a) => Text -> Text -> Parser a
required name = required_env name Derive.Prefixed

required_env :: forall a. (TrackLang.Typecheck a) => Text
    -> Derive.EnvironDefault -> Text -> Parser a
required_env name env_default doc = parser arg_doc $ \state ->
    case get_val env_default state name of
        Nothing -> Left $ Derive.ArgError $
            "expected another argument at: " <> showt name
        Just (state2, val) -> case TrackLang.from_val val of
            Just a -> Right (state2, a)
            Nothing -> Left $ Derive.TypeError (arg_error state) name expected
                (Just val)
    where
    expected = TrackLang.to_type (error "Sig.required" :: a)
    arg_doc = Derive.ArgDoc
        { Derive.arg_name = name
        , Derive.arg_type = expected
        , Derive.arg_parser = Derive.Required
        , Derive.arg_environ_default = env_default
        , Derive.arg_doc = doc
        }

-- | The argument is not required to be present, but if it is, it has to have
-- either the right type or be VNotGiven.
defaulted :: forall a. (TrackLang.Typecheck a) => Text -> a -> Text -> Parser a
defaulted name = defaulted_env name Derive.Prefixed

defaulted_env :: forall a. (TrackLang.Typecheck a) => Text
    -> Derive.EnvironDefault -> a -> Text -> Parser a
defaulted_env name env_default deflt doc = parser arg_doc $ \state ->
    case get_val env_default state name of
        Nothing -> Right (state, deflt)
        Just (state2, TrackLang.VNotGiven) -> Right (state2, deflt)
        Just (state2, val) -> case TrackLang.from_val val of
            Just a -> Right (state2, a)
            Nothing -> Left $ Derive.TypeError (arg_error state) name expected
                (Just val)
    where
    expected = TrackLang.to_type deflt
    arg_doc = Derive.ArgDoc
        { Derive.arg_name = name
        , Derive.arg_type = expected
        , Derive.arg_parser = Derive.Defaulted (ShowVal.show_val deflt)
        , Derive.arg_environ_default = env_default
        , Derive.arg_doc = doc
        }

-- | This is a phantom argument which is not actually parsed from the argument
-- list.  Instead it's looked up it the environ according to the normal
-- defaulting rules.
--
-- Of course, the call could just look in the environ itself, but this way it's
-- uniform and automatically documented.
environ :: forall a. (TrackLang.Typecheck a) => Text -> Derive.EnvironDefault
    -- ^ None doesn't make any sense, but, well, don't pass that then.
    -> a -> Text -> Parser a
environ name env_default deflt doc = parser arg_doc $ \state ->
    case lookup_default env_default state name of
        Nothing -> Right (state, deflt)
        Just val -> case TrackLang.from_val val of
            Nothing -> Left $ Derive.TypeError (environ_error state name)
                name expected (Just val)
            Just a -> Right (state, a)
    where
    expected = TrackLang.to_type deflt
    arg_doc = Derive.ArgDoc
        { Derive.arg_name = name
        , Derive.arg_type = expected
        , Derive.arg_parser = Derive.Environ (Just (ShowVal.show_val deflt))
        , Derive.arg_environ_default = env_default
        , Derive.arg_doc = doc
        }

-- | This is like 'environ', but without a default.
required_environ :: forall a. (TrackLang.Typecheck a) =>
    Text -> Derive.EnvironDefault -> Text -> Parser a
required_environ name env_default doc = parser arg_doc $ \state ->
    case lookup_default env_default state name of
        Nothing -> Left $ Derive.TypeError (environ_error state name)
            name expected Nothing
        Just val -> case TrackLang.from_val val of
            Nothing -> Left $ Derive.TypeError (environ_error state name)
                name expected (Just val)
            Just a -> Right (state, a)
    where
    expected = TrackLang.to_type (error "Sig.required_environ" :: a)
    arg_doc = Derive.ArgDoc
        { Derive.arg_name = name
        , Derive.arg_type = expected
        , Derive.arg_parser = Derive.Environ Nothing
        , Derive.arg_environ_default = env_default
        , Derive.arg_doc = doc
        }

-- | This is like 'defaulted', but if the argument is the wrong type return
-- Nothing instead of failing.  It's mostly useful with 'many' or 'many1',
-- because otherwise a 'defaulted' is less confusing.
optional :: forall a. (TrackLang.Typecheck a) => Text -> Text
    -> Parser (Maybe a)
optional name = optional_env name Derive.Prefixed

optional_env :: forall a. (TrackLang.Typecheck a) =>
    Text -> Derive.EnvironDefault -> Text -> Parser (Maybe a)
optional_env name env_default doc = parser arg_doc $ \state ->
    case get_val env_default state name of
        Nothing -> Right (state, Nothing)
        Just (state2, TrackLang.VNotGiven) -> Right (state2, Nothing)
        Just (state2, val) -> case TrackLang.from_val val of
            Just a -> Right (state2, Just a)
            Nothing -> case lookup_default env_default state name of
                Nothing -> Right (state, Nothing)
                Just val -> case TrackLang.from_val val of
                    Nothing -> Left $ Derive.TypeError
                        (arg_error state) name expected (Just val)
                    Just a -> Right (state, Just a)
    where
    expected = TrackLang.to_type (error "Sig.optional" :: a)
    arg_doc = Derive.ArgDoc
        { Derive.arg_name = name
        , Derive.arg_type = expected
        , Derive.arg_parser = Derive.Optional
        , Derive.arg_environ_default = env_default
        , Derive.arg_doc = doc
        }

-- | Collect the rest of the arguments.
many :: forall a. (TrackLang.Typecheck a) => Text -> Text -> Parser [a]
many name doc = parser arg_doc $ \state -> do
    vals <- mapM typecheck (zip [state_argnum state ..] (state_vals state))
    Right (state { state_vals = [] }, vals)
    where
    typecheck (argnum, val) = case TrackLang.from_val val of
        Just a -> Right a
        Nothing -> Left $ Derive.TypeError (Derive.TypeErrorArg argnum) name
            expected (Just val)
    expected = TrackLang.to_type (error "Sig.many" :: a)
    arg_doc = Derive.ArgDoc
        { Derive.arg_name = name
        , Derive.arg_type = expected
        , Derive.arg_parser = Derive.Many
        , Derive.arg_environ_default = Derive.None
        , Derive.arg_doc = doc
        }

-- | Collect the rest of the arguments, but there must be at least one.
many1 :: forall a. (TrackLang.Typecheck a) => Text -> Text
    -> Parser (NonEmpty a)
many1 name doc = parser arg_doc $ \state ->
    case zip [state_argnum state ..] (state_vals state) of
        [] -> Left $
            Derive.ArgError $ "many1 arg requires at least one value: " <> name
        v : vs -> do
            v <- typecheck v
            vs <- mapM typecheck vs
            Right (state { state_vals = [] }, v :| vs)
    where
    typecheck (argnum, val) = case TrackLang.from_val val of
        Just a -> Right a
        Nothing -> Left $ Derive.TypeError (Derive.TypeErrorArg argnum) name
            expected (Just val)
    expected = TrackLang.to_type (error "Sig.many1" :: a)
    arg_doc = Derive.ArgDoc
        { Derive.arg_name = name
        , Derive.arg_type = expected
        , Derive.arg_parser = Derive.Many1
        , Derive.arg_environ_default = Derive.None
        , Derive.arg_doc = doc
        }

arg_error :: State -> Derive.ErrorPlace
arg_error = Derive.TypeErrorArg . state_argnum

environ_error :: State -> Text -> Derive.ErrorPlace
environ_error state name =
    Derive.TypeErrorEnviron (prefixed_environ (state_call_name state) name)

-- ** defaults

-- | The argument's value is taken from the given signal, with the given
-- default.  If the value isn't given, the default is Untyped.
control :: Score.Control -> Signal.Y -> TrackLang.ValControl
control name deflt = typed_control name deflt Score.Untyped

-- | Like 'control', but the default can have a type.
typed_control :: Score.Control -> Signal.Y -> Score.Type -> TrackLang.ValControl
typed_control name deflt typ =
    TrackLang.DefaultedControl name (Score.Typed typ (Signal.constant deflt))

required_control :: Score.Control -> TrackLang.ValControl
required_control = TrackLang.LiteralControl

-- | Pitch signal.  There's no default because that would depend on the scale.
pitch :: Score.Control -> TrackLang.PitchControl
pitch = TrackLang.LiteralControl

-- ** util

get_val :: Derive.EnvironDefault -> State -> Text
    -> Maybe (State, TrackLang.Val)
get_val env_default state name = case state_vals state of
    [] -> (,) next <$> lookup_default env_default state name
    v : vs -> Just (next { state_vals = vs }, case v of
        TrackLang.VNotGiven -> fromMaybe TrackLang.VNotGiven $
            lookup_default env_default state name
        _ -> v)
    where next = state { state_argnum = state_argnum state + 1 }

lookup_default :: Derive.EnvironDefault -> State -> Text -> Maybe TrackLang.Val
lookup_default env_default state name =
    msum $ map lookup $ environ_keys (state_call_name state) name env_default
    where lookup key = TrackLang.lookup_val key (state_environ state)

prefixed_environ :: Text -> Text -> TrackLang.ValName
prefixed_environ call_name arg_name =
    TrackLang.Symbol $ call_name <> "-" <> arg_name

environ_keys :: Text -> Text -> Derive.EnvironDefault -> [TrackLang.ValName]
environ_keys call_name arg_name env_default = case env_default of
    Derive.None -> []
    Derive.Prefixed -> [prefixed]
    Derive.Unprefixed -> [unprefixed]
    Derive.Both -> [prefixed, unprefixed]
    where
    prefixed = prefixed_environ call_name arg_name
    unprefixed = TrackLang.Symbol arg_name


-- * call

-- | Similar to 'Derive.GeneratorFunc', but leaves the PassedArgs prev val
-- type free.  This is important for val calls, which used Tagged.
type Generator y d = Derive.PassedArgs y -> Derive.Deriver d
type Transformer y d =
    Derive.PassedArgs y -> Derive.Deriver d -> Derive.Deriver d

call :: Parser a -> (a -> Generator y d) -> Derive.WithArgDoc (Generator y d)
call parser f = (go, Derive.ArgDocs (parser_docs parser))
    where go args = run_call parser args >>= \a -> f a args

-- | Specialization of 'call' for 0 arguments.
call0 :: Generator y d -> Derive.WithArgDoc (Generator y d)
call0 f = (go, Derive.ArgDocs [])
    where go args = run_call no_args args >>= \() -> f args

callt :: Parser a -> (a -> Transformer y d)
    -> Derive.WithArgDoc (Transformer y d)
callt parser f = (go, Derive.ArgDocs (parser_docs parser))
    where go args deriver = run_call parser args >>= \a -> f a args deriver

-- | Specialization of 'callt' for 0 arguments.
call0t :: Transformer y d -> Derive.WithArgDoc (Transformer y d)
call0t f = (go, Derive.ArgDocs [])
    where
    go args deriver = run_call (Applicative.pure ()) args >>= \() ->
        f args deriver

run_call :: Parser a -> Derive.PassedArgs d -> Derive.Deriver a
run_call parser args = do
    environ <- Derive.gets (Derive.state_environ . Derive.state_dynamic)
    case run parser (make_state args environ) of
        Left err -> Derive.throw_error (Derive.CallError err)
        Right a -> return a
    where
    make_state args environ = State
        { state_vals = Derive.passed_vals args
        , state_argnum = 0
        , state_environ = environ
        , state_call_name = Derive.passed_call_name args
        }


-- * misc

-- | Cast a Val to a haskell val, or throw if it's the wrong type.
cast :: forall a. (TrackLang.Typecheck a) => Text -> TrackLang.Val
    -> Derive.Deriver a
cast name val = case TrackLang.from_val val of
        Nothing -> Derive.throw $ untxt $
            name <> ": expected " <> Pretty.prettytxt return_type
            <> " but val was " <> Pretty.prettytxt (TrackLang.type_of val)
            <> " " <> TrackLang.show_val val
        Just a -> return a
    where return_type = TrackLang.to_type (error "cast" :: a)

-- | Make a new ValCall from an existing one, by mapping over its output.
modify_vcall :: Derive.ValCall -> Text -> Text
    -> (TrackLang.Val -> TrackLang.Val) -> Derive.ValCall
modify_vcall vcall name doc f = vcall
    { Derive.vcall_name = name
    , Derive.vcall_doc = Derive.CallDoc
        { Derive.cdoc_tags = Derive.cdoc_tags (Derive.vcall_doc vcall)
        , Derive.cdoc_doc = doc
        , Derive.cdoc_args = Derive.cdoc_args (Derive.vcall_doc vcall)
        }
    , Derive.vcall_call = fmap f . Derive.vcall_call vcall
    }
