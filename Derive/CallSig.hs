{-# LANGUAGE ScopedTypeVariables #-}
{- | Functions to help define call signatures.

    This module, along with the 'TrackLang.Typecheck' class, define a little
    DSL to express function signatures.  Check existing calls for examples.

    Argument passing, in an effort to be flexible, got a bit complicated.  Each
    'Arg' has a name and a possible default.  So right off there are three ways
    to provide an argument:

    1. Pass it explicitly.

    2. If it is omitted, or @_@ is passed explicitly, it will be sought in the
    dynamic environ, under the name @<call_name>-<arg_name>@.  E.g. given
    a call @generator "name" $ \args -> call1 (required "arg1") ...@ then
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
    a gloriously complicated argument:
    @optional \"speed\" (typed_control \"tremolo-speed\" 10 Score.Real)@.  This
    argument defaults to @%tremolo-speed,10s@.  If it's not given, it will
    have the value @10s@.  If the @%tremolo-speed@ control is in scope but
    untyped, its values will be interpreted as RealTime.  If it's in scope
    and typed (e.g. with a @tremolo-speed:t@ track), then its values will
    be interpreted as ScoreTime.

    Another wrinkle in argument passing is that, in addition to being
    'required', which has no default, or being 'optional', which has a default,
    they can be 'optional' with a default of Nothing.  This passes the argument
    as a @Maybe a@ instead of @a@ and lets the call distinguish whether an
    argument was provided or not.  This is for arguments which are optional but
    need a more complicated defaulting strategy than simply a constant.

    This module is split off from "Derive.TrackLang" because it needs
    'Derive.PassedArgs' and Derive already imports TrackLang.  It could be in
    "Derive.Call" but Call is already big so let's leave it separate for now.
-}
module Derive.CallSig where
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Derive as Derive
import Derive.Derive (PassedArgs(..))
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang (Typecheck)

import qualified Perform.Signal as Signal


-- * signatures

-- | A single argument in the signature of a call.
data Arg a = Arg {
    -- | An arg that is not explicitly given will be looked for in the dynamic
    -- environment as \<callname>-\<argname>.  Of course control args get this
    -- already through the control map, but this way non control args can be
    -- defaulted, or you can default a control arg to a constant without going
    -- to the bother of making a track for it.
    arg_name :: String
    , arg_doc_ :: String
    , arg_default :: Maybe a
    } deriving (Eq, Show)

arg_type :: (Typecheck a) => Arg a -> TrackLang.Type
arg_type arg = TrackLang.to_type $
    fromMaybe (error "arg_type shouldn't evaluate this") (arg_default arg)

arg_environ_default :: String -> String -> TrackLang.ValName
arg_environ_default call_name arg_name =
    TrackLang.Symbol $ call_name ++ "-" ++ arg_name

arg_required :: Arg a -> (Bool, String)
arg_required arg = (Maybe.isNothing (arg_default arg), arg_name arg)

arg_doc :: (Typecheck a) => Arg a -> Derive.ArgDoc
arg_doc arg = Derive.ArgDoc
    { Derive.arg_name = arg_name arg
    , Derive.arg_type = arg_type arg
    , Derive.arg_default = TrackLang.show_val <$> arg_default arg
    , Derive.arg_doc = arg_doc_ arg
    }

-- Utilities to describe function signatures.

-- | Required argument with the given name.
required :: String -> String -> Arg a
required name doc = Arg name doc Nothing

-- | This requires a default value.  However, if you give the default as
-- Nothing, this will select the Maybe instance of Typecheck, and the resulting
-- value will be Nothing if the arg wasn't given or Just if it was.
--
-- TODO
-- This is sort of an ugly way to implement optional args with no default
-- because it admits the possibility of required Maybe args or even Maybe
-- (Maybe ...) args, none of which mean anything interesting.  A more
-- satisfying implementation would admit only
-- @Required a | Defaulted a | Optional (Maybe a)@, but I'm not sure how to
-- make that work with Typecheck.
optional :: String -> a -> String -> Arg a
optional name deflt doc = Arg name doc (Just deflt)

-- | The argument's value is taken from the given signal, with the given
-- default.  If the value isn't given, the default is Untyped.
control :: String -> Signal.Y -> TrackLang.ValControl
control name deflt = typed_control name deflt Score.Untyped

-- | Like 'control', but the default can have a type.
typed_control :: String -> Signal.Y -> Score.Type -> TrackLang.ValControl
typed_control name deflt typ =
    TrackLang.DefaultedControl (Score.Control name) (Score.Typed typ deflt)

required_control :: String  -> TrackLang.ValControl
required_control name = TrackLang.LiteralControl (Score.Control name)

-- * modify calls

-- | Make a new ValCall from an existing one, by mapping over its output.
modify_vcall :: Derive.ValCall -> String -> String
    -> (TrackLang.Val -> TrackLang.Val) -> Derive.ValCall
modify_vcall vcall name doc f = vcall
    { Derive.vcall_name = name
    , Derive.vcall_doc = Derive.CallDoc doc
        (Derive.cdoc_args (Derive.vcall_doc vcall))
    , Derive.vcall_call = fmap f . (Derive.vcall_call vcall)
    }

-- * extract and call

{- Here's what a typeclass based approach could look like.  However, I don't
    feel like writing call1, call2, etc. is that big of a problem, so the
    typeclass is probably not worth it, especially since it makes the zero and
    one arg cases a bit less graceful:

    class Call a where
        type ArgsOf a :: *
        call :: PassedArgs d -> ArgsOf a -> (a -> Derive.Deriver result)
            -> Derive.Deriver result

    instance Call () where
        type ArgsOf () = ()
        call vals () f = call0 vals (f ())

    newtype S a = S a
    instance (Typecheck a) => Call (S a) where
        type ArgsOf (S a) = Arg a
        call vals arg1 f = call1 vals arg1 $ \v1 -> f (S v1)

    instance (Typecheck a, Typecheck b) => Call (a, b) where
        type ArgsOf (a, b) = (Arg a, Arg b)
        call vals (arg1, arg2) f = call2 vals (arg1, arg2) $
            \v1 v2 -> f (v1, v2)
-}


-- | Just pass the arguments through, and let the call process them.
parsed_manually :: String -> a -> Derive.WithArgDoc a
parsed_manually doc f = (f, Derive.ArgsParsedSpecially doc)

with_doc :: [Derive.ArgDoc] -> a -> Derive.WithArgDoc a
with_doc docs f = (f, Derive.ArgDocs docs)

type Generator y result = Derive.PassedArgs y -> Derive.Deriver result
type Transformer y result =
    Derive.PassedArgs y -> Derive.Deriver result -> Derive.Deriver result

-- 0

-- | Call a 0 argument generator or val call.
--
-- Args are checked and defaulted with a series of functions.  The @g@ variants
-- pass only the args to the underlying call, and are for generators and val
-- calls.  The @t@ variants pass both args and a deriver, for transformers.
--
-- Previously these functions just took args to the parsed out values, so they
-- didn't have to care about other arguments, but since they now have to
-- propagate documentation up to the call constructor we can't have any lambdas
-- in the way.  I tried numerous ways to keep them from having to pass in
-- the PassedArgs and deriver themselves, but it seemed like if there was such
-- a way it would be complicated.  So just pick the right function.
--
-- The pattern is @call#g (arg, arg, ...) $ \arg arg ... args -> ...@
-- or @call#t (arg, arg, ...) $ \arg arg ... args deriver -> ...@.
call0g :: Generator y result -> Derive.WithArgDoc (Generator y result)
call0g f = with_doc [] $ \vals -> check_args vals [] >> f vals

-- | Call a 0 argument transformer.
call0t :: Transformer y result -> Derive.WithArgDoc (Transformer y result)
call0t f = with_doc [] $ \vals deriver -> check_args vals [] >> f vals deriver

-- 1

call1g :: (Typecheck a) => Arg a -> (a -> Generator y result)
    -> Derive.WithArgDoc (Generator y result)
call1g arg1 f = with_doc [arg_doc arg1] $ \vals -> do
    val1 <- extract1 vals arg1
    f val1 vals

call1t :: (Typecheck a) => Arg a -> (a -> Transformer y result)
    -> Derive.WithArgDoc (Transformer y result)
call1t arg1 f = with_doc [arg_doc arg1] $
    \vals deriver -> do
        val1 <- extract1 vals arg1
        f val1 vals deriver

extract1 :: (Typecheck a) => PassedArgs y -> Arg a -> Derive.Deriver a
extract1 vals sig0 = do
    arg0 : _ <- check_args vals [arg_required sig0]
    extract_arg 0 sig0 arg0

-- 2

call2g :: (Typecheck a, Typecheck b) =>
    (Arg a, Arg b) -> (a -> b -> Generator y result)
    -> Derive.WithArgDoc (Generator y result)
call2g (arg1, arg2) f = with_doc [arg_doc arg1, arg_doc arg2] $ \vals -> do
    (val1, val2) <- extract2 vals (arg1, arg2)
    f val1 val2 vals

call2t :: (Typecheck a, Typecheck b) => (Arg a, Arg b)
    -> (a -> b -> Transformer y result)
    -> Derive.WithArgDoc (Transformer y result)
call2t (arg1, arg2) f = with_doc [arg_doc arg1, arg_doc arg2] $
    \vals deriver -> do
        (val1, val2) <- extract2 vals (arg1, arg2)
        f val1 val2 vals deriver

extract2 :: (Typecheck a, Typecheck b) =>
    PassedArgs y -> (Arg a, Arg b) -> Derive.Deriver (a, b)
extract2 vals (sig0, sig1) = do
    arg0 : arg1 : _ <- check_args vals [arg_required sig0, arg_required sig1]
    (,) <$> extract_arg 0 sig0 arg0 <*> extract_arg 1 sig1 arg1

-- 3

call3g :: (Typecheck a, Typecheck b, Typecheck c) =>
    (Arg a, Arg b, Arg c) -> (a -> b -> c -> Generator y result)
    -> Derive.WithArgDoc (Generator y result)
call3g (arg1, arg2, arg3) f =
    with_doc [arg_doc arg1, arg_doc arg2, arg_doc arg3] $ \vals -> do
        (val1, val2, val3) <- extract3 vals (arg1, arg2, arg3)
        f val1 val2 val3 vals

call3t :: (Typecheck a, Typecheck b, Typecheck c) => (Arg a, Arg b, Arg c)
    -> (a -> b -> c -> Transformer y result)
    -> Derive.WithArgDoc (Transformer y result)
call3t (arg1, arg2, arg3) f =
    with_doc [arg_doc arg1, arg_doc arg2, arg_doc arg3] $ \vals deriver -> do
        (val1, val2, val3) <- extract3 vals (arg1, arg2, arg3)
        f val1 val2 val3 vals deriver

extract3 :: (Typecheck a, Typecheck b, Typecheck c) =>
    PassedArgs y -> (Arg a, Arg b, Arg c) -> Derive.Deriver (a, b, c)
extract3 vals (sig0, sig1, sig2) = do
    arg0 : arg1 : arg2 : _ <- check_args vals
        [arg_required sig0, arg_required sig1, arg_required sig2]
    (,,) <$> extract_arg 0 sig0 arg0 <*> extract_arg 1 sig1 arg1
        <*> extract_arg 2 sig2 arg2

-- 4

call4g :: (Typecheck a, Typecheck b, Typecheck c, Typecheck d) =>
    (Arg a, Arg b, Arg c, Arg d)
    -> (a -> b -> c -> d -> Generator y result)
    -> Derive.WithArgDoc (Generator y result)
call4g (arg1, arg2, arg3, arg4) f =
    with_doc [arg_doc arg1, arg_doc arg2, arg_doc arg3, arg_doc arg4] $
        \vals -> do
            (val1, val2, val3, val4) <- extract4 vals (arg1, arg2, arg3, arg4)
            f val1 val2 val3 val4 vals

call4t :: (Typecheck a, Typecheck b, Typecheck c, Typecheck d) =>
    (Arg a, Arg b, Arg c, Arg d)
    -> (a -> b -> c -> d -> Transformer y result)
    -> Derive.WithArgDoc (Transformer y result)
call4t (arg1, arg2, arg3, arg4) f =
    with_doc [arg_doc arg1, arg_doc arg2, arg_doc arg3, arg_doc arg4] $
        \vals deriver -> do
            (val1, val2, val3, val4) <- extract4 vals (arg1, arg2, arg3, arg4)
            f val1 val2 val3 val4 vals deriver

extract4 :: (Typecheck a, Typecheck b, Typecheck c, Typecheck d) =>
    PassedArgs y -> (Arg a, Arg b, Arg c, Arg d)
    -> Derive.Deriver (a, b, c, d)
extract4 vals (sig0, sig1, sig2, sig3) = do
    arg0 : arg1 : arg2 : arg3 : _ <- check_args vals
        [arg_required sig0, arg_required sig1, arg_required sig2,
            arg_required sig3]
    (,,,) <$> extract_arg 0 sig0 arg0 <*> extract_arg 1 sig1 arg1
        <*> extract_arg 2 sig2 arg2 <*> extract_arg 3 sig3 arg3

-- * check_args

-- | The call sequence is a little complicated because an argument can be
-- given explicitly, given implicitly by the environment, or defaulted if
-- it was declared optional.
--
-- 'check_args' defaults not-given args from the environment and verifies that
-- the number of args are correct, given the optional and environment-supplied
-- ones.  It returns Just for args given explicitly or by the environment, and
-- an infinite supply of Nothing for the rest.
--
-- Defaulting optional args and type-checking both require a typed 'Typecheck'
-- instance and happen in 'extract_arg'.
check_args :: PassedArgs y -> [(Bool, String)]
    -> Derive.Deriver [Maybe TrackLang.Val]
check_args passed args = do
    environ <- Derive.gets (Derive.state_environ . Derive.state_dynamic)
    either (Derive.throw_error . Derive.CallError) return
        (pure_check_args environ passed args)

pure_check_args :: TrackLang.Environ -> PassedArgs y
    -> [(Bool, String)] -- ^ @(arg_required?, name)@
    -> Either Derive.CallError [Maybe TrackLang.Val]
pure_check_args environ passed args
    | supplied_args < length required = Left $ Derive.ArgError $
        "too few arguments: " ++ expected
    | length vals > length args = Left $ Derive.ArgError $
        "too many arguments: " ++ expected
    | not (null bad_required) = Left $ Derive.ArgError $
        "required arg can't follow an optional one: "
            ++ Seq.join ", " [show i ++ "/" ++ name | (i, name) <- bad_required]
    | otherwise = Right $ defaulted_vals ++ repeat Nothing
    where
    (required, optional) = span (fst . snd) (zip [0..] args)
    vals = passed_vals passed
    defaulted_vals = zipWith deflt (map Just vals ++ repeat Nothing) args
        where
        deflt (Just val) _ = Just val
        deflt Nothing (_, arg_name) = TrackLang.lookup_val
            (arg_environ_default (passed_call_name passed) arg_name) environ
    supplied_args = length (filter Maybe.isJust defaulted_vals)
    bad_required = [(i, name) | (i, (True, name)) <- optional]

    from_env = max 0 (supplied_args - length vals)
    arg_range = if length required == length args
        then show (length required)
        else "from " ++ show (length required) ++ " to " ++ show (length args)
    expected = "expected " ++ arg_range ++ ", got "
        ++ show (length vals) ++ ": " ++ unwords (map TrackLang.show_val vals)
        ++ if from_env == 0 then ""
            else " (" ++ show from_env ++ " from environ)"

extract_arg :: (Typecheck a) => Int -> Arg a -> Maybe TrackLang.Val
    -> Derive.Deriver a
extract_arg argno arg maybe_val =
    either (Derive.throw_error . Derive.CallError) return
        (pure_extract_arg argno arg maybe_val)

pure_extract_arg :: (Typecheck a) => Int -> Arg a -> Maybe TrackLang.Val
    -> Either Derive.CallError a
pure_extract_arg argno arg maybe_val = case (arg_default arg, maybe_val2) of
        (Nothing, Nothing) -> err Nothing
        -- A val from the dynamic env takes precedence over the call's built-in
        -- default.
        (_, Just val) -> check val
        (Just v, Nothing) -> Right v
    where
    maybe_val2 = case maybe_val of
        Just TrackLang.VNotGiven -> Nothing
        _ -> maybe_val
    check val = case TrackLang.from_val val of
        Nothing -> err (Just val)
        Just v -> Right v
    err val = Left (Derive.TypeError argno (arg_name arg) (arg_type arg) val)

-- | Cast a Val to a haskell val, or throw if it's the wrong type.
cast :: forall a. (Typecheck a) => String -> TrackLang.Val -> Derive.Deriver a
cast name val = case TrackLang.from_val val of
        Nothing -> Derive.throw $
            name ++ ": expected " ++ Pretty.pretty return_type
            ++ " but val was " ++ Pretty.pretty (TrackLang.type_of val)
            ++ " " ++ TrackLang.show_val val
        Just a -> return a
    where return_type = TrackLang.to_type (error "cast" :: a)
