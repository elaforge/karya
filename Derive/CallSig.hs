-- | Functions to help define call signatures.
--
-- This is not in TrackLang because it needs Derive.PassedArgs and Derive
-- already imports TrackLang.  It could be in Derive.Call but Call is already
-- big so let's leaeve it separate for now.
module Derive.CallSig where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Derive.Derive (PassedArgs(..))
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang (Typecheck, TypeError(..))
import qualified Derive.Score as Score

import qualified Perform.Signal as Signal


-- * signatures

-- | A single argument in the signature of a call.
data Arg a = Arg {
    -- | An arg that is not explicitly given will be looked for in the dynamic
    -- environment as \<callid>-<argname\>.  Of course control args get this
    -- already through the control map, but this way non control args can be
    -- defaulted, or you can default a control arg to a constant without going
    -- to the bother of making a track for it.
    arg_name :: String
    , arg_default :: Maybe a
    } deriving (Eq, Show)

arg_type :: (Typecheck a) => Arg a -> TrackLang.Type
arg_type arg = TrackLang.to_type
    (maybe (error "arg_type shouldn't evaluate this") id (arg_default arg))

arg_environ_default :: TrackLang.CallId -> String -> TrackLang.ValName
arg_environ_default (TrackLang.Symbol call) arg_name =
    TrackLang.Symbol $ call ++ "-" ++ arg_name

arg_required :: Arg a -> (Bool, String)
arg_required arg = (Maybe.isNothing (arg_default arg), arg_name arg)

-- Utilities to describe function signatures.

required :: String -> Arg a
required name = Arg name Nothing

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
optional :: String -> a -> Arg a
optional name deflt = Arg name (Just deflt)

control :: String -> Signal.Y -> TrackLang.Control
control name deflt = TrackLang.DefaultedControl (Score.Control name) deflt

required_control :: String  -> TrackLang.Control
required_control name = TrackLang.Control (Score.Control name)

-- * extract and call

type WithError = Either String

with_error :: WithError result -> Either TypeError result
with_error (Left s) = Left (ArgError s)
with_error (Right v) = Right v

-- | Each call function will typecheck and call a function.  For the @_error@
-- variants, the function may also check the args and return Left, which will
-- be converted into an ArgError.
--
-- This is because call evaluation procedes in one pass to check types and
-- another to actually call the derivers.  I suppose I could do all the
-- checking in DeriveT, but this way I can keep the exceptions separate.
--
-- TODO However with extensible exceptions couldn't I do this more cleanly?
-- And I think if events consumed depends on deriver processing then this will
-- be necessary.
call0_error :: PassedArgs y -> WithError result -> Either TypeError result
call0_error vals f = check_args vals [] >> with_error f
call0 vals f = call0_error vals (return f)

extract1 :: (Typecheck a) => PassedArgs y -> Arg a -> Either TypeError a
extract1 vals sig0 = do
    arg0 : _ <- check_args vals [arg_required sig0]
    extract_arg 0 sig0 arg0

call1_error :: (Typecheck a) => PassedArgs y -> Arg a -> (a -> WithError result)
    -> Either TypeError result
call1_error vals arg0 f = with_error . f =<< extract1 vals arg0
call1 vals arg0 f = call1_error vals arg0 (return . f)

extract2 :: (Typecheck a, Typecheck b) =>
    PassedArgs y -> (Arg a, Arg b) -> Either TypeError (a, b)
extract2 vals (sig0, sig1) = do
    arg0 : arg1 : _ <- check_args vals [arg_required sig0, arg_required sig1]
    (,) <$> extract_arg 0 sig0 arg0 <*> extract_arg 1 sig1 arg1

call2_error :: (Typecheck a, Typecheck b) =>
    PassedArgs y -> (Arg a, Arg b) -> (a -> b -> WithError result)
    -> Either TypeError result
call2_error vals (arg0, arg1) f = do
    (val0, val1) <- extract2 vals (arg0, arg1)
    with_error $ f val0 val1
call2 vals args f = call2_error vals args (\a0 a1 -> return (f a0 a1))

extract3 :: (Typecheck a, Typecheck b, Typecheck c) =>
    PassedArgs y -> (Arg a, Arg b, Arg c) -> Either TypeError (a, b, c)
extract3 vals (sig0, sig1, sig2) = do
    arg0 : arg1 : arg2 : _ <- check_args vals
        [arg_required sig0, arg_required sig1, arg_required sig2]
    (,,) <$> extract_arg 0 sig0 arg0 <*> extract_arg 1 sig1 arg1
        <*> extract_arg 2 sig2 arg2

call3_error :: (Typecheck a, Typecheck b, Typecheck c) =>
    PassedArgs y -> (Arg a, Arg b, Arg c) -> (a -> b -> c -> WithError result)
    -> Either TypeError result
call3_error vals (arg0, arg1, arg2) f = do
    (val0, val1, val2) <- extract3 vals (arg0, arg1, arg2)
    with_error $ f val0 val1 val2
call3 vals args f = call3_error vals args (\a0 a1 a2 -> return (f a0 a1 a2))

extract4 :: (Typecheck a, Typecheck b, Typecheck c, Typecheck d) =>
    PassedArgs y -> (Arg a, Arg b, Arg c, Arg d)
    -> Either TypeError (a, b, c, d)
extract4 vals (sig0, sig1, sig2, sig3) = do
    arg0 : arg1 : arg2 : arg3 : _ <- check_args vals
        [arg_required sig0, arg_required sig1, arg_required sig2,
            arg_required sig3]
    (,,,) <$> extract_arg 0 sig0 arg0 <*> extract_arg 1 sig1 arg1
        <*> extract_arg 2 sig2 arg2 <*> extract_arg 3 sig3 arg3

call4_error :: (Typecheck a, Typecheck b, Typecheck c, Typecheck d) =>
    PassedArgs y -> (Arg a, Arg b, Arg c, Arg d)
    -> (a -> b -> c -> d -> WithError result)
    -> Either TypeError result
call4_error vals (arg0, arg1, arg2, arg3) f = do
    (val0, val1, val2, val3) <- extract4 vals (arg0, arg1, arg2, arg3)
    with_error $ f val0 val1 val2 val3
call4 vals args f =
    call4_error vals args (\a0 a1 a2 a3 -> return (f a0 a1 a2 a3))

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
check_args :: PassedArgs y -> [(Bool, String)] -- ^ @(arg_required?, name)@
    -> Either TypeError [Maybe TrackLang.Val]
check_args passed args
    | supplied_args < length required = Left $ ArgError $
        "too few arguments: " ++ expected
    | length vals > length args = Left $ ArgError $
        "too many arguments: " ++ expected
    | not (null bad_required) = Left $ ArgError $
        "required arg can't follow an optional one: "
            ++ Seq.join ", " [show i ++ "/" ++ name | (i, name) <- bad_required]
    | otherwise = Right $ defaulted_vals ++ repeat Nothing
    where
    (required, optional) = span (fst . snd) (zip [0..] args)
    vals = passed_vals passed
    defaulted_vals = zipWith deflt (map Just vals ++ repeat Nothing) args
        where
        deflt (Just val) _ = Just val
        deflt Nothing (_, arg_name) = Map.lookup
            (arg_environ_default (passed_call passed) arg_name)
            (passed_environ passed)
    supplied_args = length (filter Maybe.isJust defaulted_vals)
    bad_required = [(i, name) | (i, (True, name)) <- optional]

    from_env = max 0 (supplied_args - length vals)
    arg_range = if length required == length args
        then show (length required)
        else "from " ++ show (length required) ++ " to " ++ show (length args)
    expected = "expected " ++ arg_range ++ ", got "
        ++ show (length vals) ++ ": " ++ Pretty.pretty vals
        ++ if from_env == 0 then ""
            else " (" ++ show from_env ++ " from environ)"

extract_arg :: (Typecheck a) => Int -> Arg a -> Maybe TrackLang.Val
    -> Either TypeError a
extract_arg argno arg maybe_val = case (arg_default arg, maybe_val2) of
        (Nothing, Nothing) -> err Nothing
        (_, Just val) -> check val
        (Just v, Nothing) -> Right v
    where
    maybe_val2 = case maybe_val of
        Just TrackLang.VNotGiven -> Nothing
        _ -> maybe_val
    check val = case TrackLang.from_val val of
        Nothing -> err (Just val)
        Just v -> Right v
    err val = Left (TypeError argno (arg_name arg) (arg_type arg) val)
