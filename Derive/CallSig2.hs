{-# LANGUAGE ScopedTypeVariables #-}
module Derive.CallSig2 where
import qualified Control.Applicative as Applicative

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal

{- This style of argument parsing is pretty inflexible, and when I want
    something more fancy like 'num? pitch*' I have to write a parser by hand
    and awkwardly.

    It should be possible to do a monadic style parser and write e.g.:

    (,) <$> optional "num" 0.5 <*> many "pitch"

    It would be even better to use this for all arg parsing, then I can get rid
    of the call[1234] nonsense.  But how can I get docs out of it without
    actually running it?  Each combinator has to append both a doc and
    a parser.

    If possible, it has to be Applicative, because otherwise it can't generate
    a doc without values

    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (doc, v) = (doc, f v)

    pure :: a -> Parser a
    pure v = (no_doc, v)

    (doc1, f) <*> (doc2, a) = (doc1 <> doc2, f a)
-}

type Error = Derive.CallError
type Docs = [Derive.ArgDoc]

data Parser a = Parser Docs (State -> Either Error (State, a))

parser :: Derive.ArgDoc -> (State -> Either Error (State, a)) -> Parser a
parser arg_doc = Parser [arg_doc]

data State = State {
    -- | Pairs of the arg number, starting at 0, and the argument.
    state_vals :: [(Int, TrackLang.Val)]
    , state_environ :: TrackLang.Environ
    , state_call_name :: String
    } deriving (Show)

run :: Parser a -> State -> Either Error a
run (Parser _ parse) state = case parse state of
    Right (state, a) -> case state_vals state of
        [] -> Right a
        vals -> Left $ Derive.ArgError $ "trailing args: "
            ++ Seq.join ", " (map (ShowVal.show_val . snd) vals)
    Left err -> Left err

parser_docs :: Parser a -> Docs
parser_docs (Parser doc _) = doc

instance Functor Parser where
    fmap f (Parser doc parse) = Parser doc (fmap (fmap f) . parse)

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
parsed_manually :: String -> a -> Derive.WithArgDoc a
parsed_manually doc f = (f, Derive.ArgsParsedSpecially doc)

-- | The argument is required to be present, and have the right type.
required :: forall a. (TrackLang.Typecheck a) => String -> String -> Parser a
required name doc = parser arg_doc $ \state -> case get_val state name of
    Nothing -> Left $ Derive.ArgError $ "not enough args at: " ++ show name
    Just (state, argnum, val) -> case TrackLang.from_val val of
        Just a -> Right (state, a)
        Nothing -> Left $ Derive.TypeError argnum name expected (Just val)
    where
    expected = TrackLang.to_type (undefined :: a)
    arg_doc = Derive.ArgDoc
        { Derive.arg_name = name
        , Derive.arg_type = expected
        , Derive.arg_parser = Derive.Required
        , Derive.arg_doc = doc
        }

-- | The argument is not required to be present, but if it is, it has to have
-- either the right type or be VNotGiven.
defaulted :: forall a. (TrackLang.Typecheck a) => String -> a -> String
    -> Parser a
defaulted name deflt doc = parser arg_doc $ \state -> case get_val state name of
    Nothing -> Right (state, deflt)
    Just (state, _, TrackLang.VNotGiven) -> Right (state, deflt)
    Just (state, argnum, val) -> case TrackLang.from_val val of
        Just a -> Right (state, a)
        Nothing -> Left $ Derive.TypeError argnum name expected (Just val)
    where
    expected = TrackLang.to_type (undefined :: a)
    arg_doc = Derive.ArgDoc
        { Derive.arg_name = name
        , Derive.arg_type = expected
        , Derive.arg_parser = Derive.Defaulted (ShowVal.show_val deflt)
        , Derive.arg_doc = doc
        }

-- | This is like 'defaulted', but if the argument is the wrong type return
-- Nothing instead of failing.
optional :: forall a. (TrackLang.Typecheck a) => String -> String
    -> Parser (Maybe a)
optional name doc = parser arg_doc $ \state -> case get_val state name of
    Nothing -> Right (state, Nothing)
    Just (state2, _, val) -> case TrackLang.from_val val of
        Just a -> Right (state2, Just a)
        Nothing -> Right (state, Nothing)
    where
    expected = TrackLang.to_type (undefined :: a)
    arg_doc = Derive.ArgDoc
        { Derive.arg_name = name
        , Derive.arg_type = expected
        , Derive.arg_parser = Derive.Optional
        , Derive.arg_doc = doc
        }

-- | Collect the rest of the arguments.
many :: forall a. (TrackLang.Typecheck a) => String -> String -> Parser [a]
many name doc = parser arg_doc $ \state -> do
    vals <- mapM typecheck (state_vals state)
    Right (state { state_vals = [] }, vals)
    where
    typecheck (argnum, val) = case TrackLang.from_val val of
        Just a -> Right a
        Nothing -> Left $ Derive.TypeError argnum name expected (Just val)
    expected = TrackLang.to_type (undefined :: a)
    arg_doc = Derive.ArgDoc
        { Derive.arg_name = name
        , Derive.arg_type = expected
        , Derive.arg_parser = Derive.Many
        , Derive.arg_doc = doc
        }

-- | Collect the rest of the arguments, but there must be at least one.
many1 :: forall a. (TrackLang.Typecheck a) => String -> String
    -> Parser (NonEmpty a)
many1 name doc = parser arg_doc $ \state -> case state_vals state of
    [] -> Left $
        Derive.ArgError $ "many1 arg requires at least one value: " ++ name
    v : vs -> do
        v <- typecheck v
        vs <- mapM typecheck vs
        Right (state { state_vals = [] }, v :| vs)
    where
    typecheck (argnum, val) = case TrackLang.from_val val of
        Just a -> Right a
        Nothing -> Left $ Derive.TypeError argnum name expected (Just val)
    expected = TrackLang.to_type (undefined :: a)
    arg_doc = Derive.ArgDoc
        { Derive.arg_name = name
        , Derive.arg_type = expected
        , Derive.arg_parser = Derive.Many1
        , Derive.arg_doc = doc
        }

-- ** defaults

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

-- ** util

get_val :: State -> String -> Maybe (State, Int, TrackLang.Val)
get_val state name = case state_vals state of
    -- TODO make TypeError take 'ArgNum Int | DefaultedFromEnviron'
    [] -> ((,,) state (-1)) <$> deflt
    (argnum, v) : vs -> Just (state { state_vals = vs }, argnum, case v of
        TrackLang.VNotGiven -> fromMaybe TrackLang.VNotGiven deflt
        _ -> v)
    where
    deflt = TrackLang.lookup_val
        (arg_environ_default (state_call_name state) name)
        (state_environ state)

arg_environ_default :: String -> String -> TrackLang.ValName
arg_environ_default call_name arg_name =
    TrackLang.Symbol $ call_name ++ "-" ++ arg_name


-- * call

type Generator y result = Derive.PassedArgs y -> Derive.Deriver result
type Transformer y result =
    Derive.PassedArgs y -> Derive.Deriver result -> Derive.Deriver result

call :: Parser a -> (a -> Generator y d) -> Derive.WithArgDoc (Generator y d)
call parser f = (go, Derive.ArgDocs (parser_docs parser))
    where go args = run_call parser args >>= \a -> f a args

-- | Specialization of 'call' for 0 arguments.
call0 :: Generator y d -> Derive.WithArgDoc (Generator y d)
call0 f = (go, Derive.ArgDocs [])
    where go args = run_call (Applicative.pure ()) args >>= \() -> f args

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
        { state_vals = zip [0..] (Derive.passed_vals args)
        , state_environ = environ
        , state_call_name = Derive.passed_call_name args
        }


-- * misc

-- | Cast a Val to a haskell val, or throw if it's the wrong type.
cast :: forall a. (TrackLang.Typecheck a) => String -> TrackLang.Val
    -> Derive.Deriver a
cast name val = case TrackLang.from_val val of
        Nothing -> Derive.throw $
            name ++ ": expected " ++ Pretty.pretty return_type
            ++ " but val was " ++ Pretty.pretty (TrackLang.type_of val)
            ++ " " ++ TrackLang.show_val val
        Just a -> return a
    where return_type = TrackLang.to_type (error "cast" :: a)
