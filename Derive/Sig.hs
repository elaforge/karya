-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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

    In addition, an arg may be a 'DeriveT.VPControlRef' or
    'DeriveT.ControlRef', which introduces yet another way to provide the
    value.  An argument @required_control \"c\"@ will pass
    a 'DeriveT.Ref'.  Technically it's then up to the call to
    decide what to do with it, but it will likely look it up at its chosen
    point in time, which means you can provide the value by providing a @c@
    control track or binding it explicitly e.g. @%c = .5 | call@.
    TODO: out of date, there is no longer Typecheck ControlRef, they are
    resolved by Typecheck.from_val

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
    \"tremolo-speed\" 10 ScoreT.Real)@.  This argument defaults to
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
    Parser(parserDocs), Generator, Transformer
    , Arg(..)
    , check, parseOrThrow, requireRight, parse, parseVals
    , Dummy
    -- * parsers
    , noArgs
    , required, requiredEnv
    , defaulted, defaultedEnv, defaultedEnvQuoted, maybeDefaulted
    , environ, environKey, environQuoted
    , requiredEnviron, requiredEnvironKey
    , optional, optionalEnv, many, manyVals, many1, manyPairs, many1Pairs
    , requiredVals
    -- ** defaults
    , EnvironDefault(..)
    , prefixedEnviron, environKeys
    -- * call
    , call, callSub, call0, callt, call0t
) where
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Pretty as Pretty
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.SubT as SubT
import qualified Derive.Derive as Derive
import           Derive.Derive (ArgName, CallName, EnvironDefault(..))
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck
import qualified Derive.ValType as ValType

import qualified Ui.Event as Event

import           Global


type Error = Derive.CallError
type Docs = [Derive.ArgDoc]

data Parser a = Parser
    { parserDocs :: !Docs
    , parserParser :: !(State -> Either Error (State, a))
    }

parser :: Derive.ArgDoc -> (State -> Either Error (State, a)) -> Parser a
parser argDoc = Parser [argDoc]

-- | Keep track of state when parsing arguments.
data State = State
    { stateArgs :: ![Arg]
    -- | This has to be incremented every time a Val is taken.  Pairing argnums
    -- in state_args doesn't work because when I run out I don't know where
    -- I am.
    , stateArgnum :: !Int
    , stateCallName :: !CallName
    , stateDerive :: !Derive.State
    , stateContext :: !(Derive.Context Derive.Tagged)
    }

data Arg = LiteralArg !DeriveT.Val | SubTrack !SubT.Track

instance Pretty Arg where
    format (LiteralArg val) = Pretty.format val
    format (SubTrack track) = Pretty.format track

showArg :: Arg -> Text
showArg (LiteralArg val) = ShowVal.showVal val
showArg (SubTrack track) = SubT.showTrack track

runParser :: Parser a -> State -> Either Error a
runParser parser state = case parserParser parser state of
    Left err -> Left err
    Right (state, a) -> case stateArgs state of
        [] -> Right a
        args -> Left $ Derive.ArgError $ "too many arguments: "
            <> Text.intercalate ", " (map showArg args)

instance Functor Parser where
    fmap f parser =
        parser { parserParser = fmap (fmap f) . parserParser parser }

instance Applicative Parser where
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

parseOrThrow :: Derive.Taggable d => Parser a -> Derive.PassedArgs d
    -> Derive.Deriver a
parseOrThrow parser args = requireRight =<< parse parser args

requireRight :: Either Error a -> Derive.Deriver a
requireRight = either (Derive.throwError . Derive.CallError) pure

-- | Run a parser against the current derive state.
parse :: Derive.Taggable d => Parser a -> Derive.PassedArgs d
    -> Derive.Deriver (Either Error a)
parse parser args = do
    subTracks <- Sub.subTracks args
    runParser parser . makeState subTracks <$> Derive.get
    where
    makeState subTracks state = State
        { stateArgs = map LiteralArg (Derive.passedVals args)
            ++ map SubTrack subTracks
        , stateArgnum = 0
        , stateCallName = Derive.passedCallName args
        , stateDerive = state
        , stateContext = Derive.tagContext (Derive.passedCtx args)
        }

-- | Like 'parse', but transformers don't get to see 'SubTrack' args.
parseTransformer :: Derive.Taggable d => Parser a -> Derive.PassedArgs d
    -> Derive.Deriver (Either Error a)
parseTransformer parser args =
    parseVals parser (Derive.tagContext (Derive.passedCtx args))
        (Derive.passedCallName args) (Derive.passedVals args)

parseVals :: Parser a -> Derive.Context Derive.Tagged -> CallName
    -> [DeriveT.Val] -> Derive.Deriver (Either Error a)
parseVals parser ctx name vals = runParser parser . makeState <$> Derive.get
    where
    makeState state = State
        { stateArgs = map LiteralArg vals
        , stateArgnum = 0
        , stateCallName = name
        , stateDerive = state
        , stateContext = ctx
        }

-- | Now that the default can be any ToVal and may be a different type than the
-- parser, it can get ambiguous, especially if it's a Nothing or [].  Since all
-- that matters is (show_val . to_val), and show_val of Nothing or [] is always
-- _ or (list), the type doesn't matter.  I could use Int, but I'll use this to
-- explicitly mark that it doesn't matter.
data Dummy
instance Typecheck.ToVal Dummy where
    toVal _ = DeriveT.VNotGiven

-- * parsers

-- | Parser for nullary calls.  Either use this with 'call' and 'callt', or use
-- 'call0' and 'call0t' as a shortcut.
noArgs :: Parser ()
noArgs = pure ()

-- | The argument is required to be present, and have the right type.
required :: forall a. Typecheck.Typecheck a => ArgName -> Doc.Doc -> Parser a
required name = requiredEnv name Derive.Prefixed

requiredEnv :: forall a. Typecheck.Typecheck a => ArgName
    -> Derive.EnvironDefault -> Doc.Doc -> Parser a
requiredEnv name envDefault doc = parser argDoc $ \state1 ->
    case takeArg envDefault state1 name of
        Nothing -> Left $ Derive.ArgError $
            "expected an argument at " <> pretty name
        Just (state, arg) -> (state,) <$>
            checkArg state argDoc (argnumError state1) name arg
    where
    expected = Typecheck.toType (Proxy :: Proxy a)
    argDoc = Derive.ArgDoc
        { argName = name
        , argType = expected
        , argParser = Derive.Required
        , argEnvironDefault = envDefault
        , argDoc = doc
        }

-- | The argument is not required to be present, but if it is, it has to have
-- either the right type or be VNotGiven.
defaulted :: forall a deflt.  (Typecheck.Typecheck a, Typecheck.ToVal deflt)
    => ArgName -> deflt -> Doc.Doc -> Parser a
defaulted name = defaultedEnv name Derive.Prefixed

defaultedEnv :: forall a deflt.
    (Typecheck.Typecheck a, Typecheck.ToVal deflt)
    => ArgName -> Derive.EnvironDefault -> deflt -> Doc.Doc -> Parser a
defaultedEnv name envDefault deflt =
    defaultedEnv_ name envDefault (Left (Typecheck.toVal deflt))

-- | The defaulted value can be a 'DeriveT.Quoted', which will be evaluated
-- if needed.
defaultedEnvQuoted :: forall a. Typecheck.Typecheck a =>
    ArgName -> Derive.EnvironDefault -> DeriveT.Quoted -> Doc.Doc -> Parser a
defaultedEnvQuoted name envDefault = defaultedEnv_ name envDefault . Right

defaultedEnv_ :: forall a. Typecheck.Typecheck a
    => ArgName -> Derive.EnvironDefault -> Either DeriveT.Val DeriveT.Quoted
    -> Doc.Doc -> Parser a
defaultedEnv_ name envDefault defltQuoted doc = parser argDoc $ \state1 ->
    case takeArg envDefault state1 name of
        Nothing -> deflt state1
        Just (state, LiteralArg DeriveT.VNotGiven) -> deflt state
        Just (state, arg) -> (state,) <$>
            checkArg state argDoc (argnumError state1) name arg
    where
    deflt state =
        evalDefault argDoc (argnumError state) name state defltQuoted
    expected = Typecheck.toType (Proxy :: Proxy a)
    argDoc = Derive.ArgDoc
        { argName = name
        , argType = expected
        , argParser = Derive.Defaulted $ ShowVal.showVal $
            either id Typecheck.toVal defltQuoted
        , argEnvironDefault = envDefault
        , argDoc = doc
        }

-- | This is either 'required' or 'defaulted', depending on if there's a
-- default value.  Useful for making call variants with instrument-specific
-- defaults.
maybeDefaulted :: (Typecheck.Typecheck a, Typecheck.ToVal deflt) =>
    ArgName -> Maybe deflt -> Doc.Doc -> Parser a
maybeDefaulted name Nothing doc = required name doc
maybeDefaulted name (Just deflt) doc = defaulted name deflt doc

-- | Eval a Quoted default value.
evalDefault :: forall a. Typecheck.Typecheck a => Derive.ArgDoc
    -> Derive.ErrorPlace -> ArgName -> State
    -> Either DeriveT.Val DeriveT.Quoted -> Either Error (State, a)
evalDefault argDoc place name state = \case
    Left val -> (state,) <$>
        checkArg state argDoc place name (LiteralArg val)
    Right quoted -> case evalQuoted state quoted of
        Right val -> (state,) <$>
            checkArg state argDoc place name (LiteralArg val)
        Left err -> Left $ Derive.TypeError $ Derive.TypeErrorT
            { errorPlace = place
            , errorSource = Derive.Quoted quoted
            , errorArgName = name
            , errorExpected = expectedType
            , errorReceived = Nothing
            , errorDerive = Just err
            }
    where
    expectedType = Typecheck.toType (Proxy :: Proxy a)

-- | This is an argument which is not actually parsed from the argument list.
-- Instead it's looked up it the environ according to the normal defaulting
-- rules.  So it's like 'defaulted' except there is no positional argument.
--
-- Of course, the call could just look in the environ itself, but this way it's
-- uniform and automatically documented.
environ :: forall a deflt. (Typecheck.Typecheck a, Typecheck.ToVal deflt) =>
    ArgName -> Derive.EnvironDefault
    -- ^ None doesn't make any sense, but, well, don't pass that then.
    -> deflt -> Doc.Doc -> Parser a
environ name envDefault = environ_ name envDefault . Left . Typecheck.toVal

-- | A shortcut for an unprefixed environ key.
environKey :: (Typecheck.Typecheck a, Typecheck.ToVal deflt) =>
    Env.Key -> deflt -> Doc.Doc -> Parser a
environKey key = environ (Derive.ArgName key) Unprefixed

-- | This is like 'environ', but the default is a 'DeriveT.Quoted', which
-- will be evaluated if needed.
environQuoted :: forall a. Typecheck.Typecheck a =>
    ArgName -> Derive.EnvironDefault -> DeriveT.Quoted -> Doc.Doc -> Parser a
environQuoted name envDefault = environ_ name envDefault . Right

-- Internal function that handles both quoted and unquoted default.
environ_ :: forall a. Typecheck.Typecheck a => ArgName -> Derive.EnvironDefault
    -- ^ None doesn't make any sense, but, well, don't pass that then.
    -> Either DeriveT.Val DeriveT.Quoted -> Doc.Doc -> Parser a
environ_ name envDefault quoted doc = parser argDoc $ \state ->
    case lookupDefault envDefault state name of
        Nothing -> deflt state
        Just val -> (state,) <$>
            checkArg state argDoc (environError state name) name
                (LiteralArg val)
    where
    deflt state = evalDefault argDoc (argnumError state) name state quoted
    expected = Typecheck.toType (Proxy :: Proxy a)
    argDoc = Derive.ArgDoc
        { argName = name
        , argType = expected
        , argParser = Derive.Environ $ Just $
            either ShowVal.showVal ShowVal.showVal quoted
        , argEnvironDefault = envDefault
        , argDoc = doc
        }

-- | This is like 'environ', but without a default.
requiredEnviron :: forall a. Typecheck.Typecheck a =>
    ArgName -> Derive.EnvironDefault -> Doc.Doc -> Parser a
requiredEnviron name envDefault doc = parser argDoc $ \state ->
    case lookupDefault envDefault state name of
        Nothing -> Left $ Derive.TypeError $ Derive.TypeErrorT
            { errorPlace = environError state name
            , errorSource = Derive.Literal
            , errorArgName = name
            , errorExpected = expected
            , errorReceived = Nothing
            , errorDerive = Nothing
            }
        Just val -> (state,) <$>
            checkArg state argDoc (environError state name) name
                (LiteralArg val)
    where
    expected = Typecheck.toType (Proxy :: Proxy a)
    argDoc = Derive.ArgDoc
        { argName = name
        , Derive.argType = expected
        , Derive.argParser = Derive.Environ Nothing
        , Derive.argEnvironDefault = envDefault
        , argDoc = doc
        }

requiredEnvironKey :: Typecheck.Typecheck a => Env.Key -> Doc.Doc -> Parser a
requiredEnvironKey key = requiredEnviron (Derive.ArgName key) Unprefixed

-- | This is like 'defaulted', but if the argument is the wrong type return
-- the default instead of failing.  It's mostly useful with 'many' or 'many1',
-- where you can distinguish the arguments by type.
optional :: forall a. (Typecheck.Typecheck a, ShowVal.ShowVal a) => ArgName -> a
    -> Doc.Doc -> Parser a
optional name = optionalEnv name Derive.Prefixed

optionalEnv :: forall a. (Typecheck.Typecheck a, ShowVal.ShowVal a) =>
    ArgName -> Derive.EnvironDefault -> a -> Doc.Doc -> Parser a
optionalEnv name envDefault deflt doc = parser argDoc $ \state1 ->
    case takeArg envDefault state1 name of
        Nothing -> Right (state1, deflt)
        Just (state, LiteralArg DeriveT.VNotGiven) -> Right (state, deflt)
        Just (state, arg) ->
            case checkArg state argDoc (argnumError state1) name arg of
                Right a -> Right (state, a)
                Left (Derive.TypeError {}) ->
                    case lookupDefault envDefault state name of
                        Nothing -> Right (replaced, deflt)
                        Just val -> (replaced,) <$>
                            checkArg replaced argDoc (argnumError state1)
                                name (LiteralArg val)
                Left err -> Left err
            where replaced = state { stateArgs = arg : stateArgs state }
    where
    expected = Typecheck.toType (Proxy :: Proxy a)
    argDoc = Derive.ArgDoc
        { argName = name
        , argType = expected
        , argParser = Derive.Optional (ShowVal.showVal deflt)
        , argEnvironDefault = envDefault
        , argDoc = doc
        }

-- | Collect the rest of the arguments.
many :: forall a. Typecheck.Typecheck a => ArgName -> Doc.Doc -> Parser [a]
many name doc = parser argDoc $ \state -> do
    vals <- mapM (typecheck state)
        (zip [stateArgnum state ..] (stateArgs state))
    let state2 = incrementArgnum (length (stateArgs state)) $
            state { stateArgs = []}
    Right (state2, vals)
    where
    typecheck state (argnum, arg) =
        checkArg state argDoc (Derive.TypeErrorArg argnum) name arg
    expected = Typecheck.toType (Proxy :: Proxy a)
    argDoc = Derive.ArgDoc
        { argName = name
        , argType = expected
        , argParser = Derive.Many
        , argEnvironDefault = Derive.None
        , argDoc = doc
        }

-- | 'many' specialized to Vals, to avoid a type annotation.
manyVals :: ArgName -> Doc.Doc -> Parser [DeriveT.Val]
manyVals name doc = many name doc

-- | Collect the rest of the arguments, but there must be at least one.
many1 :: forall a. Typecheck.Typecheck a => ArgName -> Doc.Doc
    -> Parser (NonEmpty a)
many1 name doc = nonEmpty name $ many name doc

-- | Collect the rest of the arguments, but expect a even number of them and
-- pair them up.
manyPairs :: forall a b. (Typecheck.Typecheck a,  Typecheck.Typecheck b) =>
    ArgName -> Doc.Doc -> Parser [(a, b)]
manyPairs name doc = parser (argDoc expected) $ \state -> do
    let args = stateArgs state
    when (odd (length args)) $
        Left $ Derive.ArgError $ "many_pairs requires an even argument length: "
            <> showt (length args)
    vals <- mapM (typecheck state) $ zip [stateArgnum state ..] (pairs args)
    let state2 = incrementArgnum (length vals) $ state { stateArgs = [] }
    Right (state2, vals)
    where
    typecheck state (argnum, (arg1, arg2)) = (,)
        <$> checkArg state (argDoc (Typecheck.toType (Proxy :: Proxy a)))
            (Derive.TypeErrorArg (argnum * 2)) name arg1
        <*> checkArg state (argDoc (Typecheck.toType (Proxy :: Proxy b)))
            (Derive.TypeErrorArg (argnum * 2 + 1)) name arg2
    expected = ValType.TPair (Typecheck.toType (Proxy :: Proxy a))
        (Typecheck.toType (Proxy :: Proxy b))
    argDoc expected = Derive.ArgDoc
        { argName = name
        , argType = expected
        , argParser = Derive.Many
        , argEnvironDefault = Derive.None
        , argDoc = doc
        }
    pairs (a : b : xs) = (a, b) : pairs xs
    pairs _ = []

-- | Like 'many_pairs', but require at least one pair.
many1Pairs :: forall a b. (Typecheck.Typecheck a,  Typecheck.Typecheck b) =>
    ArgName -> Doc.Doc -> Parser (NonEmpty (a, b))
many1Pairs name doc = nonEmpty name $ manyPairs name doc

-- | Modify a 'many' parser to require at least one thing.
nonEmpty :: ArgName -> Parser [a] -> Parser (NonEmpty a)
nonEmpty name (Parser docs p) =
    Parser (map (\d -> d { Derive.argParser = Derive.Many1 }) docs) convert
    where
    convert state = case p state of
        Left err -> Left err
        Right (_, []) -> Left $ Derive.ArgError $
            "arg requires at least one value: " <> pretty name
        Right (state, x : xs) -> Right (state, x :| xs)

-- | Require one Val for each ArgDoc given, but otherwise do no typechecking.
requiredVals :: [Derive.ArgDoc] -> Parser [Arg]
requiredVals docs = Parser docs parser
    where
    -- I don't check the number of arguments because this is used for call
    -- macros, and I need to give the sub-call a chance to default its args.
    parser state = Right (state2, vals)
        where
        (vals, rest) = splitAt (length docs) (stateArgs state)
        state2 = incrementArgnum (length vals) $ state { stateArgs = rest}

argnumError :: State -> Derive.ErrorPlace
argnumError = Derive.TypeErrorArg . stateArgnum

environError :: State -> ArgName -> Derive.ErrorPlace
environError state name =
    Derive.TypeErrorEnviron (prefixedEnviron (stateCallName state) name)

incrementArgnum :: Int -> State -> State
incrementArgnum n state = state { stateArgnum = n + stateArgnum state }

-- ** util

takeArg :: Derive.EnvironDefault -> State -> ArgName -> Maybe (State, Arg)
takeArg envDefault state name = case stateArgs state of
    v : vs -> Just
        ( next { stateArgs = vs }
        , case v of
            LiteralArg DeriveT.VNotGiven ->
                LiteralArg $ fromMaybe DeriveT.VNotGiven $
                    lookupDefault envDefault state name
            _ -> v
        )
    [] -> (next,) . LiteralArg <$> lookupDefault envDefault state name
    where next = incrementArgnum 1 state

checkArg :: forall a. Typecheck.Typecheck a => State -> Derive.ArgDoc
    -> Derive.ErrorPlace -> ArgName -> Arg -> Either Error a
checkArg state argDoc place name = \case
    LiteralArg val -> case fromVal state val of
        Left err -> Left $ typeError Derive.Literal (Just val) (Just err)
        Right (Just a) -> Right a
        Right Nothing -> case val of
            -- 'val' failed to typecheck, so try to coerce a Quoted to a new
            -- qval and typecheck that.
            DeriveT.VQuoted quoted -> do
                let source = Derive.Quoted quoted
                qval <- promoteError source val $ evalQuoted state quoted
                maybeA <- promoteError source qval $ fromVal state qval
                case maybeA of
                    Just a -> Right a
                    Nothing -> Left $ typeError source (Just qval) Nothing
            _ -> Left $ typeError Derive.Literal (Just val) Nothing
    SubTrack track -> case Typecheck.fromSubtrack track of
        Just a -> Right a
        Nothing -> Left $
            typeError (Derive.SubTrack (SubT._source track)) Nothing Nothing
    where
    promoteError source val = first (typeError source (Just val) . Just)
    typeError source maybeVal derive = Derive.TypeError $ Derive.TypeErrorT
        { errorPlace = place
        , errorSource = source
        , errorArgName = name
        , errorExpected = Derive.argType argDoc
        , errorReceived = maybeVal
        , errorDerive = derive
        }

-- | Typecheck a Val, evaluating if necessary.
fromVal :: Typecheck.Typecheck a => State -> DeriveT.Val
    -> Either Derive.Error (Maybe a)
fromVal state val = run state $ case Typecheck.fromVal val of
    Typecheck.Val result -> pure $ case result of
        Typecheck.Failure -> Nothing
        Typecheck.Success a -> Just a
        Typecheck.Derive deriver -> Just $ deriver (stateContext state)
    Typecheck.Eval deriver -> deriver =<< Derive.scoreToReal start
    where start = Event.start $ Derive.ctxEvent $ stateContext state

-- This can't be defined in Derive.Typecheck, because it uses Eval, and Eval
-- imports Typecheck.  It could be in Eval, but here it's closer to 'from_val',
-- which uses its result.
instance Typecheck.Typecheck Derive.NoteDeriver where
    fromVal = quotedToDeriver
    toType _ = ValType.TDeriver "note"
    -- This means that a subtrack can coerce to a deriver arg, in addition to
    -- a SubT.Track.
    fromSubtrack = Just . Sub.derive . SubT._events
instance Typecheck.Typecheck Derive.ControlDeriver where
    fromVal = quotedToDeriver
    toType _ = ValType.TDeriver "control"
instance Typecheck.Typecheck Derive.PitchDeriver where
    fromVal = quotedToDeriver
    toType _ = ValType.TDeriver "pitch"

quotedToDeriver :: Derive.CallableExpr d => DeriveT.Val
    -> Typecheck.Checked (Derive.Deriver (Stream.Stream d))
quotedToDeriver val = case asQuoted val of
    Just quoted -> Typecheck.Val $ Typecheck.Derive $
        \ctx -> Eval.evalQuoted (Derive.untagContext ctx) quoted
    Nothing -> Typecheck.failure

asQuoted :: DeriveT.Val -> Maybe DeriveT.Quoted
asQuoted = \case
    DeriveT.VQuoted a -> Just a
    DeriveT.VStr (Expr.Str sym) ->
        Just $ DeriveT.Quoted $ Expr.Call (Expr.Symbol sym) [] :| []
    _ -> Nothing

run :: State -> Derive.Deriver a -> Either Derive.Error a
run state deriver = result
    where (result, _state, _logs) = Derive.run (stateDerive state) deriver

evalQuoted :: State -> DeriveT.Quoted -> Either Derive.Error DeriveT.Val
evalQuoted state (DeriveT.Quoted expr) = result
    where
    (result, _state, _logs) = Derive.run (stateDerive state) $ do
        call <- case expr of
            call :| [] -> pure call
            _ -> Derive.throw "expected a val call, but got a full expression"
        Eval.eval (stateContext state) (Expr.ValCall call)

lookupDefault :: Derive.EnvironDefault -> State -> ArgName -> Maybe DeriveT.Val
lookupDefault envDefault state name =
    msum $ map lookup $ environKeys (stateCallName state) name envDefault
    where lookup key = Env.lookup key (stateEnviron state)

environKeys :: CallName -> ArgName -> Derive.EnvironDefault -> [Env.Key]
environKeys callName argName = \case
    None -> []
    Prefixed
        | callName == "" -> [unprefixed]
        | otherwise -> [prefixed]
    Unprefixed -> [unprefixed]
    Both
        | callName == "" -> [unprefixed]
        | otherwise -> [unprefixed, prefixed]
    where
    prefixed = prefixedEnviron callName argName
    unprefixed = (\(Derive.ArgName n) -> n) argName

prefixedEnviron :: CallName -> ArgName -> Env.Key
prefixedEnviron (Derive.CallName callName) (Derive.ArgName argName) =
    callName <> "-" <> argName


-- * call

-- | Similar to 'Derive.GeneratorF', but leaves the PassedArgs prev val
-- type free.  This is important for val calls, which use Tagged.
type Generator y d = Derive.PassedArgs y -> Derive.Deriver d
type Transformer y d =
    Derive.PassedArgs y -> Derive.Deriver d -> Derive.Deriver d

call :: Derive.Taggable y => Parser a -> (a -> Generator y d)
    -> Derive.WithArgDoc (Generator y d)
call parser f = (go, parserDocs parser)
    where
    go args = parseTransformer parser args >>= requireRight >>= \a -> f a args

callSub :: Derive.Taggable y => Parser a -> (a -> Generator y d)
    -> Derive.WithArgDoc (Generator y d)
callSub parser f = (go, parserDocs parser)
    where go args = parse parser args >>= requireRight >>= \a -> f a args

-- | Specialization of 'call' for 0 arguments.
call0 :: Derive.Taggable y => Generator y d -> Derive.WithArgDoc (Generator y d)
call0 f = (go, [])
    where
    go args = parseTransformer (pure ()) args >>= requireRight
        >>= \() -> f args

callt :: Derive.Taggable y => Parser a -> (a -> Transformer y d)
    -> Derive.WithArgDoc (Transformer y d)
callt parser f = (go, parserDocs parser)
    where
    go args deriver = parseTransformer parser args >>= requireRight
        >>= \a -> f a args deriver

-- | Specialization of 'callt' for 0 arguments.
call0t :: Derive.Taggable y => Transformer y d
    -> Derive.WithArgDoc (Transformer y d)
call0t f = (go, [])
    where
    go args deriver = parseTransformer (pure ()) args >>= requireRight
        >>= \() -> f args deriver

stateEnviron :: State -> DeriveT.Environ
stateEnviron = Derive.stateEnviron . Derive.stateDynamic . stateDerive
