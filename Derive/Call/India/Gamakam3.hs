-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Calls for Carnatic gamakam.
module Derive.Call.India.Gamakam3 where
import qualified Control.Monad.State as State
import qualified Data.Attoparsec.Text as A
import qualified Data.DList as DList
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import qualified Data.Traversable as Traversable

import qualified Util.Map as Map
import qualified Util.ParseText as ParseText
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Args as Args
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal
import Global
import Types


module_ :: Module.Module
module_ = "india" <> "gamakam3"

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.generator_call_map [(Parse.unparsed_call, c_sequence)]

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map [("sahitya", c_sahitya)]

-- * sequence

c_sequence :: Derive.Generator Derive.Pitch
c_sequence = Derive.generator1 module_ "sequence" mempty sequence_doc
    $ Sig.call ((,) <$> Sig.required "sequence" sequence_arg_doc <*> config_env)
    $ \(text, (transition, dyn_transition)) args -> do
        let (start, end) = Args.range_or_next args
        maybe_state <- get_state transition dyn_transition args
        case maybe_state of
            Nothing -> return mempty
            Just state -> do
                Result pitches dyns_d <- Derive.at start $
                    pitch_sequence (end - start) state text
                real_start <- Derive.real start
                real_end <- Derive.real end
                let dyns = dropWhile Signal.null $ DList.toList dyns_d
                -- Extend the first sample to the start time, otherwise I wind
                -- up with whatever the previous dyn was.
                let initial = case Signal.head =<< Seq.head dyns of
                        Just (x, y) | x > real_start ->
                            Signal.signal [(real_start, y)]
                        _ -> mempty
                unless (null dyns) $
                    ControlUtil.multiply_dyn real_end $ mconcat (initial : dyns)
                return $ mconcat $ DList.toList pitches
    where
    config_env :: Sig.Parser (TrackLang.Normalized, TrackLang.Normalized)
    config_env = (,)
        <$> Sig.environ "transition" Sig.Both (TrackLang.Normalized 0.5)
            "Time for each pitch movement, in proportion of the total time\
            \ available."
        <*> Sig.environ "dyn-transition" Sig.Both (TrackLang.Normalized 1)
            "Time for each dyn movement, in proportion of the total time\
            \ available."

get_state :: TrackLang.Normalized -> TrackLang.Normalized -> Derive.PassedArgs a
    -> Derive.Deriver (Maybe State)
get_state transition dyn_transition args =
    -- If there's no pitch then this is likely at the edge of a slice, and can
    -- be ignored.  TODO I think?
    justm (get_pitch (Args.start args)) $ \cur -> do
        prev_event <- Args.lookup_prev_note
        let prev_pitch = fmap snd . PSignal.last
                . Score.event_untransformed_pitch =<< prev_event
        maybe_next <- maybe (return Nothing) get_pitch $ Args.next_start args
        return $ Just $ State
            { state_from_pitch = cur
            , state_from_dyn = fromMaybe 1 $ lookup_last_dyn =<< prev_event
            , state_transition = transition
            , state_dyn_transition = dyn_transition
            , state_current_pitch = cur
            , state_previous_pitch = fromMaybe cur prev_pitch
            , state_next_pitch = fromMaybe cur maybe_next
            }
    where
    get_pitch = Derive.pitch_at <=< Derive.real

lookup_last_dyn :: Score.Event -> Maybe Signal.Y
lookup_last_dyn event = do
    sig <- Map.lookup Controls.dynamic $
        Score.event_untransformed_controls event
    snd <$> Signal.last (Score.typed_val sig)

sequence_doc :: Text
sequence_doc = "doc doc\
    \ Currently the transition curve is hardcoded to a sigmoid curve, but\
    \ I could add a curve env var if necessary."

sequence_arg_doc :: Text
sequence_arg_doc = "Abbreviated string of calls... TODO"

-- * EvalM

type EvalM a = State.StateT State Derive.Deriver a

data State = State {
    state_from_pitch :: !PSignal.Pitch
    , state_from_dyn :: !Signal.Y
    , state_transition :: !TrackLang.Normalized
    , state_dyn_transition :: !TrackLang.Normalized
    , state_current_pitch :: !PSignal.Pitch
    , state_previous_pitch :: !PSignal.Pitch
    , state_next_pitch :: !PSignal.Pitch
    }

instance Pretty.Pretty State where
    format (State from_pitch from_dyn transition dyn_transition cur prev next) =
        Pretty.recordTitle "State"
            [ ("from_pitch", Pretty.format from_pitch)
            , ("from_dyn", Pretty.format from_dyn)
            , ("transition", Pretty.format transition)
            , ("dyn_transition", Pretty.format dyn_transition)
            , ("current_pitch", Pretty.format cur)
            , ("previous_pitch", Pretty.format prev)
            , ("next_pitch", Pretty.format next)
            ]

set_pitch :: PSignal.Pitch -> EvalM ()
set_pitch p = State.modify $ \state -> state { state_from_pitch = p }

get_from :: EvalM PSignal.Pitch
get_from = State.gets state_from_pitch

-- * sequence

data Result = Result !(DList.DList PSignal.Signal) !(DList.DList Signal.Control)
    deriving (Show)

instance Monoid.Monoid Result where
    mempty = Result mempty mempty
    mappend (Result pitch1 dyn1) (Result pitch2 dyn2) =
        Result (pitch1<>pitch2) (dyn1<>dyn2)

pitch_sequence :: ScoreTime -> State -> Text -> Derive.Deriver Result
pitch_sequence dur state arg = do
    exprs <- Derive.require_right (("parsing " <> showt arg <> ": ")<>) $
        resolve_exprs =<< parse_sequence arg
    let starts = slice_time dur (expr_durations exprs)
        ranges = zip starts (drop 1 starts)
    (results, _) <- State.runStateT (mapM eval (zip_exprs ranges exprs)) state
    return $ mconcat results

slice_time :: ScoreTime -> [Double] -> [ScoreTime]
slice_time dur slices = scanl (+) 0 $ map ((*one) . ScoreTime.double) slices
    where one = dur / ScoreTime.double (sum slices)

eval :: Expr_ (Text, DynCall) ((ScoreTime, ScoreTime), PitchCall)
    -> EvalM Result
eval (PitchExpr ((start, end), pcall) arg_) = case pcall_call pcall of
    PCall signature func -> do
        parsed_arg <- parse_args ctx arg signature
        (\p -> Result (DList.singleton p) mempty) <$> func parsed_arg ctx
    where
    arg = pcall_arg pcall arg_
    ctx = Context
        { ctx_start = start
        , ctx_end = end
        , ctx_call_name = Text.cons (pcall_name pcall) arg_
        }
eval (DynExpr (name, DynCall _ sig1 sig2 func) arg1 arg2 exprs) =
    case (start, end) of
        (Just start, Just end) -> do
            let ctx = Context
                    { ctx_start = start
                    , ctx_end = end
                    , ctx_call_name = name
                    }
            parsed_arg1 <- parse_args ctx arg1 sig1
            parsed_arg2 <- parse_args ctx arg2 sig2
            dyn <- func parsed_arg1 parsed_arg2 ctx
            pitch <- concatMapM eval exprs
            return $ Result mempty (DList.singleton dyn) <> pitch
        _ -> return mempty
    where
    start = Monoid.getFirst $
        foldMap (foldMap (Monoid.First . Just . fst . fst)) exprs
    end = Monoid.getLast $
        foldMap (foldMap (Monoid.Last . Just . snd . fst)) exprs

-- | DynExpr Name Arg1 Arg2 | PitchExpr Name Arg
data Expr_ dyn pitch = DynExpr !dyn !Text !Text ![Expr_ dyn pitch]
    | PitchExpr !pitch !Text
    deriving (Eq, Show, Functor, Foldable.Foldable, Traversable.Traversable)
type ResolvedExpr = Expr_ (Text, DynCall) PitchCall
type Expr = Expr_ Text Char

expr_durations :: [ResolvedExpr] -> [Double]
expr_durations = map pcall_duration . concatMap Foldable.toList

zip_exprs :: [a] -> [Expr_ dyn b] -> [Expr_ dyn (a, b)]
zip_exprs xs exprs = fst $ State.runState (traverse go exprs) xs
    where
    go (DynExpr c arg1 arg2 exprs) = DynExpr c arg1 arg2 <$> traverse go exprs
    go (PitchExpr call arg) = do
        x : xs <- State.get
        State.put xs
        return $ PitchExpr (x, call) arg

parse_sequence :: Text -> Either Text [Expr]
parse_sequence = ParseText.parse p_exprs

resolve_exprs :: [Expr] -> Either Text [ResolvedExpr]
resolve_exprs = concatMapM resolve
    where
    resolve (DynExpr c a1 a2 exprs) = case Map.lookup c dynamic_calls of
        Nothing -> Left $ "dynamic call not found: " <> showt c
        Just call -> do
            resolved <- resolve_exprs exprs
            return [DynExpr (c, call) a1 a2 resolved]
    resolve (PitchExpr c arg) = case Map.lookup c pcall_map of
        Nothing -> Left $ "pitch call not found: " <> showt c
        -- Apply the same argument to all of them.  But I should only get
        -- multiple PitchExprs for aliases, which expect no argument.
        Just calls -> Right [PitchExpr c arg | c <- calls]

-- * DynCall

data DynCall = forall a b. DynCall {
    dcall_doc :: Text
    , dcall_signature1 :: Sig.Parser a
    , dcall_signature2 :: Sig.Parser b
    , dcall_func :: a -> b -> Context -> EvalM Signal.Control
    }

dynamic_calls :: Map.Map Text DynCall
dynamic_calls = Map.fromList $ validate_names $ speed_permutations $
    [ ("<", dc_move LT)
    , (">", dc_move GT)
    , ("=", dc_move EQ)
    ]
    where
    validate_names = map $ first $ \name ->
        if Text.all valid_dcall_char name then name
            else error $ "invalid name: " <> show name
    speed_permutations calls =
        [ (name <> suffix, call speed)
        | (name, call) <- calls, (speed, suffix) <- speeds
        ]
    speeds = [(Nothing, ""), (Just Fast, "^"), (Just Medium, "-"),
        (Just Slow, "_")]

dc_move :: Ordering -> Maybe TransitionTime -> DynCall
dc_move ord speed = DynCall doc sig1 sig2 $ \from to args -> do
    (start, end) <- ctx_range args
    -- TODO these could come from an environ value
    transition <- case speed of
        Just Fast -> return $ TrackLang.Normalized 0.1
        Just Medium -> return $ TrackLang.Normalized 0.5
        Just Slow -> return $ TrackLang.Normalized 1
        Nothing -> State.gets state_dyn_transition
    from <- ($from) $ case ord of
        LT -> return . fromMaybe 0
        GT -> maybe get_from return
        EQ -> maybe get_from return
    to <- ($to) $ case ord of
        LT -> return . fromMaybe 1
        GT -> return . fromMaybe 0
        -- 1 should be unreached since sig2 is required when ord == EQ.
        EQ -> return . fromMaybe 1
    move_dyn transition ord start from end to
    where
    get_from = State.gets state_from_dyn
    sig1 = Sig.defaulted "from" Nothing "From value."
    sig2 = case ord of
        EQ -> Just <$> Sig.required "to" "To value."
        _ -> Sig.defaulted "to" Nothing "To value."

    doc = "Hi, doc\
        \ < -> from 0, to 1, align to start\
        \ > -> from prev, to 0, align to end\
        \ = -> from prev, to arg, align to middle"

-- Say I want to come from 0, dyn quickly at start: <^, dyn quickly at end: 0>^
-- alignment can happen by weighting start and end of the curve

move_dyn :: TrackLang.Normalized -> Ordering -> RealTime -> Signal.Y -> RealTime
    -> Signal.Y -> EvalM Signal.Control
move_dyn (TrackLang.Normalized transition) align start from end to = do
    let curve = snd ControlUtil.sigmoid_curve $ case align of
            LT -> (0, weight)
            GT -> (weight, 0)
            EQ -> (weight, weight)
        weight = 1 - transition
    State.modify $ \state -> state { state_from_dyn = to }
    lift $ ControlUtil.make_segment curve start from end to

-- . > go from prev dyn to 0, >.1 from prev to .1, < go from 0 to 1
--   .4 - jump to .4, .4>.1 from .4 to .1.
--   So a>b is jump to a move to b, a defaults to prev, b defaults to 0.
--   a<b is the same, except a defaults to 0, b defaults to 1.
-- . I might also want some transition in a short time, e.g. half of the
--   time slice.  Or I could use SMF type times, but they can align to
--   front, back, or center.  I guess < aligns to front, > aligns to end,
--   I could use = aligns to center.
--
-- 1<^0 1<_0

-- * PitchCall

data PitchCall = PitchCall {
    -- TODO take this out, as with DynCall
    -- then I can merge PCall into this, as with DynCall
    pcall_name :: !Char
    , pcall_doc :: !Text
    , pcall_duration :: !Double
    -- | If True, cons 'pcall_name' on to the arg before parsing it.
    , pcall_parse_call_name :: !Bool
    , pcall_call :: !PCall
    }

pcall_arg :: PitchCall -> Text -> Text
pcall_arg pcall arg
    | pcall_parse_call_name pcall = Text.cons (pcall_name pcall) arg
    | otherwise = arg

data PCall = forall a. PCall {
    pcall_signature :: Sig.Parser a
    , pcall_func :: a -> Context -> EvalM PSignal.Signal
    }

pcall_map :: Map.Map Char [PitchCall]
pcall_map = resolve $ Map.unique $
    [ emit '0' "Hold flat pitch." pc_flat
    , parse_name $ emit '-' "Negative relative motion." pc_move
    ] ++ [parse_name $ emit c "Relative motion." pc_move | c <- "123456789"] ++
    [ alias 'b' ["-2"], alias 'a' ["-1"]
    , alias 'y' ["-1nn"], alias 'z' ["1nn"] -- relative motion by NN
    , emit 'c' "Absolute motion to current pitch." (pc_move_absolute Current)
    , emit 'd' "Absolute motion to next pitch." (pc_move_absolute Next)
    , alias 'u' ["-1", "1"], alias 'n' ["1", "-1"] -- single turn
    -- , alias 'v' "a1 but fast, and with separate default for bottom"
    , emit 'j' "Janta." pc_janta

    , config '<' "Set from pitch to previous." (pc_set_pitch Previous)
    , config '^' "Set from pitch to current." (pc_set_pitch Current)
    , config 'P' "Set from pitch to relative steps." pc_set_pitch_relative
    , config 'F' "Fast transition time." (pc_set_transition_time Fast)
    , config 'M' "Medium transition time." (pc_set_transition_time Medium)
    , config 'S' "Slow transition time." (pc_set_transition_time Slow)
    , config 'T' "Set slice time of the next call." pc_set_next_time_slice
    ]
    where
    resolve (calls, duplicates)
        | null duplicates = either (error . untxt) id (resolve_aliases calls)
        | otherwise = error $ "duplicate calls: " <> show (map fst duplicates)
    parse_name = second $ second $ \g -> g { pcall_parse_call_name = True }
    alias name to = (name, Left to)
    emit name doc c = (name, Right $ PitchCall name doc 1 False c)
    config name doc c = (name, Right $ PitchCall name doc 0 False c)

resolve_aliases :: Map.Map Char (Either [Text] PitchCall)
    -> Either Text (Map.Map Char [PitchCall])
resolve_aliases call_map = Map.fromList <$> mapM resolve (Map.toList call_map)
    where
    resolve (name, Right call) = Right (name, [call])
    resolve (name, Left calls) = (,) name <$> mapM resolve1 calls
    resolve1 to = do
        (c, arg) <- maybe (Left "empty alias") Right $ Text.uncons to
        call <- maybe (Left $ "not found: " <> showt c) Right $
            Map.lookup c call_map
        call <- first (("alias to alias: "<>) . showt) call
        Right $ apply_arg call arg

apply_arg :: PitchCall -> Text -> PitchCall
apply_arg call arg = call
    { pcall_call = case pcall_call call of
        PCall signature func -> PCall ignore $ \_ ctx -> do
            parsed <- parse_args ctx (pcall_arg call arg) signature
            func parsed ctx
    }
    where
    -- Accept anything for an argument but ignore it.  This is because
    -- I've already hardcoded the argument, but 'eval' will want to apply it
    -- anyway, since it can't tell the difference from an alias call and
    -- a normal call.
    ignore = Sig.defaulted "ignore" (TrackLang.num 0) ""

-- ** PitchCall implementation

-- I want pcall args to get the full arg treatment, such as defaults
-- and quoted evaluation.  Then I can configure FMS.
-- Calls also get arg docs.

parse_args :: Context -> Text -> Sig.Parser a -> EvalM a
parse_args context arg sig = lift $ do
    vals <- Derive.require_right (("parsing " <> showt name <> ": ") <>) $
        if Text.null arg then return [] else (:[]) <$> Parse.parse_val arg
    Sig.require_right
        =<< Sig.parse_vals sig (Derive.dummy_call_info 0 1 name) name vals
    where name = ctx_call_name context

-- Here we are reinventing Derive.Call yet again...
-- This is the equivalent of Derive.CallInfo, only with a better name
-- TODO if the name is better, why not change CallInfo too?
data Context = Context {
    ctx_start :: !ScoreTime
    , ctx_end :: !ScoreTime
    -- | Complete call name, first char consed to arg.
    , ctx_call_name :: !Text
    } deriving (Show)

-- TODO if everyone winds up wanting RealTime I can put this in Context
-- ... but why bother, I wind up typing ctx_range either way.
ctx_range :: Context -> EvalM (RealTime, RealTime)
ctx_range context = lift $
    (,) <$> Derive.real (ctx_start context) <*> Derive.real (ctx_end context)

pc_flat :: PCall
pc_flat = PCall Sig.no_args $ \() context -> do
    pitch <- get_from
    (start, end) <- ctx_range context
    return $ PSignal.signal [(start, pitch), (end, pitch)]

pc_move :: PCall
pc_move = PCall (Sig.required "to" "To pitch.") $ \arg context -> do
    (start, end) <- ctx_range context
    case arg of
        Left (TrackLang.Symbol sym)
            | sym == "-" -> do
                pitch <- get_from
                return $ PSignal.signal [(start, pitch), (end, pitch)]
            | otherwise -> lift $ Derive.throw $ "unknown move: " <> showt sym
        Right (TrackLang.DefaultDiatonic transpose) -> do
            from_pitch <- get_from
            move start from_pitch end
                (Pitches.transpose transpose from_pitch)

move :: RealTime -> PSignal.Pitch -> RealTime -> PSignal.Pitch
    -> EvalM PSignal.Signal
move start from_pitch end to_pitch = do
    TrackLang.Normalized transition <- State.gets state_transition
    let curve = snd ControlUtil.sigmoid_curve (1-transition, 1-transition)
    set_pitch to_pitch
    lift $ PitchUtil.make_segment curve start from_pitch end to_pitch

data PitchDirection = Previous | Current | Next deriving (Show, Eq)
instance Pretty.Pretty PitchDirection where pretty = showt

get_direction_pitch :: PitchDirection -> EvalM PSignal.Pitch
get_direction_pitch dir = case dir of
    Previous -> State.gets state_previous_pitch
    Current -> State.gets state_current_pitch
    Next -> State.gets state_next_pitch

pc_move_absolute :: PitchDirection -> PCall
pc_move_absolute dir = PCall Sig.no_args $ \() context -> do
    (start, end) <- ctx_range context
    from_pitch <- get_from
    to_pitch <- get_direction_pitch dir
    move start from_pitch end to_pitch

pc_janta :: PCall
pc_janta = PCall Sig.no_args $ \() _args -> lift $ Derive.throw "janta"

pc_set_pitch :: PitchDirection -> PCall
pc_set_pitch dir = PCall Sig.no_args $ \() _args -> do
    set_pitch =<< get_direction_pitch dir
    return mempty

pc_set_pitch_relative :: PCall
pc_set_pitch_relative = PCall (Sig.required "to" "To pitch.") $
    \(TrackLang.DefaultDiatonic transpose) _args -> do
        set_pitch . Pitches.transpose transpose =<< get_from
        return mempty

data TransitionTime = Slow | Medium | Fast deriving (Show, Eq)

pc_set_transition_time :: TransitionTime -> PCall
pc_set_transition_time time = PCall Sig.no_args $ \() _args -> do
    State.modify $ \state -> state { state_transition = ttime }
    return mempty
    where
    -- TODO these could come from an environ value
    ttime = TrackLang.Normalized $ case time of
        Fast -> 0.1
        Medium -> 0.5
        Slow -> 0.9

pc_set_next_time_slice :: PCall
pc_set_next_time_slice = PCall Sig.no_args $ \() _args ->
    lift $ Derive.throw "set next time slice"

-- * parser

type Parser a = A.Parser a

p_exprs :: Parser [Expr]
p_exprs = concat <$> (A.skipSpace *> A.many1 (p_expr <* A.skipSpace))

p_expr :: Parser [Expr]
p_expr = ((:[]) <$> p_dyn_expr p_exprs) <|> (A.char '!' *> p_compact_exprs)
    <|> ((:[]) <$> p_pitch_expr)

p_dyn_expr :: Parser [Expr] -> Parser Expr
p_dyn_expr sub_expr = do
    A.char '['
    exprs <- sub_expr
    A.char ']'
    arg1 <- A.takeWhile (not . valid_dcall_char)
    sym <- A.takeWhile valid_dcall_char
    arg2 <- A.takeWhile valid_pcall_char
    return $ DynExpr sym arg1 arg2 exprs

p_compact_exprs :: Parser [Expr]
p_compact_exprs = A.many1 (p_dyn_expr p_compact_exprs <|> p_compact_pitch)

p_compact_pitch :: Parser Expr
p_compact_pitch = PitchExpr <$> A.satisfy valid_pcall_char <*> pure ""

p_pitch_expr :: Parser Expr
p_pitch_expr = PitchExpr <$> A.satisfy (\c -> c /= '!' && valid_pcall_char c)
    <*> A.takeWhile valid_pcall_char

valid_pcall_char :: Char -> Bool
valid_pcall_char c = c /= '[' && c /= ']' && c /= ' '

valid_dcall_char :: Char -> Bool
valid_dcall_char c = valid_pcall_char c && c `elem` "=<>^-_"


-- * misc

c_sahitya :: Derive.Taggable a => Derive.Transformer a
c_sahitya = Derive.transformer module_ "sahitya" mempty
    "Ignore the transformed deriver. Put this on a track to ignore its\
    \ contents, and put in sahitya."
    $ Sig.call0t $ \_args _deriver -> return mempty
