-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Calls for Carnatic gamakam.
module Derive.Call.India.Gamakam4 (
    pitch_calls, control_calls, note_calls
-- #ifdef TESTING
    , parse_sequence, parse_dyn_sequence
    , Call(..), PitchCall(..), ParsedPitch(..)
    , resolve_pitch_calls
    , dyn_call_map, pitch_call_map
-- #endif
) where
import qualified Control.Monad.State as State
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as Char
import qualified Data.DList as DList
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Map as Map
import qualified Util.ParseText as ParseText
import qualified Util.Pretty as Pretty

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Derive as Derive
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global
import Types


module_ :: Module.Module
module_ = "india" <> "gamakam4"

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.generator_call_map
    [ (Parse.unparsed_call, c_sequence)
    ]

control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.generator_call_map
    [(Parse.unparsed_call, c_dyn_sequence)]

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map [("sahitya", c_sahitya)]

-- * sequence

c_sequence :: Derive.Generator Derive.Pitch
c_sequence = Derive.generator1 module_
    "sequence" mempty sequence_doc
    $ Sig.call ((,) <$> Sig.required "sequence" sequence_arg_doc <*> config_env)
    $ \(text, transition) args -> do
        (start, end) <- Args.range_or_note_end args
        maybe_state <- get_state transition args
        case maybe_state of
            Nothing -> return mempty
            Just state -> do
                Result pitches <- Derive.at start $
                    pitch_sequence (end - start) state text
                return $ mconcat $ DList.toList pitches
    where
    config_env :: Sig.Parser Typecheck.Normalized
    config_env =
        Sig.environ "transition" Sig.Both (Typecheck.Normalized 0.5)
            "Time for each pitch movement, in proportion of the total time\
            \ available."

get_state :: Typecheck.Normalized -> Derive.PassedArgs Derive.Pitch
    -> Derive.Deriver (Maybe State)
get_state transition args =
    -- If there's no pitch then this is likely at the edge of a slice, and can
    -- be ignored.  TODO I think?
    justm (get_pitch (Args.start args)) $ \cur -> do
        prev_event <- Args.lookup_prev_note
        let prev_event_pitch = fmap snd . PSignal.last
                . Score.event_untransformed_pitch =<< prev_event
            prev_pitch = snd <$> Args.prev_pitch args
        maybe_prev <- Args.lookup_prev_logical_pitch
        maybe_next <- Args.lookup_next_logical_pitch
        return $ Just $ State
            { state_from_pitch =
                fromMaybe cur (prev_pitch <|> prev_event_pitch)
            , state_transition = transition
            , state_current_pitch = cur
            , state_previous_pitch = fromMaybe cur maybe_prev
            , state_next_pitch = fromMaybe cur maybe_next
            }
    where
    get_pitch = Derive.pitch_at <=< Derive.real

sequence_doc :: Text
sequence_doc = "doc doc\
    \ Currently the transition curve is hardcoded to a sigmoid curve, but\
    \ I could add a curve env var if necessary."

sequence_arg_doc :: Text
sequence_arg_doc = "Abbreviated string of calls... TODO"

-- * dyn-sequence

c_dyn_sequence :: Derive.Generator Derive.Control
c_dyn_sequence = Derive.generator1 module_ "dyn-sequence" mempty doc
    $ Sig.call (Sig.required "sequence" arg_doc)
    $ \text args -> do
        (start, end) <- Args.range_or_note_end args
        let state = DynState
                { state_from_dyn = maybe 0 snd (Args.prev_control args) }
        Derive.at start $ dyn_sequence (end - start) state text
    where
    doc = "doc doc"
    arg_doc = "blah blah"

newtype DynState = DynState { state_from_dyn :: Signal.Y }
    deriving (Show)

instance Pretty.Pretty DynState where
    format (DynState from_dyn) = Pretty.recordTitle "DynState"
        [ ("from_dyn", Pretty.format from_dyn)
        ]

dyn_sequence :: ScoreTime -> DynState -> Text -> Derive.Deriver Signal.Control
dyn_sequence dur state arg = do
    exprs <- Derive.require_right (("parsing " <> showt arg <> ": ")<>) $
        resolve_dyn_calls =<< parse_dyn_sequence arg
    let starts = slice_time dur (replicate (length exprs) 1)
        ranges = zip starts (drop 1 starts)
    (results, _) <- State.runStateT (mapM eval_dyn (zip ranges exprs)) state
    return $ mconcat results

eval_dyn :: ((ScoreTime, ScoreTime), Call (DynCall, Char))
    -> M DynState Signal.Control
eval_dyn ((start, end), (Call (DynCall _ sig func, name) arg)) = do
    let ctx = Context
            { ctx_start = start
            , ctx_end = end
            , ctx_call_name = Text.singleton name
            }
    parsed_arg <- parse_args (Text.singleton name) arg sig
    func parsed_arg ctx

-- ** parse

parse_dyn_sequence :: Text -> Either Text [Call Char]
parse_dyn_sequence = ParseText.parse p_dyn_calls

p_dyn_calls :: Parser [Call Char]
p_dyn_calls = A.skipSpace *> A.many1 (p_dyn_call <* A.skipSpace)

p_dyn_call :: Parser (Call Char)
p_dyn_call = do
    c <- A.anyChar
    arg <- if dyn_has_argument c then p_dyn_arg else return ""
    return $ Call c arg
    where
    p_dyn_arg = A.option "" (Text.singleton <$> A.digit)

dyn_has_argument :: Char -> Bool
dyn_has_argument c = Char.isUpper c || c == '<' || c == '>'

resolve_dyn_calls :: [Call Char] -> Either Text [Call (DynCall, Char)]
resolve_dyn_calls = mapM $ \(Call name arg) ->
    case Map.lookup name dyn_call_map of
        Nothing -> Left $ "dyn call not found: " <> showt name
        Just call -> return $ Call (call, name) arg

-- * State

type M s a = State.StateT s Derive.Deriver a

data State = State {
    state_from_pitch :: !PSignal.Pitch
    , state_transition :: !Typecheck.Normalized
    , state_current_pitch :: !PSignal.Pitch
    , state_previous_pitch :: !PSignal.Pitch
    , state_next_pitch :: !PSignal.Pitch
    }

instance Pretty.Pretty State where
    format (State from_pitch transition cur prev next) =
        Pretty.recordTitle "State"
            [ ("from_pitch", Pretty.format from_pitch)
            , ("transition", Pretty.format transition)
            , ("current_pitch", Pretty.format cur)
            , ("previous_pitch", Pretty.format prev)
            , ("next_pitch", Pretty.format next)
            ]

set_pitch :: PSignal.Pitch -> M State ()
set_pitch p = State.modify $ \state -> state { state_from_pitch = p }

get_from :: M State PSignal.Pitch
get_from = State.gets state_from_pitch

-- * sequence

newtype Result = Result (DList.DList PSignal.PSignal)
    deriving (Show)

instance Monoid Result where
    mempty = Result mempty
    mappend (Result pitch1) (Result pitch2) = Result (pitch1<>pitch2)

pitch_sequence :: ScoreTime -> State -> Text -> Derive.Deriver Result
pitch_sequence dur state arg = do
    calls <- Derive.require_right (("parsing " <> showt arg <> ": ")<>) $
        resolve_extensions =<< resolve_pitch_calls =<< parse_sequence arg
    let starts = slice_time dur (call_durations calls)
        ranges = zip starts (drop 1 starts)
    (results, _) <- State.runStateT (mapM eval_pitch (zip_calls ranges calls))
        state
    return $ mconcat results

slice_time :: ScoreTime -> [Double] -> [ScoreTime]
slice_time dur slices = scanl (+) 0 $ map ((*one) . ScoreTime.double) slices
    where one = dur / ScoreTime.double (sum slices)

eval_pitch :: Call ((ScoreTime, ScoreTime), PitchCall) -> M State Result
eval_pitch (Call ((start, end), pcall) arg_) = case pcall_call pcall of
    PCall signature func -> do
        parsed_arg <- parse_args (ctx_call_name ctx) (pcall_arg pcall arg_)
            signature
        (Result . DList.singleton) <$> func parsed_arg ctx
    where
    ctx = Context
        { ctx_start = start
        , ctx_end = end
        , ctx_call_name = Text.cons (pcall_name pcall) arg_
        }

data Call call = Call !call !Text
    deriving (Eq, Show, Functor)

call_durations :: [Call PitchCall] -> [Double]
call_durations = map pcall_duration . map (\(Call call _) -> call)

-- TODO surely I can do it in a simpler way?
zip_calls :: [a] -> [Call b] -> [Call (a, b)]
zip_calls xs calls = fst $ State.runState (traverse go calls) xs
    where
    go (Call call arg) = do
        x : xs <- State.get
        State.put xs
        return $ Call (x, call) arg

parse_sequence :: Text -> Either Text [ParsedPitch]
parse_sequence = ParseText.parse p_exprs

resolve_pitch_calls :: [ParsedPitch] -> Either Text [Call PitchCall]
resolve_pitch_calls = concatMapM resolve
    where
    resolve (PitchGroup exprs) =
        map (modify_duration (* (1 / fromIntegral (length exprs)))) <$>
            concatMapM resolve exprs
    resolve (CallArg c arg) = case Map.lookup c pitch_call_map of
        Nothing -> Left $ "pitch call not found: " <> showt c
        -- Apply the same argument to all of them.  But I should only get
        -- multiple PitchExprs for aliases, which expect no argument.
        Just calls -> Right [Call c arg | c <- calls]

resolve_extensions :: [Call PitchCall] -> Either Text [Call PitchCall]
resolve_extensions = resolve <=< check_no_args
    where
    resolve [] = Right []
    resolve (expr : exprs)
        | is_extension expr = Left "extension with no preceding call"
        | otherwise = (modify_duration (+ fromIntegral (length pre)) expr :)
            <$> resolve post
        where (pre, post) = span is_extension exprs
    check_no_args exprs
        | null errs = Right exprs
        | otherwise =
            -- The parser won't parse 1_2 anyway, but let's check anyway.
            Left $ "_ calls can't have args: " <> Text.intercalate ", " errs
        where errs = concatMap has_arg exprs
    has_arg expr@(Call _ arg)
        | is_extension expr && arg /= mempty = [arg]
        | otherwise = []
    is_extension (Call call _) = pcall_name call == extend_name

modify_duration :: (Double -> Double) -> Call PitchCall -> Call PitchCall
modify_duration modify = fmap $ \call ->
    if pcall_duration call > 0
        then call { pcall_duration = modify (pcall_duration call) }
        else call

-- * DynCall

data DynCall = forall a. DynCall {
    _dcall_doc :: Text
    , _dcall_signature :: Sig.Parser a
    , _dcall_func :: a -> Context -> M DynState Signal.Control
    }

dyn_call_map :: Map.Map Char DynCall
dyn_call_map = Map.fromList $
    [ ('=', dc_flat)
    , ('<', dc_move True)
    , ('>', dc_move False)
    ] ++ [(head (show n), dc_move_to (n/9)) | n <- [0..9]]

dc_flat :: DynCall
dc_flat = DynCall "No movement." Sig.no_args $ \() ctx -> do
    prev <- State.gets state_from_dyn
    start <- lift $ Derive.real (ctx_start ctx)
    return $ Signal.signal [(start, prev)]

-- < is from 0, to 1 or the arg
-- > is from prev value, to 0 or the arg
dc_move :: Bool -> DynCall
dc_move crescendo = DynCall doc sig1 $ \maybe_move args -> do
    (start, end) <- ctx_range args
    from <- if crescendo then return 0 else State.gets state_from_dyn
    let to = if crescendo
            then maybe 1 (from+) maybe_move
            else maybe 0 (from-) maybe_move
    dyn_curve start from end to
    where
    sig1 :: Sig.Parser (Maybe Double)
    sig1 = fmap normalize <$> Sig.defaulted "move" Nothing "Relative movement."
    normalize :: Int -> Double
    normalize = (/9) . fromIntegral
    doc = "Hi, doc\
        \ < -> from prev, to 1, align to start\
        \ > -> from prev, to 0, align to end\
        \ = -> from prev, to arg, align to middle"

dc_move_to :: Double -> DynCall
dc_move_to to = DynCall doc Sig.no_args $ \() args -> do
    (start, end) <- ctx_range args
    from <- State.gets state_from_dyn
    dyn_curve start from end to
    where
    doc = "doc doc"

dyn_curve :: RealTime -> Signal.Y -> RealTime -> Signal.Y
    -> M DynState Signal.Control
dyn_curve start from end to = do
    State.modify $ \state -> state { state_from_dyn = to }
    lift $ ControlUtil.make_segment curve start from end to
    where
    crescendo = to >= from
    weight = if crescendo then 0.9 else 0
    curve = snd ControlUtil.sigmoid_curve $
        if crescendo then (0, weight) else (weight, 0)

-- * PitchCall

data PitchCall = PitchCall {
    -- TODO take this out, as with DynCall
    -- then I can merge PCall into this, as with DynCall
    pcall_name :: !Char
    , _pcall_doc :: !Text
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
    _pcall_signature :: Sig.Parser a
    , _pcall_func :: a -> Context -> M State PSignal.PSignal
    }

pitch_call_map :: Map.Map Char [PitchCall]
pitch_call_map = resolve $ Map.unique $ concat
    [ [pcall '=' "Hold flat pitch." pc_flat]
    -- relative motion
    , [parse_name $ pcall c "Relative motion." (pc_relative True)
        | c <- "0123456789"]
    , [parse_name $ pcall '-' "Negative relative motion." (pc_relative True)]
    , [alias c 1 [showt n] | (c, n) <- zip "abc" [-1, -2 ..]]

    , [pcall 'e' "Pitch up by a little." (pc_relative_move (Pitch.Nn 1))]
    , [pcall 'f' "Pitch down by a little." (pc_relative_move (Pitch.Nn (-1)))]
    , [alias 'n' 0.5 ["e", "f"]]
    , [alias 'u' 0.5 ["f", "e"]]

    , [ pcall 'h' "Absolute motion to current pitch."
        (pc_move_direction Current)
      , pcall 'v' "Absolute motion to next pitch." (pc_move_direction Next)

      -- set config
      , config extend_name "Extend the duration of the previous call." pc_extend
      , config '<' "Set from pitch to previous." (pc_set_pitch Previous)
      , config '^' "Set from pitch to current." (pc_set_pitch Current)
      , config 'P' "Set from pitch to relative steps."
        (pc_set_pitch_relative False)
      , config 'T' "Set from pitch relative to swaram."
        (pc_set_pitch_relative True)
      , config 'F' "Fast transition time." (pc_set_transition_time Fast)
      , config 'M' "Medium transition time." (pc_set_transition_time Medium)
      , config 'S' "Slow transition time." (pc_set_transition_time Slow)
      ]
    ]
    where
    resolve (calls, duplicates)
        | null duplicates = either (error . untxt) id (resolve_aliases calls)
        | otherwise = error $ "duplicate calls: " <> show (map fst duplicates)
    parse_name = second $ second $ \g -> g { pcall_parse_call_name = True }
    alias name duration to = (name, Left to)
    pcall name doc c = (name, Right $ PitchCall name doc 1 False c)
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
            parsed <- parse_args (ctx_call_name ctx) (pcall_arg call arg)
                signature
            func parsed ctx
    }
    where
    -- Accept anything for an argument but ignore it.  This is because
    -- I've already hardcoded the argument, but 'eval_pitch' will want to apply
    -- it anyway, since it can't tell the difference from an alias call and
    -- a normal call.
    ignore = Sig.defaulted "ignore" (BaseTypes.num 0) ""

-- ** PitchCall implementation

parse_args :: State.MonadTrans m => Text -> Text -> Sig.Parser a
    -> m Derive.Deriver a
parse_args name arg sig = lift $ do
    vals <- Derive.require_right (("parsing " <> showt name <> ": ") <>) $
        if Text.null arg then return [] else (:[]) <$> Parse.parse_val arg
    Sig.require_right
        =<< Sig.parse_vals sig (Derive.dummy_context 0 1 name) name vals

-- | Here I am reinventing Derive.Call yet again.  This is the equivalent of
-- 'Derive.Context' and 'Derive.PassedArgs'.
data Context = Context {
    ctx_start :: !ScoreTime
    , ctx_end :: !ScoreTime
    -- | Complete call name, first char consed to arg.
    , ctx_call_name :: !Text
    } deriving (Show)

ctx_range :: Context -> M s (RealTime, RealTime)
ctx_range ctx = lift $
    (,) <$> Derive.real (ctx_start ctx) <*> Derive.real (ctx_end ctx)

-- | This is just a place holder, its effect is applied in 'resolve_extensions'.
pc_extend :: PCall
pc_extend = PCall Sig.no_args $ \() _ctx -> return mempty

extend_name :: Char
extend_name = '_'

pc_flat :: PCall
pc_flat = PCall Sig.no_args $ \() ctx -> do
    pitch <- get_from
    (start, end) <- ctx_range ctx
    return $ PSignal.signal [(start, pitch), (end, pitch)]

pc_relative :: Bool -> PCall
pc_relative swaram_relative = PCall (Sig.required "to" "To pitch.")
    $ \arg ctx -> case arg of
        Left (BaseTypes.Symbol sym)
            | sym == "-" -> do
                (start, end) <- ctx_range ctx
                pitch <- get_from
                return $ PSignal.signal [(start, pitch), (end, pitch)]
            | otherwise -> lift $ Derive.throw $ "unknown move: " <> showt sym
        Right (Typecheck.DefaultDiatonic transpose) -> do
            from_pitch <- if swaram_relative
                then State.gets state_current_pitch else get_from
            move_to ctx (Pitches.transpose transpose from_pitch)

pc_relative_move :: Pitch.Transpose -> PCall
pc_relative_move transpose = PCall Sig.no_args $ \() ctx -> do
    from_pitch <- get_from
    move_to ctx (Pitches.transpose transpose from_pitch)

data PitchDirection = Previous | Current | Next deriving (Show, Eq)
instance Pretty.Pretty PitchDirection where pretty = showt

get_direction_pitch :: PitchDirection -> M State PSignal.Pitch
get_direction_pitch dir = case dir of
    Previous -> State.gets state_previous_pitch
    Current -> State.gets state_current_pitch
    Next -> State.gets state_next_pitch

pc_move_direction :: PitchDirection -> PCall
pc_move_direction dir = PCall Sig.no_args $ \() ctx ->
    move_to ctx =<< get_direction_pitch dir

pc_set_pitch :: PitchDirection -> PCall
pc_set_pitch dir = PCall Sig.no_args $ \() _ctx -> do
    set_pitch =<< get_direction_pitch dir
    return mempty

pc_set_pitch_relative :: Bool -> PCall
pc_set_pitch_relative from_current = PCall (Sig.required "to" "To pitch.") $
    \arg _ctx -> do
        transpose <- lift $ parse_transpose arg
        set_pitch . Pitches.transpose transpose
            =<< if from_current then State.gets state_current_pitch
                else get_from
        return mempty

parse_transpose :: Either Pitch.Transpose BaseTypes.Symbol
    -> Derive.Deriver Pitch.Transpose
parse_transpose (Left t) = return t
parse_transpose (Right (BaseTypes.Symbol sym)) = case untxt sym of
    [c] | 'a' <= c && c <= 'z' -> return $ Pitch.Diatonic $ fromIntegral $
        fromEnum 'a' - fromEnum c - 1
    _ -> Derive.throw $ "expected a lowercase letter: " <> showt sym

data TransitionTime = Slow | Medium | Fast deriving (Show, Eq)

pc_set_transition_time :: TransitionTime -> PCall
pc_set_transition_time time = PCall Sig.no_args $ \() _ctx -> do
    State.modify $ \state -> state { state_transition = ttime }
    return mempty
    where
    -- TODO these could come from an environ value
    ttime = Typecheck.Normalized $ case time of
        Fast -> 0.1
        Medium -> 0.5
        Slow -> 0.9

-- ** util

move_to :: Context -> PSignal.Pitch -> M State PSignal.PSignal
move_to ctx pitch = do
    (start, end) <- ctx_range ctx
    from_pitch <- get_from
    move_pitch start from_pitch end pitch

move_pitch :: RealTime -> PSignal.Pitch -> RealTime -> PSignal.Pitch
    -> M State PSignal.PSignal
move_pitch start from_pitch end to_pitch = do
    Typecheck.Normalized transition <- State.gets state_transition
    let curve = snd ControlUtil.sigmoid_curve (1-transition, 1-transition)
    set_pitch to_pitch
    lift $ PitchUtil.make_segment curve start from_pitch end to_pitch

-- * parser

data ParsedPitch = CallArg !Char !Text | PitchGroup ![ParsedPitch]
    deriving (Show, Eq)

type Parser a = A.Parser a

p_exprs :: Parser [ParsedPitch]
p_exprs = A.skipSpace *> A.many1 (p_expr <* A.skipSpace)

p_expr :: Parser ParsedPitch
p_expr = p_group <|> p_pitch_expr

p_pitch_expr :: Parser ParsedPitch
p_pitch_expr = do
    c <- A.satisfy $ \c -> c /=' ' && c /= '[' && c /= ']'
    if pitch_has_argument c
        then CallArg c <$> p_pitch_expr_arg
        else return $ CallArg c ""

p_group :: Parser ParsedPitch
p_group = PitchGroup <$> (A.char '[' *> p_exprs <* A.char ']')

p_pitch_expr_arg :: Parser Text
p_pitch_expr_arg = do
    minus <- A.option False (A.char '-' >> return True)
    c <- A.satisfy (/=' ')
    return $ (if minus then ("-"<>) else id) (Text.singleton c)

pitch_has_argument :: Char -> Bool
pitch_has_argument c = Char.isUpper c || c == '-'


-- * misc

c_sahitya :: Derive.Taggable a => Derive.Transformer a
c_sahitya = Derive.transformer module_ "sahitya" mempty
    "Ignore the transformed deriver. Put this on a track to ignore its\
    \ contents, and put in sahitya."
    $ Sig.call0t $ \_args _deriver -> return Stream.empty
