-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Calls for Carnatic gamakam.
module Derive.C.India.Gamakam5 (
    library
#ifdef TESTING
    , module Derive.C.India.Gamakam5
#endif
) where
import qualified Control.Monad.State as State
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as Char
import qualified Data.DList as DList
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Map as Map
import qualified Util.ParseText as ParseText
import qualified Util.Pretty as Pretty
import qualified Util.Segment as Segment
import qualified Util.Seq as Seq

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Pitches as Pitches
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global
import Types


module_ :: Module.Module
module_ = "india" <> "gamakam5"

library :: Library.Library
library = mconcat
    [ Library.generators
        [ (Parse.unparsed_call, c_pitch_sequence)
        , (Parse.unparsed_call, c_dyn_sequence)
        ]
    , Library.transformers
        [ ("gamak", c_import_pitch)
        , ("dyn", c_import_dyn)
        ]
    , Library.transformers
        [ ("sahitya", c_sahitya :: Derive.Transformer Derive.Note)
        ]
    ]

-- * sequence

c_import_pitch :: Derive.Transformer Derive.Control
c_import_pitch = Derive.transformer module_ "gamak" mempty
    "Import calls for a gamakam track."
    $ Sig.call0t $ \_args -> Derive.with_imported False (module_ <> "pitch")

c_import_dyn :: Derive.Transformer Derive.Control
c_import_dyn = Derive.transformer module_ "dyn" mempty
    "Import calls for a dyn track."
    $ Sig.call0t $ \_args -> Derive.with_imported False (module_ <> "dyn")

c_pitch_sequence :: Derive.Generator Derive.Control
c_pitch_sequence = Derive.generator1 (module_ <> "pitch")
    "sequence" mempty pitch_sequence_doc
    $ Sig.call ((,,)
        <$> Sig.required "sequence" "Pitch calls."
        <*> transition_env
        <*> Sig.environ "gamakam-above" Sig.Unprefixed False
            "Expect pitch and gamakam tracks above the note track."
    ) $ \(text, transition, pitch_above) args -> do
        (start, end) <- if pitch_above
            then (Args.start args,) <$> infer_end args
            else Args.range_or_note_end args
        maybe_state <- initial_pitch_state pitch_above transition args
        case maybe_state of
            Nothing -> return mempty
            Just state -> do
                Result signals <- Derive.at start $
                    pitch_sequence (end - start) state text
                return $ mconcat $ DList.toList signals
    where
    transition_env :: Sig.Parser Typecheck.Normalized
    transition_env =
        Sig.environ "transition" Sig.Both (Typecheck.Normalized 0.5) $
            "Time for each pitch movement, in proportion of the total time"
            <> " available."

-- | Infer the end time for the gamakam as the next pitch in the pitch signal,
-- which should correspond to the next explicit swaram.
infer_end :: Derive.PassedArgs a -> Derive.Deriver TrackTime
infer_end args
    | Args.end args /= Args.start args = return $ Args.end args
    | otherwise = do
        pitch <- Derive.get_pitch
        start <- Args.real_start args
        case next_sample start pitch of
            Nothing -> return $ Args.start args
            Just (x, _) -> Derive.score x

initial_pitch_state :: Bool -> Typecheck.Normalized
    -> Derive.PassedArgs Derive.Control
    -> Derive.Deriver (Maybe PitchState)
initial_pitch_state pitch_above transition args = do
    start <- Args.real_start args
    -- If there's no pitch then this is likely at the edge of a slice, and can
    -- be ignored.  TODO I think?
    justm (lookup_pitch start) $ \current -> do
        (prev, next, from) <- if pitch_above
            then get_neighbor_pitches_above start
            else get_neighbor_pitches (Args.start args)

        let prev_step = maybe 0 snd $ Args.prev_control args
        let steps_from_current = fmap (fromMaybe 0) . traverse
                ((`step_difference` current) <=< Derive.resolve_pitch start)
        prev <- steps_from_current prev
        from <- steps_from_current $
            Pitches.transpose_nn (Pitch.nn prev_step) <$> from
        next <- steps_from_current next

        return $ Just $ PitchState
            { _swaram = current
            , _from = from
            , _prev = prev
            , _next = next
            , _transition = transition
            }
    where
    lookup_pitch = Call.transposed

get_neighbor_pitches :: ScoreTime -> Derive.Deriver
    (Maybe PSignal.Pitch, Maybe PSignal.Pitch, Maybe PSignal.Pitch)
get_neighbor_pitches start = (,,)
    <$> Args.lookup_prev_note_pitch start
    <*> Args.lookup_next_note_pitch start
    <*> (get_prev_pitch =<< Derive.real start)

get_neighbor_pitches_above :: RealTime -> Derive.Deriver
    (Maybe PSignal.Pitch, Maybe PSignal.Pitch, Maybe PSignal.Pitch)
get_neighbor_pitches_above start = do
    pitch <- Derive.get_pitch
    let prev = PSignal.at_negative start pitch
    let next = snd <$> next_sample start pitch
    return (prev, next, prev)

next_sample :: RealTime -> PSignal.PSignal -> Maybe (RealTime, PSignal.Pitch)
next_sample x pitch = do
    Segment.Segment _ _ x2 _ <- PSignal.segment_at x pitch
    (x2,) <$> PSignal.at x2 pitch

get_prev_pitch :: RealTime -> Derive.Deriver (Maybe PSignal.Pitch)
get_prev_pitch = Args.prev_note_pitch

-- TODO this is how it used to work, which is complicated, and only mostly
-- worked.  If I move to *_above, then maybe I can get rid of this entirely.
-- get_prev_pitch_old :: RealTime -> Derive.Deriver (Maybe PSignal.Pitch)
-- get_prev_pitch_old start = do
--     -- If this is the first call in this note, then I have to look in the
--     -- previous slice.
--     prev_event_pitch <- Args.prev_note_pitch start
--     -- TODO gives different results for test_prev_pitch, but I don't understand
--     -- which is right.
--     -- pitch_signal <- PSignal.last . PSignal.drop_after start <$> Derive.get_pitch
--     pitch_signal <- before start <$> Derive.get_pitch
--     return $ case (prev_event_pitch, pitch_signal) of
--         (Just a, Just b) -> Just $ snd $ Seq.max_on fst a b
--         (a, b) -> snd <$> (a <|> b)
--     -- Getting the previous pitch is kind of ridiculously complicated.
--     -- First, Args.prev_pitch means there was a preceding pitch call, so
--     -- I take that one if present.  Otherwise, the pitch can come from
--     -- either the previous event, or the previous pitch on the current
--     -- pitch signal, whichever is newer.  The current pitch signal will
--     -- be newer if there is a pitch on the parent pitch track with no
--     -- corresponding gamakam on this one.

before :: RealTime -> PSignal.PSignal -> Maybe (RealTime, PSignal.Pitch)
before x sig = case PSignal.segment_at x sig of
    Just (Segment.Segment x y _ _) -> Just (x, y)
    Nothing -> Nothing

step_difference :: PSignal.Transposed -> PSignal.Transposed
    -> Derive.Deriver Step
step_difference p1 p2 = fmap realToFrac $
    (-) <$> Pitches.pitch_nn p1 <*> Pitches.pitch_nn p2

pitch_sequence_doc :: Doc.Doc
pitch_sequence_doc = Doc.Doc $ mconcat
    [ "This is a mini-language, where each one or two characters is a call."
    , " An upper-case call will take a single character argument. A special"
    , " parsing rule means that `-` and its following character is considered"
    , " a single character, so `-1` is a valid call or argument."
    , " Most of these calls represent a pitch movement:\n"
    , Text.unlines (map pitch_call_doc (Map.toAscList pitch_call_map))
    , "\nCurrently the transition curve is hardcoded to a sigmoid curve, but"
    , " I could add a curve env var if necessary."
    ]

pitch_call_doc :: (Char, [PitchCall]) -> Text
pitch_call_doc (name, pcalls) =
    "`" <> Text.singleton name <> "` - "
        <> Text.intercalate "; " (map doc_of pcalls)
    where
    doc_of pcall = pcall_doc pcall
        <> if pcall_duration pcall /= 1
            then " (dur " <> pretty (pcall_duration pcall) <> ")" else ""

pitch_call_map :: Map Char [PitchCall]
pitch_call_map = resolve $ Map.unique $ concat
    [ [pcall '=' "Hold flat pitch." pc_flat]
    -- relative motion
    , [parse_name $ pcall c "Relative motion." pc_relative | c <- "0123456789"]
    , [parse_name $ pcall '-' "Negative relative motion." pc_relative]
    , [alias c 1 [showt n] | (c, n) <- zip "abc" [-1, -2 ..]]

    , [pcall 'e' "Pitch up by 1nn." (pc_relative_move (Pitch.Nn 1))]
    , [pcall 'f' "Pitch down by 1nn." (pc_relative_move (Pitch.Nn (-1)))]
    , [pcall 'g' "Pitch up by .5nn." (pc_relative_move (Pitch.Nn 0.5))]
    , [pcall 'h' "Pitch down by .5nn." (pc_relative_move (Pitch.Nn (-0.5)))]
    , [alias 'n' 0.5 ["e", "f"]]
    , [alias 'u' 0.5 ["f", "e"]]

    , [ pcall 'v' "Absolute motion to next pitch." (pc_move_direction Next)
      -- set config
      , config '<' "Set from pitch to previous." (pc_set_pitch_from Previous)
      , config '^' "Set from pitch to current." (pc_set_pitch_from Current)
      , config '&' "Set from pitch to next." (pc_set_pitch_from Next)
      , config 'T' "Set from pitch relative to swaram." pc_set_pitch
      , config 'F' "Fast transition time." (pc_set_transition_time Fast)
      , config 'M' "Medium transition time." (pc_set_transition_time Medium)
      , config 'S' "Slow transition time." (pc_set_transition_time Slow)
      ]
    -- Just a placeholder, effects are actually applied by 'resolve_postfix'.
    , [ config c postfix_doc (PCall Sig.no_args $ \() _ctx -> return mempty)
      | c <- Map.keys postfix_calls
      ]
    ]
    where
    resolve (calls, duplicates)
        | null duplicates = either errorStack id (resolve_aliases calls)
        | otherwise =
            errorStack $ "duplicate calls: " <> showt (map fst duplicates)
    parse_name = second $ second $ \g -> g { pcall_parse_call_name = True }
    alias name duration to = (name, Left (duration, to))
    pcall name doc c = (name, Right $ PitchCall doc 1 False c)
    config name doc c = (name, Right $ PitchCall doc 0 False c)

-- * dyn-sequence

c_dyn_sequence :: Derive.Generator Derive.Control
c_dyn_sequence = Derive.generator1 (module_ <> "dyn") "dyn-sequence" mempty doc
    $ Sig.call (Sig.required "sequence" "Dyn calls.")
    $ \text args -> do
        (start, end) <- Args.range_or_note_end args
        let state = DynState
                { state_from_dyn = maybe 0 snd (Args.prev_control args) }
        Derive.at start $ dyn_sequence (end - start) state text
    where
    doc = Doc.Doc $ mconcat
        [ "This is a mini-language, where each one or two characters is a call."
        , " Each character can take an argument, which can only be a single"
        , " digit. Typically this represents a dyn level / 9, so 0 is 0 and"
        , " 9 is 1.  Calls:\n"
        , Text.unlines (map dyn_call_doc (Map.toAscList dyn_call_map))
        ]

dyn_call_doc :: (Char, DynCall) -> Text
dyn_call_doc (name, dcall) = "`" <> Text.singleton name <> "` - "
    <> dcall_doc dcall

dyn_call_map :: Map Char DynCall
dyn_call_map = Map.fromList $
    [ ('=', dc_flat)
    , ('<', dc_attack)
    , ('a', dc_attack_from)
    , ('>', dc_decay 0.8 0)
    , ('d', dc_decay 0 0.8)
    , ('T', dc_set_dyn)
    , ('.', dc_move_to)
    ]

newtype DynState = DynState { state_from_dyn :: Signal.Y }
    deriving (Show)

instance Pretty DynState where
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
            , ctx_call_name = Derive.CallName $ Text.singleton name
            }
    parsed_arg <- parse_args (Derive.CallName (Text.singleton name)) arg sig
    func parsed_arg ctx

-- ** parse

parse_dyn_sequence :: Text -> Either Text [Call Char]
parse_dyn_sequence = ParseText.parse p_dyn_calls

p_dyn_calls :: Parser [Call Char]
p_dyn_calls = A.skipSpace *> A.many1 (p_dyn_call <* A.skipSpace)

p_dyn_call :: Parser (Call Char)
p_dyn_call = do
    c <- A.anyChar
    arg <- A.option "" (Text.singleton <$> A.digit)
    return $ Call c arg

resolve_dyn_calls :: [Call Char] -> Either Text [Call (DynCall, Char)]
resolve_dyn_calls = mapM $ \(Call name arg) ->
    case Map.lookup name dyn_call_map of
        Nothing -> Left $ "dyn call not found: " <> showt name
        Just call -> return $ Call (call, name) arg

-- * State

type M s a = State.StateT s Derive.Deriver a

data PitchState = PitchState {
    -- # maintained automatically
    -- | Absolute pitch of the underlying note.  The Steps are relative to
    -- this.
    _swaram :: !PSignal.Transposed
    -- | Current pitch value.  Starts as the pitch at the end of the previous
    -- note.
    , _from :: !Step
    -- | Previous swaram.
    , _prev :: !Step
    -- | Next swaram.
    , _next :: !Step

    -- # maintained automatically
    -- | Transition time between pitch movements.
    , _transition :: !Typecheck.Normalized
    } deriving (Show)

-- | Relative NoteNumbers.
type Step = Double

instance Pretty PitchState where
    format (PitchState swaram from prev next transition) =
        Pretty.recordTitle "PitchState"
            [ ("swaram", Pretty.format swaram)
            , ("from", Pretty.format from)
            , ("prev", Pretty.format prev)
            , ("next", Pretty.format next)
            , ("transition", Pretty.format transition)
            ]

set_pitch :: PSignal.Transposed -> M PitchState ()
set_pitch p = do
    swaram <- State.gets _swaram
    steps <- lift $ step_difference p swaram
    State.modify $ \state -> state { _from = steps }

get_from :: M PitchState PSignal.Transposed
get_from = do
    steps <- State.gets _from
    swaram <- State.gets _swaram
    return $ Pitches.transpose_nn (Pitch.nn steps) swaram

-- * sequence

newtype Result = Result (DList.DList Signal.Control)
    deriving (Show, Monoid)

-- | Pitch.Transpose, without the value.  TODO probably should split those?
data Transpose = Diatonic | Chromatic | Nn
    deriving (Show)

pitch_sequence :: ScoreTime -> PitchState -> Text -> Derive.Deriver Result
pitch_sequence dur state arg = do
    calls <- Derive.require_right (("parsing " <> pretty arg <> ": ")<>) $
        resolve_postfix =<< resolve_pitch_calls =<< parse_pitch_sequence arg
    let starts = slice_time dur (call_durations calls)
        ranges = zip starts (drop 1 starts)
    (results, _) <- State.runStateT (mapM eval_pitch (zip_calls ranges calls))
        state
    return $ mconcat results

slice_time :: ScoreTime -> [Double] -> [ScoreTime]
slice_time dur slices = scanl (+) 0 $ map ((*one) . ScoreTime.double) slices
    where one = dur / ScoreTime.double (sum slices)

eval_pitch :: Call ((ScoreTime, ScoreTime), (PitchCall, Char))
    -> M PitchState Result
eval_pitch (Call ((start, end), (pcall, name)) arg_) = case pcall_call pcall of
    PCall signature func -> do
        parsed_arg <- parse_args (ctx_call_name ctx) (pcall_arg pcall name arg_)
            signature
        (Result . DList.singleton) <$> func parsed_arg ctx
    where
    ctx = Context
        { ctx_start = start
        , ctx_end = end
        , ctx_call_name = Derive.CallName $ Text.cons name arg_
        }

data Call call = Call !call !Text
    deriving (Eq, Show, Functor)

call_durations :: [Call (PitchCall, a)] -> [Double]
call_durations = map $ pcall_duration . (\(Call (pcall, _) _) -> pcall)

zip_calls :: [a] -> [Call b] -> [Call (a, b)]
zip_calls xs calls = [Call (x, c) arg | (x, Call c arg) <- zip xs calls]

parse_pitch_sequence :: Text -> Either Text [ParsedPitch]
parse_pitch_sequence = ParseText.parse p_exprs

resolve_pitch_calls :: [ParsedPitch] -> Either Text [Call (PitchCall, Char)]
resolve_pitch_calls = concatMapM resolve
    where
    resolve (PitchGroup calls) =
        map (modify_duration (* (1 / fromIntegral (length calls)))) <$>
            concatMapM resolve calls
    resolve (CallArg name arg) = case Map.lookup name pitch_call_map of
        Nothing -> Left $ "pitch call not found: " <> showt name
        -- Apply the same argument to all of them.  But I should only get
        -- multiple PitchExprs for aliases, which expect no argument.
        Just calls -> Right [Call (c, name) arg | c <- calls]

resolve_postfix :: [Call (PitchCall, Char)]
    -> Either Text [Call (PitchCall, Char)]
resolve_postfix = resolve <=< ensure_no_args
    where
    resolve [] = Right []
    resolve (call : calls)
        | Maybe.isJust (is_postfix call) =
            Left "postfix call with no preceding call"
        | otherwise = (modify_duration modify call :) <$> resolve post
        where
        (pre, post) = Seq.span_while is_postfix calls
        modify dur = foldl' (flip ($)) dur pre
    -- The parser shouldn't look for args, but let's check anyway.
    ensure_no_args calls
        | null errs = Right calls
        | otherwise = Left $
            "postfix calls can't have args: " <> Text.intercalate ", " errs
        where errs = concatMap has_arg calls
    has_arg call@(Call _ arg)
        | Maybe.isJust (is_postfix call) && arg /= mempty = [arg]
        | otherwise = []
    is_postfix (Call (_, name) _) = Map.lookup name postfix_calls

postfix_calls :: Map Char (Double -> Double)
postfix_calls = Map.fromList [('_', (+1)), ('.', (/2))]

postfix_doc :: Text
postfix_doc = "Postfix call that modifies the duration of the previous call."
    <> " `_` adds 1 to it, `.` divides by 2."

modify_duration :: (Double -> Double) -> Call (PitchCall, a)
    -> Call (PitchCall, a)
modify_duration modify = fmap $ first $ \call ->
    if pcall_duration call > 0
        then call { pcall_duration = modify (pcall_duration call) }
        else call

-- * DynCall

data DynCall = forall a. DynCall {
    dcall_doc :: Text
    , _dcall_signature :: Sig.Parser a
    , _dcall_func :: a -> Context -> M DynState Signal.Control
    }

dc_flat :: DynCall
dc_flat = DynCall "No movement." Sig.no_args $ \() ctx -> do
    prev <- State.gets state_from_dyn
    start <- lift $ Derive.real (ctx_start ctx)
    return $ Signal.from_sample start prev

dc_attack :: DynCall
dc_attack = DynCall doc dyn_arg $ \maybe_to ctx ->
    make_dyn_curve (dyn_curve 0 0.8) 0 (fromMaybe 1 maybe_to) ctx
    where doc = "Attack from 0."

dc_attack_from :: DynCall
dc_attack_from = DynCall doc dyn_arg $ \maybe_to ctx -> do
    from <- State.gets state_from_dyn
    make_dyn_curve (dyn_curve 0 0.8) from (fromMaybe 1 maybe_to) ctx
    where doc = "Attack from previous value."

dc_decay :: Double -> Double -> DynCall
dc_decay w1 w2 = DynCall doc dyn_arg $ \maybe_to ctx -> do
    from <- State.gets state_from_dyn
    make_dyn_curve (dyn_curve w1 w2) from (fromMaybe 0 maybe_to) ctx
    where doc = "Decay to 0, with curve weights: " <> pretty (w1, w2)

dc_move_to :: DynCall
dc_move_to = DynCall "Move to dyn." required_dyn_arg $ \to ctx -> do
    from <- State.gets state_from_dyn
    make_dyn_curve (dyn_curve 0.5 0.5) from to ctx

make_dyn_curve :: ControlUtil.Curve -> Signal.Y -> Signal.Y -> Context
    -> M DynState Signal.Control
make_dyn_curve curve from to ctx = do
    (start, end) <- ctx_range ctx
    State.modify $ \state -> state { state_from_dyn = to }
    lift $ ControlUtil.make_segment curve start from end to

dyn_curve :: Double -> Double -> ControlUtil.Curve
dyn_curve w1 w2 = ControlUtil.Function $ ControlUtil.sigmoid w1 w2

dc_set_dyn :: DynCall
dc_set_dyn = DynCall "Set from dyn." required_dyn_arg $ \to _ctx -> do
    State.modify $ \state -> state { state_from_dyn = to }
    return mempty

dyn_arg :: Sig.Parser (Maybe Signal.Y)
dyn_arg = fmap arg_to_dyn <$> Sig.defaulted "move" Nothing "Move to n/9."

required_dyn_arg :: Sig.Parser Signal.Y
required_dyn_arg = arg_to_dyn <$> Sig.required "move" "Move to n/9."

arg_to_dyn :: Int -> Signal.Y
arg_to_dyn = (/9) . fromIntegral

-- * PitchCall

data PitchCall = PitchCall {
    pcall_doc :: !Text
    , pcall_duration :: !Double
    -- | If True, cons the call's name on to the arg before parsing it.
    , pcall_parse_call_name :: !Bool
    , pcall_call :: !PCall
    }

-- | Argument parser and call function.
data PCall = forall a. PCall
    (Sig.Parser a) (a -> Context -> M PitchState Signal.Control)

pcall_arg :: PitchCall -> Char -> Text -> Text
pcall_arg pcall name arg
    | pcall_parse_call_name pcall = Text.cons name arg
    | otherwise = arg

resolve_aliases :: Map Char (Either (Double, [Text]) PitchCall)
    -> Either Text (Map Char [PitchCall])
resolve_aliases call_map = Map.fromList <$> mapM resolve (Map.toList call_map)
    where
    resolve (name, Right call) = Right (name, [call])
    resolve (name, Left (duration, calls)) =
        (,) name . map (set_dur duration) <$> mapM resolve1 calls
    set_dur dur pcall = pcall { pcall_duration = dur }
    resolve1 to = do
        (c, arg) <- justErr "empty alias" $ Text.uncons to
        call <- justErr ("not found: " <> showt c) $ Map.lookup c call_map
        call <- first (("alias to alias: "<>) . showt) call
        Right $ apply_arg call c arg

apply_arg :: PitchCall -> Char -> Text -> PitchCall
apply_arg call name arg = call
    { pcall_call = case pcall_call call of
        PCall signature func -> PCall ignore $ \_ ctx -> do
            parsed <- parse_args (ctx_call_name ctx) (pcall_arg call name arg)
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

parse_args :: State.MonadTrans m => Derive.CallName -> Text -> Sig.Parser a
    -> m Derive.Deriver a
parse_args name arg sig = lift $ do
    vals <- Derive.require_right (("parsing " <> pretty name <> ": ") <>) $
        if Text.null arg then return [] else (:[]) <$> Parse.parse_val arg
    Sig.require_right
        =<< Sig.parse_vals sig (Derive.dummy_context 0 1 (pretty name))
            name vals

-- | Here I am reinventing Derive.Call yet again.  This is the equivalent of
-- 'Derive.Context' and 'Derive.PassedArgs'.
data Context = Context {
    ctx_start :: !ScoreTime
    , ctx_end :: !ScoreTime
    -- | Complete call name, first char consed to arg.
    , ctx_call_name :: !Derive.CallName
    } deriving (Show)

ctx_range :: Context -> M s (RealTime, RealTime)
ctx_range ctx = lift $
    (,) <$> Derive.real (ctx_start ctx) <*> Derive.real (ctx_end ctx)

pc_flat :: PCall
pc_flat = PCall Sig.no_args $ \() ctx -> do
    step <- State.gets _from
    start <- lift $ Derive.real $ ctx_start ctx
    return $ Signal.from_sample start step

-- | Move relative to the note's swaram.
pc_relative :: PCall
pc_relative = PCall (Sig.required "to" "To pitch.") $
    \(Typecheck.DefaultDiatonic transpose) ctx -> do
        from <- State.gets _swaram
        move_to ctx (Pitches.transpose transpose from)

-- | Move relative to the current pitch.
pc_relative_move :: Pitch.Transpose -> PCall
pc_relative_move transpose = PCall Sig.no_args $ \() ctx -> do
    from_pitch <- get_from
    move_to ctx (Pitches.transpose transpose from_pitch)

data PitchDirection = Previous | Current | Next deriving (Show, Eq)
instance Pretty PitchDirection where pretty = showt

pc_move_direction :: PitchDirection -> PCall
pc_move_direction dir = PCall Sig.no_args $ \() ctx ->
    move_to ctx =<< get_direction_pitch dir

pc_set_pitch_from :: PitchDirection -> PCall
pc_set_pitch_from dir = PCall Sig.no_args $ \() _ctx -> do
    set_pitch =<< get_direction_pitch dir
    return mempty

get_direction_pitch :: PitchDirection -> M PitchState PSignal.Transposed
get_direction_pitch dir = case dir of
    Previous -> Pitches.transpose_nn <$> (Pitch.nn <$> State.gets _prev)
        <*> State.gets _swaram
    Current -> State.gets _swaram
    Next -> Pitches.transpose_nn <$> (Pitch.nn <$> State.gets _next)
        <*> State.gets _swaram

pc_set_pitch :: PCall
pc_set_pitch = PCall (Sig.required "to" "To pitch.") $
    \arg _ctx -> do
        transpose <- lift $ parse_transpose arg
        set_pitch . Pitches.transpose transpose =<< State.gets _swaram
        return mempty

parse_transpose :: Either Pitch.Transpose Expr.Str
    -> Derive.Deriver Pitch.Transpose
parse_transpose (Left t) = return t
parse_transpose (Right (Expr.Str sym)) = case untxt sym of
    [c] | 'a' <= c && c <= 'z' -> return $ Pitch.Diatonic $ fromIntegral $
        fromEnum 'a' - fromEnum c - 1
    _ -> Derive.throw $ "expected a lowercase letter: " <> showt sym

data TransitionTime = Slow | Medium | Fast deriving (Show, Eq)

pc_set_transition_time :: TransitionTime -> PCall
pc_set_transition_time time = PCall Sig.no_args $ \() _ctx -> do
    State.modify $ \state -> state { _transition = ttime }
    return mempty
    where
    -- TODO these could come from an environ value
    ttime = Typecheck.Normalized $ case time of
        Fast -> 0.1
        Medium -> 0.5
        Slow -> 0.9

-- ** util

move_to :: Context -> PSignal.Transposed -> M PitchState Signal.Control
move_to ctx to = do
    (start, end) <- ctx_range ctx
    from <- get_from
    move_pitch start from end to

move_pitch :: RealTime -> PSignal.Transposed -> RealTime -> PSignal.Transposed
    -> M PitchState Signal.Control
move_pitch start from end to = do
    Typecheck.Normalized transition <- State.gets _transition
    let curve = ControlUtil.Function $
            ControlUtil.sigmoid (1-transition) (1-transition)
    set_pitch to
    swaram <- State.gets _swaram
    from <- lift $ step_difference from swaram
    to <- lift $ step_difference to swaram
    lift $ ControlUtil.make_segment curve start from end to

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
    ("Ignore the transformed deriver. Put this on a track to ignore its"
    <> " contents, and put in sahitya.")
    $ Sig.call0t $ \_args _deriver -> return Stream.empty
