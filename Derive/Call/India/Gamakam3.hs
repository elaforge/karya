-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ExistentialQuantification #-}
-- | Calls for Carnatic gamakam.
module Derive.Call.India.Gamakam3 where
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Args as Args
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Derive as Derive
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Pitches as Pitches
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import Global
import Types


module_ :: Module.Module
module_ = "india" <> "gamakam3"

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.call_maps [(Parse.unparsed_call, c_sequence)] []

c_sequence :: Derive.Generator Derive.Pitch
c_sequence = Derive.generator1 module_ "sequence" mempty sequence_doc
    $ Sig.call ((,) <$> Sig.required "text" sequence_arg_doc <*> config_env)
    $ \(text, transition) args -> do
        let (start, end) = Args.range_or_next args
        maybe_state <- get_state transition args
        case maybe_state of
            Nothing -> return mempty
            Just state ->
                Derive.at start $ pitch_sequence (end - start) state text
    where
    config_env :: Sig.Parser TrackLang.Normalized
    config_env = Sig.environ "transition" Sig.Both (TrackLang.Normalized 0.5)
        "Time for each pitch movement, in proportion of the total time\
        \ available."

get_state :: TrackLang.Normalized -> Derive.PassedArgs a
    -> Derive.Deriver (Maybe State)
get_state transition args =
    -- If there's no pitch then this is likely at the edge of a slice, and can
    -- be ignored.  TODO I think?
    justm (get_pitch (Args.start args)) $ \cur -> do
        maybe_prev <- maybe (return Nothing) get_pitch $ Args.prev_start args
        maybe_next <- maybe (return Nothing) get_pitch $ Args.next_start args
        return $ Just $ State
            { state_from_pitch = cur
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

-- * EvalM

type EvalM a = State.StateT State Derive.Deriver a

data State = State {
    state_from_pitch :: !PSignal.Pitch
    , state_transition :: !TrackLang.Normalized
    , state_current_pitch :: !PSignal.Pitch
    , state_previous_pitch :: !PSignal.Pitch
    , state_next_pitch :: !PSignal.Pitch
    }

instance Pretty.Pretty State where
    format (State from transition cur prev next) =
        Pretty.recordTitle "State"
            [ ("from_pitch", Pretty.format from)
            , ("transition", Pretty.format transition)
            , ("current_pitch", Pretty.format cur)
            , ("previous_pitch", Pretty.format prev)
            , ("next_pitch", Pretty.format next)
            ]

set_pitch :: PSignal.Pitch -> EvalM ()
set_pitch p = State.modify $ \state -> state { state_from_pitch = p }

get_from :: EvalM PSignal.Pitch
get_from = State.gets state_from_pitch

-- * sequence

pitch_sequence :: ScoreTime -> State -> Text -> Derive.Deriver PSignal.Signal
pitch_sequence dur state arg = do
    gamakams <- Derive.require_right ("parsing: "<>) $ parse_sequence arg
    let starts = slice_time dur (map (gamakam_duration . fst) gamakams)
        ranges = zip starts (drop 1 starts)
    (sigs, _) <- State.runStateT (mapM eval (zip ranges gamakams)) state
    return $ mconcat sigs

eval :: ((ScoreTime, ScoreTime), (Gamakam, Text)) -> EvalM PSignal.Signal
eval ((start, end), (gamakam, arg)) = case gamakam_call gamakam of
    Call signature func -> do
        arg <- parse_args args signature
        func arg args
    where
    args = Args
        { args_start = start
        , args_end = end
        , args_arg = if gamakam_parse_call_name gamakam then call_name else arg
        , args_call_name = call_name
        }
    call_name = Text.cons (gamakam_name gamakam) arg

slice_time :: ScoreTime -> [Double] -> [ScoreTime]
slice_time dur slices = scanl (+) 0 $ map ((*one) . ScoreTime.double) slices
    where one = dur / ScoreTime.double (sum slices)

-- Gamakam start with one character, then have one argument, with no spaces.
-- If the second letter is '-' then the stuff after it is an argument, until
-- space.
--
-- Then r--1 looks terrible.  r_-1?
-- Or use capital variant?
-- The thing is, I can't tell the difference between -1
-- Or I could mark a sequence like :12341
-- But then the common case of just one compressed arg would always have that
-- extra character.  If it was another # it would look like ##0101
--
-- Another option would be a magic character that makes the whole thing non
-- abbreviated: #123 is short, but '## 1 2 3' is long.  Or simply the presence
-- of a space, but then I can't write a single '# 1nn'... unless I count the
-- leading space.  Is that too weird?
parse_sequence :: Text -> Either Text [(Gamakam, Text)]
parse_sequence = collect_errors . concatMap (parse 0) . lex_sequence
    where
    collect_errors xs
        | null errs = Right ok
        | otherwise = Left $ Text.intercalate ", " errs
        where (errs, ok) = Seq.partition_either xs
    parse recursion (c, arg) = case Map.lookup c gamakam_map of
        Nothing -> [Left $ "not found: " <> showt c]
        Just (Left gamakam) -> [Right (gamakam, arg)]
        Just (Right to)
            | recursion >= max_recursion ->
                [Left $ "too many levels of recursion: " <> showt c]
            | otherwise -> map (first (("in alias " <> showt to <> ": ") <>)) $
                concatMap (parse (recursion+1)) (lex_word to)
    max_recursion = 4

lex_sequence :: Text -> [(Char, Text)]
lex_sequence = concatMap lex_word . Text.words

lex_word :: Text -> [(Char, Text)]
lex_word word = case Text.uncons word of
    Just (c, cs)
        | c == sequence_prefix -> [(c, "") | c <- untxt cs]
        | otherwise -> [(c, cs)]
    _ -> []

sequence_prefix :: Char
sequence_prefix = '#'

-- * Gamakam

data Gamakam = Gamakam {
    gamakam_name :: !Char
    , gamakam_doc :: !Text
    , gamakam_duration :: !Double
    -- | If True, cons 'gamakam_name' on to the arg before parsing it.
    , gamakam_parse_call_name :: !Bool
    , gamakam_call :: !Call
    }

gamakam_map :: Map.Map Char (Either Gamakam Text)
(gamakam_map, _duplicates) = Map.unique $ map to_pair $
    [ emit '0' "Hold flat pitch." g_flat
    , parse_name $ emit '-' "Negative relative motion." g_move
    ] ++ [parse_name $ emit c "Relative motion." g_move | c <- "123456789"] ++
    [ alias 'b' "-2", alias 'a' "-1"
    , alias 'y' "-1nn", alias 'z' "1nn" -- relative motion by NN
    , emit 'c' "Absolute motion to current pitch." (g_move_absolute Current)
    , emit 'd' "Absolute motion to next pitch." (g_move_absolute Next)
    , alias 'u' "#a1", alias 'n' "#1a" -- single turn
    , alias 'v' "a1 but fast, and with separate default for bottom"
    , emit 'j' "Janta." g_janta

    , config '<' "Set from pitch to previous." (g_set_pitch Previous)
    , config '^' "Set from pitch to current." (g_set_pitch Current)
    , config 'P' "Set from pitch to relative steps." g_set_pitch_relative
    , config 'F' "Fast transition time." (g_set_transition_time (Just Fast))
    , config 'M' "Medium transition time." (g_set_transition_time (Just Medium))
    , config 'S' "Slow transition time." (g_set_transition_time (Just Slow))
    , config 'T' "Set slice time of the next call." g_set_next_time_slice
    ]
    where
    parse_name = first $ \g -> g { gamakam_parse_call_name = True }
    emit name doc = Left . Gamakam name doc 1 False
    config name doc = Left . Gamakam name doc 0 False
    alias name gamakam = Right (name, gamakam)
    to_pair v = (either gamakam_name fst v, second snd v)

-- ** Gamakam implementation

-- I want gamakam args to get the full arg treatment, such as defaults
-- and quoted evaluation.  Then I can configure FMS.
-- Calls also get arg docs.

parse_args :: Args -> Sig.Parser a -> EvalM a
parse_args args sig = lift $ do
    vals <- Derive.require_right
        (("parsing " <> showt (args_call_name args) <> ": ") <>) $
        if Text.null (args_arg args) then return []
            else (:[]) <$> Parse.parse_val (args_arg args)
    Sig.require_right
        =<< Sig.parse_vals sig (Derive.dummy_call_info 0 1 name) name vals
    where name = args_call_name args

data Call = forall a. Call {
    call_signature :: Sig.Parser a
    , call_func :: a -> Args -> EvalM PSignal.Signal
    }

-- Here we are reinventing Derive.Call yet again...
data Args = Args {
    args_start :: !ScoreTime
    , args_end :: !ScoreTime
    , args_arg :: !Text
    -- | Complete call name, first char consed to arg.
    , args_call_name :: !Text
    } deriving (Show)

-- TODO if everyone winds up wanting RealTime I can put this in Args
args_range :: Args -> EvalM (RealTime, RealTime)
args_range args = lift $
    (,) <$> Derive.real (args_start args) <*> Derive.real (args_end args)

g_flat :: Call
g_flat = Call Sig.no_args $ \() args -> do
    pitch <- get_from
    (start, end) <- args_range args
    return $ PSignal.signal [(start, pitch), (end, pitch)]

g_move :: Call
g_move = Call (Sig.required "to" "To pitch.") $ \arg args -> do
    (start, end) <- args_range args
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

g_move_absolute :: PitchDirection -> Call
g_move_absolute dir = Call Sig.no_args $ \() args -> do
    (start, end) <- args_range args
    from_pitch <- get_from
    to_pitch <- get_direction_pitch dir
    move start from_pitch end to_pitch

g_janta :: Call
g_janta = Call Sig.no_args $ \() _args -> lift $ Derive.throw "janta"

g_set_pitch :: PitchDirection -> Call
g_set_pitch dir = Call Sig.no_args $ \() _args -> do
    set_pitch =<< get_direction_pitch dir
    return mempty

g_set_pitch_relative :: Call
g_set_pitch_relative = Call (Sig.required "to" "To pitch.") $
    \(TrackLang.DefaultDiatonic transpose) _args -> do
        set_pitch . Pitches.transpose transpose =<< get_from
        return mempty

data TransitionTime = Slow | Medium | Fast deriving (Show, Eq)

g_set_transition_time :: Maybe TransitionTime -> Call
g_set_transition_time _time = Call Sig.no_args $ \() _args ->
    lift $ Derive.throw "set transition time"

g_set_next_time_slice :: Call
g_set_next_time_slice = Call Sig.no_args $ \() _args ->
    lift $ Derive.throw "set next time slice"
