-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.India.Gamakam6 where
import qualified Control.Applicative as Applicative
import qualified Control.Monad.State as State
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.ParseText as ParseText
import qualified Util.Segment as Segment
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Pitches as Pitches
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Event as Event
import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


module_ :: Module.Module
module_ = "india" <> "gamakam6"

library :: Library.Library
library = mconcat
    [ Library.generators
        [ (Parse.unparsed_call, c_pitch_sequence)
        -- , (Parse.unparsed_call, c_dyn_sequence)
        ]
    , Library.transformers
        [ ("gamak", c_import_pitch)
        -- , ("dyn", c_import_dyn)
        ]
    , Library.transformers
        [ ("sahitya", c_sahitya :: Derive.Transformer Derive.Note)
        ]
    ]

-- * State

type M a = State.StateT PitchState Derive.Deriver a

data PitchState = PitchState {
    -- # maintained automatically
    -- | Pitch of the underlying note.  The Nns below are relative to this.
    _current :: !PSignal.Transposed
    -- | Current pitch value.  Starts as the pitch at the end of the previous
    -- note.
    , _from :: !Nn
    -- | Previous swaram.
    , _prev :: !Nn
    -- | Next swaram.
    , _next :: !Nn

    -- # maintained automatically
    -- | Transition time between pitch movements.
    , _transition :: !Typecheck.Normalized
    } deriving (Show)

nn_difference :: PSignal.Transposed -> PSignal.Transposed -> Derive.Deriver Nn
nn_difference p1 p2 = fmap realToFrac $
    (-) <$> Pitches.pitch_nn p1 <*> Pitches.pitch_nn p2

get_pitch :: (PitchState -> Nn) -> M PSignal.Transposed
get_pitch get = do
    nn <- State.gets get
    cur <- State.gets _current
    return $ Pitches.transpose_nn (Pitch.nn nn) cur

-- ** initial state

initial_pitch_state :: Typecheck.Normalized
    -> Derive.PassedArgs Derive.Control
    -> Derive.Deriver (Maybe PitchState)
initial_pitch_state transition args = do
    start <- Args.real_start args
    -- If there's no pitch then this is likely at the edge of a slice, and can
    -- be ignored.  TODO I think?
    justm (lookup_pitch start) $ \current -> do
        (prev, next, from) <- get_neighbor_pitches start
        let prev_step = maybe 0 snd $ Args.prev_control args
        let steps_from_current = fmap (fromMaybe 0) . traverse
                ((`nn_difference` current) <=< Derive.resolve_pitch start)
        prev <- steps_from_current prev
        from <- steps_from_current $
            Pitches.transpose_nn (Pitch.nn prev_step) <$> from
        next <- steps_from_current next

        return $ Just $ PitchState
            { _current = current
            , _from = from
            , _prev = prev
            , _next = next
            , _transition = transition
            }
    where
    lookup_pitch = Call.transposed

get_neighbor_pitches :: RealTime -> Derive.Deriver
    (Maybe PSignal.Pitch, Maybe PSignal.Pitch, Maybe PSignal.Pitch)
get_neighbor_pitches start = do
    pitch <- Derive.get_pitch
    let prev = PSignal.at_negative start pitch
    let next = snd <$> next_sample start pitch
    return (prev, next, prev)

next_sample :: RealTime -> PSignal.PSignal -> Maybe (RealTime, PSignal.Pitch)
next_sample x pitch = do
    Segment.Segment _ _ x2 _ <- PSignal.segment_at x pitch
    (x2,) <$> PSignal.at x2 pitch

-- * pitch sequence

c_import_pitch :: Derive.Transformer Derive.Control
c_import_pitch = Derive.transformer module_ "gamak" mempty
    "Import calls for a gamakam track."
    $ Sig.call0t $ \_args -> Derive.with_imported False (module_ <> "pitch")

c_pitch_sequence :: Derive.Generator Derive.Control
c_pitch_sequence = Derive.generator1 (module_ <> "pitch")
    "sequence" mempty pitch_sequence_doc
    $ Sig.call ((,)
        <$> Sig.required "sequence" "Pitch calls."
        <*> transition_env
    ) $ \(code, transition) args -> do
        end <- infer_end args
        maybe_state <- initial_pitch_state transition args
        case maybe_state of
            Nothing -> return mempty
            Just state -> do
                transpose <- Derive.at (Args.start args) $
                    pitch_sequence state (end - Args.start args) code
                -- Debug.tracepM "transpose" (Args.start args, end, transpose)
                real_end <- Derive.real end
                -- End is the next pitch sample.  So if the next event
                -- coincides with or precedes it, it will want to come from
                -- this pitch, so don't append a 0.  Otherwise, the pitch is
                -- not "attached" to a gamakam, so I add a 0, otherwise it's
                -- out of tune.
                --
                -- I have to subtract ScoreTime.eta because the pitch sample
                -- has been warped back from RealTime, so it will lose some
                -- precision.  TODO ugh.
                let next_gamakam = maybe False
                        ((<=end) . subtract ScoreTime.eta) (next_event args)
                return $ mconcat $ transpose
                    ++ if next_gamakam then []
                        else [Signal.from_sample real_end 0]
    where
    transition_env :: Sig.Parser Typecheck.Normalized
    transition_env =
        Sig.environ "transition" Sig.Both (Typecheck.Normalized 0.5) $
            "Time for each pitch movement, in proportion of the total time"
            <> " available."

pitch_sequence_doc :: Doc.Doc
pitch_sequence_doc = Doc.Doc $
    "This is a mini-language that describes a transposition curve.\
    \ The grammar is a sequence of `Pitch Duration | ']' Pitch | Alias`.\
    \ A plain Pitch moves to that pitch, `]` Pitch sets the From running pitch\
    \ to the given Pitch, but has zero duration, and Alias is a single letter,\
    \ which is itself mapped to a sequence.\
    \\nPitch is `[=<>][+\\^v]? | #?[-9-9a-d]? [+\\^v]?`.  `=<>` are the running\
    \ From pitch, Prev pitch, or Next pitch, and [+\\^v] add or subtract\
    \ 1nn, or .5nn, respectively.  A number is steps from the current swaram,\
    \ and a-d are shorthand for -1 to -4.\
    \\nDuration is a sequence of `_` or `.`, where each one doubles or halves\
    \ the duration. `:` and `;` stand for short or long absolute duration.\
    \\nDefault aliases:\n"
    <> Text.unlines [char k <> " - " <> v | (k, v) <- Map.toList aliases]
    where
    char c = "`" <> Text.singleton c <> "`"

-- | Start of the next event.  'Args.next' gets the end of the block if there
-- is no next event, but I don't want that.
next_event :: Derive.PassedArgs a -> Maybe TrackTime
next_event = fmap Event.start . Seq.head . Args.next_events

-- | Infer the end time for the gamakam as the next pitch in the pitch signal,
-- which should correspond to the next explicit swaram.
infer_end :: Derive.PassedArgs a -> Derive.Deriver TrackTime
infer_end args
    | Args.end args /= Args.start args = return $ Args.end args
    | otherwise = do
        pitch <- Derive.get_pitch
        start <- Args.real_start args
        next_pitch <- traverse (Derive.score . fst) (next_sample start pitch)
        let next = Args.next args
        return $ maybe next (min next) next_pitch

pitch_sequence :: PitchState -> ScoreTime -> Code
    -> Derive.Deriver [Signal.Control]
pitch_sequence state total_dur code = do
    calls <- Derive.require_right (("parsing " <> pretty code <> ": ")<>) $
        parse code
    calls <- Derive.require_right id $ resolve_aliases calls
    starts <- mapM Derive.real $ slice_time total_dur $ map call_duration calls
    let ranges = zip starts (drop 1 starts)
    (sigs, _) <- State.runStateT (mapM eval_call (zip ranges calls)) state
    return sigs

slice_time :: ScoreTime -> [Double] -> [ScoreTime]
slice_time dur slices =
    scanl (+) 0 $ map ((*one) . ScoreTime.from_double) slices
    where one = dur / ScoreTime.from_double (Num.sum slices)

eval_call :: ((RealTime, RealTime), Call) -> M Signal.Control
eval_call ((start, end), call) = case call of
    SetFrom pitch -> do
        set_from =<< resolve_pitch pitch
        return mempty
    Move (Movement to _) -> do
        to <- resolve_pitch to
        from <- get_pitch _from
        set_from to
        move_pitch start from end to

set_from :: PSignal.Transposed -> M ()
set_from pitch = do
    cur <- State.gets _current
    nn <- lift $ nn_difference pitch cur
    State.modify' $ \state -> state { _from = nn }

resolve_pitch :: Pitch -> M PSignal.Transposed
resolve_pitch (Pitch from steps nn) = do
    base <- case from of
        Current -> State.gets _current
        From -> get_pitch _from
        Prev -> get_pitch _prev
        Next -> get_pitch _next
    return $ apply (Pitches.transpose . Pitch.Diatonic . fromIntegral) steps $
        apply Pitches.transpose_nn (Pitch.nn nn) $
        base
    where
    apply f n
        | n == 0 = id
        | otherwise = f n

move_pitch :: RealTime -> PSignal.Transposed -> RealTime -> PSignal.Transposed
    -> M Signal.Control
move_pitch start from end to = do
    Typecheck.Normalized transition <- State.gets _transition
    let curve = ControlUtil.Function $
            ControlUtil.sigmoid (1-transition) (1-transition)
    cur <- State.gets _current
    from <- lift $ nn_difference from cur
    to <- lift $ nn_difference to cur
    lift $ ControlUtil.make_segment curve start from end to

-- * aliases

type Error = Text

resolve_aliases :: [Either Alias Call] -> Either Error [Call]
resolve_aliases = concatMapM (resolve 0)
    where
    resolve _ (Right call) = return [call]
    resolve depth (Left (Alias alias))
        | depth >= 5 = Left "too many levels of aliases"
        | otherwise = do
            expr <- tryJust ("unknown alias: " <> showt alias) $
                Map.lookup alias aliases
            calls <- first (("in alias " <> showt alias <> ": ")<>) $ parse expr
            concatMapM (resolve (depth+1)) calls

aliases :: Map Char Text
aliases = Map.fromList
    [ ('C', "]0")
    , ('N', "#0+.#0\\")
    , ('U', "#0\\.#0+.")
    , ('n', "#0^.#0v.")
    , ('u', "#0v.#0^.")
    ]

-- * call types

type Parser a = A.Parser a

data Call = SetFrom !Pitch | Move !Movement
    deriving (Eq, Show)

data Alias = Alias !Char
    deriving (Eq, Show)

data Movement = Movement !Pitch !Duration
    deriving (Eq, Show)

data Pitch = Pitch !From !Steps !Nn
    deriving (Eq, Show)
data From = From | Prev | Current | Next
    deriving (Eq, Show)
-- | Relative scale degrees.
type Steps = Int
-- | Relative NoteNumbers.
type Nn = Double

-- | How much time the movement takes.
data Duration = Relative !Double | AbsoluteShort | AbsoluteLong
    deriving (Eq, Show)

-- | Text representing unparsed Calls.
type Code = Text

call_duration :: Call -> Double
call_duration (SetFrom _) = 0
call_duration (Move (Movement _ dur)) = case dur of
    Relative dur -> dur
    -- TODO not implemented
    AbsoluteLong -> 1
    AbsoluteShort -> 1

-- * parse

parse :: Code -> Either Text [Either Alias Call]
parse = ParseText.parse1 p_calls

p_calls :: Parser [Either Alias Call]
p_calls = Applicative.many $
    Left <$> p_alias
    <|> Right <$> (p_set_from <|> Move <$> p_movement)

p_set_from :: Parser Call
p_set_from = SetFrom <$> (A.char ']' *> p_pitch)

p_alias :: Parser Alias
p_alias = Alias <$> A.satisfy is_alias
    where
    is_alias c = last_letter_negative < c && c <= 'z'
        || 'A' <= c && c <= 'Z'

p_movement :: Parser Movement
p_movement = Movement <$> p_pitch <*> p_duration

p_duration :: Parser Duration
p_duration = p_longer <|> p_shorter
    <|> choose_char [(':', AbsoluteShort), (';', AbsoluteLong)]
    <|> pure (Relative 1)
    where
    p_longer = do
        n <- A.takeWhile1 (=='_')
        return $ Relative $ fromIntegral $ Text.length n + 1
    p_shorter = do
        n <- A.takeWhile1 (=='.')
        return $ Relative $ 1 / 2^(fromIntegral (Text.length n))

-- | [=<>] [+\^v]? | #?[0-9a-d]? [+\^v]?
p_pitch :: Parser Pitch
p_pitch = do
    (matched, pitch) <- A.match $ p_pitch_from
        <|> (Pitch <$> p_from <*> (p_steps <|> pure 0) <*> p_nn)
    when (Text.null matched) $ fail "empty pitch"
    return pitch

p_pitch_from :: Parser Pitch
p_pitch_from = Pitch <$> from <*> pure 0 <*> p_nn
    where
    from = choose_char
        [ ('=', From)
        , ('<', Prev)
        , ('>', Next)
        ]

p_steps :: Parser Steps
p_steps = p_number <|> p_letter_negative

p_nn :: Parser Nn
p_nn = choose_char
    [ ('+', 1)
    , ('\\', -1)
    , ('^', 0.5)
    , ('v', -0.5)
    ] <|> pure 0
-- TODO alternately, ^v and ',.  But , looks a lot like .
-- or maybe {} and []?  No, ] is taken.
-- +? and ^v?
-- p_nn = choose_char
--     [ ('^', 1)
--     , ('v', -1)
--     , ('\'', 0.5)
--     , (',', -0.5)
--     ] <|> pure 0

p_from :: Parser From
p_from = A.option Current (A.char '#' *> pure From)

p_number :: Parser Int
p_number = do
    sign <- A.option 1 (A.char '-' >> return (-1))
    digit <- A.satisfy $ \c -> '0' <= c && c <= '9'
    return $ sign * (fromEnum digit - fromEnum '0')

p_letter_negative :: Parser Int
p_letter_negative = do
    digit <- A.satisfy $ \c -> 'a' <= c && c <= last_letter_negative
    return $ fromEnum 'a' - fromEnum digit - 1

last_letter_negative :: Char
last_letter_negative = 'd'

choose_char :: [(Char, a)] -> Parser a
choose_char = A.choice . map (\(c, a) -> A.char c *> pure a)


-- * misc

c_sahitya :: Derive.Taggable a => Derive.Transformer a
c_sahitya = Derive.transformer module_ "sahitya" mempty
    ("Ignore the transformed deriver. Put this on a track to ignore its"
    <> " contents, and put in sahitya.")
    $ Sig.call0t $ \_args _deriver -> return Stream.empty
