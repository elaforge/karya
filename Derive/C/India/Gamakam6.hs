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
import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.ParseA as ParseA
import qualified Util.Segment as Segment

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
        [ (Parse.unparsedCall, cPitchSequence)
        -- , (Parse.unparsed_call, c_dyn_sequence)
        ]
    , Library.transformers
        [ ("gamak", cImportPitch)
        -- , ("dyn", c_import_dyn)
        ]
    , Library.transformers
        [ ("sahitya", cSahitya :: Derive.Transformer Derive.Note)
        ]
    ]

-- * State

type M a = State.StateT PitchState Derive.Deriver a

data PitchState = PitchState
    -- # maintained automatically
    -- | Pitch of the underlying note.  The Nns below are relative to this.
    { _current :: !PSignal.Transposed
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

nnDifference :: PSignal.Transposed -> PSignal.Transposed -> Derive.Deriver Nn
nnDifference p1 p2 = fmap realToFrac $
    (-) <$> Pitches.pitchNn p1 <*> Pitches.pitchNn p2

getPitch :: (PitchState -> Nn) -> M PSignal.Transposed
getPitch get = do
    nn <- State.gets get
    cur <- State.gets _current
    pure $ Pitches.transposeNn (Pitch.nn nn) cur

-- ** initial state

initialPitchState :: Typecheck.Normalized
    -> Derive.PassedArgs Derive.Control
    -> Derive.Deriver (Maybe PitchState)
initialPitchState transition args = do
    start <- Args.realStart args
    -- If there's no pitch then this is likely at the edge of a slice, and can
    -- be ignored.  TODO I think?
    justm (lookupPitch start) $ \current -> do
        (prev, next, from) <- getNeighborPitches start
        let prevStep = maybe 0 snd $ Args.prevControl args
        let stepsFromCurrent = fmap (fromMaybe 0) . traverse
                ((`nnDifference` current) <=< Derive.resolvePitch start)
        prev <- stepsFromCurrent prev
        from <- stepsFromCurrent $
            Pitches.transposeNn (Pitch.nn prevStep) <$> from
        next <- stepsFromCurrent next

        pure $ Just $ PitchState
            { _current = current
            , _from = from
            , _prev = prev
            , _next = next
            , _transition = transition
            }
    where
    lookupPitch = Call.transposed

getNeighborPitches :: RealTime -> Derive.Deriver
    (Maybe PSignal.Pitch, Maybe PSignal.Pitch, Maybe PSignal.Pitch)
getNeighborPitches start = do
    pitch <- Derive.getPitch
    let prev = PSignal.atNegative pitch start
    let next = snd <$> nextSample start pitch
    pure (prev, next, prev)

nextSample :: RealTime -> PSignal.PSignal -> Maybe (RealTime, PSignal.Pitch)
nextSample x pitch = do
    Segment.Segment _ _ x2 _ <- PSignal.segmentAt pitch x
    (x2,) <$> PSignal.at pitch x2

-- * pitch sequence

cImportPitch :: Derive.Transformer Derive.Control
cImportPitch = Derive.transformer module_ "gamak" mempty
    "Import calls for a gamakam track."
    $ Sig.call0t $ \_args -> Derive.withImported False (module_ <> "pitch")

cPitchSequence :: Derive.Generator Derive.Control
cPitchSequence = Derive.generator1 (module_ <> "pitch")
    "sequence" mempty pitchSequenceDoc
    $ Sig.call ((,)
        <$> Sig.required "sequence" "Pitch calls."
        <*> transitionEnv
    ) $ \(code, transition) args -> do
        end <- inferEnd args
        maybeState <- initialPitchState transition args
        case maybeState of
            Nothing -> pure mempty
            Just state -> do
                transpose <- Derive.at (Args.start args) $
                    pitchSequence state (end - Args.start args) code
                -- Debug.tracepM "transpose" (Args.start args, end, transpose)
                realEnd <- Derive.real end
                -- End is the next pitch sample.  So if the next event
                -- coincides with or precedes it, it will want to come from
                -- this pitch, so don't append a 0.  Otherwise, the pitch is
                -- not "attached" to a gamakam, so I add a 0, otherwise it's
                -- out of tune.
                --
                -- I have to subtract ScoreTime.eta because the pitch sample
                -- has been warped back from RealTime, so it will lose some
                -- precision.  TODO ugh.
                let nextGamakam = maybe False
                        ((<=end) . subtract ScoreTime.eta) (nextEvent args)
                pure $ mconcat $ transpose
                    ++ if nextGamakam then []
                        else [Signal.fromSample realEnd 0]
    where
    transitionEnv :: Sig.Parser Typecheck.Normalized
    transitionEnv =
        Sig.environ "transition" Sig.Both (Typecheck.Normalized 0.5) $
            "Time for each pitch movement, in proportion of the total time"
            <> " available."

pitchSequenceDoc :: Doc.Doc
pitchSequenceDoc = Doc.Doc $
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
nextEvent :: Derive.PassedArgs a -> Maybe TrackTime
nextEvent = fmap Event.start . Lists.head . Args.nextEvents

-- | Infer the end time for the gamakam as the next pitch in the pitch signal,
-- which should correspond to the next explicit swaram.
inferEnd :: Derive.PassedArgs a -> Derive.Deriver TrackTime
inferEnd args
    | Args.end args /= Args.start args = pure $ Args.end args
    | otherwise = do
        pitch <- Derive.getPitch
        start <- Args.realStart args
        nextPitch <- traverse (Derive.score . fst) (nextSample start pitch)
        let next = Args.next args
        pure $ maybe next (min next) nextPitch

pitchSequence :: PitchState -> ScoreTime -> Code
    -> Derive.Deriver [Signal.Control]
pitchSequence state totalDur code = do
    calls <- Derive.requireRight (("parsing " <> pretty code <> ": ")<>) $
        parse code
    calls <- Derive.requireRight id $ resolveAliases calls
    starts <- mapM Derive.real $ sliceTime totalDur $ map callDuration calls
    let ranges = zip starts (drop 1 starts)
    (sigs, _) <- State.runStateT (mapM evalCall (zip ranges calls)) state
    pure sigs

sliceTime :: ScoreTime -> [Double] -> [ScoreTime]
sliceTime dur slices =
    scanl (+) 0 $ map ((*one) . ScoreTime.fromDouble) slices
    where one = dur / ScoreTime.fromDouble (Num.sum slices)

evalCall :: ((RealTime, RealTime), Call) -> M Signal.Control
evalCall ((start, end), call) = case call of
    SetFrom pitch -> do
        setFrom =<< resolvePitch pitch
        pure mempty
    Move (Movement to _) -> do
        to <- resolvePitch to
        from <- getPitch _from
        setFrom to
        movePitch start from end to

setFrom :: PSignal.Transposed -> M ()
setFrom pitch = do
    cur <- State.gets _current
    nn <- lift $ nnDifference pitch cur
    State.modify' $ \state -> state { _from = nn }

resolvePitch :: Pitch -> M PSignal.Transposed
resolvePitch (Pitch from steps nn) = do
    base <- case from of
        Current -> State.gets _current
        From -> getPitch _from
        Prev -> getPitch _prev
        Next -> getPitch _next
    pure $ apply (Pitches.transpose . Pitch.Diatonic . fromIntegral) steps $
        apply Pitches.transposeNn (Pitch.nn nn) $
        base
    where
    apply f n
        | n == 0 = id
        | otherwise = f n

movePitch :: RealTime -> PSignal.Transposed -> RealTime -> PSignal.Transposed
    -> M Signal.Control
movePitch start from end to = do
    Typecheck.Normalized transition <- State.gets _transition
    let curve = ControlUtil.Function $
            ControlUtil.sigmoid (1-transition) (1-transition)
    cur <- State.gets _current
    from <- lift $ nnDifference from cur
    to <- lift $ nnDifference to cur
    lift $ ControlUtil.makeSegment curve start from end to

-- * aliases

type Error = Text

resolveAliases :: [Either Alias Call] -> Either Error [Call]
resolveAliases = concatMapM (resolve 0)
    where
    resolve _ (Right call) = pure [call]
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

callDuration :: Call -> Double
callDuration (SetFrom _) = 0
callDuration (Move (Movement _ dur)) = case dur of
    Relative dur -> dur
    -- TODO not implemented
    AbsoluteLong -> 1
    AbsoluteShort -> 1

-- * parse

parse :: Code -> Either Text [Either Alias Call]
parse = ParseA.parse1 pCalls

pCalls :: Parser [Either Alias Call]
pCalls = Applicative.many $
    Left <$> pAlias
    <|> Right <$> (pSetFrom <|> Move <$> pMovement)

pSetFrom :: Parser Call
pSetFrom = SetFrom <$> (A.char ']' *> pPitch)

pAlias :: Parser Alias
pAlias = Alias <$> A.satisfy isAlias
    where
    isAlias c = lastLetterNegative < c && c <= 'z'
        || 'A' <= c && c <= 'Z'

pMovement :: Parser Movement
pMovement = Movement <$> pPitch <*> pDuration

pDuration :: Parser Duration
pDuration = pLonger <|> pShorter
    <|> chooseChar [(':', AbsoluteShort), (';', AbsoluteLong)]
    <|> pure (Relative 1)
    where
    pLonger = do
        n <- A.takeWhile1 (=='_')
        pure $ Relative $ fromIntegral $ Text.length n + 1
    pShorter = do
        n <- A.takeWhile1 (=='.')
        pure $ Relative $ 1 / 2^(fromIntegral (Text.length n))

-- | [=<>] [+\^v]? | #?[0-9a-d]? [+\^v]?
pPitch :: Parser Pitch
pPitch = do
    (matched, pitch) <- A.match $ pPitchFrom
        <|> (Pitch <$> pFrom <*> (pSteps <|> pure 0) <*> pNn)
    when (Text.null matched) $ fail "empty pitch"
    pure pitch

pPitchFrom :: Parser Pitch
pPitchFrom = Pitch <$> from <*> pure 0 <*> pNn
    where
    from = chooseChar
        [ ('=', From)
        , ('<', Prev)
        , ('>', Next)
        ]

pSteps :: Parser Steps
pSteps = pNumber <|> pLetterNegative

pNn :: Parser Nn
pNn = chooseChar
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

pFrom :: Parser From
pFrom = A.option Current (A.char '#' *> pure From)

pNumber :: Parser Int
pNumber = do
    sign <- A.option 1 (A.char '-' >> pure (-1))
    digit <- A.satisfy $ \c -> '0' <= c && c <= '9'
    pure $ sign * (fromEnum digit - fromEnum '0')

pLetterNegative :: Parser Int
pLetterNegative = do
    digit <- A.satisfy $ \c -> 'a' <= c && c <= lastLetterNegative
    pure $ fromEnum 'a' - fromEnum digit - 1

lastLetterNegative :: Char
lastLetterNegative = 'd'

chooseChar :: [(Char, a)] -> Parser a
chooseChar = A.choice . map (\(c, a) -> A.char c *> pure a)


-- * misc

cSahitya :: Derive.Taggable a => Derive.Transformer a
cSahitya = Derive.transformer module_ "sahitya" mempty
    ("Ignore the transformed deriver. Put this on a track to ignore its"
    <> " contents, and put in sahitya.")
    $ Sig.call0t $ \_args _deriver -> pure Stream.empty
