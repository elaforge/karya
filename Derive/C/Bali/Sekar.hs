-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
-- | Pattern based derivation.
module Derive.C.Bali.Sekar where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Num as Num

import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.SubT as SubT
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Flags as Flags
import qualified Derive.Library as Library
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig

import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


library :: Library.Library
library = Library.generators
    [ ("sekar", cSekarDirect)
    , ("sekar-e", cSekarEven)
    ]

module_ :: Module.Module
module_ = "bali" <> "sekar"

cSekarDirect :: Derive.Generator Derive.Note
cSekarDirect = Derive.generator module_ "sekar" (Tags.inst <> Tags.subs)
    "Arrange sub-notes according to a pattern.\
    \\nIn the direct substitution style, each note retains its relative\
    \ duration as it is rearranged by the pattern. A rest is considered a\
    \ note, but just one note, so you can't have two rests in a row."
    $ Sig.call ((,)
    <$> Sig.required "pattern" ("If there is a list of patterns, they are for\
        \ different numbers of notes, starting with 1. A single pattern is\
        \ applied to all numbers though. " <> patternDoc)
    <*> arriveEnv
    ) $ \(patternText, arrive) args -> do
        patterns <- mapM makePattern patternText
        patterns <- Derive.requireRight id $
            checkPatterns (zip patterns patternText)
        let range = Args.range args
        let derive
                | arrive = sekarDirectArrive range patterns
                | otherwise = sekarDirect range patterns
        mconcatMap derive =<< Sub.subRestEvents arrive True args

cSekarEven :: Derive.Generator Derive.Note
cSekarEven = Derive.generator module_ "sekar" (Tags.inst <> Tags.subs)
    "Arrange sub-notes according to a pattern.\
    \\nIn the even subdivision style, the range is divided evenly based\
    \ on the highest index of the pattern (so `abcac` would divide into 3\
    \ parts). The melody is sampled at those points for note attacks,\
    \ sustains, and rests, which are then rearranged by the pattern.\
    \ Thus, the output is always notes in a regular tempo determined by the\
    \ length of the pattern."
    $ Sig.call ((,)
    <$> Sig.required "pattern" patternDoc
    <*> arriveEnv
    ) $ \(pattern, arrive) args -> do
        pattern <- makePattern pattern
        let derive = sekarEven arrive (Args.range args) pattern
        mconcatMap derive =<< Sub.subRestEvents arrive True args

arriveEnv :: Sig.Parser Bool
arriveEnv = Sig.environ "arrive" Sig.Prefixed True $
    "If true, the last note of the pattern is aligned to the end of the event,\
    \ and given " <> Doc.pretty Flags.inferDuration <> "."

patternDoc :: Doc.Doc
patternDoc =
    "The pattern is letters from a-z, where `a` is the first note and `z` is\
    \ the 26th. Capital letters replace that note with a rest. Gaps in the\
    \ input notes count as rest notes."

checkPatterns :: [(Pattern, Text)] -> Either Text (NonEmpty Pattern)
checkPatterns [(pattern, _)] = Right (pattern :| [])
checkPatterns patterns = do
    mapM_ check (zip [1..] patterns)
    case NonEmpty.nonEmpty (map fst patterns) of
        Nothing -> Left "require at least one pattern"
        Just ps -> Right ps
    where
    check (n, (pattern, ptext))
        | patternLength pattern /= n = Left $
            "expected pattern of length " <> showt n <> " but got "
            <> showt ptext
        | otherwise = pure ()

-- ** even subdivision

data DivNote a = DivNote !a | DivRest | DivContinue
    deriving (Eq, Show, Functor)

sekarEven :: Bool -> (ScoreTime, ScoreTime) -> Pattern -> [Sub.RestEvent]
    -> Derive.NoteDeriver
sekarEven arrive (start, end) pattern events =
    Sub.derive $ (if arrive then nudge else id) $ map (SubT.at start) $
        divRealize dur notes pattern
    where
    notes = divExtract events samples
    samples = (if arrive then drop 1 else id) $ Lists.range start end ndur
    ndur = (end - start) / fromIntegral (patternLength pattern)
    dur = (end - start) / fromIntegral (length pattern)
    nudge = Lists.mapLast (fmap addLastNoteFlags) . map (SubT.at dur)

divRealize :: ScoreTime -> [DivNote a] -> Pattern -> [SubT.EventT a]
divRealize dur notes = combine . zip (Lists.range_ 0 dur) . map resolve
    where
    resolve (i, element) = case element of
        Rest -> DivRest
        Note -> fromMaybe DivRest $ Lists.at notes i
    combine ((start, note) : notes) = case note of
        DivRest -> continue
        DivContinue -> continue
        DivNote d -> SubT.EventT start (dur * fromIntegral cs) d : continue
            where cs = length (takeWhile isContinue notes) + 1
        where continue = combine notes
    combine [] = []
    isContinue (_, DivContinue) = True
    isContinue _ = False

-- | Convert 'SubT.EventT's to DivNotes at the given times.
divExtract :: [SubT.EventT (Maybe a)] -> [ScoreTime] -> [DivNote a]
divExtract events = snd . List.mapAccumL go events
    where
    go events t = case dropUntilNext (past t) events of
        events@(event : _) -> (events,) $ case SubT._note event of
            Nothing -> DivRest
            Just d
                | t ScoreTime.== SubT._start event -> DivNote d
                | t ScoreTime.> SubT.end event -> DivRest
                | otherwise -> DivContinue
        [] -> ([], DivRest)
    past t = (ScoreTime.> t) . SubT._start

-- | Drop until the predicate is true for the next event.  This is like
-- @dropWhile (not . f)@, but you get the element before the predicate becomes
-- false.
dropUntilNext :: (a -> Bool) -> [a] -> [a]
dropUntilNext f xs = case xs of
    xs@(_ : x2 : _) | f x2 -> xs
    [x] -> [x]
    _ : xs -> dropUntilNext f xs
    [] -> []

-- ** direct substitution

-- | Like 'sekar_direct', but expect sub-events excluding the start and
-- including the end, and align the last note to the end of the call.
sekarDirectArrive :: (ScoreTime, ScoreTime) -> NonEmpty Pattern
    -> [Sub.RestEvent] -> Derive.NoteDeriver
sekarDirectArrive range patterns events_ =
    Sub.derive $ addFlags $ align $ map (SubT.stretch factor) $
        Sub.stripRests realized
    where
    -- The first event should be a rest, since I passed end_bias=True to
    -- Sub.sub_rest_events.  The stretch factor assumes the event durations add
    -- up to 'range'.  For that to be true, I have to give the initial rest
    -- duration to the destination note.
    events = case events_ of
        [] -> []
        rest : events -> case Lists.unsnoc events of
            Nothing -> events
            Just (initial, final)
                | SubT._start final >= snd range ->
                    initial ++ [final { SubT._duration = dur }]
                | otherwise -> events ++ [SubT.EventT (snd range) dur Nothing]
                where dur = SubT._duration rest

    realized = realizeGroups patterns events
    factor = sumDuration events / sumDuration realized
    -- Align notes to the end of the range.
    align es = case Lists.last es of
        Nothing -> []
        Just e -> map (SubT.at (snd range - SubT._start e)) es
    addFlags = Lists.mapLast $ fmap addLastNoteFlags

addLastNoteFlags :: Derive.NoteDeriver -> Derive.NoteDeriver
addLastNoteFlags = fmap $ Post.emap1_ $ Score.addFlags $
    Flags.inferDuration <> Flags.strong

sumDuration :: [SubT.EventT a] -> ScoreTime
sumDuration = Num.sum . map SubT._duration

-- | Sekaran derivation via direct substitution of the sub-events.
sekarDirect :: (ScoreTime, ScoreTime) -> NonEmpty Pattern -> [Sub.RestEvent]
    -> Derive.NoteDeriver
sekarDirect range patterns events =
    Sub.derive $ map (SubT.place (fst range) factor) $ Sub.stripRests realized
    where
    realized = realizeGroups patterns events
    factor = sumDuration events / sumDuration realized

realizeGroups :: NonEmpty Pattern -> [Sub.RestEvent] -> [Sub.RestEvent]
realizeGroups _ [] = []
realizeGroups patterns events@(event:rest) =
    go (SubT._start event) $ splitGroups (patternLength pattern) events
    where
    pattern = indexWith (event :| rest) patterns
    go _ [] = []
    go start (group : groups) = notes ++ go (start + sumDuration notes) groups
        where notes = realize start group pattern

indexWith :: NonEmpty a -> NonEmpty b -> b
indexWith (x :| xs) (y :| ys) = go x xs y ys
    where
    go _ (x:xs) _ (y:ys) = go x xs y ys
    go _ [] y _ = y
    go _ _ y [] = y

splitGroups :: Int -> [a] -> [[a]]
splitGroups n notes = case splitAt n notes of
    ([], _) -> []
    (pre, post) -> pre : splitGroups n post

type Pattern = [(Index, Element)]
-- | Index into melody notes.
type Index = Int
data Element = Note | Rest deriving (Show)

patternLength :: Pattern -> Int
patternLength = maximum . (0:) . map ((+1) . fst)

makePattern :: Text -> Derive.Deriver Pattern
makePattern pattern = do
    when (Text.null pattern || Text.any (not . aToZ . Char.toLower) pattern) $
        Derive.throw $ "pattern chars must be a-z: " <> showt pattern
    pure
        [ (fromEnum (Char.toLower c) - fromEnum 'a',
            if Char.isUpper c then Rest else Note)
        | c <- Text.unpack pattern
        ]
    where aToZ c = 'a' <= c && c <= 'z'

-- | Apply the pattern to the events.
realize :: ScoreTime -> [Sub.RestEvent] -> Pattern -> [Sub.RestEvent]
realize start events = map place . addStarts . mapMaybe resolve
    where
    resolve (i, element) = resolve1 element <$> Lists.at events i
    -- Rests have a duration, but no deriver.
    resolve1 element (SubT.EventT _ dur d) = case d of
        Nothing -> (dur, Nothing)
        Just deriver -> (,) dur $ case element of
            Note -> Just deriver
            Rest -> Nothing
    addStarts events = zip (scanl (+) start (map fst events)) events
    place (start, (dur, maybeDeriver)) = SubT.EventT start dur maybeDeriver
