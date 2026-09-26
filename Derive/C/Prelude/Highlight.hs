-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls to highlight events.
module Derive.C.Prelude.Highlight (
    library
    , cHighlightStringsNote
    , outOfRange, openStrings, warnNonOpen
) where
import qualified Data.Maybe as Maybe

import qualified Util.Test.ApproxEq as ApproxEq
import qualified Derive.Args as Args
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Ui.Color as Color

import           Global
import           Types


library :: Library.Library
library = Library.transformers
    [ ("highlight", cHighlight)
    , ("highlight-strings", cHighlightStrings)
    , ("highlight-out-of-range", cHighlightOutOfRange)
    ]

cHighlight :: Derive.Transformer Derive.Note
cHighlight = Derive.transformer Module.prelude "highlight" mempty
    "Add a highlight color."
    $ Sig.callt (Sig.required "highlight" "Highlight code.")
    $ \highlight _ deriver -> Post.emap1_ (addHighlight highlight) <$> deriver

instance ShowVal.ShowVal Color.Highlight
instance Typecheck.Typecheck Color.Highlight


-- * open strings

-- | This is a generator that goes on null call, not a transformer over
-- everything.
cHighlightStringsNote :: Derive.Generator Derive.Note
cHighlightStringsNote = Note.transformedNoteName "note-highlight"
    ("Highlight any notes whose initial pitch isn't in "
        <> ShowVal.doc EnvKey.openStrings <> ".")
    mempty $ \args deriver -> do
        pos <- Args.realStart args
        outOfRange (Args.start args) $ openStrings pos warnNonOpen deriver

cHighlightStrings :: Derive.Transformer Derive.Note
cHighlightStrings = Derive.transformer Module.prelude "highlight-strings"
    mempty ("Highlight any notes whose initial pitch either is or isn't in "
        <> ShowVal.doc EnvKey.openStrings <> ".")
    $ Sig.callt
    ( Sig.environ "open" Sig.Prefixed False
        "If true, put Info on open strings, else put Warning on non-open ones."
    ) $ \highlightOpen args deriver -> do
        pos <- Args.realStart args
        openStrings pos (if highlightOpen then noticeOpen else warnNonOpen)
            deriver

warnNonOpen :: Bool -> Maybe Color.Highlight
warnNonOpen open = if open then Nothing else Just Color.Warning

noticeOpen :: Bool -> Maybe Color.Highlight
noticeOpen open = if open then Just Color.Notice else Nothing

openStrings :: RealTime -> (Bool -> Maybe Color.Highlight)
    -- ^ True if this note is on an open string.
    -> Derive.NoteDeriver -> Derive.NoteDeriver
openStrings pos highlight deriver = do
    maybePitches <- Derive.lookupVal EnvKey.openStrings
    maybePitches <- case maybePitches of
        Just pitches -> Just <$> mapM (Derive.resolvePitch pos) pitches
        Nothing -> pure Nothing
    maybe id (\pitches -> fmap (Post.emap1_ (apply pitches))) maybePitches
        deriver
    where
    apply :: [PSignal.Transposed] -> Score.Event -> Score.Event
    apply pitches event = case Score.initialNn event of
        Just nn -> case highlight (any (samePitch nn) pitches) of
            Nothing -> event
            Just highlight -> addHighlight highlight event
        _ -> event
    samePitch nn pitch = case PSignal.pitchNn pitch of
        Right stringNn -> ApproxEq.eq 0.05 nn stringNn
        _ -> False


-- * out of range

cHighlightOutOfRange :: Derive.Transformer Derive.Note
cHighlightOutOfRange = Derive.transformer Module.prelude
    "highlight-out-of-range" mempty
    ("Error on notes whose initial pitch is below "
        <> ShowVal.doc EnvKey.instrumentBottom <> " or above "
        <> ShowVal.doc EnvKey.instrumentTop <> ". The range must be \
        \ in NNs.")
    $ Sig.call0t $ \args deriver -> outOfRange (Args.start args) deriver

-- | Highlight with 'Color.Warning' if there is 'EnvKey.instrument_top' or
-- 'EnvKey.instrument_bottom' and the pitch is above or below it,
-- respectively.
outOfRange :: ScoreTime -> Derive.NoteDeriver -> Derive.NoteDeriver
outOfRange pos deriver = do
    (maybeTop, maybeBottom) <- getInstrumentRange pos
    if all Maybe.isNothing [maybeTop, maybeBottom] then deriver
        else Post.emap1_ (apply maybeTop maybeBottom) <$> deriver
    where
    apply maybeTop maybeBottom event
        | isOor = addHighlight Color.Error event
        | otherwise = event
        where
        isOor = fromMaybe False $ do
            nn <- Score.initialNn event
            pure $ maybe False (<nn) maybeTop
                || maybe False (>nn) maybeBottom

getInstrumentRange :: ScoreTime
    -> Derive.Deriver (Maybe Pitch.NoteNumber, Maybe Pitch.NoteNumber)
getInstrumentRange pos = do
    (_parseP, showP, _) <- Call.getPitchFunctions
    maybeTop <- get showP EnvKey.instrumentTop
    maybeBottom <- get showP EnvKey.instrumentBottom
    pure (maybeTop, maybeBottom)
    where
    get showP key =
        maybe (pure Nothing)
            (fmap Just . either (resolvePitch showP pos) pure)
        =<< Derive.lookupVal key

resolvePitch :: (Pitch.Pitch -> Maybe Pitch.Note) -> ScoreTime -> Pitch.Pitch
    -> Derive.Deriver Pitch.NoteNumber
resolvePitch showP pos = Pitches.pitchNn <=< Call.evalPitch showP pos

_initialPitch :: Scale.Scale -> Score.Event -> Maybe Pitch.Pitch
_initialPitch scale event = do
    note <- Score.initialNote event
    either (const Nothing) Just $ Scale.scaleRead scale mempty note


-- * util

addHighlight :: Color.Highlight -> Score.Event -> Score.Event
addHighlight highlight event = event
    { Score.eventHighlight = highlight <> Score.eventHighlight event }
