-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Idiomatic things for various instruments.
module Derive.C.Post.Idiom where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Library as Library
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream

import qualified Perform.RealTime as RealTime

import           Global
import           Types


library :: Library.Library
library = Library.transformers
    [ ("pizz-arp", cPizzArp)
    , ("avoid-overlap", cAvoidOverlap)
    , ("zero-duration-mute", cZeroDurationMute)
    , ("extend-duration", cExtendDuration)
    , ("apply-attributes", cApplyAttributes)
    ]

-- * pizz arp

cPizzArp :: Derive.Transformer Derive.Note
cPizzArp = Derive.transformer Module.prelude "pizz-arp"
    (Tags.postproc <> Tags.inst)
    "Arpeggiate simultaneous notes with `+pizz`. The order is arbitrary but\
    \ probably in track order.  TODO sort by pitch?" $
    Sig.callt (Sig.defaulted "time" (0.02 :: RealTime)
        "Insert this much time between each note."
    ) $
    \time _args deriver -> Ly.whenLilypond deriver $
        pizzArp time =<< deriver

pizzArp :: RealTime -> Stream.Stream Score.Event -> Derive.NoteDeriver
pizzArp time = mapSimultaneous 0.025 (Score.hasAttribute Attrs.pizz) $
    \(event :| chord) -> pure
        [ Score.move (+t) event
        | (t, event) <- zip (Lists.range_ 0 time) (event : chord)
        ]

mapSimultaneous :: RealTime
    -- ^ events starting closer than this amount are considered simultaneous
    -> (Score.Event -> Bool)
    -- ^ only process events that pass this predicate
    -> (NonEmpty Score.Event -> Derive.Deriver [Score.Event])
    -- ^ process simultaneous events
    -> Stream.Stream Score.Event -> Derive.NoteDeriver
mapSimultaneous eta accept f =
    fmap Stream.fromSortedList . go . Stream.toList
    where
    go [] = pure []
    go (LEvent.Log log : events) = (LEvent.Log log :) <$> go events
    go (LEvent.Event event : events)
        | accept event = collect event events
        | otherwise = (LEvent.Event event :) <$> go events
    collect event events = do
        out <- f (event :| wanted)
        outRest <- go rest
        pure $ map LEvent.Event (out ++ unwanted) ++ map LEvent.Log logs
            ++ outRest
        where
        start = Score.eventStart event
        (with, rest) = span
            (LEvent.logOr ((<=start) . subtract eta . Score.eventStart))
            events
        (chord, logs) = LEvent.partition with
        (wanted, unwanted) = List.partition accept chord

-- * avoid overlap

cAvoidOverlap :: Derive.Transformer Derive.Note
cAvoidOverlap = Derive.transformer Module.prelude "avoid-overlap"
    (Tags.postproc <> Tags.inst)
    "Notes with the same instrument and starting pitch are shortened so they\
    \ don't overlap with each other.  This simulates keyboard instruments, \
    \ where you have to release a key before striking the same key again.\
    \ This also happens to be what MIDI expects, since it's based on keyboards."
    $ Sig.callt (Sig.defaulted "time" (0.1 :: Double)
        "Ensure at least this much time between two notes of the same pitch.")
    $ \time _args deriver -> Ly.whenLilypond deriver $
        avoidOverlap time <$> deriver

avoidOverlap :: RealTime -> Stream.Stream Score.Event
    -> Stream.Stream Score.Event
avoidOverlap time = Post.emap1_ modify . nextSamePitch
    where
    modify (event, []) = event
    modify (event, next : _)
        | overlaps event next = Score.setDuration dur event
        | otherwise = event
        where
        dur = max Note.minDuration $
            Score.eventStart next - time - Score.eventStart event
    overlaps event next = Score.eventEnd event + time > Score.eventStart next

-- | For each event, get the next events with the same instrument and starting
-- pitch.
nextSamePitch :: Stream.Stream Score.Event
    -> Stream.Stream (Score.Event, [Score.Event])
nextSamePitch = Post.emap1_ check . Stream.zipOn Post.nexts
    where
    check (nexts, event) = (event, filter (same event) nexts)
    same event next =
        Score.eventInstrument event == Score.eventInstrument next
        && Maybe.isJust nn && nn == Score.initialNn next
        where nn = Score.initialNn event


-- * zero dur mute

-- | See DUtil.zero_duration for a version that can apply any kind of
-- transformation.  This one is limited to attrs because it's a postproc, and
-- it's a postproc because otherwise it's hard to tell if a note is really
-- zero duration and not just an infer-duration note.
cZeroDurationMute :: Derive.Transformer Derive.Note
cZeroDurationMute = Derive.transformer Module.prelude
    "zero-duration-mute" (Tags.postproc <> Tags.inst)
    "Add attributes to zero duration events."
    $ Sig.callt ((,)
    <$> Sig.defaulted "attr" Attrs.mute "Add this attribute."
    <*> Sig.defaulted "dyn" (0.75 :: Double) "Scale dynamic by this amount."
    ) $ \(attrs, dyn) _args deriver -> Post.emap1_ (add attrs dyn) <$> deriver
    where
    add attrs dyn event
        | Score.eventDuration event == 0 =
            Score.modifyDynamic (*dyn) $ Score.addAttributes attrs event
        | otherwise = event


-- * extend duration

cExtendDuration :: Derive.Transformer Derive.Note
cExtendDuration = Derive.transformer Module.prelude "extend-duration"
    Tags.postproc ("Extend the duration of notes with certain attributes.\
    \ This is appropriate for attributes like " <> ShowVal.doc Attrs.staccato
    <> ", which might already have their own built-in duration, and sound\
    \ better when given as much time to ring as possible."
    ) $ Sig.callt ((,)
    <$> Sig.required "attrs" "Extend durations of notes with these attrs."
    <*> Sig.defaulted "dur" (RealTime.seconds 2)
        "Extend to a minimum of this duration."
    ) $ \(attrs, dur) _args deriver -> Ly.whenLilypond deriver $
        extendDuration attrs dur <$> deriver

-- | Don't overlap with another note with the same pitch, as in 'avoid_overlap'.
extendDuration :: [Attrs.Attributes] -> RealTime -> Stream.Stream Score.Event
    -> Stream.Stream Score.Event
extendDuration attrs dur = Post.emap1_ extend . nextSamePitch
    where
    extend (event, nexts)
        | any (\a -> Score.hasAttribute a event) attrs =
            Score.duration (max $ maybe dur (min dur) maxDur) event
        | otherwise = event
        where maxDur = diff event <$> Lists.head nexts
    diff e1 e2 = max 0 $ Score.eventStart e2 - Score.eventStart e1 - 0.05


-- * apply attributes

cApplyAttributes :: Derive.Transformer Derive.Note
cApplyAttributes = Derive.transformer Module.prelude "apply-attributes"
    Tags.postproc ("Apply attributes by control signal. This looks for\
        \ controls with a " <> ShowVal.doc controlPrefix <> " prefix.\
        \ A control named " <> ShowVal.doc (controlPrefix <> "a-b")

        <> " will, when non-zero, add the `+a+b` attributes to its events."
    ) $ Sig.call0t $ \_args deriver -> Post.emap1_ applyAttributes <$> deriver

-- | For all controls that start with 'control_prefix' and are positive during
-- the event start, add those attributes to the event.
--
-- TODO a possible variation would be to take 0<v<1 as a probability of
-- applying the attribute.
applyAttributes :: Score.Event -> Score.Event
applyAttributes event = Score.addAttributes (mconcat attrsToApply) event
    where
    controls :: [(Attrs.Attributes, ScoreT.Control)]
    controls = Lists.keyOnJust controlAttributes $ Map.keys $
        Score.eventControls event
    attrsToApply = map fst $ filter ((>0) . get . snd) controls
    get c = maybe 0 ScoreT.valOf $
        Score.controlAt (Score.eventStart event) c event

controlAttributes :: ScoreT.Control -> Maybe Attrs.Attributes
controlAttributes = fmap (Attrs.attrs . Text.split (=='-'))
    . Text.stripPrefix controlPrefix . ScoreT.controlName

controlPrefix :: Text
controlPrefix = "attr-"
