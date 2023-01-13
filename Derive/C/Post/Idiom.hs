-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Idiomatic things for various instruments.
module Derive.C.Post.Idiom where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.Call as Call
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.LEvent as LEvent
import qualified Derive.Library as Library
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import           Derive.Sig (control, defaulted)
import qualified Derive.Stream as Stream

import qualified Perform.RealTime as RealTime

import           Global
import           Types


library :: Library.Library
library = Library.transformers
    [ ("pizz-arp", c_pizz_arp)
    , ("avoid-overlap", c_avoid_overlap)
    , ("zero-duration-mute", c_zero_duration_mute)
    , ("extend-duration", c_extend_duration)
    , ("apply-attributes", c_apply_attributes)
    ]

-- * pizz arp

c_pizz_arp :: Derive.Transformer Derive.Note
c_pizz_arp = Derive.transformer Module.prelude "pizz-arp"
    (Tags.postproc <> Tags.inst)
    "Arpeggiate simultaneous notes with `+pizz`. The order is arbitrary but\
    \ probably in track order.  TODO sort by pitch?" $
    Sig.callt (defaulted "time" (control "pizz-arp-time" 0.02)
        "Insert this much time between each note.") $
    \time _args deriver -> Ly.when_lilypond deriver $
        pizz_arp time =<< deriver

pizz_arp :: DeriveT.ControlRef -> Stream.Stream Score.Event
    -> Derive.NoteDeriver
pizz_arp time = map_simultaneous 0.025 (Score.has_attribute Attrs.pizz) $
    \(event :| chord) -> do
        let start = Score.event_start event
        time <- RealTime.seconds <$> Call.control_at time start
        return [Score.move (+t) event
            | (t, event) <- zip (Seq.range_ 0 time) (event : chord)]

map_simultaneous :: RealTime
    -- ^ events starting closer than this amount are considered simultaneous
    -> (Score.Event -> Bool)
    -- ^ only process events that pass this predicate
    -> (NonEmpty Score.Event -> Derive.Deriver [Score.Event])
    -- ^ process simultaneous events
    -> Stream.Stream Score.Event -> Derive.NoteDeriver
map_simultaneous eta accept f =
    fmap Stream.from_sorted_list . go . Stream.to_list
    where
    go [] = return []
    go (LEvent.Log log : events) = (LEvent.Log log :) <$> go events
    go (LEvent.Event event : events)
        | accept event = collect event events
        | otherwise = (LEvent.Event event :) <$> go events
    collect event events = do
        out <- f (event :| wanted)
        out_rest <- go rest
        return $ map LEvent.Event (out ++ unwanted) ++ map LEvent.Log logs
            ++ out_rest
        where
        start = Score.event_start event
        (with, rest) = span
            (LEvent.log_or ((<=start) . subtract eta . Score.event_start))
            events
        (chord, logs) = LEvent.partition with
        (wanted, unwanted) = List.partition accept chord

-- * avoid overlap

c_avoid_overlap :: Derive.Transformer Derive.Note
c_avoid_overlap = Derive.transformer Module.prelude "avoid-overlap"
    (Tags.postproc <> Tags.inst)
    "Notes with the same instrument and starting pitch are shortened so they\
    \ don't overlap with each other.  This simulates keyboard instruments, \
    \ where you have to release a key before striking the same key again.\
    \ This also happens to be what MIDI expects, since it's based on keyboards."
    $ Sig.callt (defaulted "time" (0.1 :: Double)
        "Ensure at least this much time between two notes of the same pitch.")
    $ \time _args deriver -> Ly.when_lilypond deriver $
        avoid_overlap time <$> deriver

avoid_overlap :: RealTime -> Stream.Stream Score.Event
    -> Stream.Stream Score.Event
avoid_overlap time = Post.emap1_ modify . next_same_pitch
    where
    modify (event, []) = event
    modify (event, next : _)
        | overlaps event next = Score.set_duration dur event
        | otherwise = event
        where
        dur = max Note.min_duration $
            Score.event_start next - time - Score.event_start event
    overlaps event next = Score.event_end event + time > Score.event_start next

-- | For each event, get the next events with the same instrument and starting
-- pitch.
next_same_pitch :: Stream.Stream Score.Event
    -> Stream.Stream (Score.Event, [Score.Event])
next_same_pitch = Post.emap1_ check . Stream.zip_on Post.nexts
    where
    check (nexts, event) = (event, filter (same event) nexts)
    same event next =
        Score.event_instrument event == Score.event_instrument next
        && Maybe.isJust nn && nn == Score.initial_nn next
        where nn = Score.initial_nn event


-- * zero dur mute

-- | See DUtil.zero_duration for a version that can apply any kind of
-- transformation.  This one is limited to attrs because it's a postproc, and
-- it's a postproc because otherwise it's hard to tell if a note is really
-- zero duration and not just an infer-duration note.
c_zero_duration_mute :: Derive.Transformer Derive.Note
c_zero_duration_mute = Derive.transformer Module.prelude
    "zero-duration-mute" (Tags.postproc <> Tags.inst)
    "Add attributes to zero duration events."
    $ Sig.callt ((,)
    <$> defaulted "attr" Attrs.mute "Add this attribute."
    <*> defaulted "dyn" (0.75 :: Double) "Scale dynamic by this amount."
    ) $ \(attrs, dyn) _args deriver -> Post.emap1_ (add attrs dyn) <$> deriver
    where
    add attrs dyn event
        | Score.event_duration event == 0 =
            Score.modify_dynamic (*dyn) $ Score.add_attributes attrs event
        | otherwise = event


-- * extend duration

c_extend_duration :: Derive.Transformer Derive.Note
c_extend_duration = Derive.transformer Module.prelude "extend-duration"
    Tags.postproc ("Extend the duration of notes with certain attributes.\
    \ This is appropriate for attributes like " <> ShowVal.doc Attrs.staccato
    <> ", which might already have their own built-in duration, and sound\
    \ better when given as much time to ring as possible."
    ) $ Sig.callt ((,)
    <$> Sig.required "attrs" "Extend durations of notes with these attrs."
    <*> Sig.defaulted "dur" (RealTime.seconds 2)
        "Extend to a minimum of this duration."
    ) $ \(attrs, dur) _args deriver -> Ly.when_lilypond deriver $
        extend_duration attrs dur <$> deriver

-- | Don't overlap with another note with the same pitch, as in 'avoid_overlap'.
extend_duration :: [Attrs.Attributes] -> RealTime -> Stream.Stream Score.Event
    -> Stream.Stream Score.Event
extend_duration attrs dur = Post.emap1_ extend . next_same_pitch
    where
    extend (event, nexts)
        | any (\a -> Score.has_attribute a event) attrs =
            Score.duration (max $ maybe dur (min dur) max_dur) event
        | otherwise = event
        where max_dur = diff event <$> Seq.head nexts
    diff e1 e2 = max 0 $ Score.event_start e2 - Score.event_start e1 - 0.05


-- * apply attributes

c_apply_attributes :: Derive.Transformer Derive.Note
c_apply_attributes = Derive.transformer Module.prelude "apply-attributes"
    Tags.postproc ("Apply attributes by control signal. This looks for\
        \ controls with a " <> ShowVal.doc control_prefix <> " prefix.\
        \ A control named " <> ShowVal.doc (control_prefix <> "a-b")

        <> " will, when non-zero, add the `+a+b` attributes to its events."
    ) $ Sig.call0t $ \_args deriver -> Post.emap1_ apply_attributes <$> deriver

-- | For all controls that start with @+@ and are positive during the event
-- start, add those attributes to the event.
--
-- TODO a possible variation would be to take 0<v<1 as a probability of
-- applying the attribute.
apply_attributes :: Score.Event -> Score.Event
apply_attributes event = Score.add_attributes (mconcat attrs_to_apply) event
    where
    controls :: [(Attrs.Attributes, ScoreT.Control)]
    controls = Seq.key_on_just control_attributes $ Map.keys $
        Score.event_controls event
    attrs_to_apply = map fst $ filter ((>0) . get . snd) controls
    get c = maybe 0 ScoreT.typed_val $
        Score.control_at (Score.event_start event) c event

control_attributes :: ScoreT.Control -> Maybe Attrs.Attributes
control_attributes = fmap (Attrs.attrs . Text.split (=='-'))
    . Text.stripPrefix control_prefix . ScoreT.control_name

control_prefix :: Text
control_prefix = "attr-"
