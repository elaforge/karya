-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Implement the post-processor that fixes up negative durations.

    Arriving beats are implemented with negative duration.  An arriving beat is
    a beat which arrives at the end of a unit of time, instead of departing
    from the beginning.  This concept is usually associated with Indonesian
    music, but Western music includes arriving patterns such as cadences and
    grace notes, while Indonesian music includes departing patterns such as
    nruktuk.

    An arriving note is not a change in the sounding duration of the note, but
    is a higher level concept.  The harmonic scope of of a departing note
    extends after the note in the pitch context established by that note, while
    the scope of an arriving note precedes the sounding of the note, preparing
    for its arrival.  A note whose harmonic scope precedes the sounding of the
    note itself is naturally represented as an event with negative duration.

    However, since the physical sounding duration (which of course must always
    be positive) and harmonic scope of a note no longer coincide, the sounding
    duration of an arriving note is implicit.

    If the event lines up with the following event then the note will sound
    until the next note begins:

    @
        --|--|
          |++|++...
    @

    If the event doesn't line up, then the empty space is taken as a rest,
    and the note is played up until the arrival of the rest:

    @
        --|   --|
          |+++  |++...
    @

    An arriving note followed by a departing note will sound until the
    beginning of the departing note, and a departing note followed by an
    arriving note is unchanged.

    Note that final note, if it is an arriving note, has an ambiguous duration
    (represented as @...@ above).  The @negative-duration@ post processor takes
    an argument for that, but you can also specify the duration of the final
    note easily enough by making it a departing note.
-}
module Derive.Call.Post.ArrivalNote where
import qualified Data.List as List

import Util.Control
import qualified Util.Seq as Seq
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls [("arrival-note", c_arrival_note)]

c_arrival_note :: Derive.NoteCall
c_arrival_note = Derive.transformer "arrival-note" Tags.postproc
    "Postprocess events to replace negative durations with postive ones.\
    \ See 'Derive.Call.Post.ArrivalNote' for details.\
    \\nThis is only needed for events with `+arrival-note` set, which the\
    \ default note deriver puts on events at the end of a block.\
    \ Since figuring out the sounding duration requires the next event,\
    \ and there may be no track correspondance between blocks, another way\
    \ way to figure out the next event is needed. This call looks for the\
    \ next event with the same (instrument, hand, voice)."
    $ Sig.callt
    ( Sig.defaulted "final-duration" 1
        "Notes with negative duration have an implicit sounding duration\
        \ which depends on the following note. This means that the\
        \ duration of the final note of the score is ambiguous."
    ) $ \final_dur _args deriver -> arrival_note final_dur <$> deriver

arrival_note :: RealTime -> Derive.Events -> Derive.Events
arrival_note final_dur events = zipWith go events (drop 1 (List.tails events))
    where
    go log@(LEvent.Log _) _ = log
    go event@(LEvent.Event e) _
        | not (Score.has_attribute Attrs.arrival_note e) = event
    go (LEvent.Event event) rest = case find_next event rest of
        Nothing -> LEvent.Event $ set_dur final_dur event
        Just next -> LEvent.Event $ flip set_dur event $ Note.adjust_duration
            (Score.event_start event) (Score.event_duration event)
            (Score.event_start next) (Score.event_duration next)
    set_dur dur = Score.remove_attributes Attrs.arrival_note
        . Score.set_duration dur

find_next :: Score.Event -> Derive.Events -> Maybe Score.Event
find_next event = Seq.head . filter ((==voice) . voice_of) . LEvent.events_of
    where voice = voice_of event

voice_of :: Score.Event -> (Score.Instrument, Maybe TrackLang.Symbol, Maybe Int)
voice_of event =
    (Score.event_instrument event, lookup Environ.hand, lookup Environ.voice)
    where lookup name = TrackLang.maybe_val name (Score.event_environ event)
