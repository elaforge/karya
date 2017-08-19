-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.Constants where
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Env as Env
import qualified Derive.Score as Score
import qualified Derive.Typecheck as Typecheck

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes

import Global
import Types


-- * ly-global instrument

-- | This is a pseudo-instrument used to mark notes which are actually global
-- lilypond directives.  E.g., meter changes, page breaks, movement titles.
ly_global :: Score.Instrument
ly_global = Score.Instrument "ly-global"

ly_qualified :: InstTypes.Qualified
ly_qualified = InstTypes.Qualified "ly" "global"

ly_synth :: code -> Inst.SynthDecl code
ly_synth code = Inst.SynthDecl "ly" "Fake synth for fake lilypond instrument."
    [("global", Inst.Inst Inst.Dummy (Common.doc #= doc $ Common.common code))]
    where
    doc = "The lilypond deriver will automatically allocate `>ly-global`, and\
        \ instruments with global lilypond directives will get this instrument."

-- * code fragments

-- TODO get rid of the ly_ prefix, they all have it

-- | String: prepend this lilypond code to the note.  If the event has
-- 0 duration, its pitch will be ignored and it's a freestanding code fragment
-- and will preceed notes starting at the same time.  If the event has no
-- pitch then its also considered a freestanding code fragment, but will
-- occupy the given amount of duration.
v_prepend :: BaseTypes.Key
v_prepend = "ly-prepend"

-- | String: like 'v_prepend' but append the code to all the notes in a tied
-- sequence.  This is the only append variant accepted for zero-dur notes.
--
-- TODO The behaviour for v_append_all for zero-dur notes seems to be
-- v_append_first.  I should make zero-durs understand all the append
-- variations for consistency, though I don't have examples of where they would
-- be useful.
v_append_all :: BaseTypes.Key
v_append_all = "ly-append-all"

-- | String: like 'v_append_all', but it goes after notes inside a chord,
-- instead of once after the chord itself.
v_note_append_all :: BaseTypes.Key
v_note_append_all = "ly-note-append-all"

-- | String: append code to the first note in a tied sequence.
v_append_first :: BaseTypes.Key
v_append_first = "ly-append-first"

-- | String: like 'v_append_all_first', but it goes after notes inside a chord,
-- instead of once after the chord itself.
v_note_append_first :: BaseTypes.Key
v_note_append_first = "ly-note-append-first"

-- | String: append code to the last note in a tied sequence.
v_append_last :: BaseTypes.Key
v_append_last = "ly-append-last"

-- | String: append after the pitch, and before the duration.  This is for
-- pitch modifiers like reminder accidentals (!) and cautionary accidentals
-- (?).
v_append_pitch :: BaseTypes.Key
v_append_pitch = "ly-append-pitch"

-- | String: \"^\" or \"_\", manually sets tie direction, if this note is
-- tied.
v_tie_direction :: BaseTypes.Key
v_tie_direction = "ly-tie-direction"

-- * tuplet

-- | Set the env vars that signals that the lilypond converter should make
-- the following notes into a tuplet.
set_tuplet :: RealTime -- ^ score_dur is the visible duration in the score
    -> RealTime -- ^ real_dur is the duration it actually consumes, so
    -- 3 quarters into 1 whole will be 3/4.
    -> Env.Environ
set_tuplet score_dur real_dur = Env.from_list
    [ ("ly-tuplet-score-dur", Typecheck.to_val score_dur)
    , ("ly-tuplet-real-dur", Typecheck.to_val real_dur)
    ]

get_tuplet :: Env.Environ -> Maybe (RealTime, RealTime)
get_tuplet env = (,) <$> get "ly-tuplet-score-dur" <*> get "ly-tuplet-real-dur"
    where get k = Env.maybe_val k env

-- * ly-global

-- | String: should be parseable by 'Meter.parse_meter',
-- e.g. @\'3/4\'@.  Used only on @>ly-global@ events.
v_meter :: BaseTypes.Key
v_meter = "ly-meter"

-- | String: this has the same format as 'v_meter', but it affects the rhythmic
-- spelling for the instrument.
v_subdivision :: BaseTypes.Key
v_subdivision = "ly-subdivision"

-- | String: Gives the title of a new movement.  An event with 'ly_global'
-- instrument and this env val will cause a movement break.
v_movement :: BaseTypes.Key
v_movement = "ly-movement"

-- * common code

-- | Emit Ped___^___/ style pedal markings.
mixed_pedal_style :: Text
mixed_pedal_style = "\\set Staff.pedalSustainStyle = #'mixed"
