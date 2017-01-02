-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.Constants where
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Score as Score
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes

import Global


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

-- | String: prepend this lilypond code to the note.  If the event has
-- 0 duration, its pitch will be ignored and it's a freestanding code fragment
-- and will preceed notes starting at the same time.  If the event has no
-- pitch then its also considered a freestanding code fragment, but will
-- occupy the given amount of duration.
v_ly_prepend :: BaseTypes.Key
v_ly_prepend = "ly-prepend"

-- | String: like 'v_ly_prepend' but append the code to all the notes in a tied
-- sequence.  This is the only append variant accepted for zero-dur notes.
--
-- TODO The behaviour for v_ly_append_all for zero-dur notes seems to be
-- v_ly_append_first.  I should make zero-durs understand all the append
-- variations for consistency, though I don't have examples of where they would
-- be useful.
v_ly_append_all :: BaseTypes.Key
v_ly_append_all = "ly-append-all"

-- | String: append code to the first note in a tied sequence.
v_ly_append_first :: BaseTypes.Key
v_ly_append_first = "ly-append-first"

-- | String: append code to the last note in a tied sequence.
v_ly_append_last :: BaseTypes.Key
v_ly_append_last = "ly-append-last"

-- | String: append after the pitch, and before the duration.  This is for
-- pitch modifiers like reminder accidentals (!) and cautionary accidentals
-- (?).
v_ly_append_pitch :: BaseTypes.Key
v_ly_append_pitch = "ly-append-pitch"

-- | String: \"^\" or \"_\", manually sets tie direction, if this note is
-- tied.
v_ly_tie_direction :: BaseTypes.Key
v_ly_tie_direction = "ly-tie-direction"

-- * ly-global

-- | String: should be parseable by 'Meter.parse_meter',
-- e.g. @\'3/4\'@.  Used only on @>ly-global@ events.
v_meter :: BaseTypes.Key
v_meter = "meter"

-- | String: Gives the title of a new movement.  An event with 'ly_global'
-- instrument and this env val will cause a movement break.
v_movement :: BaseTypes.Key
v_movement = "movement"

-- * common code

-- | Emit Ped___^___/ style pedal markings.
mixed_pedal_style :: Text
mixed_pedal_style = "\\set Staff.pedalSustainStyle = #'mixed"
