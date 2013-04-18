module Perform.Lilypond.Constants where
import Util.Control
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang


-- | This is a pseudo-instrument used to mark notes which are actually global
-- lilypond directives.  E.g., meter changes, page breaks, movement titles.
ly_global :: Score.Instrument
ly_global = Score.Instrument "ly-global"

-- | String: @\'right\'@ or @\'left\'@.
v_hand :: TrackLang.ValName
v_hand = TrackLang.Symbol "hand"

-- | Number: should be an integer from 1 to 4
v_voice :: TrackLang.ValName
v_voice = TrackLang.Symbol "voice"

-- * code fragments

-- | String: prepend this lilypond code to the note.  If the event has
-- 0 duration, its pitch will be ignored and it's a freestanding code fragment
-- and will preceed notes starting at the same time.  If the event has no
-- pitch then its also considered a freestanding code fragment, but will
-- occupy the given amount of duration.
v_ly_prepend :: TrackLang.ValName
v_ly_prepend = TrackLang.Symbol "ly-prepend"

-- | String: like 'v_ly_prepend' but append the code to all the notes in a tied
-- sequence.  This is the only append variant accepted for zero-dur notes.
v_ly_append_all :: TrackLang.ValName
v_ly_append_all = TrackLang.Symbol "ly-append-all"

-- | String: append code to the first note in a tied sequence.
v_ly_append_first :: TrackLang.ValName
v_ly_append_first = TrackLang.Symbol "ly-append-first"

-- | String: append code to the last note in a tied sequence.
v_ly_append_last :: TrackLang.ValName
v_ly_append_last = TrackLang.Symbol "ly-append-last"

-- | String: append after the pitch, and before the duration.  This is for
-- pitch modifiers like reminder accidentals (!) and cautionary accidentals
-- (?).
v_ly_append_pitch :: TrackLang.ValName
v_ly_append_pitch = TrackLang.Symbol "ly-append-pitch"

-- | String: \"^\" or \"_\", manually sets tie direction, if this note is
-- tied.
v_ly_tie_direction :: TrackLang.ValName
v_ly_tie_direction = TrackLang.Symbol "ly-tie-direction"

-- * ly-global

-- | String: should be parseable by 'Meter.parse_meter',
-- e.g. @\'3/4\'@.  Used only on @>ly-global@ events.
v_meter :: TrackLang.ValName
v_meter = TrackLang.Symbol "meter"

-- | Gives the title of a new movement.
v_movement :: TrackLang.ValName
v_movement = TrackLang.Symbol "movement"

-- * common code

-- | Emit Ped___^___/ style pedal markings.
mixed_pedal_style :: Text
mixed_pedal_style = "\\set Staff.pedalSustainStyle = #'mixed"
