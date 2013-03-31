module Perform.Lilypond.Constants where
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang


-- | This is a pseudo-instrument used to mark notes which are actually global
-- lilypond directives.  E.g., meter changes, page breaks, movement titles.
ly_global :: Score.Instrument
ly_global = Score.Instrument "ly-global"

-- | String: @\'right\'@ or @\'left\'@.
v_hand :: TrackLang.ValName
v_hand = TrackLang.Symbol "hand"

-- | String: should be parseable by 'Meter.parse_meter',
-- e.g. @\'3/4\'@.
v_meter :: TrackLang.ValName
v_meter = TrackLang.Symbol "meter"

-- | Number: should be an integer from 1 to 4
v_voice :: TrackLang.ValName
v_voice = TrackLang.Symbol "voice"

-- | String: prepend this lilypond code to the note.  If the note has
-- 0 duration, it's a freestanding expression and should go before notes
-- starting at the same time.
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
