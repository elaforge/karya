-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for kontakt.
module Local.Instrument.KontaktUtil where
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Vector

import Util.Control
import qualified Perform.Midi.Instrument as Instrument


-- | Create a script in Kontakt's hilariously incompetent KSP language to
-- retune a 12TET patch to the given scale.
tuning_ksp :: Instrument.PatchScale -> Text
tuning_ksp (Instrument.PatchScale name scale) =
    "on init\n\
    \    set_script_title(" <> showt name <> ")\n" <> pitch_table scale
    <> "end on\n\
    \\n\
    \on note\n\
    \    change_tune($EVENT_ID, %pitches[$EVENT_NOTE], 0)\n\
    \end on\n"
    -- To ignore notes that don't have a tuning, I could set another %set array
    -- with 0 or one, and do ignore_event($EVENT_ID).

pitch_table :: Vector.Vector Double -> Text
pitch_table scale = Text.unlines $ map ("    "<>) $
    "declare %pitches[" <> showt (Vector.length scale) <> "]"
    : ["%pitches[" <> showt key <> "] := " <> from_nn key nn |
        (key, nn) <- zip [0..] (Vector.toList scale)]
    where
    from_nn key nn
        | nn == 0 = "0"
        | otherwise = showt (round ((nn - fromIntegral key) * millicent) :: Int)
    millicent = 1000 * 100 -- silly scent willy sent
