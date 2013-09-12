module Derive.Scale.ChromaticScales_test where
import Util.Control
import Util.Test
import qualified Cmd.CmdTest as CmdTest
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Twelve as Twelve
import qualified Perform.Pitch as Pitch


test_input_to_note = do
    let f smap key = maybe "" Pitch.note_text <$>
            ChromaticScales.input_to_note smap (Just (Pitch.Key key))
        abs = Twelve.absolute_scale_map
        rel = Twelve.relative_scale_map
        ascii (oct, pc, accs) =
            Pitch.Input Pitch.AsciiKbd (CmdTest.pitch oct pc accs) 0
    equal (map (f abs "c-maj" . ascii) [(5, 6, 0), (5, 6, 1), (5, 7, 0)])
        ["5b", "6c", "6c"]
    equal (map (f rel "d-min" . ascii) [(4, 2, 0), (4, 2, 1), (4, 3, 0)])
        ["4g", "4g#", "4m"]
