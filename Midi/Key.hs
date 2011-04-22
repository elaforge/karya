-- | Symbolic names for all the MIDI white keys.  You can get sharps and flats
-- by adding and subtracting 1.
--
-- The octave convention is that C4 is middle C, as in
-- http://en.wikipedia.org/wiki/Scientific_pitch_notation.
--
-- 60 is the grand-staff's middle C, which is C4.
module Midi.Key (
    c_1, d_1, e_1, f_1, g_1, a_1, b_1
    , c0, d0, e0, f0, g0, a0, b0
    , c1, d1, e1, f1, g1, a1, b1
    , c2, d2, e2, f2, g2, a2, b2
    , c3, d3, e3, f3, g3, a3, b3
    , c4, d4, e4, f4, g4, a4, b4
    , c5, d5, e5, f5, g5, a5, b5
    , c6, d6, e6, f6, g6, a6, b6
    , c7, d7, e7, f7, g7, a7, b7
    , c8, d8, e8, f8, g8, a8, b8
    , c9, d9, e9, f9, g9
) where
import Midi.Midi (Key)

notes :: [Key]
notes = [0, 2, 4, 5, 7, 9, 11] -- intervals starting from C

[c_1, d_1, e_1, f_1, g_1, a_1, b_1] = notes
[c0, d0, e0, f0, g0, a0, b0] = map (+12) notes
[c1, d1, e1, f1, g1, a1, b1] = map (+24) notes
[c2, d2, e2, f2, g2, a2, b2] = map (+36) notes
[c3, d3, e3, f3, g3, a3, b3] = map (+48) notes
[c4, d4, e4, f4, g4, a4, b4] = map (+60) notes
[c5, d5, e5, f5, g5, a5, b5] = map (+72) notes
[c6, d6, e6, f6, g6, a6, b6] = map (+84) notes
[c7, d7, e7, f7, g7, a7, b7] = map (+96) notes
[c8, d8, e8, f8, g8, a8, b8] = map (+108) notes
[c9, d9, e9, f9, g9, _, _] = map (+120) notes
