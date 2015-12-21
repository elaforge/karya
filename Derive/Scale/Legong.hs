-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Saih Pelegongan.
--
-- Tuning from my gender rambat.
--
-- TODO: pengisep and pengumbang
module Derive.Scale.Legong where
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import qualified Util.Seq as Seq
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch
import Global


scales :: [Scale.Make]
scales = map Scale.Simple
    [ Scales.add_doc "Saih pelegongan, from my instruments." $
        BaliScales.make_scale scale_id complete_scale
    , BaliScales.make_scale "legong-c" cipher_scale
    , Scales.add_doc
        "Pemade scale. This can be used to give the the same score to both\
            \ pemade and kantilan." $
        BaliScales.make_scale "legong-pemade" pemade
    , Scales.add_doc
        "Kantilan scale. This can be used to give the the same score to both\
            \ pemade and kantilan." $
        BaliScales.make_scale "legong-kantilan" kantilan
    ]

complete_scale :: BaliScales.ScaleMap
complete_scale = scale_map
    (BaliScales.ioeua_relative True default_key all_keys)
    (0, Vector.length (BaliScales.nn_umbang note_numbers) - 1)

cipher_scale :: BaliScales.ScaleMap
cipher_scale = scale_map
    (BaliScales.cipher_relative_dotted 4 default_key all_keys)
    (0, Vector.length (BaliScales.nn_umbang note_numbers) - 1)

jegog, calung, penyacah :: BaliScales.ScaleMap
jegog = inst_scale_map 1 (0, 0) (1, -1)
calung = inst_scale_map 2 (1, 0) (2, -1)
penyacah = inst_scale_map 3 (2, 0) (3, -1)

pemade :: BaliScales.ScaleMap
pemade = inst_scale_map 4 (2, 1) (4, 0)

kantilan :: BaliScales.ScaleMap
kantilan = inst_scale_map 5 (3, 1) (5, 0)

ugal_range, rambat_range, trompong_range, reyong_range :: Scale.Range
ugal_range = Scale.Range (Pitch.pitch 2 1) (Pitch.pitch 4 0)
rambat_range = Scale.Range (Pitch.pitch 2 2) (Pitch.pitch 5 0)
trompong_range = Scale.Range (Pitch.pitch 2 5) (Pitch.pitch 4 4)
reyong_range = Scale.Range (Pitch.pitch 3 2) (Pitch.pitch 5 4)

inst_scale_map :: Pitch.Octave -> (Pitch.Octave, Pitch.Semi)
    -> (Pitch.Octave, Pitch.Semi) -> BaliScales.ScaleMap
inst_scale_map center_octave (low_oct, low_pc) (high_oct, high_pc) = scale_map
    (BaliScales.ioeua_relative_arrow center_octave True default_key all_keys)
    (low_oct * 7 + low_pc, high_oct * 7 + high_pc)

scale_map :: TheoryFormat.Format -> (Pitch.Semi, Pitch.Semi)
    -> BaliScales.ScaleMap
scale_map fmt range =
    BaliScales.scale_map layout fmt all_keys default_key note_numbers range

scale_id :: Pitch.ScaleId
scale_id = "legong"

layout :: Theory.Layout
layout = Theory.layout [1, 1, 2, 1, 2]

-- | These are from Tenzer's kebyar book, for Semar Pegulingan.  McPhee's
-- book has different names for gambuh, but Tenzer's book is more modern.
all_keys :: ChromaticScales.Keys
all_keys = BaliScales.make_keys layout $ map make_key
    [ ("selisir", [1, 2, 3, 5, 6])
    , ("slendro-gede", [2, 3, 4, 6, 7])
    , ("baro", [1, 3, 4, 5, 7])
    , ("tembung", [1, 2, 4, 5, 6])
    , ("sunaren", [2, 3, 5, 6, 7])
    , ("pengenter-alit", [1, 3, 4, 6, 7])
    , ("pengenter", [1, 2, 4, 5, 7])
    -- TODO these all have a hardcoded layout that assumes some "accidentals".
    -- For lebeng I can just use selisir with all the notes.
    -- , ("lebeng", [1, 2, 3, 4, 5, 6, 7])
    ]

make_key :: (Text, [Pitch.Semi]) -> (Text, Pitch.Semi, [Pitch.Semi])
make_key (_, []) = error "no semis for scale"
make_key (name, n : ns) = (name, n - 1, zipWith (-) (ns ++ [n+7]) (n:ns))

default_key :: Theory.Key
Just default_key = Map.lookup (Pitch.Key "selisir") all_keys

note_numbers :: BaliScales.NoteNumbers
note_numbers = BaliScales.NoteNumbers
    (Vector.fromList (extend umbang)) (Vector.fromList (extend isep))

{- | Extended scale.

 @
  1i 1o 1e 1u 1a 2i 2o 2e 2u 2a 3i 3o 3e 3u 3a 4i 4o 4e 4u 4a 5i 5o 5e 5u 5a 6i
  jegog---------
                 calung--------
                    ugal-------------------------
                       rambat-----------------------------------
  0              7              14             21             28             35
  1i 1o 1e 1u 1a 2i 2o 2e 2u 2a 3i 3o 3e 3u 3a 4i 4o 4e 4u 4a 5i 5o 5e 5u 5a 6i
                             trompong---------------------
                                   pemade-----------------------
                                                  kantilan---------------------
                                      reyong-----------------------------
                                      |------|---       |---|---
                                               |------|---    |---------|
  1i 1o 1e 1u 1a 2i 2o 2e 2u 2a 3i 3o 3e 3u 3a 4i 4o 4e 4u 4a 5i 5o 5e 5u 5a 6i
 @
-}
umbang :: [Pitch.NoteNumber]
umbang =
    [ 51.82 -- deng, rambat begin
    , 54 -- TODO deng#
    , 55.7
    , 56.82 -- dang, trompong begin
    , 58 -- TODO

    , 60.73
    , 62.8 -- dong, pemade begin
    , 63.35 -- deng, reyong begin
    , 65 -- TODO
    , 67.7
    , 68.2
    , 70 -- TODO

    , 72.46 -- ding
    , 73.9 -- dong, kantilan begin
    , 75.5
    , 78 -- TODO
    , 79.4 -- dung, trompong end
    , 80.5
    , 83 -- TODO

    , 84.46 -- ding, rambat end, pemade end
    , 86
    , 87.67
    , 90 -- TODO
    , 91.74 -- dung, reyong end
    , 92.5
    , 95 -- TODO

    , 96.46 -- ding, kantilan end
    ]

-- TODO
isep :: [Pitch.NoteNumber]
isep = map (Pitch.add_hz 4) umbang

-- | I don't actually have the pemero notes on my instruments, so strip them
-- back out to get the actually recorded scale.
strip_pemero :: [a] -> [a]
strip_pemero = go
    where
    go [] = []
    go nns = strip nns ++ go (drop 7 nns)
    strip nns = mapMaybe (Seq.at nns) [0, 1, 2, 4, 5]

-- | Add one octave on the bottom, so I get down to 1i, which is jegog range.
extend :: [Pitch.NoteNumber] -> [Pitch.NoteNumber]
extend nns = map (subtract 12) (take oct from_ding) ++ from_ding
    where
    from_ding = map (subtract 12) (take 2 (drop (oct-2) nns)) ++ nns
    oct = 7

-- * instrument integration

-- | A PatchScale with the entire theoretical range.  This is for instruments
-- that are normalized to 12tet and then tuned in the patch (e.g. using KSP).
complete_patch_scale :: BaliScales.Tuning -> Instrument.PatchScale
complete_patch_scale = patch_scale id

patch_scale ::
    ([(Midi.Key, Pitch.NoteNumber)] -> [(Midi.Key, Pitch.NoteNumber)])
    -- ^ drop and take keys for the instrument's range
    -> BaliScales.Tuning -> Instrument.PatchScale
patch_scale take_range tuning =
    Instrument.make_patch_scale ("legong " <> ShowVal.show_val tuning) $
        take_range $ zip midi_keys (extend nns)
    where
    nns = case tuning of
        BaliScales.Umbang -> umbang
        BaliScales.Isep -> isep

-- | Emit from i1 on up.
midi_keys :: [Midi.Key]
midi_keys = trim $ concatMap keys [1..]
    where
    trim = take (5*7 + 1)
    keys oct = map (Midi.to_key (oct * 12) +) -- i o e e# u a a#
        [Key.c_1, Key.d_1, Key.e_1, Key.f_1, Key.g_1, Key.a_1, Key.b_1]
