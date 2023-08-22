-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Javanese scales.

    @
    01 02 03 05 06 11 12 13 15 16 21 22 23 25 26 31 32 33 35 36 41 42 43 45 46
    gong---------->   kempul----------->            kenong----------->
                suwukan->
    11 12 13 15 16 21 22 23 25 26 31 32 33 35 36 41 42 43 45 46 51 52 53 55 56
                               kethuk                        pyang
                   slenthem------>saron demung-->saron barung-->peking-------->
                gender barung---------------------------->
                6..1. 2. 3. 5. 6. 1  2  3  5  6  1^ 2^ 3^
                               gender panerus--------------------------->
                               6..1. 2. 3. 5. 6. 1  2  3  5  6  1^ 2^ 3^
                gambang------------------------------------------------->
           (?)  6..1. 2. 3. 5. 6  1  2  3  5  6  1^ 2^ 3^ 5^ 6^ 1^^2^^3^^
    11 12 13 15 16 21 22 23 25 26 31 32 33 35 36 41 42 43 45 46 51 52 53 55 56
                      siter------------------------------>
                                  bonang barung---------------->
                                                 bonang panerus--------------->
    @
-}
module Derive.Scale.Java (scales, old_scales) where
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.JavaScales as JavaScales
import qualified Derive.Sig as Sig

import qualified Perform.Pitch as Pitch

import           Global


type Pathet = Text

scales :: Map Derive.CallName Derive.ScaleCall
scales = Map.fromList
    [ ("pelog", s_pelog)
    ]

s_pelog :: Derive.ScaleCall
s_pelog = Derive.scale_call "pelog" "doc" $
    Scale.call ((,)
    <$> Sig.defaulted "pathet" ("lima" :: Text) "doc"
    <*> Sig.defaulted "scale-inst" (Nothing :: Maybe Text) "doc"
    ) $ \(pathet, mb_inst) -> do
        layout <- Derive.require ("unknown pathet: " <> pathet) $
            Map.lookup pathet keys
        mb_inst <- traverse
            (\inst -> Derive.require ("unknown scale-inst: " <> inst) $
                Map.lookup inst instruments)
            mb_inst
        return $ make_scale "pelog" layout mb_inst

-- TODO name and "doc" are now redundant, they now come from ScaleCall
make_scale :: Text -> JavaScales.Layout -> Maybe JavaScales.Instrument
    -> Scale.Scale
make_scale name layout mb_inst =
    JavaScales.make_scale (Pitch.ScaleId name) smap "doc"
    where
    smap = JavaScales.ScaleMap
        { layout
        , default_laras
        , laras_map
        , format = case mb_inst of
            Nothing -> JavaScales.cipher_absolute layout
            Just inst -> JavaScales.cipher_octave_relative layout inst
        }

keys :: Map Pathet JavaScales.Layout
keys = Map.fromList
    [ ("lima", JavaScales.make_layout 0 [1, 1, 2, 1, 2]) -- 12356
    , ("nem", JavaScales.make_layout 0 [1, 1, 2, 1, 2]) -- 12356
    , ("barang", JavaScales.make_layout 1 [1, 2, 1, 1, 2]) -- 23567
    ]

instruments :: Map Text JavaScales.Instrument
instruments = Map.fromList
    [ ( "gender-barung"
      , JavaScales.Instrument
        { center = 3
        , bottom = JavaScales.Absolute 1 6
        , top = JavaScales.Absolute 4 3
        }
      )
    , ("gender-panerus"
      , JavaScales.Instrument
        { center = 4
        , bottom = JavaScales.Absolute 2 6
        , top = JavaScales.Absolute 5 3
        }
      )
    ]

laras_map :: Map Text BaliScales.Laras
laras_map = Map.fromList $ Lists.keyOn BaliScales.laras_name
    [ laras_sequoia_pelog
    ]

default_laras :: BaliScales.Laras
default_laras = laras_sequoia_pelog

laras_sequoia_pelog :: BaliScales.Laras
laras_sequoia_pelog = BaliScales.laras "sequoia-pelog" (Pitch.pitch 1 5) id
    "Tuning of Sekar Sequoia." $ map (\nn -> (nn, nn)) $
    [ 46.68 -- 16
    , 48.13 -- 17
    , 50.18 -- 21
    , 51.65 -- 22
    , 53    -- 23
    , 56    -- 24
    , 57.05 -- 25
    -- ^^ TODO guess

    , 58.68 -- 26 6.. (as3 + 0.5)
    , 60.13 -- 27 7.. (c4)
    , 62.18 -- 31 1. approx, no resonator
    , 63.65 -- 32 2.
    , 65    -- 33 3.
    , 68    -- 34 4. guess, no key
    , 69.05 -- 35 5.
    , 70.5  -- 36 6.
    , 72.14 -- 37 7.
    , 74.25 -- 41 1 approx
    , 75.68 -- 42 2
    , 77    -- 43 3
    , 80    -- 44 4 guess
    , 81.03 -- 45 5
    , 82.48 -- 46 6
    , 84.14 -- 47 7
    , 86.4  -- 51 51 1^ approx
    , 87.7  -- 52 2^
    , 88.98 -- 53 3^
    ]
    -- vv TODO guess
    ++ map (+12)
    [ 80
    , 81.03
    , 82.48
    , 84.14
    , 86.4
    , 87.7
    , 88.98
    ]


-- * old


old_scales :: [Scale.Definition]
old_scales = map Scale.Simple $
    -- TODO This permutation game is no good, let's go back to key=pathet
    -- and maybe even scale-range=gender-panerus
    [ make_scale "pelog-lima" lima Nothing
    , make_scale "pelog-nem" lima Nothing
    , make_scale "pelog-barang" barang Nothing
    ] ++ concatMap (uncurry inst_scale) (Map.toList instruments)
    where
    lima = JavaScales.make_layout 0 [1, 1, 2, 1, 2] -- 12356
    barang = JavaScales.make_layout 1 [1, 2, 1, 1, 2] -- 23567
    inst_scale name inst =
        [ make_scale ("pelog-lima-" <> name) lima (Just inst)
        , make_scale ("pelog-barang-" <> name) barang (Just inst)
        ]
    make_scale name layout mb_inst =
        JavaScales.make_scale (Pitch.ScaleId name) smap "doc"
        where
        smap = JavaScales.ScaleMap
            { layout
            , default_laras
            , laras_map
            , format = case mb_inst of
                Nothing -> JavaScales.cipher_absolute layout
                Just inst -> JavaScales.cipher_octave_relative layout inst
            }
