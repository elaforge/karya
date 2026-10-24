-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.Parse.Instruments_test where

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Vector.Unboxed

import qualified Util.ParseM as ParseM
import qualified Derive.Controls as Controls
import qualified Derive.Parse.Instruments as I
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Signal as Signal
import qualified Ui.UiConfig as UiConfig

import           Global
import           Util.Test


test_parse_instruments :: Test
test_parse_instruments = do
    let parse = fmap (Map.elems . UiConfig.unallocations)
            . parseInstruments
    let alloc synth name = UiConfig.Allocation (InstT.Qualified synth name)
    rightEqual (parse "") []
    rightEqual (parse ">i im/xyz [ms] im")
        [alloc "im" "xyz" config UiConfig.Im]
    rightEqual (parse ">i midi/p [ms] dev 1")
        [alloc "midi" "p" config (UiConfig.Midi (midi [0]))]
    rightEqual (parse ">i sc/ [ms] sc\n{controls: {dyn: .25}}")
        [alloc "sc" "" (config_ [(Controls.dynamic, 0.25)]) UiConfig.Sc]
    leftLike (parse ">i sc/ [ms] sc {decay: 1}") "midi fields for sc"
    leftLike (parse ">i sc/ [ms] sc\n>i sc/ [ms] sc\n")
        "duplicate instrument names: i"
    rightEqual
        (parse ">i1 midi/ [ms] dev 1 2 {decay: 1}\n-- hi\n>i2 im/ [ms] im\n")
        [ alloc "midi" "" config $
            UiConfig.Midi $ Patch.settings#Patch.decay #= Just 1 $
                midi [0, 1]
        , alloc "im" "" config UiConfig.Im
        ]

test_equal :: Test
test_equal = do
    let parse = fmap (Map.elems . UiConfig.unallocations) . parseInstruments
    let alloc synth name = UiConfig.Allocation (InstT.Qualified synth name)
    let legong = Patch.Scale "legong-umbang" $
            Vector.Unboxed.fromList $ [1, 2] <> replicate (128 - 2) (-1)
    rightEqual (parse
        ">i1 / [ms] dev 1\n\
        \>i2 / [ms] dev 2 { scale: legong-umbang }\n\
        \legong-umbang = [1, 2]\n")
        [ alloc "" "" config $ UiConfig.Midi $ midi [0]
        , alloc "" "" config $ UiConfig.Midi $
            Patch.settings#Patch.scale #= Just legong $
            midi [1]
        ]

test_un_instruments :: Test
test_un_instruments = do
    let parse = parseInstruments
    let un = I.unInstruments (const Nothing)
    let ky =
            ">i1 midi/ [Ms] dev 1\n\
            \    {decay: 1s}\n\
            \>i2 im/ [mS] im\n\
            \    { env: {a: b}\n\
            \    }\n\
            \>i3 midi/ [ms] dev 2..4\n\
            \>i4 im/ [ms] im\n"
    rightEqual (un =<< parse ky) ky
    let ky =
            ">i1 midi/ [ms] dev 1\n\
            \{ scale: { name: legong, key_to_nn: [1, 2, 3] } }\n\
            \>i2 midi/ [ms] dev 2\n\
            \{ scale: { name: legong, key_to_nn: [1, 2, 3] } }\n"
    rightEqual (un =<< parse ky)
        ">i1 midi/ [ms] dev 1\n\
        \    {scale: legong}\n\
        \>i2 midi/ [ms] dev 2\n\
        \    {scale: legong}\n\
        \legong = [1, 2, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1]\n"

parseInstruments :: Text -> Either Text UiConfig.Allocations
parseInstruments = ParseM.parse I.pInstruments

config :: Common.Config
config = Common.emptyConfig

config_ :: [(ScoreT.Control, Signal.Y)] -> Common.Config
config_ cs = Common.emptyConfig { Common.configControls = Map.fromList cs }

midi :: [Midi.Channel] -> Patch.Config
midi chans = Patch.config [((dev, chan), Nothing) | chan <- chans]

dev :: Midi.WriteDevice
dev = Midi.writeDevice "dev"

test_p_alloc_line :: Test
test_p_alloc_line = do
    let f = pAllocLine
    let syn = InstT.Qualified "syn" ""
    rightEqual (f ">i syn/p [ms] im -- hi") $
        I.Allocation "i" (InstT.Qualified "syn" "p") I.emptyConfig I.Im
    let loop1 = Midi.writeDevice "loop1"
    rightEqual (f ">i syn/ [ms] loop1 2 1") $
        I.Allocation "i" syn I.emptyConfig (I.Midi loop1 [0, 1])
    rightEqual (f ">i syn/ [ms] loop1 1..3 5") $
        I.Allocation "i" syn I.emptyConfig (I.Midi loop1 [0, 1, 2, 4])
    leftLike (f ">i syn/ [ms] loop1 0") "should be in range"
    leftLike (f ">i syn/ [ms] loop1") "expecting * nat"
    leftLike (f ">i syn/ [ms] loop1 x") "expecting * nat"
    rightEqual (f ">i syn/ [ms] sc") $
        I.Allocation "i" syn I.emptyConfig I.Sc
    rightEqual (f ">i syn/ [Ms] dummy") $
        I.Allocation "i" syn (I.Config True False) I.Dummy
    leftLike (f ">i syn/ [msq]") "expecting ']'"

test_alloc_line_roundtrip :: Test
test_alloc_line_roundtrip = do
    let syn = InstT.Qualified "syn" "p"
    let trip alloc =
            ( Right alloc
            , pAllocLine $ Text.unlines $ I.unAllocLine alloc
            )
    uncurry equal $ trip $
        I.Allocation "i" syn I.emptyConfig I.Im
    uncurry equal $ trip $
        I.Allocation "i" syn (I.Config False True) (I.Midi dev [0, 2])
    uncurry equal $ trip $
        I.Allocation "i" syn (I.Config True False) (I.Midi dev [2])

pAllocLine :: Text -> Either Text I.Allocation
pAllocLine = ParseM.parse I.pAllocLine
