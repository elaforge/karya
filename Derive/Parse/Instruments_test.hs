-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.Parse.Instruments_test where

import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Parse as Parse
import qualified Derive.Controls as Controls
import qualified Derive.Parse.Instruments as I
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Midi.Patch
import qualified Perform.Signal as Signal
import qualified Ui.UiConfig as UiConfig

import           Global
import           Util.Test


test_p_instruments :: Test
test_p_instruments = do
    let parse = fmap (Map.elems . UiConfig.unallocations)
            . Parse.parse I.p_instruments
    let alloc synth name = UiConfig.Allocation (InstT.Qualified synth name)
    right_equal (parse "") []
    right_equal (parse ">i im/xyz [ms] im")
        [alloc "im" "xyz" config UiConfig.Im]
    right_equal (parse ">i midi/p [ms] dev 1")
        [alloc "midi" "p" config (UiConfig.Midi (midi [0]))]
    right_equal (parse ">i sc/ [ms] sc\n{controls: {dyn: .25}}")
        [alloc "sc" "" (config_ [(Controls.dynamic, 0.25)]) UiConfig.Sc]
    left_like (parse ">i sc/ [ms] sc {decay: 1}") "midi fields for sc"
    left_like (parse ">i sc/ [ms] sc\n>i sc/ [ms] sc\n")
        "duplicate instrument names: i"
    right_equal
        (parse ">i1 midi/ [ms] dev 1 2 {decay: 1}\n-- hi\n>i2 im/ [ms] im\n")
        [ alloc "midi" "" config $
            UiConfig.Midi $ Midi.Patch.settings#Midi.Patch.decay #= Just 1 $
                midi [0, 1]
        , alloc "im" "" config UiConfig.Im
        ]

test_un_instruments :: Test
test_un_instruments = do
    let parse = Parse.parse I.p_instruments
    let un = I.un_instruments
    let ky =
            ">i1 midi/ [Ms] dev 1\n\
            \{decay: 1s}\n\
            \>i2 im/ [mS] im\n\
            \{ env: {a: b}\n\
            \}\n\
            \>i3 midi/ [ms] dev 2..4\n\
            \>i4 im/ [ms] im\n"
    right_equal (un =<< parse ky) ky

config :: Common.Config
config = Common.empty_config

config_ :: [(ScoreT.Control, Signal.Y)] -> Common.Config
config_ cs = Common.empty_config { Common.config_controls = Map.fromList cs }

midi :: [Midi.Channel] -> Midi.Patch.Config
midi chans = Midi.Patch.config [((dev, chan), Nothing) | chan <- chans]

dev :: Midi.WriteDevice
dev = Midi.write_device "dev"

test_p_alloc_line :: Test
test_p_alloc_line = do
    let f = p_alloc_line
    let syn = InstT.Qualified "syn" ""
    right_equal (f ">i syn/p [ms] im -- hi") $
        I.Allocation "i" (InstT.Qualified "syn" "p") I.empty_config I.Im
    let loop1 = Midi.write_device "loop1"
    right_equal (f ">i syn/ [ms] loop1 2 1") $
        I.Allocation "i" syn I.empty_config (I.Midi loop1 [0, 1])
    right_equal (f ">i syn/ [ms] loop1 1..3 5") $
        I.Allocation "i" syn I.empty_config (I.Midi loop1 [0, 1, 2, 4])
    left_like (f ">i syn/ [ms] loop1 0") "should be in range"
    left_like (f ">i syn/ [ms] loop1") "expecting * nat"
    left_like (f ">i syn/ [ms] loop1 x") "expecting * nat"
    right_equal (f ">i syn/ [ms] sc") $
        I.Allocation "i" syn I.empty_config I.Sc
    right_equal (f ">i syn/ [Ms] dummy") $
        I.Allocation "i" syn (I.Config True False) I.Dummy
    left_like (f ">i syn/ [msq]") "expecting ']'"

test_alloc_line_roundtrip :: Test
test_alloc_line_roundtrip = do
    let syn = InstT.Qualified "syn" "p"
    let trip alloc =
            ( Right alloc
            , p_alloc_line $ Text.unlines $
                I.unparse_allocations [(Just alloc, "")]
            )
    uncurry equal $ trip $ I.Allocation "i" syn I.empty_config I.Im
    uncurry equal $ trip $ I.Allocation "i" syn (I.Config False True)
        (I.Midi dev [0, 2])
    uncurry equal $ trip $ I.Allocation "i" syn (I.Config True False)
        (I.Midi dev [2])

p_alloc_line :: Text -> Either Text I.Allocation
p_alloc_line = Parse.parse I.p_alloc_line
