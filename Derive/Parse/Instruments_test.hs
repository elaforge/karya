-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.Parse.Instruments_test where

import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Parse as Parse
import qualified Derive.Parse.Instruments as I
import qualified Derive.ScoreT as ScoreT
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT

import qualified Midi.Midi as Midi
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Midi.Patch as Midi.Patch
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_update_ui :: Test
test_update_ui = do
    let f allocs = fmap (Map.toList . UiConfig.unallocations)
            . I.update_ui lookup_backend (map mkalloc allocs)
            . UiTest.mk_allocations
    let midi = "midi/"
    right_equal (f [] []) []
    right_equal (f [] [("i", midi, Just [0])]) []
    right_equal
        (f [("i", midi, (True, False), Just [1])] [("i", midi, Just [0])])
        [ ( "i"
          , (UiTest.midi_allocation midi (UiTest.midi_config [1]))
            { UiConfig.alloc_config = Common.empty_config
                { Common.config_mute = True }
            }
          )
        ]
    let im = [("i", "im/", (False, False), Nothing)]
    right_equal (f im [("i", midi, Nothing)]) $
        map UiTest.mk_allocation [("i", "im/", Nothing)]
    right_equal (f im []) $
        map UiTest.mk_allocation [("i", "im/", Nothing)]

lookup_backend :: InstT.Qualified -> Maybe Inst.Backend
lookup_backend qual
    | q == "midi/" = Just $ Inst.Midi $ Midi.Patch.patch (-2, 2) "name"
    | q == "im/" = Just $ Inst.Im Im.Patch.patch
    | otherwise = Nothing
    where q = InstT.show_qualified qual

mkalloc :: (ScoreT.Instrument, Text, (Bool, Bool), Maybe [Midi.Channel])
    -> I.Allocation
mkalloc (name, qual, (mute, solo), backend) = I.Allocation
    { alloc_name =  name
    , alloc_qualified = InstT.parse_qualified qual
    , alloc_config = I.Config mute solo
    , alloc_backend = case backend of
        Just chans -> I.Midi UiTest.wdev chans
        Nothing -> I.NonMidi
    }

test_parse_allocation :: Test
test_parse_allocation = do
    let f = parse
    let syn = InstT.Qualified "syn" ""
    right_equal (f ">i syn/p -- hi") $
        I.Allocation "i" (InstT.Qualified "syn" "p") I.empty_config I.NonMidi
    let loop1 = Midi.write_device "loop1"
    right_equal (f ">i syn/ loop1 1 2") $
        I.Allocation "i" syn I.empty_config (I.Midi loop1 [0, 1])
    left_like (f ">i syn/ loop1 0") "should be in range"
    left_like (f ">i syn/ loop1") "expecting * nat"
    left_like (f ">i syn/ loop1 x") "expecting * nat"
    right_equal (f ">i syn/ [ms]") $
        I.Allocation "i" syn I.empty_config I.NonMidi
    right_equal (f ">i syn/ [Ms]") $
        I.Allocation "i" syn (I.Config True False) I.NonMidi
    left_like (f ">i syn/ [msq]") "flags must be"

test_allocation_roundtrip :: Test
test_allocation_roundtrip = do
    let syn = InstT.Qualified "syn" "p"
    let loop1 = Midi.write_device "loop1"
    let trip alloc =
            ( Right alloc
            , parse $ Text.unlines $ I.unparse_allocations [(Just alloc, "")]
            )
    uncurry equal $ trip $ I.Allocation "i" syn I.empty_config I.NonMidi
    uncurry equal $ trip $ I.Allocation "i" syn (I.Config False True)
        (I.Midi loop1 [0, 2])
    uncurry equal $ trip $ I.Allocation "i" syn (I.Config True False)
        (I.Midi loop1 [2])

parse :: Text -> Either Text I.Allocation
parse = Parse.parse I.p_allocation
