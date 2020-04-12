-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Elaforge.Instrument.Kontakt.KendangBali_test where
import qualified Data.List as List

import qualified Util.Seq as Seq
import qualified Cmd.Instrument.KendangBali as K
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Ui.UiTest as UiTest
import qualified User.Elaforge.Instrument.Kontakt.KendangBali as KendangBali
import qualified User.Elaforge.Instrument.Kontakt.KontaktTest as KontaktTest

import           Global
import           Util.Test


test_kendang = do
    let run extract inst notes = DeriveTest.extract extract $
            derive [(">" <> inst <> inst_title, mknotes notes)]
        e_instrument e = (DeriveTest.e_instrument e, DeriveTest.e_attributes e)
        mknotes ns = [(t, 0, n) | (t, n) <- zip (Seq.range_ 0 1) ns]
    equal (run e_instrument "k" ["PL", "P", "o"])
        ([("k", "+plak"), ("k", "+pak"), ("k", "+tut")], [])
    equal (run e_instrument "pasang"
            ["PL", "k", "P", "t", "T", "u", "U"])
        ([("l", "+plak"), ("w", "+pak"), ("l", "+pak"),
            ("w", "+pang"), ("l", "+pang"), ("w", "+tut"), ("l", "+tut")], [])

    -- Soft attributes.
    let e_dyn e = (DeriveTest.e_attributes e, Score.initial_dynamic e)
    equal (run e_dyn "k" [".", "..", "-", "+"])
        ([("+ka+soft", 0.4), ("+ka", 1), ("+de+soft", 0.4), ("+de", 1)], [])

    -- Both strokes.
    let run_both stroke = run e_instrument "pasang" [stroke]
    equal (run_both "PLPL") ([("w", "+plak"), ("l", "+plak")], [])
    equal (run_both "+Ø") ([("w", "+de"), ("l", "+left+tut")], [])
    equal (run_both "+ø") ([("w", "+de"), ("l", "+left+soft+tut")], [])

test_pasang_calls = do
    -- every pasang call dispatch to a valid tunggal call
    let tunggal = [K.to_call note | (_, note, _) <- K.tunggal_table]
    forM_ K.pasang_calls $ \(_, _, pstroke) ->
        forM_ (K.notes_of pstroke) $ \note -> do
            let sym = K.to_call note
            equal (Just sym) (List.find (==sym) tunggal)

test_resolve = do
    equal KendangBali.resolve_errors []

derive :: [UiTest.TrackSpec] -> Derive.Result
derive = KontaktTest.derive allocs ""
    where
    allocs =
        [ ("k", "kontakt/kendang-bali")
        , ("pasang", "kontakt/kendang-bali-pasang")
        , ("w", "kontakt/kendang-bali")
        , ("l", "kontakt/kendang-bali")
        ]

inst_title :: Text
inst_title = " | wadon = w | lanang = l"
