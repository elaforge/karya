-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Elaforge.Instrument.Kontakt.KendangBali_test where
import qualified Data.List as List

import qualified Util.Lists as Lists
import qualified Cmd.Instrument.KendangBali as K
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Ui.UiTest as UiTest
import qualified User.Elaforge.Instrument.Kontakt.KendangBali as KendangBali
import qualified User.Elaforge.Instrument.Kontakt.KontaktTest as KontaktTest

import           Global
import           Util.Test


test_kendang :: Test
test_kendang = do
    let run extract inst notes = DeriveTest.extract extract $
            derive [(">" <> inst <> instTitle, mknotes notes)]
        eInstrument e = (DeriveTest.eInstrument e, DeriveTest.eAttributes e)
        mknotes ns = [(t, 0, n) | (t, n) <- zip (Lists.range_ 0 1) ns]
    equal (run eInstrument "kw" ["PL", "P", "o"])
        ([("kw", "+plak"), ("kw", "+pak"), ("kw", "+tut")], [])
    equal (run eInstrument "k"
            ["PL", "k", "P", "t", "T", "u", "U"])
        ( [ ("kl", "+plak"), ("kw", "+pak"), ("kl", "+pak"),
            ("kw", "+pang"), ("kl", "+pang"), ("kw", "+tut"), ("kl", "+tut")
          ]
        , []
        )

    -- Soft attributes.
    let eDyn e = (DeriveTest.eAttributes e, Score.initialDynamic e)
    equal (run eDyn "kw" [".", "..", "-", "+"])
        ([("+ka+soft", 0.4), ("+ka", 1), ("+de+soft", 0.4), ("+de", 1)], [])

    -- Both strokes.
    let runBoth stroke = run eInstrument "k" [stroke]
    equal (runBoth "PLPL") ([("kw", "+plak"), ("kl", "+plak")], [])
    equal (runBoth "+Ø") ([("kw", "+de"), ("kl", "+left+tut")], [])
    equal (runBoth "+ø") ([("kw", "+de"), ("kl", "+left+soft+tut")], [])

test_pasang_calls :: Test
test_pasang_calls = do
    -- every pasang call dispatch to a valid tunggal call
    let tunggal = [K.toCall note | (_, note, _) <- K.tunggalTable]
    forM_ K.pasangCalls $ \(_, _, pstroke) ->
        forM_ (K.notesOf pstroke) $ \note -> do
            let sym = K.toCall note
            equal (Just sym) (List.find (==sym) tunggal)

test_resolve :: Test
test_resolve = do
    equal KendangBali.resolveErrors []

derive :: [UiTest.TrackSpec] -> Derive.Result
derive = KontaktTest.derive allocs ""
    where allocs = KendangBali.allocations "k" "test"

instTitle :: Text
instTitle = " | wadon = kw | lanang = kl"
