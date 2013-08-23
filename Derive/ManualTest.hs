-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Skeleton for one-off tests, presumably using the output of
-- LDebug.dump_block.
module Derive.ManualTest where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Local.Instrument.Kontakt as Kontakt


run :: IO ()
run = do
    let res = DeriveTest.derive_dump synths dump (UiTest.bid "thani-s")
        (pevents, msgs, logs) = DeriveTest.perform_dump synths dump res
    prettyp $ DeriveTest.extract id res
    prettyp pevents
    prettyp msgs
    -- prettyp logs

synths = Kontakt.synth_descs


dump :: UiTest.Dump
dump = undefined
