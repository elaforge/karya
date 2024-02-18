-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Dsl.Mridangam_test where
import qualified Data.Text as Text

import qualified Solkattu.Dsl.Mridangam as G
import           Solkattu.Dsl.Mridangam (__, k, o, su, t, (&))
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S

import           Global
import           Util.Test


test_merge :: Test
test_merge = do
    let f a = Text.unwords . map pretty <$> realize a
    equal (f $ o & k) $ Right "K"
    equal (f $ k <> o&k) $ Right "k K"
    equal (f $ (k<>k<>k) & (o<>__<>o)) $ Right "K k K"
    equal (f $ (k <> su (k<>k)) & (o <> su (o<>__))) $ Right "K _ K k"
    throws (f $ k & k) "requires thoppi & valantalai"
    -- Pad with rests.
    equal (f $ o & (k<>k)) $ Right "K k"
    -- Merge unequal speeds.
    equal (f $ (k <> su (k<>t) <> k) & (o<>o<>o)) $ Right "K _ K t K _"

realize :: G.Sequence -> Either Text [Realize.Note Mridangam.Stroke]
realize seq = fmap S.flattenedNotes $ fmap fst $ head $
    Korvai.realize Korvai.IMridangam (G.korvaiS1 G.adi seq)
