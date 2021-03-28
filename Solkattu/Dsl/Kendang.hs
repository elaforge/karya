-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE RecordWildCards #-}
-- | Like "Solkattu.Dsl.Mridangam", but for kendang.
module Solkattu.Dsl.Kendang (
    module Solkattu.Dsl.Kendang
    , module Dsl.Solkattu
    , module Solkattu.Dsl.Generic
    , module Solkattu.Dsl.Interactive
) where
import           Prelude hiding ((.))

import           Solkattu.Dsl.Interactive (diff, diffw)
import qualified Solkattu.Dsl.Solkattu as Dsl.Solkattu
import           Solkattu.Dsl.Solkattu (realizeScoreM)
import qualified Solkattu.Format.Terminal as Terminal
import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tala as Tala

import           Global
import           Solkattu.Dsl.Generic


type Sequence = SequenceT Stroke
type Stroke = Realize.Stroke KendangTunggal.Stroke
type Section = Korvai.Section Sequence

korvai :: Tala.Tala -> [Section] -> Korvai.Korvai
korvai tala = Korvai.kendangTunggalKorvai tala KendangTunggal.defaultPatterns

korvai1 :: Tala.Tala -> Section -> Korvai.Korvai
korvai1 tala section = korvai tala [section]

-- | Infer Section types, as init is development, last is ending.
korvaiS :: Tala.Tala -> [Sequence] -> Korvai.Korvai
korvaiS tala = korvai tala • Korvai.inferSections

korvaiS1 :: Tala.Tala -> Sequence -> Korvai.Korvai
korvaiS1 tala sequence = korvaiS tala [sequence]

KendangTunggal.Strokes {..} = KendangTunggal.notes

-- * interactive utilities

realize, realizep :: Korvai.Korvai -> IO ()
realize = realizeK id
realizep = realizeK concrete

realizeK :: (Terminal.Config -> Terminal.Config) -> Korvai.Korvai -> IO ()
realizeK = Dsl.Solkattu._printInstrument Just Korvai.IKendangTunggal

-- * strokes

nakatiku :: Sequence
nakatiku = namedT Solkattu.GPattern "8n" KendangTunggal.nakatiku

pp :: Sequence
pp = hv p
