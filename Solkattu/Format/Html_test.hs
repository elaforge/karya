-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Solkattu.Format.Html_test where
import qualified Data.Text.IO as Text.IO

import qualified Util.Doc as Doc
import qualified Solkattu.Dsl.Solkattu as G
import qualified Solkattu.Format.Format as Format
import qualified Solkattu.Format.Html as Html
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Tala as Tala

import Global
import Util.Test


manual_test = do
    let p = Text.IO.putStrLn
    -- p $ render Format.defaultAbstraction $ korvai $ G.sarvaM_ 4

    p $ render mempty $ korvai $ mconcat $
        G.p6 : replicate 5 G.__
    p $ render Format.defaultAbstraction $ korvai $ mconcat $
        G.p6 : replicate 5 G.__


test_spellRests = do
    let f = mconcat . map toSpace . map Doc.un_html
            . Html.spellRests . zip [0..]
    equal (f ["_", "_", "_", "_", "_", "_", "_"]) "‗   _  "
    equal (f ["x", "_", "_", "_", "_", "_", "_"]) "x _ _  "

toSpace :: Text -> Text
toSpace "" = " "
toSpace s = s

korvai :: Korvai.Sequence -> Korvai.Korvai
korvai = Korvai.korvai Tala.adi_tala (G.makeMridangam [])
    . Korvai.inferSections . (:[])

render :: Format.Abstraction -> Korvai.Korvai -> Text
render abstraction =
    Doc.un_html . mconcat . Html.sectionHtmls Korvai.mridangam config
    where
    config = Html.Config
        { _abstraction = abstraction
        , _font = Html.instrumentFont
        , _rulerEach = 4
        }

defaultStrokeMap :: Korvai.StrokeMaps
defaultStrokeMap = mempty
    { Korvai.smapMridangam = Realize.strokeMap Mridangam.defaultPatterns [] }
    where Mridangam.Strokes {..} = Mridangam.notes
