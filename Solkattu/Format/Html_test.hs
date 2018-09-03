-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Solkattu.Format.Html_test where
import qualified Data.Text.IO as Text.IO

import qualified Util.Doc as Doc
import qualified Solkattu.Format.Format as Format
import qualified Solkattu.Format.Html as Html
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.SolkattuGlobal as G
import qualified Solkattu.Tala as Tala

import Global
import Util.Test


-- manual test
show_format_sarva = do
    Text.IO.putStrLn $ render abstraction $ korvai $
        G.sarvaM_ 4

format :: Korvai.Sequence -> Text
format = Doc.un_html . Html.render abstraction . korvai

korvai :: Korvai.Sequence -> Korvai.Korvai
korvai = Korvai.korvaiInferSections Tala.adi_tala defaultStrokeMap . (:[])

render :: Format.Abstraction -> Korvai.Korvai -> Text
render abstraction =
    Doc.un_html . mconcat . Html.sectionHtmls Korvai.mridangam config
    where
    config = Html.Config
        { _abstraction = abstraction
        , _font = Html.instrumentFont
        }

abstraction :: Format.Abstraction
abstraction = mempty -- Format.defaultAbstraction

defaultStrokeMap :: Korvai.StrokeMaps
defaultStrokeMap = mempty
    { Korvai.smapMridangam = Realize.strokeMap Mridangam.defaultPatterns [] }
    where Mridangam.Strokes {..} = Mridangam.notes
