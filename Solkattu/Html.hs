-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Format korvais as HTML.
module Solkattu.Html (writeHtmlKorvai) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time.Calendar as Calendar

import qualified Util.Doc as Doc
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Metadata as Metadata
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as Sequence
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tags as Tags
import qualified Solkattu.Tala as Tala

import Global


type Error = Text

-- | Write HTML with all the instrument realizations.
writeHtmlKorvai :: FilePath -> Bool -> Korvai.Korvai -> IO ()
writeHtmlKorvai fname realizePatterns korvai = do
    Text.IO.writeFile fname $ Doc.un_html $ render realizePatterns korvai
    putStrLn $ "wrote " <> fname

render :: Bool -> Korvai.Korvai -> Doc.Html
render realizePatterns korvai =
    Realize.htmlPage title (korvaiMetadata korvai) body
    where
    (_, _, title) = Korvai._location (Korvai.korvaiMetadata korvai)
    body = mconcat $ mapMaybe htmlInstrument $ Seq.sort_on (order . fst) $
        Map.toList Korvai.instruments
    htmlInstrument (name, Korvai.GInstrument inst)
        | Realize.isInstrumentEmpty strokeMap = Nothing
        | otherwise = Just $ "<h3>" <> Doc.html name <> "</h3>\n"
            <> TextUtil.join "\n\n" sectionHtmls
        where
        strokeMap = Korvai.instFromStrokes inst (Korvai.korvaiStrokeMaps korvai)
        sectionHtmls :: [Doc.Html]
        sectionHtmls =
            zipWith (renderSection (Korvai.korvaiTala korvai) (font name))
                (Korvai.genericSections korvai)
                (Korvai.realize inst realizePatterns korvai)
    order name = (fromMaybe 999 $ List.elemIndex name prio, name)
        where prio = ["konnakol", "mridangam"]
    font name
        | name == "konnakol" = konnakolFont
        | otherwise = instrumentFont

konnakolFont, instrumentFont :: Realize.Font
konnakolFont = Realize.Font
    { _sizePercent = 75
    , _monospace = False
    }
instrumentFont = Realize.Font
    { _sizePercent = 125
    , _monospace = True
    }

renderSection :: Solkattu.Notation stroke => Tala.Tala -> Realize.Font
    -> Korvai.Section x
    -> Either Error ([Sequence.Flat g (Realize.Note stroke)], Error)
    -> Doc.Html
renderSection _ _ _ (Left err) = "<p> ERROR: " <> Doc.html err
renderSection tala font section (Right (notes, warn)) = mconcat
    [ sectionMetadata section
    , Realize.formatHtml tala font notes
    , if Text.null warn then "" else "<br> WARNING: " <> Doc.html warn
    ]

-- TODO this actually looks pretty ugly, but I'll worry about that later
sectionMetadata :: Korvai.Section sollu -> Doc.Html
sectionMetadata section = TextUtil.join "; " $ map showTag (Map.toAscList tags)
    where
    tags = Tags.untags $ Korvai.sectionTags section
    showTag (k, []) = Doc.html k
    showTag (k, vs) = Doc.html k <> ": "
        <> TextUtil.join ", " (map (htmlTag k) vs)

korvaiMetadata :: Korvai.Korvai -> Doc.Html
korvaiMetadata korvai = TextUtil.join "<br>\n" $ concat $
    [ ["Tala: " <> Doc.html (Tala._name (Korvai.korvaiTala korvai))]
    , ["Date: " <> Doc.html (showDate date) | Just date <- [Korvai._date meta]]
    , [showTag ("Eddupu", map pretty eddupu) | not (null eddupu)]
    , map showTag (Map.toAscList (Map.delete "tala" tags))
    ]
    where
    meta = Korvai.korvaiMetadata korvai
    eddupu = Seq.unique $ filter (/="0") $
        Map.findWithDefault [] Tags.eddupu sectionTags
    sectionTags = Tags.untags $ mconcat $ Metadata.sectionTags korvai
    tags = Tags.untags $ Korvai._tags meta
    showTag (k, []) = Doc.html k
    showTag (k, vs) = Doc.html k <> ": "
        <> TextUtil.join ", " (map (htmlTag k) vs)
    showDate = txt . Calendar.showGregorian

htmlTag :: Text -> Text -> Doc.Html
htmlTag k v
    | k == Tags.recording = case Metadata.parseRecording v of
        Nothing -> Doc.html $ "can't parse: " <> v
        Just (url, range) -> link $ url <> case range of
            Nothing -> ""
            -- TODO assuming youtube
            Just (start, _) -> "#t=" <> Metadata.showTime start
    | otherwise = Doc.html v
    where
    link s = Doc.link s s
