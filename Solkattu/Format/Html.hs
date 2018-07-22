-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Format korvais as HTML.
module Solkattu.Format.Html (indexHtml, writeAll) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time.Calendar as Calendar

import qualified Util.Doc as Doc
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Solkattu.Format.Format as Format
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Metadata as Metadata
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tags as Tags
import qualified Solkattu.Tala as Tala

import Global


type Error = Text

-- * interface

-- | Make a summary page with all the korvais.
indexHtml :: (Korvai.Korvai -> FilePath) -> [Korvai.Korvai] -> Doc.Html
indexHtml korvaiFname korvais = TextUtil.join "\n" $
    [ "<html> <head>"
    , "<meta charset=utf-8>"
    , "<title>solkattu db</title>"
    , "<style>"
    , "table {"
    , "    border-collapse: collapse;"
    , "}"
    , "tr:nth-child(even) {"
    , "    background-color: #f2f2f2;"
    , "}"
    , "th, td {"
    , "    padding: 4px;"
    , "    border-left: 1px solid black;"
    , "}"
    , "</style>"
    , "</head> <body>"
    , "<table>"
    , "<tr>" <> mconcat ["<th>" <> c <> "</th>" | c <- columns] <> "</tr>"
    ] ++ map row korvais ++
    [ "</table>"
    , "</body></html>"
    ]
    where
    row korvai = mconcat
        [ "<tr>"
        , mconcat ["<td>" <> cell <> "</td>" | cell <- cells korvai]
        , "</tr>"
        ]
    columns = ["", "type", "tala", "nadai", "date", "instruments", "source"]
    cells korvai = Doc.link variableName (txt (korvaiFname korvai))
        : map Doc.html
        [ Text.unwords $ Metadata.korvaiTag "type" korvai
        , Tala._name $ Korvai.korvaiTala korvai
        , Text.intercalate ", " $ Metadata.sectionTag "nadai" korvai
        , maybe "" (txt . Calendar.showGregorian) $ Korvai._date meta
        , Text.intercalate ", " $ Metadata.korvaiTag "instrument" korvai
        , Text.intercalate ", " $ Metadata.korvaiTag "source" korvai
        ]
        where
        meta = Korvai.korvaiMetadata korvai
        (_, _, variableName) = Korvai._location meta


-- | Write HTML with all the instrument realizations.
writeAll :: FilePath -> Bool -> Korvai.Korvai -> IO ()
writeAll fname realizePatterns korvai =
    Text.IO.writeFile fname $ Doc.un_html $ render realizePatterns korvai


-- * high level

render :: Bool -> Korvai.Korvai -> Doc.Html
render realizePatterns korvai = htmlPage title (korvaiMetadata korvai) body
    where
    (_, _, title) = Korvai._location (Korvai.korvaiMetadata korvai)
    body = mconcat $ mapMaybe htmlInstrument $ Seq.sort_on (order . fst) $
        Korvai.korvaiInstruments korvai
    htmlInstrument (name, Korvai.GInstrument inst) =
        Just $ "<h3>" <> Doc.html name <> "</h3>\n"
            <> TextUtil.join "\n\n" sectionHtmls
        where
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

htmlPage :: Text -> Doc.Html -> Doc.Html -> Doc.Html
htmlPage title meta body = mconcat
    [ htmlHeader title
    , meta
    , body
    , htmlFooter
    ]

htmlHeader :: Text -> Doc.Html
htmlHeader title = TextUtil.join "\n"
    [ "<html><head>"
    , "<meta charset=utf-8>"
    , "<title>" <> Doc.html title <> "</title></head>"
    , "<body>"
    , ""
    , "<style type=\"text/css\">"
    , tableCss
    , "</style>"
    , ""
    ]

htmlFooter :: Doc.Html
htmlFooter = "</body></html>\n"

tableCss :: Doc.Html
tableCss =
    "table.konnakol {\n\
    \   table-layout: fixed;\n\
    \   width: 100%;\n\
    \}\n\
    \table.konnakol th {\n\
    \   text-align: left;\n\
    \   border-bottom: 1px solid;\n\
    \}\n\
    \.onAnga { border-left: 3px double }\n\
    \.onAkshara { border-left: 1px solid }\n\
    \.inG { background-color: lightgray }\n\
    \.startG { background:\
        \ linear-gradient(to right, lightgreen, lightgray, lightgray) }\n\
    \.endG { background:\
        \ linear-gradient(to right, lightgray, lightgray, white) }"

data Font = Font { _sizePercent :: Int, _monospace :: Bool } deriving (Show)

formatHtml :: Solkattu.Notation stroke => Tala.Tala -> Font
    -> [S.Flat g (Realize.Note stroke)] -> Doc.Html
formatHtml tala font notes =
    formatTable tala font (map Doc.html ruler) avartanams
    where
    ruler = maybe [] (concatMap akshara . Format.inferRuler tala 1 . map fst)
        (Seq.head avartanams)
    akshara :: (Text, Int) -> [Text]
    akshara (n, spaces) = n : replicate (spaces-1) ""
    avartanams =
        Format.breakAvartanams $
        map (\(startEnd, (state, note)) -> (state, (startEnd, note))) $
        Format.normalizeSpeed tala notes

formatTable :: Solkattu.Notation stroke => Tala.Tala -> Font -> [Doc.Html]
    -> [[(S.State, ([Format.StartEnd], S.Stroke (Realize.Note stroke)))]]
    -> Doc.Html
formatTable tala font header rows = mconcatMap (<>"\n") $ concat
    [ [ "<p> <table style=\"" <> fontStyle
        <> "\" class=konnakol cellpadding=0 cellspacing=0>"
      , "<tr>" <> mconcatMap th header <> "</tr>\n"
      ]
    , map row (snd $ mapAccumL2 addGroups 0 rows)
    , ["</table>"]
    ]
    where
    fontStyle = "font-size: " <> Doc.html (showt (_sizePercent font)) <> "%"
        <> if _monospace font then "; font-family: Monaco, monospace" else ""
    th col = Doc.tag_attrs "th" [] (Just col)
    td (tags, body) = Doc.tag_attrs "td" tags (Just body)
    row cells = TextUtil.join ("\n" :: Doc.Html)
        [ "<tr>"
        , TextUtil.join "\n" $ map td . Format.mapSnd spellRests . map mkCell $
            List.groupBy groupSustains cells
        , "</tr>"
        , ""
        ]
    addGroups prevDepth (state, (startEnds, a)) =
        -- TODO this is out of control
        ( depth
        , ( state
          , ( ( depth
              , Format.Start `elem` startEnds
              , Format.End `elem` startEnds
              )
            , Format.normalizeRest a
            )
          )
        )
        where
        depth = (prevDepth+) $ sum $ flip map startEnds $ \n -> case n of
            Format.Start -> 1
            Format.End -> -1
    groupSustains (_, (_, note1)) (state2, (_, note2)) =
        not (hasLine state2) && merge note1 note2
        where
        -- For Pattern, the first cell gets the p# notation, the rest get <hr>.
        -- For Sarva, there's no notation, so they all get the <hr>.
        merge (S.Attack (Realize.Space Solkattu.Sarva))
            (S.Sustain (Realize.Space Solkattu.Sarva)) = True
        merge (S.Sustain (Realize.Space Solkattu.Sarva))
            (S.Sustain (Realize.Space Solkattu.Sarva)) = True
        merge (S.Sustain (Realize.Pattern {})) (S.Sustain (Realize.Pattern {}))
            = True
        merge _ _ = False

    -- not reached, List.groupBy shouldn't return empty groups
    mkCell [] = ([], "")
    mkCell ((state, ((depth :: Int, start, end), note)) : ns) =
        (,) tags $ case note of
            S.Attack (Realize.Space Solkattu.Sarva) -> sarva
            S.Sustain (Realize.Space Solkattu.Sarva) -> sarva
            S.Sustain (Realize.Pattern {}) -> "<hr noshade>"
            S.Sustain a -> notation a
            S.Attack a -> notation a
            S.Rest -> Doc.html "_"
        where
        notation = bold . Solkattu.notationHtml
            where bold = if Format.onAkshara state then Doc.tag "b" else id
        sarva = "<hr style=\"border: 4px dotted\">"
        tags = concat
            [ [("class", Text.unwords classes) | not (null classes)]
            , [("colspan", showt (length ns + 1)) | not (null ns)]
            ]
        classes = concat
            [ if
                | Format.onAnga angas state -> ["onAnga"]
                | Format.onAkshara state -> ["onAkshara"]
                | otherwise -> []
            , if
                | start -> ["startG"]
                | end -> ["endG"]
                | depth > 0 -> ["inG"]
                | otherwise -> []
            ]
    hasLine = Format.onAkshara
    angas = Format.angaSet tala

-- | This is the HTML version of 'Format.spellRests'.
spellRests :: [Doc.Html] -> [Doc.Html]
spellRests = map set . zip [0..] . Seq.zip_neighbors
    where
    set (col, (prev, sym, next))
        | not (isRest sym) = sym
        | even col && maybe False isRest next =
            Doc.html (Text.singleton Realize.doubleRest)
        | odd col && maybe False isRest prev = ""
        | otherwise = sym

isRest :: Doc.Html -> Bool
isRest = (=="_") . Text.strip . Doc.un_html

mapAccumL2 :: (state -> a -> (state, b)) -> state -> [[a]] -> (state, [[b]])
mapAccumL2 f = List.mapAccumL (List.mapAccumL f)


-- * implementation

konnakolFont, instrumentFont :: Font
konnakolFont = Font
    { _sizePercent = 75
    , _monospace = False
    }
instrumentFont = Font
    { _sizePercent = 125
    , _monospace = True
    }

renderSection :: Solkattu.Notation stroke => Tala.Tala -> Font
    -> Korvai.Section x
    -> Either Error ([S.Flat g (Realize.Note stroke)], Error)
    -> Doc.Html
renderSection _ _ _ (Left err) = "<p> ERROR: " <> Doc.html err
renderSection tala font section (Right (notes, warn)) = mconcat
    [ sectionMetadata section
    , formatHtml tala font notes
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
