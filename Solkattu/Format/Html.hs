-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Format korvais as HTML.
module Solkattu.Format.Html (
    indexHtml, writeAll
#ifdef TESTING
    , module Solkattu.Format.Html
#endif
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Time.Calendar as Calendar

import qualified Util.Doc as Doc
import qualified Util.File as File
import qualified Util.Seq as Seq
import qualified Util.Styled as Styled
import qualified Util.Texts as Texts

import qualified Solkattu.Format.Format as Format
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Metadata as Metadata
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tags as Tags
import qualified Solkattu.Tala as Tala

import           Global


-- * interface

-- | Make a summary page with all the korvais.
indexHtml :: (Korvai.Score -> FilePath) -> [Korvai.Score] -> Doc.Html
indexHtml scoreFname scores = Texts.join "\n" $
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
    , "<table id=korvais>"
    , "<tr>" <> mconcat ["<th>" <> c <> "</th>" | c <- columns] <> "</tr>"
    ] ++ map row (Seq.sort_on scoreDate scores) ++
    [ "</table>"
    , "<script>"
    , javascriptIndex
    , "</script>"
    , "</body></html>"
    ]
    where
    row score = mconcat
        [ "<tr>"
        , mconcat ["<td>" <> cell <> "</td>" | cell <- cells score]
        , "</tr>"
        ]
    columns = ["name", "type", "tala", "nadai", "date", "instruments", "source"]
    cells score = Doc.link variableName (txt (scoreFname score))
        : map Doc.html
        [ Text.unwords $ Metadata.scoreTag "type" score
        , commas $ scoreTalas score
        , commas $ Metadata.sectionTag "nadai" score
        , maybe "" showDate $ scoreDate score
        , commas $ Metadata.scoreTag "instrument" score
        , commas $ Metadata.scoreTag "source" score
        ]
        where
        meta = Korvai.scoreMetadata score
        (_, _, variableName) = Korvai._location meta
    commas = Text.intercalate ", "

scoreDate :: Korvai.Score -> Maybe Calendar.Day
scoreDate = \case
    Korvai.Single k -> date k
    Korvai.Tani meta parts -> case Korvai._date meta of
        Just day -> Just day
        Nothing -> Seq.maximum $ mapMaybe date [k | Korvai.K k <- parts]
    where date = Korvai._date . Korvai.korvaiMetadata

javascriptIndex :: Doc.Html
javascriptIndex =
    "const table = document.getElementById('korvais');\n\
    \const headers = table.querySelectorAll('th');\n\
    \const headerTexts = Array.from(headers).map(\n\
    \    function (h) { return h.innerText; });\n\
    \const up = '↑';\n\
    \const down ='↓';\n\
    \const both = '↕︎';\n\
    \[].forEach.call(headers, function(header, index) {\n\
    \    header.addEventListener('click', function() {\n\
    \        sortColumn(index);\n\
    \    });\n\
    \});\n\
    \const tableBody = table.querySelector('tbody');\n\
    \const rows = tableBody.querySelectorAll('tr');\n\
    \const directions = Array.from(headers).map(\n\
    \    function(header) { return 1; });\n\
    \\n\
    \const sortColumn = function(index) {\n\
    \    [].forEach.call(headers, function(header, i) {\n\
    \        var arrow = both;\n\
    \        if (i == index) {\n\
    \            arrow = directions[index] == 1 ? down : up;\n\
    \        } else {\n\
    \            directions[i] = 1;\n\
    \        }\n\
    \        header.innerText = headerTexts[i] + ' ' + arrow;\n\
    \    });\n\
    \    const newRows = Array.from(rows).slice(1);\n\
    \    newRows.sort(function(rowA, rowB) {\n\
    \        const cellA = rowA.querySelectorAll('td')[index].innerText;\n\
    \        const cellB = rowB.querySelectorAll('td')[index].innerText;\n\
    \        if (cellA > cellB)\n\
    \            return 1 * directions[index];\n\
    \        else if (cellA < cellB)\n\
    \            return -1 * directions[index];\n\
    \        else\n\
    \            return 0;\n\
    \    });\n\
    \    directions[index] = -directions[index];\n\
    \    [].forEach.call(rows, function(row) {\n\
    \        tableBody.removeChild(row);\n\
    \    });\n\
    \    tableBody.appendChild(rows[0]);\n\
    \    newRows.forEach(function(newRow) {\n\
    \        tableBody.appendChild(newRow);\n\
    \    });\n\
    \};\n\
    \sortColumn(headerTexts.indexOf('date'));\n"

-- | Write HTML with all the instrument realizations at all abstraction levels.
writeAll :: FilePath -> Korvai.Score -> IO ()
writeAll fname score = File.writeLines fname $ map Doc.un_html $
    render defaultAbstractions score


-- * high level

data Config = Config {
    _abstraction :: !Format.Abstraction
    , _font :: !Font
    -- | Show the ruler on multiples of this line as a reminder.  The ruler is
    -- always shown if it changes.  It should be a multiple of 2 to avoid
    -- getting the second half of a talam in case it's split in half.
    , _rulerEach :: !Int
    } deriving (Show)

-- | HTML output has vertical lines for ruler marks, so they can be rarer.
defaultRulerEach :: Int
defaultRulerEach = 8

data Font = Font { _sizePercent :: Int, _monospace :: Bool }
    deriving (Show)

defaultAbstractions :: [(Text, Format.Abstraction)]
defaultAbstractions =
    [ ("none", mempty)
    , ("sarva", Format.abstract Solkattu.GSarva)
    , ("patterns", Format.defaultAbstraction)
    , ("all", Format.allAbstract)
    ]

defaultAbstraction :: Text
defaultAbstraction = "patterns"

-- | Render all 'Abstraction's, with javascript to switch between them.
render :: [(Text, Format.Abstraction)] -> Korvai.Score -> [Doc.Html]
render abstractions score = htmlPage title (scoreMetadata score) body
    where
    (_, _, title) = Korvai._location (Korvai.scoreMetadata score)
    body :: [Doc.Html]
    body = concatMap htmlInstrument $ Seq.sort_on (order . fst) $
        Format.scoreInstruments score
    order name = (fromMaybe 999 $ List.elemIndex name prio, name)
        where prio = ["konnakol", "mridangam"]
    htmlInstrument (instName, Korvai.GInstrument inst) =
        "<h3>" <> Doc.html instName <> "</h3>"
        : chooseAbstraction abstractions instName
        : concatMap (renderAbstraction instName inst score) abstractions

htmlPage :: Text -> [Doc.Html] -> [Doc.Html] -> [Doc.Html]
htmlPage title meta body = htmlHeader title : meta ++ body ++ [htmlFooter]

renderAbstraction :: Solkattu.Notation stroke => Text
    -> Korvai.Instrument stroke -> Korvai.Score
    -> (Text, Format.Abstraction) -> [Doc.Html]
renderAbstraction instName inst score (aname, abstraction) =
    Doc.tag_attrs "div" attrs Nothing
    : (case score of
        Korvai.Single korvai -> render korvai
        Korvai.Tani _ parts -> concatMap partHtmls parts)
    ++ ["</div>"]
    where
    partHtmls (Korvai.Comment cmt) = ["<h3>" <> Doc.html cmt <> "</h3>"]
    partHtmls (Korvai.K korvai) = render korvai
    render = sectionHtmls inst (config abstraction)
    attrs =
        [ ("class", "realization")
        , ("instrument", instName)
        , ("abstraction", aname)
        ] ++ if aname == defaultAbstraction
        then [("", "")] else [("hidden", "")]
    config abstraction = Config
        { _abstraction = abstraction
        , _font = if instName == "konnakol"
            then konnakolFont else instrumentFont
        , _rulerEach = defaultRulerEach
        }

sectionHtmls :: Solkattu.Notation stroke => Korvai.Instrument stroke
    -> Config -> Korvai.Korvai -> [Doc.Html]
sectionHtmls inst config korvai =
    -- Group rows by fst, which is whether it has a ruler, and put <table>
    -- around each group.  This is because each ruler may have a different
    -- nadai and hence a different number of columns.
    concatMap (scoreTable (_font config) . concat . map snd) $
    Seq.split_before fst $
    concat $ snd $ List.mapAccumL show1 (Nothing, 0) $
    zip3 [1..] (Korvai.genericSections korvai) sectionNotes
    where
    sectionNotes = Format.convertGroups (Korvai.realize inst korvai)
    show1 prevRuler (i, section, notes) = case notes of
        Left err -> (prevRuler, [msgRow $ "ERROR: " <> err])
        Right (notes, warnings) -> (nextRuler,) $
            formatTable tala i section avartanams
            ++ map showWarning warnings
            where
            (nextRuler, avartanams) =
                formatAvartanams config toSpeed prevRuler tala notes
            tala = Korvai.korvaiTala korvai
    showWarning (Realize.Warning _i msg) = msgRow $ "WARNING: " <> msg
    toSpeed = maximum $ 0 : map S.maxSpeed (mapMaybe notesOf sectionNotes)
    notesOf (Right (notes, _)) = Just notes
    notesOf _ = Nothing

msgRow :: Text -> (Bool, [Doc.Html])
msgRow msg =
    (False, ["<tr><td colspan=100>" <> Doc.html msg <> "</td></tr>"])

scoreTable :: Font -> [Doc.Html] -> [Doc.Html]
scoreTable _ [] = [] -- Seq.split_before produces []s
scoreTable font rows = concat
    [ ["\n<p><table style=\"" <> fontStyle
        <> "\" class=konnakol cellpadding=0 cellspacing=0>"]
    , rows
    , ["</table>"]
    ]
    where
    fontStyle = "font-size: " <> Doc.html (showt (_sizePercent font)) <> "%"
        <> if _monospace font then "; font-family: Monaco, monospace" else ""

htmlHeader :: Text -> Doc.Html
htmlHeader title = Texts.join "\n"
    [ "<html><head>"
    , "<meta charset=utf-8>"
    , "<title>" <> Doc.html title <> "</title></head>"
    , "<body>"
    , ""
    , "<style type=\"text/css\">"
    , allCss
    , "</style>"
    , ""
    , "<script>"
    , javascript
    , "</script>"
    , ""
    ]

javascript :: Doc.Html
javascript =
    "function showAbstraction(instrument, abstraction) {\n\
    \    var tables = document.getElementsByClassName('realization');\n\
    \    for (var i = 0; i < tables.length; i++) {\n\
    \        var attrs = tables[i].attributes;\n\
    \        if (attrs.instrument.value == instrument) {\n\
    \            tables[i].hidden = attrs.abstraction.value != abstraction;\n\
    \        }\n\
    \    }\n\
    \}\n"

chooseAbstraction :: [(Text, Format.Abstraction)] -> Text -> Doc.Html
chooseAbstraction abstractions instrument =
    "\n<p> " <> mconcatMap ((<>"\n") . radio . fst) abstractions
    where
    -- <label> makes the text clickable too.
    radio val = Doc.tag "label" $
        Doc.tag_attrs "input" (attrs val) (Just (Doc.html val))
    attrs val =
        [ ("type", "radio")
        , ("onchange", "showAbstraction('" <> instrument <> "', this.value)")
        , ("name", "abstraction-" <> instrument)
        , ("value", val)
        ] ++ if val == defaultAbstraction then [("checked", "")] else []

htmlFooter :: Doc.Html
htmlFooter = "</body></html>\n"

allCss :: Doc.Html
allCss = Texts.join "\n" [tableCss, Doc.Html typeCss]

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
    \.finalLine { border-bottom: 1px solid gray; }\n"

-- | Unused, because I don't really like the tooltips.
_metadataCss :: Doc.Html
_metadataCss =
    ".tooltip {\n\
    \    position: relative;\n\
    \    display: inline-block;\n\
    \}\n\
    \.tooltip .tooltiptext {\n\
    \    visibility: hidden;\n\
    \    width: 120px;\n\
    \    background-color: black;\n\
    \    color: #fff;\n\
    \    text-align: center;\n\
    \    padding: 5px 0;\n\
    \    border-radius: 6px;\n\
    \    position: absolute;\n\
    \    z-index: 1;\n\
    \    left: 105%;\n\
    \    bottom: 100%;\n\
    \}\n\
    \.tooltip:hover .tooltiptext {\n\
    \    visibility: visible;\n\
    \}\n"

typeCss :: Text
typeCss = Text.unlines $ concat
    [ styles gtype (cssColor start) (cssColor end)
    | ((start, end), gtype) <- Seq.key_on typeColors Solkattu.groupTypes
    ]
    where
    styles gtype start end =
        [ "." <> groupStyle gtype Start
            <> " { background: linear-gradient(to right, "
                <> start <> ", " <> end <> ", " <> end <> ") }"
        , "." <> groupStyle gtype In <> " { background-color: " <> end <> " }"
        , "." <> groupStyle gtype End
            <> " { background: linear-gradient(to right, "
                <> end <> ", " <> end <> ", white) }"
        ]
    cssColor c = "rgb(" <> Text.intercalate ", " (map to8 [r, g, b]) <> ")"
        where (r, g, b) = Styled.rgbComponents c
    to8 = showt . round . (*255)

data Pos = Start | In | End
    deriving (Eq, Show)

-- | Get the class name for a GroupType at the Start, In, or End of the
-- highlight.
groupStyle :: Solkattu.GroupType -> Pos -> Text
groupStyle gtype pos = "g" <> showt gtype <> showt pos

typeColors :: Solkattu.GroupType -> (Styled.RgbColor, Styled.RgbColor)
typeColors = \case
    Solkattu.GGroup -> groupc
    Solkattu.GReductionT -> groupc
    Solkattu.GFiller -> both $ gray 0.9
    Solkattu.GPattern -> patternc
    Solkattu.GExplicitPattern -> patternc
    Solkattu.GSarva -> both $ rgb 0.7 0.85 0.7
    -- This shouldn't be here, so make it red.
    Solkattu.GCheckDuration {} -> both $ rgb 0.75 0 0
    where
    groupc = (rgb 0.5 0.8 0.5, gray 0.8)
    patternc = (rgb 0.65 0.65 0.8, rgb 0.8 0.8 0.95)
    both n = (n, n)
    rgb = Styled.rgbColor
    gray n = rgb n n n

-- * render

-- | TODO unused, later I should filter out only the interesting ones and
-- cram them in per-section inline
_sectionMetadata :: Korvai.Section sollu -> Doc.Html
_sectionMetadata section = Texts.join "; " $ map showTag (Map.toAscList tags)
    where
    tags = Tags.untags $ Korvai.sectionTags section
    showTag (k, []) = Doc.html k
    showTag (k, vs) = Doc.html k <> ": "
        <> Texts.join ", " (map (htmlTag k) vs)

scoreMetadata :: Korvai.Score -> [Doc.Html]
scoreMetadata score = Seq.map_init (<>"<br>") $ concat $
    [ ["Tala: " <> Doc.html (Text.intercalate ", " (scoreTalas score))]
    , ["Date: " <> Doc.html (showDate date) | Just date <- [scoreDate score]]
    , [showTag ("Eddupu", map pretty eddupus) | not (null eddupus)]
    , map showTag (Map.toAscList (Map.delete "tala" tags))
    ]
    where
    eddupus = Seq.unique $ filter (/="0") $
        Map.findWithDefault [] Tags.eddupu sectionTags
    sectionTags = Tags.untags $ mconcat $
        Metadata.sectionTags score
    tags = Tags.untags $ Korvai._tags meta
    meta = Korvai.scoreMetadata score
    showTag (k, []) = Doc.html k
    showTag (k, vs) = Doc.html k <> ": "
        <> Texts.join ", " (map (htmlTag k) vs)

showDate :: Calendar.Day -> Text
showDate = txt . Calendar.showGregorian

scoreTalas :: Korvai.Score -> [Text]
scoreTalas = Seq.unique . map (Tala.tala_name . Korvai.korvaiTala)
    . Korvai.scoreKorvais

formatAvartanams :: Solkattu.Notation stroke => Config -> S.Speed
    -> Format.PrevRuler -> Tala.Tala
    -> [S.Flat Solkattu.Meta (Realize.Note stroke)]
    -> (Format.PrevRuler, [(Maybe Format.Ruler, Line)])
formatAvartanams config toSpeed prevRuler tala =
    second concat
    . Format.pairWithRuler (_rulerEach config) prevRuler tala 1
    . Format.formatFinalAvartanam (isRest . _html) . map (:[])
    . Format.breakAvartanams
    . concatMap makeSymbols
    . Format.makeGroupsAbstract (_abstraction config)
    . Format.normalizeSpeed toSpeed tala

type Line = [(S.State, Symbol)]

data Symbol = Symbol {
    _html :: !Doc.Html
    , _isSustain :: !Bool
    , _style :: !(Maybe Text)
    } deriving (Eq, Show)

instance Pretty Symbol where
    pretty (Symbol html _ _) = pretty html

-- | Flatten the groups into linear [Symbol].
makeSymbols :: Solkattu.Notation stroke => Format.NormalizedFlat stroke
    -> [(S.State, Symbol)]
makeSymbols = go
    where
    go (S.FNote _ (state, note_)) =
        (:[]) $ (state,) $ Symbol
            { _html = noteHtml state note
            , _isSustain = case note of
                S.Sustain {} -> True
                _ -> False
            , _style = Nothing
            }
        where note = normalizeSarva note_
    go (S.FGroup _ group children) = modify (concatMap go children)
        where
        modify = case Solkattu._type group of
            -- Highlight only when non-abstract.
            Solkattu.GSarva -> case children of
                S.FNote _ (_, S.Attack (Realize.Abstract {})) : _ -> id
                _ -> setStyle Solkattu.GSarva
            gtype -> setStyle gtype
    setStyle gtype = Seq.map_last (second (set End))
        . Seq.map_head_tail (second (set Start)) (second (set In))
        where set pos sym = sym { _style = Just (groupStyle gtype pos) }
    noteHtml state = \case
        S.Sustain (Realize.Abstract meta) -> case Solkattu._type meta of
            -- TODO this is actually pretty ugly
            Solkattu.GSarva -> "<hr style=\"border: 4px dotted\">"
            _ -> "<hr noshade>"
        S.Sustain a -> notation state a
        S.Attack a -> notation state a
        S.Rest -> Doc.html "_"
    -- Because sarva is <hr> all the way through, don't separate the attack
    -- from sustain.
    normalizeSarva (S.Attack n@(Realize.Abstract meta))
        | Solkattu._type meta == Solkattu.GSarva = S.Sustain n
    normalizeSarva n = n
    notation state = bold . Solkattu.notationHtml
        where bold = if Format.onAkshara state then Doc.tag "b" else id

formatTable :: Tala.Tala -> Int -> Korvai.Section ()
    -> [(Maybe Format.Ruler, [(S.State, Symbol)])] -> [(Bool, [Doc.Html])]
formatTable tala _sectionIndex section rows = map row $ zipFirstFinal rows
    where
    td (tags, body) = Doc.tag_attrs "td" tags (Just body)
    row (isFirst, (mbRuler, cells), isFinal) = case mbRuler of
        Just ruler ->
            ( True
            , "<tr><td></td>" <> formatRuler ruler <> "</tr>" : notes
            )
        Nothing -> (False, notes)
        where
        notes = ("<tr><td>" <> sectionTags isFirst <> "</td>") : cellTds
            ++ ["</tr>\n"]
        cellTds = map td . Format.mapSnd spellRests
            . map (mkCell isFinal) . List.groupBy merge . zip [0..] $ cells
    sectionTags isFirst
        | not isFirst || Text.null tags = ""
        | otherwise = Doc.tag_attrs "span" [("style", "font-size:50%")] $
            Just $ Doc.html tags
        where tags = Format.showTags (Korvai.sectionTags section)
    -- Merge together the sustains after an attack.  They will likely have an
    -- <hr> in them, which will expand to the full colspan width.
    merge (_, (_, sym1)) (_, (state2, sym2)) =
        _isSustain sym1 && _isSustain sym2 && not (Format.onAkshara state2)
        -- Split sustains on aksharas.  Otherwise, the colspan prevents the
        -- vertical lines that mark them.
    mkCell :: Bool -> [(Int, (S.State, Symbol))]
        -> ([(Text, Text)], (Int, Doc.Html))
    mkCell _ [] = ([], (0, "")) -- List.groupBy shouldn't return empty groups
    mkCell isFinal syms@((index, (state, sym)) : _) = (tags, (index, _html sym))
        where
        tags = concat
            [ [("class", Text.unwords classes) | not (null classes)]
            , [("colspan", showt cells) | cells > 1]
            ]
            where cells = length syms
        classes = concat
            [ if
                | Format.onAnga angas state -> ["onAnga"]
                | Format.onAkshara state -> ["onAkshara"]
                | otherwise -> []
            , maybe [] (:[]) (_style sym)
            , ["finalLine" | isFinal]
            ]
    angas = Format.angaSet tala

-- showh :: Show a => a -> Doc.Html
-- showh = Doc.html . showt

zipFirstFinal :: [a] -> [(Bool, a, Bool)]
zipFirstFinal =
    map (\(prev, x, next) -> (Maybe.isNothing prev, x, Maybe.isNothing next))
    . Seq.zip_neighbors

formatRuler :: Format.Ruler -> Doc.Html
formatRuler =
    mconcatMap mark . map (second Maybe.isNothing) . Seq.zip_next
    . concatMap akshara
    where
    -- If the final one is th, then it omits the underline, which looks a bit
    -- nicer.
    mark (m, isFinal) = Doc.tag (if isFinal then "td" else "th") (Doc.html m)
    akshara :: (Text, Int) -> [Text]
    akshara (n, spaces) = n : replicate (spaces-1) ""

-- | This is the HTML version of 'Terminal.spellRests'.
--
-- It uses 3 levels of rests: space, single, and double.
spellRests :: [(Int, Doc.Html)] -> [Doc.Html]
spellRests = spell
    where
    spell [] = []
    spell ((col, sym) : syms)
        | not (isRest sym) = sym : spell syms
        | Just post <- checkRests col syms 4 =
            (double : replicate 3 "") ++ spell post
        | Just post <- checkRests col syms 2 =
            single : "" : spell post
        | otherwise = "" : spell syms
    checkRests col syms n
        | col `mod` n == 0 && length rests == n-1 = Just $ drop (n-1) syms
        | otherwise = Nothing
        where
        rests = take (n-1) $ takeWhile (isRest . snd) syms
    double = Doc.html (Text.singleton Realize.doubleRest)
    single = "_"

isRest :: Doc.Html -> Bool
isRest = (=="_") . Text.strip . Doc.un_html


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
