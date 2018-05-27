-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Convert realized 'S.Flat' output to text or HTML for display.
module Solkattu.Format.Format (
    -- * text
    format
    -- * html
    , formatHtml, htmlPage
    , Font(..)

    -- * testing
    , StartEnd(..), formatLines, Symbol(..), annotateGroups

-- TODO can't because CPP doesn't like \s
-- #ifdef TESTING
--     , module Solkattu.Format.Format
-- #endif
) where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.MultiSet as MultiSet
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Solkattu.Format.Terminal as Terminal
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tala as Tala

import Global


type Line = [(S.State, Symbol)]
type Ruler = [(Text, Int)]

-- | Format the notes according to the tala.
--
-- The line breaking for rulers is a bit weird in that if the line is broken,
-- I only emit the first part of the ruler.  Otherwise I'd have to have
-- a multiple line ruler too, which might be too much clutter.  I'll have to
-- see how it works out in practice.
format :: Solkattu.Notation stroke => Int -> Maybe Int -> Int -> Tala.Tala
    -> [S.Flat g (Realize.Note stroke)] -> Text
format rulerEach overrideStrokeWidth width tala notes =
    Text.stripEnd $ Terminal.fix $ Text.intercalate "\n" $
        map formatAvartanam $
        pairWithRuler rulerEach tala strokeWidth avartanamLines
    where
    formatAvartanam = Text.intercalate "\n" . map formatRulerLine
    formatRulerLine (ruler, line) = Text.intercalate "\n" $
        maybe [] ((:[]) . textRuler strokeWidth) ruler
        ++ [formatLine (map snd line)]

    avartanamLines :: [[Line]] -- [avartanam] [[line]] [[[sym]]]
    (avartanamLines, strokeWidth) = case overrideStrokeWidth of
        Just n -> (formatLines n width tala notes, n)
        Nothing -> case formatLines 1 width tala notes of
            ([line] : _)
                | sum (map (textLength . _text . snd) line) <= width `div` 2 ->
                    (formatLines 2 width tala notes, 2)
            result -> (result, 1)
    formatLine :: [Symbol] -> Text
    formatLine = Text.stripEnd . mconcat . map formatSymbol . thinRests

pairWithRuler :: Int -> Tala.Tala -> Int -> [[Line]] -> [[(Maybe Ruler, Line)]]
pairWithRuler rulerEach tala strokeWidth =
    snd . List.mapAccumL (List.mapAccumL strip) (Nothing, 0)
        . map (map addRuler)
    where
    addRuler line = (inferRuler tala strokeWidth (map fst line), line)
    -- Strip rulers when they are unchanged.  "Changed" is by structure, not
    -- mark text, so a wrapped ruler with the same structure will also be
    -- suppressed.
    strip (prev, lineNumber) (ruler, line) =
        ( (Just ruler, lineNumber+1)
        , (if wanted then Just ruler else Nothing, line)
        )
        where
        wanted = lineNumber `mod` rulerEach == 0
            || Just (map snd ruler) /= (map snd <$> prev)

textRuler :: Int -> Ruler -> Text
textRuler strokeWidth = mconcat . snd . List.mapAccumL render 0
    where
    render debt (mark, spaces) =
        ( max 0 (-append) -- debt is how many spaces I'm behind
        , mark <> Text.replicate append " "
        )
        where
        append = spaces * strokeWidth - Text.length mark - debt

-- | Drop single character rests on odd columns, to make the output look less
-- cluttered.
thinRests :: [Symbol] -> [Symbol]
thinRests = snd . List.mapAccumL thin 0
    where
    thin column sym
        | Text.all (=='_') (_text sym) =
            let (column2, stroke2) = Text.mapAccumL clear column (_text sym)
            in (column2, sym { _text = stroke2 })
        | otherwise = (column + textLength (_text sym), sym)
    clear column _ = (column+1, if even column then '_' else ' ')

-- | If the final non-rest is at sam, drop trailing rests, and don't wrap it
-- onto the next line.
formatFinalAvartanam :: [[[(a, Symbol)]]] -> [[[(a, Symbol)]]]
formatFinalAvartanam avartanams = case reverse avartanams of
    [final : rests] : penultimate : prevs
        | not (isRest (snd final)) && all (isRest . snd) rests ->
            reverse $ (Seq.map_last (++[final]) penultimate) : prevs
        | otherwise -> avartanams
    _ -> avartanams
    where
    -- This should be (== Space Rest), but I have to showStroke first to break
    -- lines.
    isRest = (=="_") . Text.strip . _text

-- | Break into [avartanam], where avartanam = [line].
formatLines :: Solkattu.Notation stroke => Int -> Int -> Tala.Tala
    -> [S.Flat g (Realize.Note stroke)] -> [[[(S.State, Symbol)]]]
formatLines strokeWidth width tala =
    formatFinalAvartanam . map (breakLine width) . breakAvartanams
        . map combine . Seq.zip_prev . map makeSymbol . normalizeSpeed tala
    where
    combine (prev, (state, sym)) = (state, text (Text.drop overlap) sym)
        where
        overlap = maybe 0 (subtract strokeWidth . textLength . _text . snd)
            prev
    makeSymbol (startEnds, (state, note)) = (state,) $ make $ case note of
        S.Attack a ->
            justifyLeft strokeWidth (Solkattu.extension a) (Solkattu.notation a)
        S.Sustain a ->
            Text.replicate strokeWidth (Text.singleton (Solkattu.extension a))
        S.Rest -> justifyLeft strokeWidth ' ' "_"
        where
        make text = Symbol
            { _text = text
            , _emphasize = shouldEmphasize tala angas state
            , _bounds = startEnds
            }
    angas = angaSet tala

normalizeSpeed :: Tala.Tala -> [S.Flat g (Realize.Note a)]
    -> [([StartEnd], (S.State, S.Stroke (Realize.Note a)))]
normalizeSpeed tala =
    annotateGroups . S.normalizeSpeed tala . S.filterFlat (not . isAlignment)
    where
    isAlignment (Realize.Alignment {}) = True
    isAlignment _ = False

-- | Put StartEnd on the strokes to mark group boundaries.  This discards all
-- other group data.
annotateGroups :: [S.Flat g a] -> [([StartEnd], a)]
annotateGroups =
    Maybe.catMaybes . snd . List.mapAccumL go (mempty, 0) . zip [0..]
        . concatMap flatten
    where
    go (groups, starts) (i, Left count) =
        ((MultiSet.insert (i + count) groups, starts + 1), Nothing)
    go (groups, starts) (i, Right note) =
        ( (groups, 0)
        , Just (replicate starts Start ++ replicate ends End, note)
        )
        where ends = MultiSet.lookup i groups
    flatten (S.FGroup _ _ children) = Left (length flat) : flat
        where flat = concatMap flatten children
    flatten (S.FNote _ note) = [Right note]

-- TODO these don't distinguish between different groups, but I'll probably
-- want to do that once I have fancier formatting.
data StartEnd = Start | End deriving (Eq, Show)

instance Pretty StartEnd where pretty = showt

onAnga :: Set Tala.Akshara -> S.State -> Bool
onAnga angas state =
    S.stateMatra state == 0 && Set.member (S.stateAkshara state) angas

onAkshara :: S.State -> Bool
onAkshara state = S.stateMatra state == 0

-- | Chapus are generally fast, so only emphasize the angas.  Other talas are
-- slower, and without such a strong beat, so emphasize every akshara.
shouldEmphasize :: Tala.Tala -> Set Tala.Akshara -> S.State -> Bool
shouldEmphasize tala angas state
    | isChapu = onAnga angas state
    | otherwise = onAkshara state
    where
    isChapu = case Tala._angas tala of
        Tala.Wave _ : _ -> True
        Tala.Clap _ : _ -> True
        _ -> False

angaSet :: Tala.Tala -> Set Tala.Akshara
angaSet = Set.fromList . scanl (+) 0 . Tala.tala_angas

breakAvartanams :: [(S.State, a)] -> [[(S.State, a)]]
breakAvartanams = dropWhile null . Seq.split_before (isSam . fst)
    where isSam state = S.stateMatra state == 0 && S.stateAkshara state == 0

-- | If the text goes over the width, break at the middle akshara, or the
-- last one before the width if there isn't a middle.
breakLine :: Int -> [(S.State, Symbol)] -> [[(S.State, Symbol)]]
breakLine maxWidth notes
    | width <= maxWidth = [notes]
    | even aksharas = breakAt (aksharas `div` 2) notes
    | otherwise = breakBefore maxWidth notes
    where
    width = sum $ map (textLength . _text . snd) notes
    aksharas = Seq.count (atAkshara . fst) notes
    breakAt akshara =
        pairToList . break ((==akshara) . S.stateAkshara . fst)
    pairToList (a, b) = [a, b]

-- | Yet another word-breaking algorithm.  I must have 3 or 4 of these by now.
breakBefore :: Int -> [(S.State, Symbol)] -> [[(S.State, Symbol)]]
breakBefore maxWidth = go . dropWhile null . Seq.split_before (atAkshara . fst)
    where
    go aksharas =
        case breakFst (>maxWidth) (zip (runningWidth aksharas) aksharas) of
            ([], []) -> []
            (pre, []) -> [concat pre]
            ([], post:posts) -> post : go posts
            (pre, post) -> concat pre : go post
    -- drop 1 so it's the width at the end of each section.
    runningWidth =
        drop 1 . scanl (+) 0 . map (sum . map (textLength . _text . snd))

breakFst :: (key -> Bool) -> [(key, a)] -> ([a], [a])
breakFst f = (map snd *** map snd) . break (f . fst)

-- | Rather than generating the ruler purely from the Tala, I use the States
-- to figure out the mark spacing.  Otherwise I wouldn't know where nadai
-- changes occur.  But it does mean I can't generate ruler if I run out of
-- strokes, which is a bit annoying for incomplete korvais or ones with eddupu.
inferRuler :: Tala.Tala -> Int -> [S.State] -> Ruler
inferRuler tala strokeWidth =
    (++ [("|", 0)])
    . map (second length)
    . concat . snd . List.mapAccumL insertNadai 0
    . concatMap insertDots
    . zip (Tala.tala_labels tala)
    . dropWhile null
    . Seq.split_before atAkshara
    where
    insertNadai :: S.Nadai -> (Text, [S.State])
        -> (S.Nadai, [(Text, [S.State])])
    insertNadai prevNadai (label, states) =
        ( maybe prevNadai fst (Seq.last groups)
        , case groups of
            (nadai, states) : rest | nadai == prevNadai ->
                (label, states) : map (first nadaiChange) rest
            _ -> (label, []) : map (first nadaiChange) groups
        )
        where
        groups = Seq.keyed_group_adjacent nadaiOf states
        nadaiOf = S._nadai . S.stateTempo
    -- Marker for a nadai change.  It has a colon to separate it from the ruler
    -- mark, in case it coincides with one.
    nadaiChange n = ":" <> showt n
    insertDots (label, states)
        | (spaces * strokeWidth > 8) && spaces `mod` 2 == 0 =
            [(label, pre) , (".", post)]
        | otherwise = [(label, states)]
        where
        (pre, post) = splitAt (spaces `div` 2) states
        spaces = length states

atAkshara :: S.State -> Bool
atAkshara = (==0) . S.stateMatra

justifyLeft :: Int -> Char -> Text -> Text
justifyLeft n c text
    | len >= n = text
    | otherwise = text <> Text.replicate (n - len) (Text.singleton c)
    where len = textLength text

-- ** formatting

data Symbol = Symbol {
    _text :: !Text
    , _emphasize :: !Bool
    , _bounds :: ![StartEnd]
    } deriving (Eq, Show)

instance Pretty Symbol where
    pretty (Symbol text emphasize bounds) =
        text <> (if emphasize then "(b)" else "")
            <> pretty bounds

text :: (Text -> Text) -> Symbol -> Symbol
text f sym = sym { _text = f (_text sym) }

formatSymbol :: Symbol -> Text
formatSymbol (Symbol text emph bounds) = mconcat
    [ Text.replicate (Seq.count (==End) bounds) Terminal.bgDefault
    , Text.replicate (Seq.count (==Start) bounds)
        (Terminal.setBg Terminal.Normal Terminal.White)
    , (if emph then emphasize  else id) text
    ]

emphasize :: Text -> Text
emphasize word
    -- A bold _ looks the same as a non-bold one, so put a bar to make it
    -- more obvious.
    | word == "_ " = emphasize "_|"
    | otherwise = Terminal.boldOn <> pre <> Terminal.boldOff <> post
    where (pre, post) = Text.break (==' ') word

textLength :: Text -> Int
textLength = sum . map len . untxt
    where
    -- Combining characters don't contribute to the width.  I'm sure it's way
    -- more complicated than this, but for the moment this seems to work.
    len c
        | Char.isMark c = 0
        | otherwise = 1

-- * format html

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
    ruler = maybe [] (concatMap akshara . inferRuler tala 1 . map fst)
        (Seq.head avartanams)
    akshara (n, spaces) = n : replicate (spaces-1) ""
    -- I don't thin rests for HTML, it seems to look ok with all explicit rests.
    -- thin = map (Doc.Html . _text) . thinRests . map (symbol . Doc.un_html)
    avartanams = breakAvartanams $
        map (\(startEnd, (state, note)) -> (state, (startEnd, note))) $
        normalizeSpeed tala notes

-- symbol :: Text -> Symbol
-- symbol text = Symbol text False []

formatTable :: Solkattu.Notation stroke => Tala.Tala -> Font
    -> [Doc.Html] -> [[(S.State, ([StartEnd], S.Stroke (Realize.Note stroke)))]]
    -> Doc.Html
formatTable tala font header rows = mconcatMap (<>"\n") $
    [ "<p> <table style=\"" <> fontStyle
        <> "\" class=konnakol cellpadding=0 cellspacing=0>"
    , "<tr>" <> mconcatMap th header <> "</tr>\n"
    ] ++ map row (snd $ mapAccumL2 addGroups 0 rows)
    ++ ["</table>"]
    where
    fontStyle = "font-size: " <> Doc.html (showt (_sizePercent font)) <> "%"
        <> if _monospace font then "; font-family: Monaco, monospace" else ""
    th col = Doc.tag_attrs "th" [] (Just col)
    row cells = TextUtil.join ("\n" :: Doc.Html)
        [ "<tr>"
        , TextUtil.join "\n" $ map td (List.groupBy groupSustains cells)
        , "</tr>"
        , ""
        ]
    addGroups prevDepth (state, (startEnds, a)) =
        (depth, (state,
            ((depth, Start `elem` startEnds, End `elem` startEnds), a)))
        where
        depth = (prevDepth+) $ sum $ flip map startEnds $ \n -> case n of
            Start -> 1
            End -> -1
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

    td [] = "" -- not reached, List.groupBy shouldn't return empty groups
    td ((state, ((depth :: Int, start, end), note)) : ns) =
        Doc.tag_attrs "td" tags $ Just $ case note of
            S.Attack (Realize.Space Solkattu.Sarva) -> sarva
            S.Sustain (Realize.Space Solkattu.Sarva) -> sarva
            S.Sustain (Realize.Pattern {}) -> "<hr noshade>"
            S.Sustain a -> notation a
            S.Attack a -> notation a
            S.Rest -> Doc.html "_"
        where
        notation = bold . Solkattu.notationHtml
            where bold = if onAkshara state then Doc.tag "b" else id
        sarva = "<hr style=\"border: 4px dotted\">"
        tags = concat
            [ [("class", Text.unwords classes) | not (null classes)]
            , [("colspan", showt (length ns + 1)) | not (null ns)]
            ]
        classes = concat
            [ if
                | onAnga angas state -> ["onAnga"]
                | onAkshara state -> ["onAkshara"]
                | otherwise -> []
            , if
                | start -> ["startG"]
                | end -> ["endG"]
                | depth > 0 -> ["inG"]
                | otherwise -> []
            ]
    hasLine = onAkshara
    angas = angaSet tala

mapAccumL2 :: (state -> a -> (state, b)) -> state -> [[a]] -> (state, [[b]])
mapAccumL2 f = List.mapAccumL (List.mapAccumL f)
