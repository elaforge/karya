-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Convert realized 'S.Flat' output to text for the terminal.
--
-- TODO this should probably be called Terminal, but then what should I call
-- the existing Terminal module?
module Solkattu.Format.Format (
    printInstrument
    , printKonnakol

    -- * shared with Format.Html
    , StartEnd(..)
    , breakAvartanams, normalizeSpeed, inferRuler
    , onAkshara, onAnga, angaSet

#ifdef TESTING
    , module Solkattu.Format.Format
#endif
) where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.File as File
import qualified Util.MultiSet as MultiSet
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Solkattu.Format.Terminal as Terminal
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tags as Tags
import qualified Solkattu.Tala as Tala

import Global


type Error = Text

-- | Show the ruler on multiples of this line as a reminder.  The ruler is
-- always shown if it changes.  It should be a multiple of 2 to avoid
-- getting the second half of a talam in case it's split in half.
rulerEach :: Int
rulerEach = 4

-- * write

-- | Write all instrument realizations.
writeAll :: FilePath -> Bool -> Korvai.Korvai -> IO ()
writeAll fname realizePatterns korvai =
    File.writeLines fname $ List.intersperse "" $ concatMap write1 $
    Korvai.korvaiInstruments korvai
    where
    write1 (name, Korvai.GInstrument inst) =
        name <> ":" : formatInstrument inst realizePatterns korvai

-- * format

printInstrument :: Solkattu.Notation stroke => Korvai.Instrument stroke -> Bool
    -> Korvai.Korvai -> IO ()
printInstrument instrument realizePatterns =
    mapM_ Text.IO.putStrLn . formatInstrument instrument realizePatterns

printKonnakol :: Int -> Bool -> Korvai.Korvai -> IO ()
printKonnakol width realizePatterns =
    mapM_ Text.IO.putStrLn . formatKonnakol width realizePatterns

formatInstrument :: Solkattu.Notation stroke => Korvai.Instrument stroke -> Bool
    -> Korvai.Korvai -> [Text]
formatInstrument instrument realizePatterns korvai =
    formatResults defaultWidth Nothing korvai $ zip (korvaiTags korvai) $
        Korvai.realize instrument realizePatterns korvai

defaultWidth :: Int
defaultWidth = 78

formatKonnakol :: Int -> Bool -> Korvai.Korvai -> [Text]
formatKonnakol width realizePatterns korvai =
    formatResults width (Just 4) korvai $ zip (korvaiTags korvai) $
        Korvai.realize Korvai.konnakol realizePatterns korvai

korvaiTags :: Korvai.Korvai -> [Tags.Tags]
korvaiTags = map Korvai.sectionTags . Korvai.genericSections

formatResults :: Solkattu.Notation stroke => Int -> Maybe Int -> Korvai.Korvai
    -> [(Tags.Tags, Either Error ([S.Flat g (Realize.Note stroke)], Error))]
    -> [Text]
formatResults width overrideStrokeWidth korvai =
    snd . List.mapAccumL show1 (Nothing, 0) . zip [1..]
    where
    show1 _ (section, (_, Left err)) =
        ((Nothing, 0), sectionFmt section mempty $ "ERROR:\n" <> err)
    show1 prevRuler (section, (tags, Right (notes, warning))) =
        ( nextRuler
        , TextUtil.joinWith "\n" (sectionFmt section tags out) warning
        )
        where
        (nextRuler, out) = format rulerEach prevRuler overrideStrokeWidth
            width (Korvai.korvaiTala korvai) notes
    sectionFmt section tags = Text.intercalate "\n"
        . Seq.map_last (<> showTags tags)
        . mapHT (sectionNumber section <>) (Text.replicate leader " " <>)
        . Text.lines
    sectionNumber section = Text.justifyLeft leader ' ' (showt section <> ":")
    leader = 4
    mapHT f g (x:xs) = f x : map g xs
    mapHT _ _ [] = []

showTags :: Tags.Tags -> Text
showTags tags = case Map.lookup Tags.times (Tags.untags tags) of
    Just [n] -> "   x" <> n
    _ -> ""


-- * implementation

-- | Keep state about the last ruler across calls to 'format', so I can
-- suppress unneeded ones.  (prevRuler, lineNumber)
type PrevRuler = (Maybe Ruler, Int)

type Line = [(S.State, Symbol)]
type Ruler = [(Text, Int)]

-- | Format the notes according to the tala.
--
-- The line breaking for rulers is a bit weird in that if the line is broken,
-- I only emit the first part of the ruler.  Otherwise I'd have to have
-- a multiple line ruler too, which might be too much clutter.  I'll have to
-- see how it works out in practice.
format :: Solkattu.Notation stroke => Int -> PrevRuler -> Maybe Int -> Int
    -> Tala.Tala -> [S.Flat g (Realize.Note stroke)] -> (PrevRuler, Text)
format rulerEach prevRuler overrideStrokeWidth width tala notes =
    second (Text.stripEnd . Terminal.fix . Text.intercalate "\n"
            . map formatAvartanam) $
        pairWithRuler rulerEach prevRuler tala strokeWidth avartanamLines
    where
    formatAvartanam = Text.intercalate "\n" . map formatRulerLine
    formatRulerLine (ruler, line) = Text.intercalate "\n" $
        maybe [] ((:[]) . formatRuler strokeWidth) ruler
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

pairWithRuler :: Int -> PrevRuler -> Tala.Tala -> Int -> [[Line]]
    -> (PrevRuler, [[(Maybe Ruler, Line)]])
pairWithRuler rulerEach prevRuler tala strokeWidth =
    List.mapAccumL (List.mapAccumL strip) prevRuler . map (map addRuler)
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

formatRuler :: Int -> Ruler -> Text
formatRuler strokeWidth =
    mconcat . (bg:) . (++[Terminal.bgDefault]) . snd . List.mapAccumL render 0
    where
    -- Make rulers distinct.  TODO This is buggy because it interrupts group
    -- highlights, but I need to switch to Util.Styled to get fix that.
    bg = Terminal.setBg Terminal.Bright Terminal.White
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
breakFst f = bimap (map snd) (map snd) . break (f . fst)

-- | Rather than generating the ruler purely from the Tala, I use the States
-- to figure out the mark spacing.  Otherwise I wouldn't know where nadai
-- changes occur.  But it does mean I can't generate ruler if I run out of
-- strokes, which is a bit annoying for incomplete korvais or ones with eddupu.
inferRuler :: Tala.Tala -> Int -> [S.State] -> Ruler
inferRuler tala strokeWidth =
    (++ [("|", 0)])
    . merge
    . map (second length)
    . concat . snd . List.mapAccumL insertNadai 0
    . concatMap insertDots
    . zip (Tala.tala_labels tala)
    . dropWhile null
    . Seq.split_before atAkshara
    where
    -- Merge 0 dur marks with the next mark.  HTML output puts one mark per
    -- matra, so it can't have 0 dur marks.
    merge ((n1, 0) : (n2, spaces) : xs) = merge ((n1<>n2, spaces) : xs)
    merge ((n, spaces) : xs) = (n, spaces) : merge xs
    merge xs = xs
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
