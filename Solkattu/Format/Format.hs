-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Convert realized 'S.Flat' output to text for the terminal.
--
-- TODO this should probably be called Terminal, but then what should I call
-- the existing Terminal module?
module Solkattu.Format.Format (
    Abstraction(..)
    , writeAll
    , printInstrument, printKonnakol

    -- * shared with Format.Html
    , StartEnd(..)
    , breakAvartanams, normalizeSpeed, annotateGroups, inferRuler
    , onAkshara, onAnga, angaSet
    , normalizeRest, mapSnd

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
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Solkattu.Format.Terminal as Terminal
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tags as Tags
import qualified Solkattu.Tala as Tala

import Global


type Error = Text

data Config = Config {
    -- | Show the ruler on multiples of this line as a reminder.  The ruler is
    -- always shown if it changes.  It should be a multiple of 2 to avoid
    -- getting the second half of a talam in case it's split in half.
    _rulerEach :: !Int
    , _terminalWidth :: !Int
    -- | Normally 'format' tries to figure out a with for each stroke according
    -- to what will fit on the screen.  But it assumes notation is always at
    -- most one character per time unit.  This hardcodes the width for e.g.
    -- konnakol, where a sollu like `thom` can be 4 characters wide.
    , _overrideStrokeWidth :: !(Maybe Int)
    , _abstraction :: !Abstraction
    } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
    { _rulerEach = 4
    , _terminalWidth = 78
    , _overrideStrokeWidth = Nothing
    , _abstraction = Patterns
    }

konnakolConfig :: Config
konnakolConfig = Config
    { _rulerEach = 4
    , _terminalWidth = 100
    , _overrideStrokeWidth = Just 3
    , _abstraction = Patterns
    }

-- | Control what is rendered as strokes, and what is rendered as abstract
-- groups with durations.  This gets succesively more abstract.
data Abstraction = None | Patterns | Groups
    deriving (Eq, Ord, Show)

-- * write

-- | Write all instrument realizations.
writeAll :: FilePath -> Abstraction -> Korvai.Korvai -> IO ()
writeAll fname abstraction korvai =
    File.writeLines fname $ List.intersperse "" $ concatMap write1 $
    Korvai.korvaiInstruments korvai
    where
    write1 (name, Korvai.GInstrument inst) =
        name <> ":" : formatInstrument config inst korvai
        where
        config = (if name == "konnakol" then konnakolConfig else defaultConfig)
            { _abstraction = abstraction }

-- * format

printInstrument :: Solkattu.Notation stroke => Korvai.Instrument stroke
    -> Abstraction -> Korvai.Korvai -> IO ()
printInstrument instrument abstraction =
    mapM_ Text.IO.putStrLn
    . formatInstrument (defaultConfig { _abstraction = abstraction }) instrument

printKonnakol :: Int -> Abstraction -> Korvai.Korvai -> IO ()
printKonnakol width abstraction =
    mapM_ Text.IO.putStrLn . formatInstrument config Korvai.konnakol
    where
    config = konnakolConfig
        { _terminalWidth = width
        , _abstraction = abstraction
        }

formatInstrument :: Solkattu.Notation stroke => Config
    -> Korvai.Instrument stroke -> Korvai.Korvai -> [Text]
formatInstrument config instrument korvai =
    formatResults config korvai $ zip (korvaiTags korvai) $ convertGroups $
        Korvai.realize instrument (_abstraction config < Patterns) korvai

korvaiTags :: Korvai.Korvai -> [Tags.Tags]
korvaiTags = map Korvai.sectionTags . Korvai.genericSections

-- | Format-level Group.  This has just the group data which is needed to
-- format.
type Group = Text

type Flat stroke = S.Flat Group (Realize.Note stroke)

-- | Reduce 'Realize.Group's to local 'Group's.
convertGroups :: [Either Korvai.Error ([Korvai.Flat stroke], Korvai.Error)]
    -> [Either Korvai.Error ([Flat stroke], Korvai.Error)]
convertGroups = map (fmap (first mapGroups))

mapGroups :: [S.Flat (Realize.Group stroke) a] -> [S.Flat Group a]
mapGroups = S.mapGroupFlat (fromMaybe "" . Realize._name)

formatResults :: Solkattu.Notation stroke => Config -> Korvai.Korvai
    -> [(Tags.Tags, Either Error ([Flat stroke], Error))]
    -> [Text]
formatResults config korvai =
    snd . List.mapAccumL show1 (Nothing, 0) . zip [1..]
    where
    show1 _ (section, (_, Left err)) =
        ((Nothing, 0), sectionFmt section mempty $ "ERROR:\n" <> err)
    show1 prevRuler (section, (tags, Right (notes, warning))) =
        ( nextRuler
        , TextUtil.joinWith "\n" (sectionFmt section tags out) warning
        )
        where
        (nextRuler, out) =
            format config prevRuler (Korvai.korvaiTala korvai) notes
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
format :: Solkattu.Notation stroke => Config -> PrevRuler
    -> Tala.Tala -> [Flat stroke] -> (PrevRuler, Text)
format config prevRuler tala notes =
    second (Text.stripEnd . Terminal.fix . Text.intercalate "\n"
            . map formatAvartanam) $
        pairWithRuler (_rulerEach config) prevRuler tala strokeWidth
            avartanamLines
    where
    formatAvartanam = Text.intercalate "\n" . map formatRulerLine
    formatRulerLine (ruler, line) = Text.intercalate "\n" $
        maybe [] ((:[]) . formatRuler strokeWidth) ruler
        ++ [formatLine (map snd line)]

    avartanamLines :: [[Line]] -- [avartanam] [[line]] [[[sym]]]
    (avartanamLines, strokeWidth) = case _overrideStrokeWidth config of
        Just n -> (fmt n width tala notes, n)
        Nothing -> case fmt 1 width tala notes of
            [line] : _
                | sum (map (symLength . snd) line) <= width `div` 2 ->
                    (fmt 2 width tala notes, 2)
            result -> (result, 1)
        where fmt = formatLines (_abstraction config >= Groups)
    formatLine :: [Symbol] -> Text
    formatLine = Text.stripEnd . mconcat . map formatSymbol
    width = _terminalWidth config

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

-- | Replace two rests starting on an even note, with a Realize.doubleRest.
-- This is an elementary form of rhythmic spelling.
--
-- But if strokeWidth=1, then replace replace odd _ with ' ', to avoid clutter.
spellRests :: Int -> [Symbol] -> [Symbol]
spellRests strokeWidth
    | strokeWidth == 1 = map thin . zip [0..]
    | otherwise = map set . zip [0..] . Seq.zip_neighbors
    where
    thin (col, sym)
        | isRest sym && odd col = sym { _text = " " }
        | otherwise = sym
    set (col, (prev, sym, next))
        | not (isRest sym) = sym
        | even col && maybe False isRest next = sym
            { _text = justifyLeft (symLength sym) ' ' double }
        | odd col && maybe False isRest prev = sym
            { _text = Text.replicate (symLength sym) " " }
        | otherwise = sym
    double = Text.singleton Realize.doubleRest

-- | This assumes the function doesn't change the length of the list!
mapSnd :: ([a] -> [b]) -> [(x, a)] -> [(x, b)]
mapSnd f xas = zip xs (f as)
    where (xs, as) = unzip xas

-- | If the final non-rest is at sam, drop trailing rests, and don't wrap it
-- onto the next line.
formatFinalAvartanam :: [[[(a, Symbol)]]] -> [[[(a, Symbol)]]]
formatFinalAvartanam avartanams = case reverse avartanams of
    [final : rests] : penultimate : prevs
        | not (isRest (snd final)) && all (isRest . snd) rests ->
            reverse $ (Seq.map_last (++[final]) penultimate) : prevs
        | otherwise -> avartanams
    _ -> avartanams

-- This should be (== Space Rest), but I have to makeSymbol first to break
-- lines.
isRest :: Symbol -> Bool
isRest = (=="_") . Text.strip . _text

-- | Break into [avartanam], where avartanam = [line].
formatLines :: Solkattu.Notation stroke => Bool -> Int -> Int -> Tala.Tala
    -> [Flat stroke] -> [[[(S.State, Symbol)]]]
formatLines abstractGroups strokeWidth width tala =
    map (map (mapSnd (spellRests strokeWidth)))
        . formatFinalAvartanam . map (breakLine width) . breakAvartanams
        . map combine . Seq.zip_prev
        . concatMap (makeSymbol strokeWidth tala angas)
        . (if abstractGroups then makeGroupsAbstract2 else id)
        . normalizeSpeed tala
    where
    combine (prev, (state, sym)) = (state, text (Text.drop overlap) sym)
        where overlap = maybe 0 (subtract strokeWidth . symLength . snd) prev
    angas = angaSet tala

makeSymbol :: Solkattu.Notation stroke => Int -> Tala.Tala -> Set Tala.Akshara
    -> NormalizedFlat stroke -> [(S.State, Symbol)]
makeSymbol strokeWidth tala angas = go
    where
    go (S.FNote _ (state, note)) =
        (:[]) $ (state,) $ make state $ case normalizeRest note of
            S.Attack a -> justifyLeft strokeWidth (Solkattu.extension a)
                (Solkattu.notation a)
            S.Sustain a -> Text.replicate strokeWidth
                (Text.singleton (Solkattu.extension a))
            S.Rest -> justifyLeft strokeWidth ' ' "_"
    go (S.FGroup _ _group children) =
        Seq.map_last (second (set EndHighlight)) $
        headTail (second (set StartHightlight)) (second (set Highlight))
            (concatMap go children)
    set h sym = sym { _highlight = Just h }
    make state text = Symbol
        { _text = text
        , _emphasize = shouldEmphasize tala angas state
        , _highlight = Nothing
        }

-- | Rests are special in that S.normalizeSpeed can produce them.  Normalize
-- them to force them to all be treated the same way.
normalizeRest :: S.Stroke (Realize.Note a) -> S.Stroke (Realize.Note a)
normalizeRest (S.Attack (Realize.Space Solkattu.Rest)) = S.Rest
normalizeRest (S.Sustain (Realize.Space Solkattu.Rest)) = S.Rest
normalizeRest a = a

type NormalizedFlat stroke =
    S.Flat Group (S.State, S.Stroke (Realize.Note stroke))

makeGroupsAbstract2 :: [NormalizedFlat stroke] -> [NormalizedFlat stroke]
makeGroupsAbstract2 = concatMap combine
    where
    combine (S.FGroup tempo _group children) =
        headTail (make S.Attack) (make S.Sustain) flattened
        where
        flattened  = S.tempoNotes children
        make c (tempo, (state, _)) = S.FNote tempo (state, c note)
        note = Realize.Pattern (Solkattu.PatternM (Just name) 1)
        fmatra = S.normalizeFmatra tempo (fromIntegral (length flattened))
        -- TODO I should preserve the group name
        name = Pretty.fraction True fmatra
    combine n = [n]

headTail :: (a -> b) -> (a -> b) -> [a] -> [b]
headTail f g (x : xs) = f x : map g xs
headTail _ _ [] = []

-- | Because a group can be a non-integral number of matras, it may not be
-- possible to represent with a 'Solkattu.Pattern', but I can
-- post-'normalizeSpeed', since its whole job is to multiply out to an integral
-- number of matras.
-- TODO unused
makeGroupsAbstract :: [([StartEnd], (S.State, S.Stroke (Realize.Note stroke)))]
    -> [([StartEnd], (S.State, S.Stroke (Realize.Note stroke)))]
makeGroupsAbstract =
    concatMap combine . collectPairs ((==0) . fst) . groupDepth
    where
    combine (outside, inside) = map snd outside ++ case map snd inside of
        [] -> []
        (startEnd, (state, _)) : rest -> (startEnd, (state, S.Attack note))
            : map (fmap (fmap (const (S.Sustain note)))) rest
            where
            note = Realize.Pattern (Solkattu.PatternM (Just name) 1)
            -- dur = S.matraDuration tempo * fromIntegral (length rest + 1)
            -- fmatra = S.durationFmatra (S._nadai tempo) dur
            fmatra = S.normalizeFmatra tempo (fromIntegral (length rest + 1))
            tempo = S.stateTempo state
            -- TODO I should preserve the group name
            name = Pretty.fraction True fmatra

normalizeSpeed :: Tala.Tala -> [Flat stroke] -> [NormalizedFlat stroke]
normalizeSpeed tala = S.normalizeSpeed tala . S.filterFlat (not . isAlignment)
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

-- annotateGroups2 :: [S.Flat Group a] -> [([Group], a, [Group])]
-- annotateGroups2 = concatMap $ \case
--     S.FNote _ note -> [([], note, [])]
--     S.FGroup _ g children -> firstLast
--         (\(starts, n, ends) -> (g:starts, n, ends))
--         (\(starts, n, ends) -> (starts, n, g:ends))
--         (annotateGroups2 children)

firstLast :: (a -> a) -> (a -> a) -> [a] -> [a]
firstLast start end [x] = [start (end x)]
firstLast start end xs = Seq.first_last start end xs

groupDepth :: [([StartEnd], a)] -> [(Int, ([StartEnd], a))]
groupDepth = snd . List.mapAccumL count 0
    where
    count n (startEnds, a) = (n + starts - ends, (n + starts, (startEnds, a)))
        where
        (starts, ends) = bimap length length $
            List.partition (==Start) startEnds

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

-- | Split on sam.
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
    width = sum $ map (symLength . snd) notes
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
    runningWidth = drop 1 . scanl (+) 0 . map (sum . map (symLength . snd))

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

-- | This is like 'Text.justifyLeft', except it understands the actual length
-- of unicode characters, courtesty of 'textLength'.
justifyLeft :: Int -> Char -> Text -> Text
justifyLeft n c text
    | len >= n = text
    | otherwise = text <> Text.replicate (n - len) (Text.singleton c)
    where len = textLength text

-- ** formatting

data Symbol = Symbol {
    _text :: !Text
    , _emphasize :: !Bool
    , _highlight :: !(Maybe Highlight)
    } deriving (Eq, Show)

data Highlight = StartHightlight | Highlight | EndHighlight
    deriving (Eq, Show)

instance Pretty Symbol where
    pretty (Symbol text emphasize highlight) =
        text <> (if emphasize then "(b)" else "")
        <> case highlight of
            Nothing -> ""
            Just StartHightlight -> "+"
            Just Highlight -> "-"
            Just EndHighlight -> "|"

text :: (Text -> Text) -> Symbol -> Symbol
text f sym = sym { _text = f (_text sym) }

formatSymbol :: Symbol -> Text
formatSymbol (Symbol text emph highlight) = mconcat
    [ case highlight of
        Nothing -> ""
        Just StartHightlight -> Terminal.setBg Terminal.Normal Terminal.White
        Just Highlight -> ""
        Just EndHighlight -> Terminal.bgDefault
    , (if emph then emphasize  else id) text
    ]

emphasize :: Text -> Text
emphasize word
    -- A bold _ looks the same as a non-bold one, so put a bar to make it
    -- more obvious.
    | "_ " `Text.isPrefixOf` word = emphasize "_|"
    | "‗ " `Text.isPrefixOf` word = emphasize "‗|"
    | otherwise = Terminal.boldOn <> pre <> Terminal.boldOff <> post
    where (pre, post) = Text.break (==' ') word

symLength :: Symbol -> Int
symLength = textLength . _text

textLength :: Text -> Int
textLength = sum . map len . untxt
    where
    -- Combining characters don't contribute to the width.  I'm sure it's way
    -- more complicated than this, but for the moment this seems to work.
    len c
        | Char.isMark c = 0
        | otherwise = 1

-- ** util

collectPairs :: (a -> Bool) -> [a] -> [([a], [a])] -- ^ [(notTrue, true)]
collectPairs f = go
    where
    go [] = []
    go xs = (pre, within) : go post2
        where
        (pre, post1) = span f xs
        (within, post2) = break f post1
