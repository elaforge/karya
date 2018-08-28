-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Convert realized 'S.Flat' output to text for the terminal.
module Solkattu.Format.Terminal (
    writeAll, printInstrument, printKonnakol
    , Config(..), defaultConfig
    , formatInstrument
#ifdef TESTING
    , module Solkattu.Format.Terminal
#endif
) where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.File as File
import qualified Util.Seq as Seq
import qualified Util.Styled as Styled
import qualified Util.TextUtil as TextUtil

import qualified Solkattu.Format.Format as Format
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
    , _abstraction :: !Format.Abstraction
    } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
    { _rulerEach = 4
    , _terminalWidth = 78
    , _overrideStrokeWidth = Nothing
    , _abstraction = Format.defaultAbstraction
    }

konnakolConfig :: Config
konnakolConfig = Config
    { _rulerEach = 4
    , _terminalWidth = 100
    , _overrideStrokeWidth = Just 3
    , _abstraction = Format.defaultAbstraction
    }

-- * write

-- | Write all instrument realizations.
writeAll :: FilePath -> Format.Abstraction -> Korvai.Korvai -> IO ()
writeAll fname abstraction korvai =
    File.writeLines fname $ List.intersperse "" $ concatMap write1 $
    Korvai.korvaiInstruments korvai
    where
    write1 (name, Korvai.GInstrument inst) =
        name <> ":" : fst (formatInstrument config inst korvai)
        where
        config = (if name == "konnakol" then konnakolConfig else defaultConfig)
            { _abstraction = abstraction }

-- * format

printInstrument :: Solkattu.Notation stroke => Korvai.Instrument stroke
    -> Format.Abstraction -> Korvai.Korvai -> IO ()
printInstrument instrument abstraction =
    mapM_ Text.IO.putStrLn . fst
    . formatInstrument (defaultConfig { _abstraction = abstraction }) instrument

printKonnakol :: Int -> Format.Abstraction -> Korvai.Korvai -> IO ()
printKonnakol width abstraction =
    mapM_ Text.IO.putStrLn . fst . formatInstrument config Korvai.konnakol
    where
    config = konnakolConfig
        { _terminalWidth = width
        , _abstraction = abstraction
        }

formatInstrument :: Solkattu.Notation stroke => Config
    -> Korvai.Instrument stroke -> Korvai.Korvai -> ([Text], Bool)
formatInstrument config instrument korvai =
    formatResults config korvai $ zip (korvaiTags korvai) $
        Format.convertGroups $
        Korvai.realize instrument realizePatterns korvai
    where
    realizePatterns = not $
        Format.isAbstract (_abstraction config) Format.Patterns

korvaiTags :: Korvai.Korvai -> [Tags.Tags]
korvaiTags = map Korvai.sectionTags . Korvai.genericSections

formatResults :: Solkattu.Notation stroke => Config -> Korvai.Korvai
    -> [(Tags.Tags, Either Error ([Format.Flat stroke], Error))]
    -> ([Text], Bool)
formatResults config korvai results =
    ( snd . List.mapAccumL show1 (Nothing, 0) . zip [1..] $ results
    , any (Either.isLeft . snd) results
    )
    where
    show1 _ (section, (_, Left err)) =
        ((Nothing, 0), sectionFmt section mempty $ "ERROR:\n" <> err)
    show1 prevRuler (section, (tags, Right (notes, warning))) =
        ( nextRuler
        , TextUtil.joinWith "\n" (sectionFmt section tags (Styled.toText out))
            warning
        )
        where
        (nextRuler, out) =
            format config prevRuler (Korvai.korvaiTala korvai) notes
    sectionFmt section tags = Text.intercalate "\n"
        . Seq.map_last (<> showTags tags)
        . Seq.map_head_tail
            (sectionNumber section <>) (Text.replicate leader " " <>)
        . map Text.strip
        . Text.lines
    sectionNumber section = Text.justifyLeft leader ' ' (showt section <> ":")
    leader = 4

showTags :: Tags.Tags -> Text
showTags tags = case Map.lookup Tags.times (Tags.untags tags) of
    Just [n] -> "   x" <> n
    _ -> ""


-- * implementation

-- | Keep state about the last ruler across calls to 'format', so I can
-- suppress unneeded ones.  (prevRuler, lineNumber)
type PrevRuler = (Maybe Format.Ruler, Int)

type Line = [(S.State, Symbol)]

-- | Format the notes according to the tala.
--
-- The line breaking for rulers is a bit weird in that if the line is broken,
-- I only emit the first part of the ruler.  Otherwise I'd have to have
-- a multiple line ruler too, which might be too much clutter.  I'll have to
-- see how it works out in practice.
format :: Solkattu.Notation stroke => Config -> PrevRuler
    -> Tala.Tala -> [Format.Flat stroke] -> (PrevRuler, Styled.Styled)
format config prevRuler tala notes =
    second (Styled.join "\n" . map formatAvartanam) $
        pairWithRuler (_rulerEach config) prevRuler tala strokeWidth
            avartanamLines
    where
    formatAvartanam = Styled.join "\n" . map formatRulerLine
    formatRulerLine (ruler, line) = Styled.join "\n" $
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
        where fmt = formatLines (_abstraction config)
    formatLine :: [Symbol] -> Styled.Styled
    formatLine = mconcat . map formatSymbol
    width = _terminalWidth config

pairWithRuler :: Int -> PrevRuler -> Tala.Tala -> Int -> [[Line]]
    -> (PrevRuler, [[(Maybe Format.Ruler, Line)]])
pairWithRuler rulerEach prevRuler tala strokeWidth =
    List.mapAccumL (List.mapAccumL strip) prevRuler . map (map addRuler)
    where
    addRuler line = (Format.inferRuler tala strokeWidth (map fst line), line)
    -- Strip rulers when they are unchanged.  "Changed" is by structure, not
    -- mark text, so a wrapped ruler with the same structure will also be
    -- suppressed.
    strip (prev, lineNumber) (ruler, line) =
        ( (Just ruler, 1 + if wanted then 0 else lineNumber)
        , (if wanted then Just ruler else Nothing, line)
        )
        where
        wanted = lineNumber `mod` rulerEach == 0
            || Just (map snd ruler) /= (map snd <$> prev)

formatRuler :: Int -> Format.Ruler -> Styled.Styled
formatRuler strokeWidth =
    Styled.bg (Styled.bright Styled.white)
        . mconcat . snd . List.mapAccumL render 0
    where
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
            { _text = Realize.justifyLeft (symLength sym) ' ' double }
        | odd col && maybe False isRest prev = sym
            { _text = Text.replicate (symLength sym) " " }
        | otherwise = sym
    double = Text.singleton Realize.doubleRest

-- This should be (== Space Rest), but I have to 'makeSymbols' first to break
-- lines.
isRest :: Symbol -> Bool
isRest = (=="_") . Text.strip . _text

-- | Break into [avartanam], where avartanam = [line].
formatLines :: Solkattu.Notation stroke => Format.Abstraction -> Int
    -> Int -> Tala.Tala -> [Format.Flat stroke] -> [[[(S.State, Symbol)]]]
formatLines abstraction strokeWidth width tala =
    map (map (Format.mapSnd (spellRests strokeWidth)))
        . Format.formatFinalAvartanam isRest . map (breakLine width)
        . Format.breakAvartanams
        . map combine . Seq.zip_prev
        . concatMap (makeSymbols strokeWidth tala angas)
        . Format.makeGroupsAbstract abstraction
        . Format.normalizeSpeed tala
    where
    combine (prev, (state, sym)) = (state, text (Text.drop overlap) sym)
        where overlap = maybe 0 (subtract strokeWidth . symLength . snd) prev
    angas = Format.angaSet tala

makeSymbols :: Solkattu.Notation stroke => Int -> Tala.Tala -> Set Tala.Akshara
    -> Format.NormalizedFlat stroke -> [(S.State, Symbol)]
makeSymbols strokeWidth tala angas = go
    where
    go (S.FNote _ (state, note)) =
        (:[]) $ (state,) $ make state $ case note of
            S.Attack a -> Realize.justifyLeft strokeWidth (Solkattu.extension a)
                (Solkattu.notation a)
            S.Sustain a -> Text.replicate strokeWidth
                (Text.singleton (Solkattu.extension a))
            S.Rest -> Realize.justifyLeft strokeWidth ' ' "_"
    go (S.FGroup _ group children) = modify (concatMap go children)
        where
        modify = case Format._type group of
            Realize.Unhighlighted -> id
            Realize.Highlighted -> setHighlights
            -- TODO special highlight, but only when non-abstract
            Realize.Sarva -> id
    setHighlights =
        Seq.map_last (second (set Format.EndHighlight))
        . Seq.map_head_tail
            (second (set Format.StartHighlight))
            (second (set Format.Highlight))
        where set h sym = sym { _highlight = Just h }
    make state text = Symbol
        { _text = text
        , _emphasize = shouldEmphasize tala angas state
        , _highlight = Nothing
        }

-- | Chapus are generally fast, so only emphasize the angas.  Other talas are
-- slower, and without such a strong beat, so emphasize every akshara.
shouldEmphasize :: Tala.Tala -> Set Tala.Akshara -> S.State -> Bool
shouldEmphasize tala angas state
    | isChapu = Format.onAnga angas state
    | otherwise = Format.onAkshara state
    where
    isChapu = case Tala._angas tala of
        Tala.Wave _ : _ -> True
        Tala.Clap _ : _ -> True
        _ -> False

-- | If the text goes over the width, break at the middle akshara, or the
-- last one before the width if there isn't a middle.
breakLine :: Int -> [(S.State, Symbol)] -> [[(S.State, Symbol)]]
breakLine maxWidth notes
    | width <= maxWidth = [notes]
    | even aksharas = breakAt (aksharas `div` 2) notes
    | otherwise = breakBefore maxWidth notes
    where
    width = sum $ map (symLength . snd) notes
    aksharas = Seq.count (Format.onAkshara . fst) notes
    breakAt akshara =
        pairToList . break ((==akshara) . S.stateAkshara . fst)
    pairToList (a, b) = [a, b]

-- | Yet another word-breaking algorithm.  I must have 3 or 4 of these by now.
breakBefore :: Int -> [(S.State, Symbol)] -> [[(S.State, Symbol)]]
breakBefore maxWidth =
    go . dropWhile null . Seq.split_before (Format.onAkshara . fst)
    where
    go aksharas =
        case breakFst (>maxWidth) (zip (runningWidth aksharas) aksharas) of
            ([], []) -> []
            (pre, []) -> [concat pre]
            ([], post:posts) -> post : go posts
            (pre, post) -> concat pre : go post
    -- drop 1 so it's the width at the end of each section.
    runningWidth = drop 1 . scanl (+) 0 . map (sum . map (symLength . snd))

-- ** formatting

data Symbol = Symbol {
    _text :: !Text
    , _emphasize :: !Bool
    , _highlight :: !(Maybe Format.Highlight)
    } deriving (Eq, Show)

instance Pretty Symbol where
    pretty (Symbol text emphasize highlight) =
        text <> (if emphasize then "(b)" else "")
        <> case highlight of
            Nothing -> ""
            Just Format.StartHighlight -> "+"
            Just Format.Highlight -> "-"
            Just Format.EndHighlight -> "|"

text :: (Text -> Text) -> Symbol -> Symbol
text f sym = sym { _text = f (_text sym) }

formatSymbol :: Symbol -> Styled.Styled
formatSymbol (Symbol text emph highlight) =
    (case highlight of
        Nothing -> id
        Just Format.StartHighlight -> Styled.bg (Styled.rgb 0.5 0.75 0.5)
        Just _ -> Styled.bg (Styled.rgb 0.75 0.75 0.75)
    ) $
    (if emph then emphasize else Styled.plain) text
    where
    emphasize word
        -- A bold _ looks the same as a non-bold one, so put a bar to make it
        -- more obvious.
        | "_ " `Text.isPrefixOf` word = emphasize "_|"
        | "‗ " `Text.isPrefixOf` word = emphasize "‗|"
        | otherwise = Styled.bold word

symLength :: Symbol -> Int
symLength = Realize.textLength . _text

-- * util

breakFst :: (key -> Bool) -> [(key, a)] -> ([a], [a])
breakFst f = bimap (map snd) (map snd) . break (f . fst)
