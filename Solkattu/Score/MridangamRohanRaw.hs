-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Solkattu.Score.MridangamRohanRaw where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.Lists as Lists
import           Solkattu.Dsl.Mridangam (Sequence, __, __4, realize, (&))
import qualified Solkattu.S as S

import           Global


{-
    * export as md
    * read .md, split on |
    * assert each line ==8
    * infer _s for 8 words
    * map sollus to chars, ignore _
    * process _:
        . all 1st speed as-is
        / can I take kt -> x, pk -> q?
        . Then if akshara has su, expand to su "" the whole thing.
    - some will be kandam!
-}

t0 = parseMd <$> Text.IO.readFile "featuring-fives.md"
t1 = do
    ts <- t0
    Text.IO.putStrLn $ rowsToHs ts
t2 = map processLine $ take 2 featuringFivesRowsRaw
t3 = mapM_ pr (zip [0..] t2)
    where
    pr (i, s) = Text.IO.putStrLn $ showt i <> " " <> s

realize5s = mapM_ Text.IO.putStrLn $ processLines featuringFivesRowsRaw

parseMd :: Text -> [[Text]]
parseMd = map toColumns . Text.lines
    where
    toColumns = map (Text.strip . Text.replace "|" "") . Text.splitOn " | "

processLines :: [[Text]] -> [Text]
processLines = linesToHs . map (expandSu . processLine)

linesToHs :: [Text] -> [Text]
linesToHs =
    (++ [indent <> "]"])
        . Lists.mapHeadTail ((indent <>"[ ") <>) ((indent <> ", ")<>)
    where
    indent = "    "

-- | If there are ane ()s on the line, expand the whole thing.
expandSu :: Text -> Text
expandSu line
    | Text.strip line == "" = "--"
    | not $ "(" `Text.isInfixOf` line = quoted line
    | otherwise =
        "su " <> quoted
            (mconcat $ snd $ List.mapAccumL expand False $ Text.unpack line)
    where
    quoted line = "\"" <> line <> "\""
    expand False '(' = (True, "")
    expand True ')' = (False, "")
    expand isSu c | c `elem` ['(', ')'] =
        error $ "unmatched paren: " <> show (isSu, c)
    expand isSu c
        | c == ' ' = (isSu, " ")
        | otherwise = (isSu, Text.singleton c <> if isSu then "" else "_")

processLine :: [Text] -> Text
processLine = Text.unwords . map (toChars . inferSu) . check
    where
    -- Fix dom- to dom -, but _- remains the same.
    separateRests = Text.replace "- _" "-_" . Text.replace "_ -" "_-"
        . Text.replace "-" " - "
    check ws
        | length ws == 8 = ws
        | otherwise = error $ "/= 8: " <> show ws
    inferSu w
        | length (Text.words w) == 8 = "_" <> w <> "_"
        | otherwise = w
    toChars = mconcat . map replace . Text.words . separateRests
    replace w = mconcat
        [ if "_" `Text.isPrefixOf` w then "(" else ""
        , realizeSollu (Text.replace "_" "" w)
        , if "_" `Text.isSuffixOf` w then ")" else ""
        ]

rowsToHs :: [[Text]] -> Text
rowsToHs (row : rows) = Text.unlines $
    "    [ " <> showt row : map (("    , "<>) . showt) rows ++ ["    ]"]
rowsToHs [] = ""

sollusText :: Map Text Text
sollusText = Map.fromList
    [ ("cha", "u")
    , ("cha/ta", "A") -- pu or pv, also have E Y
    , ("cha/dom", "U")
    , ("di/ta", "P")
    , ("ka/ta", "P")
    , ("ki/ta", "P")
    , ("di/Ta", "X")
    , ("ka/Ta", "X")
    , ("ki/Ta", "X")
    , ("Ta/dom", "T")
    , ("ta", "p")
    , ("Ta", "t")
    , ("tam", "N")
    , ("ka", "k")
    , ("di", "k")
    , ("ki", "k")
    , ("lam", "u")
    , ("nam", "n")
    , ("dom", "o")
    , ("din", "d")
    , ("dim", "D") -- Some of these are dim, some are od
    , ("-", "_")
    ]

cookRaw :: [[Text]] -> IO ()
cookRaw = Text.IO.putStrLn . fmtText . processText

fmtText :: [(Title, [[Text]])] -> Text
fmtText = Text.unlines . map ("    " <>) . (++ ["]"]) . concatMap fmt
    where
    fmt (title, sections) = ("-- " <> title)
        : concatMap fmtSection sections
    fmtSection as = Lists.mapHead (<> " --") $
        Lists.mapHeadTail (", "<>) (". "<>) $
        map (\a -> "\"" <> a <> "\"") as

type Title = Text
type Error = Text

processText :: [[Text]] -> [(Title, [[Text]])]
processText = map realizeSectionT . inferNadai . process

realizeSectionT :: (S.Nadai, (Title, [[[Text]]])) -> (Title, [[Text]])
realizeSectionT (nadai, (title, sections)) =
    (title, map (map (realizeAvartanamT nadai)) sections)

process :: [[Text]] -> [(Title, [[[Text]]])]
process =
    collectGroups . filter (/= ("", []))
    . snd . Lists.splitWith isTitle
    . Lists.dropWith (\a b -> all Text.null a && all Text.null b)
    where
    collectGroups = map (\((name, as) : bs) -> (name, as : map snd bs))
        . drop 1 . Lists.splitBefore (not . Text.null . fst)
    isTitle (name : row)
        | (name == "" || Char.isUpper (Text.head name)) && all (=="") row =
            Just name
        | otherwise = Nothing
    isTitle [] = Nothing

inferNadai :: [(Title, a)] -> [(S.Nadai, (Title, a))]
inferNadai = snd . List.mapAccumL infer 4
    where
    infer nadai (title, xs) = case nameToNadai title of
        Nothing -> (nadai, (nadai, (title, xs)))
        Just nadai -> (nadai, (nadai, ("nadai " <> showt nadai, xs)))

realizeAvartanamT :: S.Nadai -> [Text] -> Text
realizeAvartanamT nadai = Text.unwords . map expand
    where
    expand w
        | extra == 0 = "+" <> mconcat ws
        | r /= 0 = "*" <> mconcat ws
        | otherwise = mconcatMap (<> Text.replicate (extra-1) "_") ws
        where
        ws = realizeAksharaT w
        (extra, r) = nadai `divMod` length ws

speed :: S.Nadai -> Int -> S.Speed
speed nadai len
    | len == 0 = speed nadai 1
    | frac /= 0 = error $ show len <> " doesn't go into " <> show nadai
    | otherwise = log
    where
    (log, frac) = properFraction (logBase 2 (fi len / fi nadai))
    fi = fromIntegral

nameToNadai :: Text -> Maybe S.Nadai
nameToNadai = flip Map.lookup $ Map.fromList
    [ ("Chatusram", 4)
    , ("Chaturshra Nadai", 4)
    , ("Chaturshram", 4)
    , ("Tisram", 3)
    , ("Tisra Nadai", 3)
    ]

realizeAksharaT :: Text -> [Text]
realizeAksharaT akshara
    | akshara == "" = ["_"]
    | otherwise = map realizeSollu $ Text.words $ preproc akshara
    where
    preproc = Text.replace "-" " - " . Text.replace " / " "/"

realizeSollu :: Text -> Text
realizeSollu w = case Map.lookup w sollusText of
    Just c -> c
    Nothing -> case split w of
        [a, b]
            -- TODO except fromString can't parse this, because it's two chars
            | b == "ta" -> realizeSollu a <> overline
            | b == "dom" -> Text.toUpper (realizeSollu a)
        _ -> error $ "unknown sollu: " <> untxt w
    where
    split = Lists.sortOn (`elem` ["ta", "dom"]) . Text.splitOn "/"
    overline = "\x0305"

{-
    There are underlines in there!  I don't know how to get them to text.
    I also want to rewrite korvais structured.
    So what if I manually add the underlines.
    Then textually convert to stroke names.
    Then print out and paste back in.

    -- t0 = mapM_ print $ process kandaRowsRaw
    -- t1 = processText kandaRowsRaw
    -- t3 = either (error . Text.unpack) id $ realizeSections kandaRowsRaw
    realizeSections = mapM realizeSection . inferNadai . process

    -- realizeSection :: [[Text]] -> Either Error Sequence
    realizeSection (nadai, (title, sections)) = do
        (title,) <$> mapM (mapM (realizeAvartanam nadai)) sections

    realizeAvartanam :: S.Nadai -> [Text] -> Either Error Sequence
    realizeAvartanam nadai ws = do
        strokes <- traverse realizeAkshara ws
        pure $ mconcatMap realize1 $ Lists.groupAdjacentFst $
            Lists.keyOn (speed nadai . length) strokes
        where realize1 (s, ns) = Mridangam.speed s (mconcat (mconcat ns))

    realizeAkshara :: Text -> Either Error [Sequence]
    realizeAkshara akshara =
        mapM find $ if akshara == "" then ["-"] else Text.words akshara
        where
        find w = case Text.splitOn "/" w of
            [a, b] -> (&) <$> find a <*> find b
            _ -> maybe (Left $ "unknown: " <> w) Right $ Map.lookup w sollus

    sollus :: Map Text Sequence
    sollus = Map.fromList
        [ ("cha", u)
        , ("cha/ta", p&u)
        , ("cha/dom", o&u)
        , ("ta", p)
        , ("Ta", t)
        , ("tam", o&n)
        , ("ka", k)
        , ("di", k)
        , ("ki", k)
        , ("lam", u)
        , ("nam", n)
        , ("dom", o)
        , ("din", d)
        , ("dim", od)
        , ("-", __)
        ]
        where Instrument.Mridangam.Strokes {..} = Instrument.Mridangam.notes
-}

featuringFivesRowsRaw :: [[Text]]
featuringFivesRowsRaw =
    [ ["ki Ta ki nam","dom - ki Ta","ki nam dom ki","Ta ki nam dom","- ki Ta ki","nam dom ki Ta","ki nam dom ki","Ta ki nam dom"]
    , ["_ki Ta ki Ta ta ka ta ka_","dom - _ki Ta ki Ta_","_ta ka ta ka_ dom _ki Ta_","_ki Ta ta ka ta ka_ dom","- _ki Ta ki Ta ta ka_","_ta ka_ dom _ki Ta ki Ta_","_ta ka ta ka_ dom _ki Ta_","_ki Ta ta ka ta ka_ dom"]
    , ["_dom ka Ta ta cha ta ki Ta_","dom - _dom ki Ta ta_","_cha ta ki Ta_ dom _dom ki_","_Ta ta cha ta ki Ta_ dom","- _dom ki Ta ta cha ta_","_ki Ta_ dom _dom ki Ta ta_","_cha ta ki Ta_ dom _dom ki_","_Ta ta cha ta ki Ta_ dom"]
    , ["","","","","","","",""]
    , ["tam din - ka","nam din - ka","nam din - ka","nam din - ka","tam din - ka","nam din - ka","nam din - ka","nam cha - -"]
    , ["tam din - ka","nam din - ka","nam din - ka","nam din - ka","tam din - ka","nam din - ka","nam din - ka","nam dim - -"]
    , ["tam din - ka","nam din - ka","nam din - ka","nam dom _ki Ta ta ka_","tam din - ka","nam din - ka","nam din - ka","nam dom _ki Ta ta ka_"]
    , ["tam din - ka","nam din - ka","nam din - ka","_nam dom_ dom _ki Ta ta ka_","tam din - ka","nam din - ka","nam din - ka","_nam dom_ dom _ki Ta ta ka_"]
    , ["tam din - ka","nam din - ka","nam din - ka","_Ta di dom dom  di dom dom ka_","tam din - ka","nam din - ka","nam din - ka","_Ta di dom dom  di dom dom ka_"]
    , ["tam din - ka","nam din - ka","nam din - ka","_dom ka dom dom ka dom dom ka_","tam din - ka","nam din - ka","nam din - ka","_dom ka dom dom ka dom dom ka_"]
    , ["tam din - ka","nam din - ka","nam din - ka","nam dom _ki Ta ta ka_","tam din - ka","nam din ka -","_ki Ta ki nam dom ka Ta di_","_Ta ta cha ta ki Ta ta ka_"]
    , ["tam din - ka","nam din ka -","_ki Ta ki nam dom ka Ta di_","_Ta ta cha ta ki Ta ta ka_","tam din - ka","nam din ka -","_ki Ta ki nam dom ka Ta di_","_Ta ta cha ta ki Ta ta ka_"]
    , ["","","","","","","",""]
    -- , ["**Arudhi**","","","","","","",""]
    , ["","","","","","","",""]
    , ["ka -  _ki Ta ki nam_","_dom ki Ta di Ta ta cha ta_","_ki Ta ta ka di - ki Ta_","_- ka nam dom_ tam -","- - - ka","-  _ki Ta ki nam dom ki_","_Ta di Ta ta cha ta ki Ta_","_ta ka di - ki Ta - ka_"]
    , ["_nam dom di - ki Ta - ka_","_nam dom_ tam - -","- - ka -","_ki Ta ki nam dom ki Ta di_","_Ta ta cha ta ki Ta ta ka_","_di - ki Ta - ka nam dom_","_di - ki Ta - ka nam dom_","_di - ki Ta - ka nam dom_"]
    , ["","","","","","","",""]
    , ["tam - ta ka","nam dom dom ka","Ta di dom dom","nam dom dom ka","Ta dom _ki Ta ta ka_","tam - Ta dom","_ki Ta ta ka_ tam -","Ta dom _ki Ta ta ka_"]
    , ["","","","","","","",""]
    , ["tam dom ka tam","ka tam dom ka","tam ka tam dom","ka tam _ki Ta ta ka_","tam dom ka tam","ka tam dom ka","tam ka tam dom","ka tam _ki Ta ta ka_"]
    , ["tam dom ka tam","ka tam dom ka","tam ka tam dom","ka tam _ki Ta ta ka_","tam dom ka tam","dom ka ki -","Ta - ki -","nam - dom -"]
    , ["tam dom ka tam","dom ka ki -","Ta - ki -","nam - dom -","tam dom ka tam","dom ka ki -","Ta - ki -","nam - dom -"]
    , ["","","","","","","",""]
    -- , ["**Arudhi**","","","","","","",""]
    , ["","","","","","","",""]
    , ["ki - - -","dim - - -","ki - - -","nam - - -","dom - - -","ki Ta ka nam","dom din - ki","- - dim -"]
    , ["- ki - -","nam - - dom","- - ki Ta","ka nam dom din","- ki - Ta","- ka - nam","- dom - ki","Ta ka nam dom"]
    , ["","","","","","","",""]
    , ["ki - - -","dim - - -","ki - - -","nam - - -","dom - - -","_ki Ta ki Ta ta ka ta ka_","dom din - ki","- - dim -"]
    , ["- ki - -","nam - - dom","- - _ki Ta ki Ta_","_ta ka ta ka_ dom din","- ki - Ta","- ka - nam","- dom - _ki Ta_","_ki Ta ta ka ta ka_ dom"]
    , ["","","","","","","",""]
    , ["ki - - -","dim - - -","ki - - -","nam - - -","dom - - -","_dom ki Ta ta cha ta ki Ta_","dom din - ki","- - dim -"]
    , ["- ki - -","nam - - dom","- - _dom ka Ta ta_","_cha ta ki Ta_ dom din","- ki - Ta","- ka - nam","- dom - _dom ki_","_Ta ta cha ta ki Ta_ dom"]
    , ["","","","","","","",""]
    , ["tam - ta ka","nam dom dom ka","Ta di dom dom","nam dom dom ka","_ki Ta dom dom_  ka -","dim  - _ki Ta dom dom_","ka - dim -","_ki Ta dom dom_  ka -"]
    , ["","","","","","","",""]
    , ["tam - - ta","cha dom dom ka","dim - - ta","cha dom dom ka","dim - - ta","cha dom dom ka","dim - - ta","cha dom dom ka"]
    -- , ["","","","","","","*Khandam*","*Khandam*"]
    , ["tam dom dom ta","cha dom dom ka","tam dom dom ta","cha dom dom ka","tam dom dom ta","cha dom dom ka","tam dom ka tam ki","Ta/dom di nam ta ka"]
    , ["","","","","","","",""]
    , ["","","","","","","",""]
    -- , ["","","*Khandam*","*Khandam*","","","*Khandam*","*Khandam*"]
    , ["tam - - ka","din dom dom ka","tam dom ka tam ki","Ta/dom di nam ta ka","tam - - ka","din dom dom ka","tam dom ka tam ki","Ta/dom di nam ta ka"]
    , ["","","","","","","",""]
    -- , ["Khanda Nadai","","","","","","",""]
    , ["tam dom ka tam ki","Ta/dom di nam ta ka","nam ta ka nam ka","Ta/dom di nam ta ka","tam dom ka tam ki","Ta/dom di nam ta ka","nam ta ka nam ka","Ta/dom di nam ta ka"]
    , ["tam - tam - ka","din - nam - ka","din - nam - ka","din - nam - ka","tam - tam - ka","din - nam - ka","din - nam - ka","din - nam - ka"]
    , ["tam - tam - ka","din - nam - ka","ta/nam - ta/nam - ka","din - nam - ka","tam - tam - ka","din - nam - ka","ta/nam - ta/nam - ka","din - nam - ka"]
    , ["tam - tam - ka","ta/nam - ta/nam - ka","tam - tam - ka","ta/nam - ta/nam - ka","tam - tam - ka","ta/nam - ta/nam - ka","tam - tam - ka","ki Ta ki nam dom"]
    , ["tam - tam - ka","ta/nam - ta/nam - ka","tam - tam - ka","ki Ta ki nam dom","tam - tam - ka","ta/nam - ta/nam - ka","tam - tam - ka","ki Ta ki nam dom"]
    , ["ki Ta ki nam dom","din","ki Ta ki nam dom","ki Ta ki nam dom","din","ki Ta ki nam dom","ki Ta ki nam dom","ki Ta ki nam dom"]
    , ["ki Ta ki nam dom","din","ki Ta ki nam dom","ki Ta ki nam dom","din","ki Ta ki nam dom","ki Ta ki nam dom","ki Ta ki nam dom"]
    , ["ki Ta ki nam dom","din","ki Ta ki nam dom","ki Ta ki nam dom","din","ki Ta ki nam dom","ki Ta ki nam dom","ki Ta ki nam dom"]
    , ["","","","","","","",""]
    , ["tam - tam - ka","ta/nam - ta/nam - ka","tam - tam - ka","ta/nam - ta/nam - ka","tam - tam - ka","ta/nam - ta/nam - ka","tam - tam - ka","ta/nam - ta/nam - ka"]
    , ["","","","","","","",""]
    -- , ["**Korvai**","","","","","","",""]
    , ["","","","","","","",""]
    , ["ta - di - tam","- - ki Ta ka","nam dom di - tam","- - ki Ta ka","nam dom tam - -","ki Ta ki nam dom","ki Ta ki nam dom","ki Ta ki nam dom"]
    , ["ta - di - tam","- - ki Ta ka","nam dom di - tam","- - ki Ta ka","nam dom tam - -","ki Ta ki nam dom","ki Ta ki nam dom","ki Ta ki nam dom"]
    , ["ta - di - tam","- - ki Ta ka","nam dom di - tam","- - ki Ta ka","nam dom tam - -","ki Ta ki nam dom","ki Ta ki nam dom","ki Ta ki nam dom"]
    , ["","","","","","","",""]
    -- , ["**Chaturshram**","","","","","","",""]
    , ["","","","","","","",""]
    , ["tam - ta ka","nam dom dom ka","Ta di dom dom","nam dom dom ka","- _dom ka_ ka -","din - - _dom ka_","ka - din -","- _dom ka_ ka -"]
    , ["","","","","","","",""]
    , ["dom dom dom cha","- dom dom cha","dom dom dom cha","- dom dom cha","ta ta ta cha","- ta ta cha","ta ta ta cha","- ta ta cha"]
    , ["dom dom dom cha","- ta ta cha","dom dom dom cha","- ta ta cha","dom dom dom cha","- ta ta cha","dom dom dom cha","- ta ta cha"]
    , ["","","","","","","",""]
    , ["tam - ta ka din - ta ka","nam dom dom ka din - ta ka","tam - ta ka din - ta ka","nam dom dom ka din - ta ka","tam - ta ka din - ta ka","di dom dom ka din - ta ka","tam - ta ka din - ta ka","di dom dom ka din - ta ka"]
    , ["tam - ta ka di dom dom ka","tam - ta ka di dom dom ka","tam - ta ka di dom dom ka","tam - ta ka di dom dom ka","di dom dom ka di dom dom ka","tam - di dom dom ka","di dom dom ka tam -","di dom dom ka di dom dom ka"]
    , ["","","","","","","",""]
    , ["tam - _ki Ta ta ka_","nam dom _ki Ta ta ka_","tam - _ki Ta ta ka_","nam dom _ki Ta ta ka_","tam - _ki Ta ta ka_","nam dom _ki Ta ta ka_","tam - _ki Ta ta ka_","_Ta ta cha ta ki Ta ta ka_"]
    , ["tam - _ki Ta ta ka_","nam dom _ki Ta ta ka_","tam - _ki Ta ta ka_","_Ta ta cha ta ki Ta ta ka_","tam - _ki Ta ta ka_","nam dom _ki Ta ta ka_","tam - _ki Ta ta ka_","_Ta ta cha ta ki Ta ta ka_"]
    , ["tam - _ki Ta ta ka_","_Ta ta cha ta ki Ta ta ka_","tam - _ki Ta ta ka_","_Ta ta cha ta ki Ta ta ka_","_Ta ta cha ta ki Ta ta ka_","tam - _Ta ta cha ta_","_ki Ta ta ka_ tam -","_Ta ta cha ta ki Ta ta ka_"]
    , ["","","","","","","",""]
    , ["tam - - _ki Ta_","_ki Ta ta ka_ din -","- _ki Ta ki Ta Ta ka_","tam - ka -","tam - - _ki Ta_","_ki Ta ta ka_ din -","- _ki Ta ki Ta Ta ka_","tam - ka -"]
    , ["tam - - _ki Ta_","_ki Ta ta ka_ din -","- _ki Ta ki Ta Ta ka_","_Ta ta cha ta ki Ta ta ka_","tam - - _ki Ta_","_ki Ta ta ka_ din -","- _ki Ta ki Ta Ta ka_","_Ta ta cha ta ki Ta ta ka_"]
    , ["","","","","","","",""]
    -- , ["**Farans**","","","","","","",""]
    , ["","","","","","","",""]
    , ["Ta - dom - ki Ta ta ka","Ta ta cha ta ki Ta ta ka","ta lam - dom ki Ta ta ka","Ta ta cha ta ki Ta ta ka","ta ka dom - tam - ta ka","Ta ta cha ta ki Ta ta ka","tam - ta ka nam dom dom ka","Ta ta cha ta ki Ta ta ka"]
    , ["tam - ta ka Ta - dom -","ki Ta ta ka tam -","ta ka Ta - dom - ki Ta ta ka","Ta ta cha ta ki Ta ta ka","tam - ta ka Ta ta cha ta","ki Ta ta ka tam - ta ka","Ta ta cha ta ki Ta ta ka","Ta ta cha ta ki Ta ta ka"]
    , ["ta lam - ka dom- ki Ta","ki Ta ta ka ta lam- ka","dom- ki Ta ki Ta ta ka","Ta ta cha ta ki Ta ta ka","ta lam- ka dom- ta lam","- ka dom - ta lam- ka","dom- ki Ta ki Ta ta ka","Ta ta cha ta ki Ta ta ka"]
    , ["nam ta ki Ta ta ka nam ta","ki Ta ta ka nam ta ki Ta","ta ka nam ta ki Ta ta ka","Ta ta cha ta ki Ta ta ka","nam- ki Ta ta ka nam -","ki Ta ta ka nam - ki Ta","ta ka nam - ki Ta ta ka","Ta ta cha ta ki Ta ta ka"]
    , ["Ta ta ki Ta ta ka Ta ta","ki Ta ta ka Ta ta ki Ta","ta ka Ta ta ki Ta ta ka","Ta ta cha ta ki Ta ta ka","tam- ki Ta ta ka tam-","ki Ta ta ka tam - ki Ta","ta ka tam - ki Ta ta ka","Ta ta cha ta ki Ta ta ka"]
    , ["tam - tam -  ki Ta ta ka","Ta ta cha ta ki Ta ta ka","tam - ka - tam -","tam - tam - ki Ta ta ka","Ta ta cha ta ki Ta ta ka","tam - ka - tam -","tam - tam - ki Ta ta ka","Ta ta cha ta ki Ta ta ka"]
    , ["","","","","","","",""]
    -- , ["**Mora**","","","","","","",""]
    , ["","","","","","","",""]
    , ["ta - di -","_tam - ta ka Ta ta cha ta_","_ki Ta ta ka_ di -","tam- ta ka Ta ta cha ta","ki Ta ta ka tam - ta ka","Ta ta cha ta ki Ta ta ka","_ta lam - ka_ dom ka","dom ka dom -"]
    , ["ta - di -","_tam - ta ka Ta ta cha ta_","_ki Ta ta ka_ di -","tam- ta ka Ta ta cha ta","ki Ta ta ka tam - ta ka","Ta ta cha ta ki Ta ta ka","_ta lam - ka_ dom ka","dom ka dom -"]
    , ["ta - di -","_tam - ta ka Ta ta cha ta_","_ki Ta ta ka_ di -","tam- ta ka Ta ta cha ta","ki Ta ta ka tam - ta ka","Ta ta cha ta ki Ta ta ka","_ta lam - ka_ dom -","di - _tam - ta ka_"]
    , ["Ta ta cha ta ki Ta ta ka","_ta lam - ka_ dom -","di - _tam - ta ka_","Ta ta cha ta ki Ta ta ka","_ta lam - ka_ dom ka","tam - _ta lam - ka_","dom ka tam -","_ta lam - ka_ dom ka"]
    , ["","","","","","","",""]
    -- , ["**Korvai**","","","","","","",""]
    , ["","","","","","","",""]
    , ["ta - di -","di dom dom ka","tam - - ki","- Ta - ki","- nam - dom","di - di dom","dom ka tam -","-  ki - Ta"]
    , ["- ki - nam","- dom di dom","dom ka tam -","- ki - Ta","- ki - nam","- dom dim -","- - ki -","Ta - ki -"]
    , ["nam - dom dim","- - - ki","- Ta - ki","- nam - dom","ta - di -","di dom dom ka","tam - - ki","- Ta - ki"]
    , ["- nam - dom","di - di dom","dom ka tam -","-  ki - Ta","- ki - nam","- dom di dom","dom ka tam -","- ki - Ta"]
    , ["- ki - nam","- dom dim -","- - ki -","Ta - ki -","nam - dom dim","- - - ki","- Ta - ki","- nam - dom"]
    -- , ["**Third Round in Khandam**","","","","","","",""]
    , ["ta - di - di","dom dom ka tam -","- ki - Ta -","ki - nam - dom","di - di dom dom","ka tam - -  ki","- Ta - ki -","nam - dom di dom"]
    , ["dom ka tam - -","ki - Ta - ki","- nam - dom dim","- - - ki -","Ta - ki - nam","- dom dim - -","- ki - Ta -","ki - nam - dom"]
    , ["","","","","","","",""]
    -- , ["**Chaturshram**","","","","","","",""]
    , ["tam - ta ka","nam dom dom ka","Ta di dom dom","nam dom dom ka","_Ta ta cha ta ki Ta ta ka_","tam - _Ta ta cha ta_","_ki Ta ta ka_ tam -","Ta ta cha ta ki Ta ta ka"]
    , ["Ta ta cha ta ki Ta ta ka","tam _ta ka Ta ta cha ta_","_ki Ta ta ka_ tam _ta ka_","_Ta ta cha ta ki Ta ta ka_","Ta ta cha ta ki Ta ta ka","tam - ki Ta ta ka tam -","ki Ta ta ka  tam - ki Ta","_ta ka_ tam dom ka"]
    ]

{-
kandaRowsRaw :: [[Text]]
kandaRowsRaw =
    [ ["Chatusram", "", "", "", ""]
    , ["cha / ta", "", "di dom", "dim", "di dom"]
    , ["dim", "dim", "di dom", "dim", "ki Ta ta ka"]
    , ["dom ka", "dom dom", "di dom", "dim", "di dom"]
    , ["dim", "- tam", "- tam", "- cha", ""]
    , ["", "", "", "", ""]
    , ["", "", "di dom", "dim", "di dom"]
    , ["dim", "dim", "di dom", "dim", "ki Ta ta ka"]
    , ["dom ka", "dom dom", "di dom", "dim", "di dom"]
    , ["dim", "- tam", "- tam", "- cha", ""]
    , ["", "", "", "", ""]
    , ["- nam", "ta ka", "di dom", "dim", "di dom"]
    , ["dim", "dim", "di dom", "dim", "ki Ta ta ka"]
    , ["dom ka ka", "dom dom", "di dom", "dim", "di dom"]
    , ["dim", "- tam", "- tam", "- dim", ""]
    , ["", "", "", "", ""]
    , ["- nam", "ta ka", "di dom", "dim", "di dom"]
    , ["dim", "dim", "di dom", "dim", "ki Ta ta ka"]
    , ["dom ka ka", "dom dom", "di dom", "dim", "di dom"]
    , ["dim", "- tam", "- tam", "- cha", ""]
    , ["", "", "", "", ""]
    , ["cha / ta", "nam ka", "di dom", "dim", "di dom"]
    , ["dim", "dim", "di dom", "dim", "ki Ta ta ka"]
    , ["dom ka", "dom dom", "di dom", "dim", "nam"]
    , ["cha / ta", "ki Ta ki nam", "dom ka Ta di", "Ta ta cha ta", "ki Ta ta ka"]
    , ["", "", "", "", ""]
    , ["dim", "- ka", "di dom", "dim", "nam"]
    , ["dim", "dim", "di dom", "dim", "ki Ta ta ka"]
    , ["dom ka ka", "dom dom", "di dom", "dim", "nam"]
    , ["cha / ta", "ki Ta ki nam", "dom ka Ta di", "Ta ta cha ta", "ki Ta ta ka"]
    , ["", "", "", "", ""]
    , ["dom ka", "dom dom", "di dom", "dim", "nam"]
    , ["cha / ta", "ki Ta ki nam", "dom ka Ta di", "Ta ta cha ta", "ki Ta ta ka"]
    , ["", "", "", "", ""]
    , ["dom ka", "dom dom", "di dom", "dim", "ki Ta ki nam"]
    , ["dom ka Ta di", "Ta ta cha ta", "ki Ta ta ka", "tam", ""]
    , ["ki Ta ki nam", "dom ka Ta di", "Ta ta cha ta", "ki Ta ta ka", "tam - ta ka"]
    , ["Ta ta cha ta", "ki Ta ta ka", "tam - ta ka", "nam ta di", ""]
    , ["", "", "", "", ""]
    , ["tam", "dom", "nam dom", "dim", "nam dom"]
    , ["dim", "dom din", "dim", "dim", "nam"]
    , ["", "", "", "", ""]
    , ["cha / ta", "ki Ta ki nam", "dom ka Ta di", "Ta ta cha ta", "ki Ta ta ka"]
    , ["di - ki Ta", "- ka nam dom", "di", "", "dim"]
    , ["", "cha / ta", "ki Ta ki nam", "dom ka Ta di", "Ta ta cha ta"]
    , ["ki Ta ta ka", "di - ki Ta", "- ka nam dom", "di - ki Ta", "- ka nam dom"]
    , ["di", "", "dim", "", "cha / ta"]
    , ["ki Ta ki nam", "dom ka Ta di", "Ta ta cha ta", "ki Ta ta ka", "di - - di"]
    , ["- ka nam dom", "di - - di", "- ka nam dom", "di - - di", "- ka nam dom"]
    , ["", "", "", "", ""]
    , ["tam", "", "nam dom", "dim", "nam dom"]
    , ["tam", "", "ki Ta - ki", "nam dom tam", "- di -"]
    , ["ki Ta - ki", "nam dom tam", "- di", "Ta - ki Ta", "- ki nam dom"]
    , ["", "", "", "", ""]
    , ["", "", "", "", "di dom dom ka"]
    , ["di", "din din", "nam ka", "nam nam", "din din"]
    , ["nam ka", "Ta ka", "nam ka", "nam nam", "din din"]
    , ["nam", "din din", "nam ka", "din nam", "din"]
    , ["nam ki Ta", "di ta ki Ta", "di ta ki Ta", "di ta ki Ta", "di ta ka"]
    , ["", "", "", "", ""]
    , ["di", "din din", "nam ka", "din nam", "din"]
    , ["nam ki Ta", "di ta ki Ta", "di ta ki Ta", "di ta ki Ta", "di ta ka"]
    , ["di", "din din", "nam ki Ta", "di ta di", "di -"]
    , ["- -", "- di", "di -", "- -", "- di"]
    , ["", "", "", "", ""]
    , ["di", "din din", "nam ka", "nam nam", "din din"]
    , ["di", "din ka", "nam ki ta", "ta ka nam", "din ka"]
    , ["nam ka", "din ka", "nam ka", "nam nam", "din ka"]
    , ["nam ka", "din ka", "cha/dom cha/dom", "- cha/dom", ""]
    , ["", "", "", "", ""]
    , ["dim", "tam", "ka din", "- nam", "- ka"]
    , ["din", "nam", "ka din", "- nam", "- ka"]
    , ["dim", "tam", "ka din", "- nam", "- ka"]
    , ["din / ta", "nam / ta", "ka din", "- nam", "- ka"]
    , ["dim", "tam", "ka din", "- nam", "- ka"]
    , ["dim - tam -", "ka din - nam", "- ka din -", "nam - ka din", "- nam - ka"]
    , ["", "", "", "", ""]
    , ["dim", "tam", "ka din", "- nam", "- ka"]
    , ["dim - tam -", "ka din - nam", "- ka din -", "nam - ka din", "- nam - ka"]
    , ["", "", "", "", ""]
    , ["dim", "tam", "ka din", "- nam", "- ka"]
    , ["dim - tam -", "ka din - nam", "- ka din -", "nam - ka din", "- nam - ka"]
    , ["", "", "", "", ""]
    , ["dim - tam -", "ka dim - tam", "- ka din", "- nam", "- ka"]
    , ["dim - tam -", "ka dim - tam", "- ka din", "- nam", "- ka"]
    , ["dim - tam -", "ka dim - tam", "- ka din", "- nam", "- ka"]
    , ["dim - tam -", "ka dim - tam", "- ka din", "- nam", "- ka"]
    , ["", "", "", "", ""]
    , ["dim - tam -", "ka dim - tam", "- ka dim -", "tam - ka dim", "- tam - ka"]
    , ["din - nam -", "ka din - nam", "- ka din -", "nam - ka din", "- nam - ka"]
    , ["Tisram", "", "", "", ""]
    , ["dim - nam", "- ka din", "- nam -", "ka din -", "nam - ka"]
    , ["dim - nam", "- ka din", "- nam -", "ka din -", "nam - ka"]
    , ["ki Ta ki", "nam dom ki", "Ta ki nam", "dom ki Ta", "ki nam dom"]
    , ["", "", "", "", ""]
    , ["cha", "dim - ka", "tam - ka", "dim - ka", "tam dom ka"]
    , ["din", "nam - ka", "din - ka", "nam - ka", "tam dom ka"]
    , ["dim", "dim - ka", "tam - ka", "dim - ka", "dom dom ka"]
    , ["din", "nam - ka", "din - ka", "nam - ka", "tam dom ka"]
    , ["din", "nam - ka", "din - ka", "nam - ka", "tam dom ka"]
    , ["tam - -", "di - -", "ki Ta di", "nam ta cha", "dom dom ka"]
    , ["", "", "", "", ""]
    , ["dim", "dim - ka", "tam - ka", "dim - ka", "tam dom ka"]
    , ["tam - -", "di - -", "ki Ta di", "nam ta cha", "dom dom ka"]
    , ["", "", "", "", ""]
    , ["din", "din - ka", "nam - ka", "din - ka", "tam dom ka"]
    , ["tam - -", "di - -", "ki Ta di", "nam ta cha", "dom dom ka"]
    , ["Korvai", "", "", "", ""]
    , ["di - -", "ki Ta di", "nam ta cha", "dom dom ka", "dom dom ka"]
    , ["tam - -", "di - -", "ki Ta di", "nam ta cha", "dom dom ka"]
    , ["dom dom ka", "tam - -", "di - -", "ki Ta di", "nam ta cha"]
    , ["dom dom ka", "dom dom ka", "tam - -", "ki Ta ki", "nam dom ki"]
    , ["- Ta -", "ki nam dom", "ki Ta ki", "nam dom ki", "- Ta -"]
    , ["ki nam dom", "ki Ta ki", "nam dom ki", "- Ta -", "ki nam dom"]
    , ["", "", "", "", ""]
    , ["di - -", "ki Ta di", "nam ta cha", "dom dom ka", "dom dom -"]
    , ["dom dom -", "dom nam -", "ki Ta di", "nam ta cha", "dom dom ka"]
    , ["dom dom -", "dom dom -", "dom nam -", "ki Ta di", "nam ta cha"]
    , ["dom dom ka", "dom dom -", "dom dom -", "ki Ta ki", "nam dom ki"]
    , ["- dim -", "ki nam dom", "ki Ta ki", "nam dom ki", "- dim -"]
    , ["ki nam dom", "ki Ta ki", "nam dom ki", "- dim -", "ki nam dom"]
    , ["", "", "", "", ""]
    , ["di - -", "ki Ta di", "nam ta cha", "dom dom ka", "dom dom ka"]
    , ["dom dom ka", "dom nam -", "ki Ta di", "nam ta cha", "dom dom ka"]
    , ["dom dom ka", "dom dom ka", "dom nam -", "ki Ta di", "nam ta cha"]
    , ["dom dom ka", "dom dom ka", "dom dom ka", "ki Ta ki", "nam dom ki"]
    , ["- dim -", "ki nam dom", "ki Ta ki", "nam dom ki", "- dim -"]
    , ["ki nam dom", "ki Ta ki", "nam dom ki", "- dim -", "ki nam dom"]
    , ["Chatusram", "", "", "", ""]
    , ["cha", "- tam", "- tam", "- tam", "- ka"]
    , ["din", "- nam", "- nam", "- nam", "- ka"]
    , ["cha -", "- ki Ta", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["din -", "- nam ta", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["", "", "", "", ""]
    , ["din -", "- ki Ta", "ki Ta ta ka", "din -", "- ki Ta"]
    , ["ki Ta ta ka", "din -", "- ki Ta", "ki Ta ta ka", "tam ka"]
    , ["din -", "- ki Ta", "ki Ta ta ka", "din -", "- ki Ta"]
    , ["ki Ta ta ka", "din -", "tam - ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["Farans", "", "", "", ""]
    , ["tam", "Ta - dom -", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["tam tam", "ta lam - dom", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["tam tam", "ta ka dom -", "tam - ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["tam tam", "tam - ta ka", "nam dom dom ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["", "", "", "", ""]
    , ["tam - ta ka", "Ta - dom -", "tam - ta ka", "Ta - dom -", "ki Ta ta ka"]
    , ["tam - ta ka", "Ta ta cha ta", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["", "", "", "", ""]
    , ["ta lam - ka", "dom - ka -", "ta lam -  ka", "dom  - ki Ta", "ki Ta ta ka"]
    , ["tam tam", "ta lam - ka", "dom - ta lam", "- ka dom -", "ta lam -  ka"]
    , ["Mora", "", "", "", ""]
    , ["ki Ta", "ki nam", "dom di", "- tam", "- ki Ta"]
    , ["ki Ta ta ka", "Ta dom", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["dom ki", "Ta ki", "nam dom", "di tam", "- ki Ta"]
    , ["ki Ta ta ka", "tam ka", "dom - - ka", "tam ka", "dom"]
    , ["", "", "", "", ""]
    , ["ki Ta", "ki nam", "dom di", "- tam", "- ki Ta"]
    , ["ki Ta ta ka", "Ta dom", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["dom ki", "Ta ki", "nam dom", "di tam", "- ki Ta"]
    , ["ki Ta ta ka", "tam ka", "dom - - ka", "tam ka", "dom"]
    , ["", "", "", "", ""]
    , ["ki Ta", "ki nam", "dom di", "- tam", "- ki Ta"]
    , ["ki Ta ta ka", "Ta dom", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["dom ki", "Ta ki", "nam dom", "di tam", "- ki Ta"]
    , ["ki Ta ta ka", "dom ka", "dom", "ki Ta", "ki nam"]
    , ["dom di", "- tam", "- ki Ta", "ki Ta ta ka", "dom ka"]
    , ["dom", "ki Ta", "ki nam", "dom di", "- tam"]
    , ["- ki Ta", "ki Ta ta ka", "ta lam ka", "dom ka", "tam -"]
    , ["ta lam ka", "dom ka", "tam -", "ta lam ka", "dom ka"]
    , ["Korvai", "", "", "", ""]
    , ["ta ka", "Ta di", "ki Ta dom", "ta ka", "Ta di"]
    , ["ki Ta dom", "ta ka", "Ta di", "ki Ta dom", "di Ta"]
    , ["di ki Ta", "dom di", "Ta di", "ki Ta dom", "di Ta"]
    , ["di ki Ta", "dom di", "di ki Ta", "dom di", "di ki Ta"]
    , ["dom di", "di ki Ta", "dom di", "ki Ta dom", "di ki Ta"]
    , ["dom di", "ki Ta dom", "ki Ta dom", "ki Ta dom", "ki Ta dom"]
    , ["tam", "tam", "ki Ta dom", "ki Ta dom", "ki Ta dom"]
    , ["tam", "tam", "ki Ta dom", "ki Ta dom", "ki Ta dom"]
    , ["", "", "", "", ""]
    , ["ta ka", "Ta di", "ki Ta dom", "ta ka", "Ta di"]
    , ["ki Ta dom", "ta ka", "Ta di", "ki Ta dom", "di Ta"]
    , ["di ki Ta", "dom di", "Ta di", "ki Ta dom", "di Ta"]
    , ["di ki Ta", "dom di", "di ki Ta", "dom di", "di ki Ta"]
    , ["dom di", "di ki Ta", "dom di", "ki Ta dom", "di ki Ta"]
    , ["dom di", "ki Ta dom", "ki Ta dom", "ki Ta dom", "ki Ta dom"]
    , ["tam", "tam", "ki Ta dom", "ki Ta dom", "ki Ta dom"]
    , ["dim", "dim", "ki Ta dom", "ki Ta dom", "ki Ta dom"]
    , ["", "", "", "", ""]
    , ["ta ka", "Ta di", "ki Ta dom", "ta ka", "Ta di"]
    , ["ki Ta dom", "ta ka", "Ta di", "ki Ta dom", "di Ta"]
    , ["di ki Ta", "dom di", "Ta di", "ki Ta dom", "di Ta"]
    , ["di ki Ta", "dom di", "di ki Ta", "dom di", "di ki Ta"]
    , ["dom di", "di ki Ta", "dom di", "ki Ta dom", "di ki Ta"]
    , ["dom di", "ki Ta dom", "ki Ta dom", "ki Ta dom", "ki Ta dom"]
    , ["tam tam", "tam -", "ki Ta dom", "ki Ta dom", "ki Ta dom"]
    , ["tam tam", "tam -", "ki Ta dom", "ki Ta dom", "ki Ta dom"]
    , ["", "", "", "", ""]
    , ["tam", "", "nam dom", "dim", "nam dom"]
    , ["di ki Ta", "dom di", "ki Ta dom", "di ki Ta", "dom tam"]
    , ["- di", "di ki Ta", "dom di", "di ki Ta", "dom di"]
    , ["di ki Ta", "dom tam", "- dom ka", "Ta ta cha ta", "ki Ta dom -"]
    , ["dom ka Ta ta", "cha ta ki Ta", "dom - dom ka", "Ta ta cha ta", "ki Ta dom -"]
    ]

misraRowsRaw :: [[Text]]
misraRowsRaw =
    [ ["Chatusram", "", "", "", "", "", ""]
    , ["di", "dim", "", "tam", "tam", "din", "nam ka"]
    , ["di dom", "dim", "", "tam", "tam", "cha", ""]
    , ["", "dim", "", "tam", "tam", "dim", "nam ka"]
    , ["di dom", "dim", "", "tam", "tam", "cha", ""]
    , ["di di", "dom dim", "", "tam", "tam", "dim", "nam ka"]
    , ["di dom", "dim", "", "tam", "tam", "cha", ""]
    , ["dom ka ka dom", "dom", "", "tam", "tam", "dim", "nam ka"]
    , ["di dom", "dom ka", "dom di", "dom dom", "ka dom", "di dom", "dom ka"]
    , ["di -", "- dim", "", "tam", "tam", "dim", "nam ka"]
    , ["di dom", "dom ka", "dom di", "dom dom", "ka dom", "di dom", "dom ka"]
    , ["di di", "dom dim", "", "tam ka", "tam ka", "dim", "nam ka"]
    , ["di dom", "dom ka", "dom di", "dom dom", "ka dom", "di dom", "dom ka"]
    , ["di dom dom ka", "di dom dom ka", "dom di dom", "dom ka di dom", "dom ka dom", "di dom dom ka", "di dom dom ka"]
    , ["di dom dom ka", "dom di dom", "dom ka di dom", "dom ka dom", "di dom dom ka", "di dom dom ka", "di dom dom ka"]
    , ["", "", "", "", "", "", ""]
    , ["cha", "", "cha -", "- cha", "- -", "cha-", "- cha"]
    , ["- -", "cha -", "- cha", "- -", "cha -", "-  nam - ta ka", "Ta ta cha Ta ki Ta Ta ka"]
    , ["", "", "", "", "", "", ""]
    , ["dim", "nam nam", "din", "", "din", "nam nam", "din"]
    , ["cha", "nam nam", "din", "nam", "din", "nam nam", "din"]
    , ["tam", "tam tam", "dim", "", "dim", "tam tam", "dim"]
    , ["cha", "nam nam", "din", "nam din", "din dim", "nam dim", "dim din"]
    , ["nam din", "din nam", "din din", "nam din", "din dim", "nam dim", "dim din"]
    , ["", "", "", "", "", "", ""]
    , ["din -", "nam ki Ta", "ta ka nam", "din ka", "nam ki Ta", "ta ka nam", "din ka"]
    , ["dim -", "nam ki Ta", "ta ka nam", "din ka", "nam ki Ta", "ta ka nam", "din ka"]
    , ["Ta di", "nam ki Ta", "ta ka nam", "din ka", "nam ki Ta", "ta ka nam", "din ka"]
    , ["ta -", "di -", "di dom", "dom ka", "tam -", "ta -di -", "di dom dom ka"]
    , ["dim -", "tam ki Ta", "ta ka tam", "dim ka", "tam ki Ta", "ta ka tam", "dim ka"]
    , ["ta -", "di -", "di dom", "dom ka", "tam -", "ta -di -", "di dom dom ka"]
    , ["Korvai", "", "", "", "", "", ""]
    , ["", "", "", "", "", "", ""]
    , ["ta -", "di -", "di dom", "dom ka", "ta -di -", "di dom dom ka", "di -di dom"]
    , ["dom ka di dom", "dom ka di", "- di", "dom dom", "ka di -", "di dom dom ka", "di dom dom ka"]
    , ["di dom", "dom ka", "di dom dom ka", "ki  -", "- dim", "- -", "ki -"]
    , ["- nam", "- -", "dom -", "-ki", "- dim", "- ki", "- nam"]
    , ["- dom", "- ki", "- dim", "- ki", "- nam", "- dom", "- ki"]
    , ["Ta ki", "nam dom", "ki Ta", "ki nam", "dom ki", "Ta ki", "nam dom"]
    , ["", "", "", "", "", "", ""]
    , ["ta -", "di -", "di dom", "dom ka", "ta -di -", "di dom dom ka", "di -di dom"]
    , ["dom ka di dom", "dom ka di", "- di", "dom dom", "ka di -", "di dom dom ka", "di dom dom ka"]
    , ["di dom", "dom ka", "di dom dom ka", "ki  -", "- dim", "- -", "ki -"]
    , ["- nam", "- -", "dom -", "- ki", "- dim", "- ki", "- nam"]
    , ["- dom", "- ki", "- dim", "- ki", "- nam", "- dom", "- ki Ta"]
    , ["ki Ta ta ka", "ta ka dom", "ki Ta ki Ta", "ta ka ta ka", "dom ki Ta", "ki Ta ta ka", "ta ka dom"]
    , ["", "", "", "", "", "", ""]
    , ["ta -", "di -", "di dom", "dom ka", "ta -di -", "di dom dom ka", "di -di dom"]
    , ["dom ka di dom", "dom ka di", "- di", "dom dom", "ka di -", "di dom dom ka", "di dom dom ka"]
    , ["di dom", "dom ka", "di dom dom ka", "ki  -", "- dim", "- -", "ki -"]
    , ["- nam", "- -", "dom -", "-ki", "- dim", "- ki", "- nam"]
    , ["- dom", "- ki", "- dim", "- ki", "- nam", "- dom", "- dom ka"]
    , ["Ta ta cha ta", "ki Ta dom", "dom ka Ta ta", "cha ta ki Ta", "dom dom ka", "Ta ta cha ta", "ki Ta dom"]
    , ["", "", "", "", "", "", ""]
    , ["tam", "- di", "di dom", "dom ka", "di dom", "dom ka", "ki Ta dom dom"]
    , ["di", "tam -", "ki Ta dom dom", "di", "tam di", "- tam", "di"]
    , ["", "", "", "", "", "", ""]
    , ["tam din", "- ka", "nam ka", "nam din", "- ka", "nam dom", "ki Ta ta ka"]
    , ["tam din", "- ka", "nam ka", "nam din", "- ka", "nam dom dom", "ki Ta ta ka"]
    , ["tam din", "- ka", "nam ka", "nam din", "- ka", "Ta di dom dom", "di dom dom ka"]
    , ["tam din", "- ka", "nam ka", "nam din", "- ka", "dom ka dom dom", "ka dom dom ka"]
    -- , ["", "", "", "", "Tisram", "", ""]
    , ["tam din", "- ka", "nam ka", "nam din", "nam ka din", "nam ka din", "nam ka din"]
    -- , ["", "", "", "", "Tisram", "", ""]
    , ["tam din", "- ka", "nam ka", "nam din", "nam ka din", "nam ka din", "nam ka din"]
    , ["", "", "", "", "", "", ""]
    , ["Tisra Nadai", "", "", "", "", "", ""]
    , ["", "", "", "", "", "", ""]
    , ["tam ka din", "nam ka din", "nam ka din", "nam ka din", "nam ka din", "nam ka din", "nam ka din"]
    , ["tam ka din", "nam ka din", "nam ka din", "nam/ta ka din", "nam ka din", "nam/ta ka din", "nam ka din"]
    , ["tam ka din", "tam ka din", "nam ka din", "nam/ta ka din", "nam ka din", "nam/ta ka din", "nam ka din"]
    , ["ki - Ta", "- ki nam", "dom ki -", "Ta - ki", "nam dom ki", "- Ta -", "ki nam dom"]
    , ["tam ka din", "tam ka din", "nam ka din", "nam/ta ka din", "nam ka din", "nam/ta ka din", "nam ka din"]
    , ["ki - Ta", "- ki nam", "dom ki -", "Ta - ki", "nam dom ki", "- Ta -", "ki nam dom"]
    , ["ki/ta - dim", "- ki nam", "dom ki -", "dim - ki", "nam dom ki", "- dim -", "ki nam dom"]
    , ["ki/ta - dim", "- ki Ta ki Ta", "dom ki -", "dim - ki Ta", "ki Ta dom ki", "- dim -", "ki Ta ki Ta dom"]
    , ["", "", "", "", "", "", ""]
    , ["din", "ki Ta ka", "din", "cha", "ki Ta ka", "din", "ki Ta ka"]
    , ["din", "ki Ta ka", "din", "din", "ki Ta ka", "din", "di ta ka"]
    , ["dom", "ki Ta ka", "din", "cha", "dom dom ka", "dom dom ka", "dom dom ka"]
    , ["din", "ki Ta ka", "din", "di - ta", "ki Ta di", "nam - ta ka Ta ta", "cha ta ki Ta ta ka"]
    , ["din", "ki Ta ka", "din", "di", "ki Ta di", "nam ta cha", "dom dom ka"]
    , ["dom dom ka", "dom dom ka", "din", "di", "ki Ta di", "nam ta cha", "dom dom ka"]
    , ["", "", "", "", "", "", ""]
    , ["Korvai", "", "", "", "", "", ""]
    , ["", "", "", "", "", "", ""]
    , ["di - -", "ki Ta di", "nam ta cha", "dom dom ka", "dom dom ka", "tam - -", "di - -"]
    , ["ki Ta di", "nam ta cha", "dom dom ka", "dom dom ka", "tam - -", "di - -", "ki Ta di"]
    , ["nam ta cha", "dom dom ka", "dom dom ka", "tam - -", "ki Ta -", "ki nam dom", "tam"]
    , ["", "ki Ta -", "ki nam dom", "tam", "", "ki Ta -", "ki nam dom"]
    , ["", "", "", "", "", "", ""]
    , ["di - -", "ki Ta di", "nam ta cha", "dom dom ka", "dom dom ka", "tam - -", "dom nam -"]
    , ["ki Ta di", "nam ta cha", "dom dom ka", "dom dom ka", "tam - -", "dom nam -", "ki Ta di"]
    , ["nam ta cha", "dom dom ka", "dom dom ka", "tam - -", "ki Ta -", "di ki Ta dom", "cha"]
    , ["", "ki Ta -", "di ki Ta dom", "cha", "", "ki Ta -", "di ki Ta dom"]
    , ["", "", "", "", "", "", ""]
    , ["di - -", "ki Ta di", "nam ta cha", "dom dom ka", "dom dom ka", "dom dom ka", "dom nam -"]
    , ["ki Ta di", "nam ta cha", "dom dom ka", "dom dom ka", "dom dom ka", "dom nam -", "ki Ta di"]
    , ["nam ta cha", "dom dom ka", "dom dom ka", "dom dom ka", "ki Ta -", "di ki Ta dom", "cha"]
    , ["", "ki Ta -", "di ki Ta dom", "ki Ta -", "di ki Ta dom", "ki Ta -", "di ki Ta dom"]
    , ["", "", "", "", "", "", ""]
    , ["Chaturshra Nadai", "", "", "", "", "", ""]
    , ["", "", "", "", "", "", ""]
    , ["cha", "- di", "ta ka", "Ta di", "tam - ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["tam - ka din", "- ka tam -", "ka din - ka", "tam - ka din", "- ka Ta di", "Ta di dom dom", "ki Ta dom ka dom / Ta ka"]
    , ["tam - ka din", "- ka nam -", "ka din - ka", "nam - ka din", "- ka Ta di", "Ta di dom dom", "ki Ta dom ka dom / Ta ka"]
    , ["dom nam ka din", "- ka tam -", "ka din - ka", "tam - ka din", "- ka Ta di", "Ta di dom dom", "ki Ta dom ka dom / Ta ka"]
    , ["Ta di dom dom", "ki Ta dom ka dom / Ta ka", "Ta di dom dom", "ki Ta dom ka dom / Ta ka", "Ta di dom dom", "ki Ta dom ka dom / Ta ka", "Ta di dom dom"]
    , ["ki Ta dom ka dom / Ta ka", "tam", "Ta di dom dom", "ki Ta dom ka dom / Ta ka", "tam", "Ta di dom dom", "ki Ta dom ka dom / Ta ka"]
    , ["", "", "", "", "", "", ""]
    , ["tam -", "- ki Ta", "ki Ta ta ka", "din -", "- ki Ta", "ki Ta ta ka", "din -"]
    , ["- ki Ta", "ki Ta ta ka", "din -", "- ki Ta", "ki Ta ta ka", "tam -", "ka -"]
    , ["", "", "", "", "", "", ""]
    , ["tam -", "- ki Ta", "ki Ta ta ka", "din -", "- ki Ta", "ki Ta ta ka", "din -"]
    , ["- ki Ta", "ki Ta ta ka", "din -", "- ki Ta", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["", "", "", "", "", "", ""]
    , ["Farans", "", "", "", "", "", ""]
    , ["", "", "", "", "", "", ""]
    , ["Ta - dom -", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka", "tam - ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["ta lam - dom", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka", "tam - ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["tam - ta ka", "Ta - dom -", "ki Ta ta ka", "tam - ta ka", "Ta - dom -", "Ta ta cha ta", "ki Ta ta ka"]
    , ["ta lam - ka", "dom - ta lam", "- ka dom -", "ta lam - ka", "dom -dom ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["Mora", "", "", "", "", "", ""]
    , ["", "", "", "", "", "", ""]
    , ["cha ta ki Ta", "dom di", "- tam", "- nam ta", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["Ta ta cha ta", "ki Ta dom", "di tam", "- nam ta", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["cha ta ki Ta", "dom di", "- tam", "- nam ta", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["ta lam - ka", "dom ka", "tam -", "- ta lam", "- ka dom", "ka tam", "- -"]
    , ["", "", "", "", "", "", ""]
    , ["cha ta ki Ta", "dom di", "- tam", "- nam ta", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["Ta ta cha ta", "ki Ta dom", "di tam", "- nam ta", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["cha ta ki Ta", "dom di", "- tam", "- nam ta", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["ta lam - ka", "dom ka", "tam -", "- ta lam", "- ka dom", "ka tam", "- -"]
    , ["", "", "", "", "", "", ""]
    , ["cha ta ki Ta", "dom di", "- tam", "- nam ta", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["Ta ta cha ta", "ki Ta dom", "di tam", "- nam ta", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["cha ta ki Ta", "dom di", "- tam", "- nam ta", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["ta lam - ka", "dom ka", "tam -", "- cha ta", "ki Ta dom", "di", "tam"]
    , ["nam ta ki Ta", "ta ka Ta ta", "cha ta ki Ta", "ta ka ta lam", "- ka dom", "ka tam", "- -"]
    , ["Ta ta cha ta", "ki Ta dom", "di tam", "- nam ta", "ki Ta ta ka", "Ta ta cha ta", "ki Ta ta ka"]
    , ["", "", "", "", "", "", ""]
    , ["ta lam - ka", "dom ka", "ta lam - ka", "dom ka", "tam ka", "ta lam - ka", "dom ka"]
    , ["ta lam - ka", "dom ka", "tam ta ka", "ta lam - ka", "dom ka", "ta lam - ka", "dom ka"]
    , ["", "", "", "", "", "", ""]
    , ["Korvai", "", "", "", "", "", ""]
    , ["", "", "", "", "", "", ""]
    , ["di -", "dim -", "di dim", "- di", "dom dim", "- di", "dim -"]
    , ["dom ka", "dim -", "di dim", "- ki", "- -", "- dim", "- -"]
    , ["- ki", "- -", "-  nam", "- -", "-  dom", "- -", "- ki"]
    , ["- dim", "- ki", "- nam", "- dom", "- ki", "Ta ki", "nam dom"]
    , ["", "", "", "", "", "", ""]
    , ["di -", "dim -", "di dim", "- di", "dom dim", "- di", "dim -"]
    , ["dom ka", "dim -", "di dim", "- ki", "- -", "- dim", "- -"]
    , ["- ki", "- -", "-  nam", "- -", "-  dom", "- -", "- ki"]
    , ["- dim", "- ki", "- nam", "- dom", "- ki", "Ta ki", "nam dom"]
    , ["", "", "", "", "", "", ""]
    , ["di -", "dim -", "di dim", "- di", "dom dim", "- di", "dim -"]
    , ["dom ka", "dim -", "di dim", "- ki", "- -", "- dim", "- -"]
    , ["- ki", "- -", "-  nam", "- -", "-  dom", "- -", "- ki"]
    , ["Ta ki", "nam dom", "ki Ta", "ki nam", "dom ki", "Ta ki", "nam dom"]
    , ["", "", "", "", "", "", ""]
    , ["dim", "", "", "", "di di", "di dom", "dom ka"]
    , ["dim", "tam dim", "- tam", "dim -", "tam dim", "- tam", "dim -"]
    , ["", "", "", "", "", "", ""]
    , ["Tisram", "", "", "", "", "", ""]
    , ["nam ta cha", "dom dom ka", "dim - -", "nam ta cha", "dom dom ka", "nam ta cha", "dom dom ka"]
    , ["dim - -", "nam ta cha", "dom dom ka", "nam ta cha", "dom dom ka", "dom dom ka", "dom dom ka"]
    , ["dim - -", "", "", "", "", "", ""]
    ]

rupakaRowsRaw :: [[Text]]
rupakaRowsRaw =
    [ ["Chatusram", "", "", "", ""]
    , ["", "", "", "cha ta ki Ta dom - cha ta", "ki Ta dom - cha ta ki Ta", "dom - cha ta ki Ta dom -"]
    , ["cha / ta - dom ka", "di dom tam tam", "- dom tam ka", "dim - dim -", "nam dom dim -", "nam dom dom ka"]
    , ["cha / ta - dom ka", "di dom dom ka", "dom ka ka -", "dim - dim -", "nam dom dim -", "nam dom dom ka"]
    , ["cha - dom dom", "di dom dim -", "ki Ta dom ka dom Ta dom ka", "dom - dom -", "di dom dim -", "ki Ta dom ka dom Ta dom ka"]
    , ["tam - dom -", "di dom dim -", "ki Ta dom ka dom Ta dom ka", "tam - ki Ta dom ka", "dom Ta dom ka tam - dom Ta", "dom ka tam - dom Ta dom ka"]
    , ["", "", "", "", "", ""]
    , ["cha / ta - di di", "di dom dim -", "dim - tam -", "din - din -", "nam nam din -", "din - nam -"]
    , ["- - cha -", "- - cha -", "- - cha -", "- - cha -", "- - cha -", "- dom dom ka"]
    , ["cha / ta - di di", "di dom dim -", "dim - tam -", "dim", "- - ki Ta", "di ki Ta - ki nam dom"]
    , ["dim", "- - ki Ta", "di ki Ta - ki nam dom", "cha cha - -", "- - ki Ta", "di ki Ta - ki nam dom"]
    , ["cha", "- - ki Ta", "di ki Ta - ki nam dom", "ki Ta di ki Ta", "- ki nam dom ki Ta", "di ki Ta - ki nam dom"]
    , ["", "", "", "", "", ""]
    , ["Korvai", "", "", "", "", ""]
    , ["", "", "", "", "", ""]
    , ["ki Ta di ki Ta", "- ki nam dom dim -", "- di di ki Ta", "- ki nam dom dim -", "- di ki Ta - ki", "nam dom dim - -"]
    , ["ki Ta - ki nam dom dim", "- - ki Ta - ki", "nam dom ki Ta - ki nam dom", "dim - - ki Ta", "- ki nam dom ki Ta - ki", "nam dom ki Ta - ki nam dom"]
    , ["", "", "", "", "", ""]
    , ["ki / ta Ta / ta di / ta ki Ta", "- ki nam dom dim dim", "- di / ta di / ta ki Ta", "- ki nam dom dim dim", "- di / ta ki Ta - ki", "nam dom dim dim -"]
    , ["ki Ta - ki nam dom dim", "dim - ki Ta - ki", "nam dom ki Ta - ki nam dom", "dim dim - ki Ta", "- ki nam dom ki Ta - ki", "nam dom ki Ta - ki nam dom"]
    , ["", "", "", "", "", ""]
    , ["ki / ta Ta / ta di / ta ki Ta", "- ki nam dom dim dim", "dim di / ta di / ta ki Ta", "- ki nam dom dim dim", "dim di / ta ki Ta - ki", "nam dom dim dim dim"]
    , ["ki Ta - ki nam dom dim", "dim dim ki Ta - ki", "nam dom ki Ta - ki nam dom", "dim dim dim ki Ta", "dom ki Ta dom ki Ta dom ki", "Ta dom ki Ta dom ki Ta dom"]
    , ["", "", "", "", "", ""]
    , ["di", "", "nam ka dom dom di -", "tam - nam ka dom dom", "di - tam di", "- tam di -"]
    , ["", "", "", "", "", ""]
    , ["tam - dim -", "nam ka nam -", "din", "- - din -", "nam ka nam -", "din"]
    , ["tam - dim -", "nam ka nam -", "din", "- - din / ta -", "nam ka nam -", "din - nam ka"]
    , ["tam ka dim -", "nam ka nam -", "din", "- - din / ta -", "nam ka nam -", "din - nam ka"]
    , ["dom nam - ka dim ka", "tam ka tam -", "dim", "- - dim -", "tam ka tam dom", "ka tam dom ka"]
    , ["", "", "", "", "", ""]
    , ["tam - nam nam", "din ka nam ka", "nam nam din ka", "- - nam nam", "din ka nam ka", "nam nam din ka"]
    , ["tam - tam tam", "dim ka tam ka", "tam tam dim ka", "- - tam tam", "dim ka tam ka", "tam tam dim ka"]
    , ["", "", "", "", "", ""]
    , ["nam ki Ta ta ka nam", "din ka nam ka", "nam nam din ka", "- - nam nam", "din ka nam ka", "nam nam din ka"]
    , ["nam ta ki Ta ta ka nam", "din ka nam ka", "nam nam din ka", "nam ta nam din - ta nam din", "- ta nam din - ta nam din", "- ta nam din - dom tam -"]
    , ["cha - cha -", "din ka nam ka", "nam nam din ka", "nam ta nam din - ta nam din", "- ta nam din - ta nam din", "- ta nam din - dom tam -"]
    , ["cha - cha -", "din ka nam ka", "nam ta nam din - dom tam -", "cha -  nam ta nam din", "- dom tam - cha - - ka", "din dom dim ka"]
    , ["", "", "", "", "", ""]
    , ["tam - ka dim", "- ka nam -", "ka din - ka", "nam - ka din", "- ka nam -", "ka din - ka"]
    , ["tam - ka dim", "- ka nam -", "ka din - ka", "tam dom ka tam", "dom ka tam dom", "ka tam dom ka"]
    , ["tam - ka dim", "- ka nam -", "ka din - ka", "dom - ki Ta ta ka dom -", "ki Ta ta ka dom - ki Ta", "ta ka dom - ki Ta ta ka"]
    , ["", "", "", "", "", ""]
    -- , ["", "", "", "Tisram", "", ""]
    , ["tam - ka dim", "- ka nam -", "ka dim - ka", "tam - ka dim - ka", "tam - ka dim - ka", "tam - ka dim - ka"]
    , ["", "", "", "", "", ""]
    -- , ["", "", "", "Tisram", "", ""]
    , ["tam - ka dim", "- ka nam -", "ka dim - ka", "tam - ka dim - ka", "tam - ka dim - ka", "tam - ka dim - ka"]
    , ["Tisra Nadai", "", "", "", "", ""]
    , ["tam - ka dim - ka", "nam - ka din - ka", "nam - ka din - ka", "tam - ka dim - ka", "nam - ka din - ka", "nam - ka din - ka"]
    , ["dom dom ka dim - ka", "nam - ka din - ka", "nam - ka din - ka", "ta ta ka din - ka", "nam - ka din - ka", "nam - ka din - ka"]
    , ["dom dom ka dim - ka", "nam - ka din - ka", "ta ta ka din - ka", "nam - ka din - ka", "dom dom ka dim - ka", "nam - ka din - ka"]
    , ["dom dom ka dim - ka", "ta ta ka din - ka", "dom dom ka dim - ka", "ta ta ka din - ka", "dom dom ka ta ta ka", "dom dom ka ta ta ka"]
    , ["tam - ki Ta dom ka dom Ta di", "nam - ki Ta dom ka dom Ta di", "tam - ki Ta dom ka dom Ta di", "nam - ki Ta dom ka dom Ta di", "tam - ki Ta dom ka dom Ta di", "nam - ki Ta dom ka dom Ta di"]
    , ["tam - ki Ta dom ka dom Ta di", "nam - ki Ta dom ka dom Ta di", "tam - ki Ta dom ka dom Ta di", "nam - ki Ta dom ka dom Ta di", "tam - ki Ta dom ka dom Ta di", "nam - ki Ta dom ka dom Ta di"]
    , ["dom Ta di tam - ki Ta dom ka", "ta Ta di nam - ki Ta dom ka", "dom Ta di tam - ki Ta dom ka", "ta Ta di nam - ki Ta dom ka", "dom Ta di tam - ki Ta dom ka", "ta Ta di nam - ki Ta dom ka"]
    , ["", "", "", "", "", ""]
    , ["", "", "", "", "", ""]
    , ["Korvai", "", "", "", "", ""]
    , ["", "", "", "", "", ""]
    , ["di - - ki Ta di", "nam ta cha dom dom ka", "dom dom ka dom - -", "di - - ki Ta di", "nam ta cha dom dom ka", "dom dom ka dom - -"]
    , ["di - - ki Ta di", "nam ta cha dom dom ka", "dom dom ka dom - -", "ki Ta ki nam dom ki", "Ta ki nam dom ki Ta", "ki nam dom di - -"]
    , ["ki Ta di nam ta cha", "dom dom ka dom dom ka", "dom - - di - -", "ki Ta di nam ta cha", "dom dom ka dom dom ka", "dom - - di - -"]
    , ["ki Ta di nam ta cha", "dom dom ka dom dom ka", "dom - - ki Ta -", "ki nam dom ki Ta -", "ki nam dom ki Ta -", "ki nam dom di - -"]
    , ["ki Ta di nam ta cha", "dom dom ka dom dom ka", "dom - - di - -", "ki Ta di nam ta cha", "dom dom ka dom dom ka", "dom - - di - -"]
    , ["ki Ta di nam ta cha", "dom dom ka dom dom ka", "dom - - ki - Ta", "- ki nam dom ki -", "Ta - ki nam dom ki", "-Ta - ki nam dom"]
    , ["Chaturshram", "", "", "", "", ""]
    , ["din - - nam", "ki Ta ta ka din -", "- nam ki Ta ta ka", "din - - nam", "ki Ta ta ka dim dim", "- tam ki Ta ta ka"]
    , ["dim - - nam ta", "ki Ta ta ka din -", "- nam ta ki Ta ta ka", "din - - nam ta", "ki Ta ta ka dim dim", "- tam ki Ta ta ka"]
    , ["Ta / dom di dom ka Ta di nam ta", "ki Ta ta ka Ta / dom di dom ka", "Ta di nam ta ki Ta ta ka", "Ta / dom di dom ka Ta di nam ta", "ki Ta ta ka tam - ta ka", "Ta ta cha ta ki Ta ta ka"]
    , ["", "", "", "", "", ""]
    , ["Farans", "", "", "", "", ""]
    , ["", "", "", "", "", ""]
    , ["Ta - dom - ki Ta ta ka", "Ta ta cha ta ki Ta ta ka", "Ta ta cha ta ki Ta ta ka", "ta lam - dom ki Ta ta ka", "Ta ta cha ta ki Ta ta ka", "Ta ta cha ta ki Ta ta ka"]
    , ["ta ka dom - tam - ta ka", "Ta ta cha ta ki Ta ta ka", "Ta ta cha ta ki Ta ta ka", "tam - ta ka nam dom dom ka", "Ta ta cha ta ki Ta ta ka", "Ta ta cha ta ki Ta ta ka"]
    , ["tam - ta ka Ta - dom -", "ki Ta ta ka tam - ta ka", "Ta - dom -  ki Ta ta ka", "tam - ta ka Ta ta cha ta", "ki Ta ta ka tam - ta ka", "Ta ta cha ta ki Ta ta ka"]
    , ["ta lam - ka dom - ki Ta", "ki Ta ta ka ta lam - ka", "dom - ki Ta ki Ta ta ka", "ta lam - ka dom - ta lam", "- ka dom - ta lam - ka", "dom - ta lam - ka dom -"]
    , ["", "", "", "", "", ""]
    , ["Mora", "", "", "", "", ""]
    , ["", "", "", "", "", ""]
    , ["tam - ta ka Ta ta cha ta", "ki Ta ta ka dom ka Ta ta", "cha - tam - ta ka", "Ta ta cha ta ki Ta ta ka", "dom ka tam - - ka", "dom ka tam -"]
    , ["tam - ta ka Ta ta cha ta", "ki Ta ta ka dom ka Ta ta", "cha - tam - ta ka", "Ta ta cha ta ki Ta ta ka", "dom ka tam - - ka", "dom ka tam -"]
    , ["tam - ta ka Ta ta cha ta", "ki Ta ta ka dom ka Ta ta", "cha - tam - ta ka", "Ta ta cha ta ki Ta ta ka", "ta lam - ka dom -", "tam - ta ka Ta ta cha ta"]
    , ["ki Ta ta ka ta lam - ka", "dom - tam - ta ka", "Ta ta cha ta ki Ta ta ka", "ta lam - ka dom ka", "ta lam - ka dom ka", "ta lam - ka dom ka"]
    , ["Korvai", "", "", "", "", ""]
    , ["", "", "", "", "", ""]
    , ["ki - Ta -", "di dom dim - tam", "- di - di dom", "dim- tam -", "di dom dim - tam", "- di/ta di/ta di/ta"]
    , ["- ki Ta ki", "nam dom di/ta di/ta", "di/ta - ki Ta", "ki nam dom di/ta", "di/ta di/ta - ki", "Ta ki nam dom"]
    , ["", "", "", "", "", ""]
    , ["ki - Ta -", "di dom dim- tam", "- di - di dom", "dim- tam -", "di dom dim - tam", "- di/ta di/ta di/ta"]
    , ["- ki Ta ki", "nam dom di/ta di/ta", "di/ta - ki Ta", "ki nam dom di/ta", "di/ta di/ta - ki", "Ta ki nam dom"]
    , ["", "", "", "", "", ""]
    , ["ki - Ta -", "di dom dim- tam", "- di - di dom", "dim- tam -", "di dom dim - tam", "- dom ka Ta ta cha"]
    , ["- ki Ta ki", "nam dom dom ka Ta ta", "cha - ki Ta", "ki nam dom dom ka", "Ta ta cha - ki", "Ta ki nam dom"]
    , ["", "", "", "", "", ""]
    , ["tam ka tam tam", "din ka tam ka", "tam tam din ka", "tam din - tam", "din - tam din", "- tam din -"]
    , ["Ta ta cha ta ki Ta ta ka", "tam - Ta ta cha ta", "ki Ta ta ka tam -", "Ta ta cha ta ki Ta ta ka", "Ta ta cha ta ki Ta ta ka", "tam ta ka Ta ta cha ta"]
    , ["ki Ta ta ka tam ta ka", "Ta ta cha ta ki Ta ta ka", "Ta ta cha ta ki Ta ta ka", "tam - ki Ta ta ka tam -", "ki Ta ta ka  tam - ki Ta", "ta ka tam dom ka"]
    , ["tam", "", "", "", "", ""]
    ]
-}
