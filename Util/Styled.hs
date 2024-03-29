-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | A simple Styled Text implementation.  There are a few others on hackage
-- (terminal-text, rainbow, ...), but they're all too complicated for me.
--
-- Examples:
-- > printLn $ fgs (bright red) "hi" <> fgs red "there"
-- > printLn $ bgs (bright red) "hi" <> bgs red "there"
-- > printLn $ underlines "hi" <> " " <> bolds "there"
-- > printLn $ underlines $ fgs (bright red) "hi" <> fgs red "there"
--
-- > printLn $ bgs cyan "hello\nthere"
-- > printLn $ bgs cyan "hello" <> "\n" <> bgs cyan "there"
module Util.Styled (
    Styled, Style(..)
    , print, printLn
    , toByteString, toByteStrings
    , toText, toTexts
    , Color(..), RgbColor, AnsiColor
    , black, red, green, yellow, blue, magenta, cyan, white
    , rgb, rgbGray, rgbColor, toRgb
    , styled, plain
    , bright
    , fgs, bgs, bolds, underlines
    , fg, bg, bold, underline
    -- * text util
    , join
    -- * html
    , toHtml, toHtmls
    , styleHtml
) where
import           Prelude hiding (print)
import           Control.Applicative ((<|>))
import           Data.Bifunctor (second)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Char as Char
import qualified Data.Colour as Colour
import qualified Data.Colour.Names as Colour.Names
import qualified Data.Colour.SRGB as SRGB
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.Encoding as Encoding

import qualified System.Console.ANSI as ANSI

import qualified Util.Html as Html
import qualified Util.Num as Num
import qualified Util.Lists as Lists
import qualified Util.Then as Then


data Styled = Branch Styled Styled | Styled !Style !Text
    deriving (Eq, Show)

instance Semigroup Styled where
    Styled _ t1 <> s2 | Text.null t1 = s2
    s1 <> Styled _ t2 | Text.null t2 = s1
    s1 <> s2 = Branch s1 s2

instance Monoid Styled where
    mempty = Styled mempty ""
    mappend = (<>)

instance String.IsString Styled where
    fromString = Styled mempty . String.fromString

class ToStyled a where toStyled :: a -> Styled
instance ToStyled Text where toStyled = Styled mempty
instance ToStyled Styled where toStyled = id

mapStyle :: (Style -> Style) -> Styled -> Styled
mapStyle f (Branch t1 t2) = Branch (mapStyle f t1) (mapStyle f t2)
mapStyle f (Styled style t) = Styled (f style) t

print :: Styled -> IO ()
print = mapM_ ByteString.putStr . toByteStrings

printLn :: Styled -> IO ()
printLn s = print (s <> "\n")

toByteString :: Styled -> ByteString
toByteString = mconcat . toByteStrings

toByteStrings :: Styled -> [ByteString]
toByteStrings = filter (/="") . concatMap render . toSGRs
    where
    render (sgrs, text) =
        [ if null sgrs then "" else ByteString.pack (ANSI.setSGRCode sgrs)
        , Encoding.encodeUtf8 text
        ]

toText :: Styled -> Text
toText = mconcat . toTexts

toTexts :: Styled -> [Text]
toTexts = filter (/="") . concatMap render . toSGRs
    where
    render (sgrs, text)
        | null sgrs = [text]
        | otherwise = [Text.pack (ANSI.setSGRCode sgrs), text]

-- | Render in order, but only emit escape codes if the Style changed.
toSGRs :: Styled -> [([ANSI.SGR], Text)]
toSGRs = Then.map render1 [([ANSI.Reset], "")] . zipPrev (mempty, "") . toList
    where
    render1 ((prevStyle, _), (style, text)) =
        (if prevStyle == style then [] else styleSGR style, text)
    zipPrev fst xs = zip (fst : xs) xs

toList :: Styled -> [(Style, Text)]
toList xs = go xs []
    where
    go (Branch as bs) xs = go as (go bs xs)
    go (Styled style text) xs = (style, text) : xs

styleSGR :: Style -> [ANSI.SGR]
styleSGR (Style fg bg bold underline) = ANSI.Reset : concat
    [ case fg of
        Nothing -> []
        Just (Ansi (AnsiColor intensity color)) ->
            [ANSI.SetColor ANSI.Foreground intensity color]
        Just (Rgb (RgbColor color)) -> [ANSI.SetRGBColor ANSI.Foreground color]
    , case bg of
        Nothing -> []
        Just (Ansi (AnsiColor intensity color)) ->
            [ANSI.SetColor ANSI.Background intensity color]
        Just (Rgb (RgbColor color)) -> [ANSI.SetRGBColor ANSI.Background color]
    , [ANSI.SetConsoleIntensity ANSI.BoldIntensity | bold]
    , [ANSI.SetUnderlining ANSI.SingleUnderline | underline]
    ]

data Style = Style {
    _foreground :: !(Maybe Color)
    , _background :: !(Maybe Color)
    , _bold :: !Bool
    , _underline :: !Bool
    } deriving (Eq, Show)

-- | Left side overrides the right side, for consistency with other Semigroups.
instance Semigroup Style where
    Style fg1 bg1 bold1 underline1 <> Style fg2 bg2 bold2 underline2 =
        Style (fg1 <|> fg2) (bg1 <|> bg2)
            (bold1 || bold2) (underline1 || underline2)

instance Monoid Style where
    mempty = Style
        { _foreground = Nothing
        , _background = Nothing
        , _bold = False
        , _underline = False
        }
    mappend = (<>)

data Color = Ansi !AnsiColor | Rgb !RgbColor
    deriving (Eq, Show)

data AnsiColor = AnsiColor !ANSI.ColorIntensity !ANSI.Color
    deriving (Eq, Show)

data RgbColor = RgbColor !(Colour.Colour Float)
    deriving (Eq, Show)

black, red, green, yellow, blue, magenta, cyan, white :: Color
(black, red, green, yellow, blue, magenta, cyan, white) =
    ( c ANSI.Black, c ANSI.Red, c ANSI.Green, c ANSI.Yellow, c ANSI.Blue
    , c ANSI.Magenta, c ANSI.Cyan, c ANSI.White
    )
    where c = Ansi . AnsiColor ANSI.Dull

rgb :: Float -> Float -> Float -> Color
rgb r g b = Rgb $ rgbColor r g b

rgbGray :: Float -> Color
rgbGray n = rgb n n n

rgbColor :: Float -> Float -> Float -> RgbColor
rgbColor r g b = RgbColor $ SRGB.sRGB r g b

toRgb :: RgbColor -> (Float, Float, Float)
toRgb (RgbColor c) = (r, g, b)
    where (SRGB.RGB r g b) = SRGB.toSRGB c

styled :: ToStyled a => Style -> a -> Styled
styled style = mapStyle (style<>) . toStyled

plain :: Text -> Styled
plain = Styled mempty

bright :: Color -> Color
bright (Ansi (AnsiColor _ color)) = Ansi $ AnsiColor ANSI.Vivid color
bright (Rgb (RgbColor color)) =
    Rgb $ RgbColor $ Colour.blend 0.5 color Colour.Names.white

fgs, bgs :: Color -> Styled -> Styled
fgs color = mapStyle (\style -> style { _foreground = Just color })
bgs color = mapStyle (\style -> style { _background = Just color })

fg, bg :: ToStyled a => Color -> a -> Styled
fg color = fgs color . toStyled
bg color = bgs color . toStyled

bolds, underlines :: Styled -> Styled
bolds = mapStyle (\style -> style { _bold = True })
underlines = mapStyle (\style -> style { _underline = True })

bold, underline :: ToStyled a => a -> Styled
bold = bolds . toStyled
underline = underlines . toStyled

-- * text util

join :: Styled -> [Styled] -> Styled
join sep = mconcat . List.intersperse sep


-- * html

toHtml :: Styled -> Html.Html
toHtml = mconcat . toHtmls

toHtmls :: Styled -> [Html.Html]
toHtmls = map fmt . groupFst . toList
    where fmt (style, texts) = styleHtml style (Html.html (mconcat texts))

styleHtml :: Style -> Html.Html -> Html.Html
styleHtml (Style fg bg bold underline) = foldr (.) id . concat $
    [ case Maybe.catMaybes [("color:",)<$>fg, ("background-color:",) <$> bg] of
        [] -> []
        pairs -> [spanStyle (Text.intercalate ";" css)]
            where css = [name <> colorHtml c | (name, c) <- pairs]
    , [Html.tag "b" | bold]
    , [Html.tag "u" | underline]
    ]

spanStyle :: Text -> Html.Html -> Html.Html
spanStyle style = Html.tag_attrs "span" [("style", style)] . Just

colorHtml :: Color -> Text
colorHtml = \case
    Ansi color -> ansiHtml color
    Rgb color -> rgbHtml color

rgbHtml :: RgbColor -> Text
rgbHtml color = mconcat ["#", hex r, hex g, hex b]
    where
    hex = Num.hex 2 . Num.clamp 0 255 . (round :: Float -> Int) . (*255)
    (r, g, b) = toRgb color

ansiHtml :: AnsiColor -> Text
ansiHtml (AnsiColor intensity color) = case (intensity, color) of
    (ANSI.Dull, ANSI.Black) -> "black"
    (ANSI.Vivid, ANSI.Black) -> "darkgray"
    _ -> cdark <> cname
    where
    cname = Text.pack $ map Char.toLower $ show color
    cdark = case intensity of
        ANSI.Dull -> "dark"
        ANSI.Vivid -> ""

-- | Group adjacent by fst.
groupFst :: Eq a => [(a, b)] -> [(a, [b])]
groupFst =  map (second (map snd)) . Lists.keyedGroupAdjacent fst
