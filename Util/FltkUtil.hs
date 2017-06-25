-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for "Util.Fltk".
--
-- It's a separate module so it can avoid foreign imports, which break ghci.
module Util.FltkUtil where
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import qualified System.Console.GetOpt as GetOpt
import qualified System.Info


type Pixels = Int

data Geometry = Geometry (Maybe (Pixels, Pixels)) (Maybe Pixels) (Maybe Pixels)
    deriving (Eq, Show)

option :: (Geometry -> a) -> GetOpt.OptDescr a
option flag =
    GetOpt.Option [] ["geometry"]
        (GetOpt.ReqArg (flag . parse) "<w>x<h>+<x>+<y>")
        "set initial window geometry"
    where
    parse str = fromMaybe (error $ "couldn't parse --geometry: " ++ show str)
        (geometry str)

xywh :: Pixels -> Pixels -> Pixels -> Pixels -> Maybe Geometry
    -> (Pixels, Pixels, Pixels, Pixels)
xywh x y w h Nothing = (x, y, w, h)
xywh x y w h (Just (Geometry dimensions mx my)) =
    ( fromMaybe x mx, fromMaybe y my
    , maybe w fst dimensions + bump, maybe h snd dimensions + bump
    )
    where
    -- TODO for some reason fltk on linux subtracts 100 from width and height
    bump = if System.Info.os == "linux" then 100 else 0

-- | Parse 1x2+3+4, or 1x2 or +3+4
geometry :: String -> Maybe Geometry
geometry str = case tokenize str of
    [I w, S "x", I h, S "+", I x, S "+", I y] ->
        Just $ Geometry (Just (w, h)) (Just x) (Just y)
    [I w, S "x", I h] -> Just $ Geometry (Just (w, h)) Nothing Nothing
    [S "+", I x, S "+", I y] -> Just $ Geometry Nothing (Just x) (Just y)
    [S "+", I x] -> Just $ Geometry Nothing (Just x) Nothing
    [S "+", S "+", I y] -> Just $ Geometry Nothing Nothing (Just y)
    _ -> Nothing

data Token = S String | I Int deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize str =
    nonnull (I . read) digits ++ nonnull S nondigits
        ++ if null rest then [] else tokenize rest
    where
    (digits, (nondigits, rest)) = token1 str
    token1 = fmap (span (not . Char.isDigit)) . span Char.isDigit
    nonnull f str
        | null str = []
        | otherwise = [f str]
