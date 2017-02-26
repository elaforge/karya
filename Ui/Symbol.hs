-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | A Symbol is a bit of text enclosed in ``s, such as `sharp`.  When
    rendered, it's turned into a special graphic that may be made up of
    multiple glyphs from different fonts.  The intent is to be able to write
    symbols in plain ascii but have them rendered in an attractive way on the
    score.

    Symbols are rendered at the UI level and are generally static, so instead
    of sending the symbol data over every time I want to draw one, the UI
    level maintains a mapping between symbol names and data on how to render
    them.

    This module is split from "Ui.SymbolC" so importers can avoid
    a C dependency.
-}
module Ui.Symbol where
import Data.Text (Text)


symbol :: Text -> [Glyph] -> Symbol
symbol name = Symbol name False

-- | Make a simple symbol with only text.
simple :: Text -> Text -> Symbol
simple name chars = Symbol name True [glyph chars]

glyph :: Text -> Glyph
glyph s = Glyph s Nothing 0 (0, 0) 0

size :: Int -> Glyph -> Glyph
size n g = g { glyph_size = n }

glyph_at :: Int -> (Double, Double) -> Glyph -> Glyph
glyph_at size align glyph = glyph { glyph_size = size, glyph_align = align }

type Font = String

-- | A Symbol has a name and a list of the Glyphs that make it up.
--
-- If the bounding box is not given, it will be inferred from the first glyph.
-- The bounding box will be scaled by the eventual font size.  Don't pass an
-- empty glyphs list.
data Symbol = Symbol {
    name :: Text
    -- | Turn on absolute y placement, disabling automatic y placement.  If
    -- the glyphs have descenders and you want them to actually descend, turn
    -- this on.
    , absolute_y :: Bool
    , glyphs :: [Glyph]
    } deriving (Show)

data Glyph = Glyph {
    -- | Unicode characters that make up the glyph.
    glyph_text :: Text
    , glyph_font :: Maybe Font
    -- | Relative size.  This is added to the font size when the glyph is
    -- drawn.
    , glyph_size :: Int
    -- | This is scaled by the font size and added to the position of the
    -- glyph.  In a symbol with only one glyph, automatic y placement will
    -- defeat a y value here unless you set 'sym_absolute_y'.
    , glyph_align :: (Double, Double)
    -- | Rotate the glyph in degrees.
    , glyph_rotate :: Int
    } deriving (Show)
