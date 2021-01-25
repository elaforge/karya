-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.SymbolC (get_fonts, insert) where
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Foreign
import Foreign.C

import qualified Util.FFI as FFI
import qualified Ui.Symbol as Symbol
import qualified App.Config as Config

import Global


#include "Ui/c_interface.h"

-- | Get all the loaded fonts.  There is currently no way to load new fonts.
get_fonts :: IO [Symbol.Font]
get_fonts = do
    fontspp <- c_get_fonts
    fontsp <- peekArray0 nullPtr fontspp
    fonts <- mapM peekCString fontsp
    mapM_ free fontsp
    free fontspp
    return fonts

foreign import ccall "get_fonts" c_get_fonts :: IO (Ptr CString)


-- | Insert the given symbol into the symbol map.  Return any missing Fonts.
-- If the return is non-null, the symbol wasn't inserted.
insert :: Symbol.Symbol -> IO [Symbol.Font]
insert (Symbol.Symbol name absolute_y glyphs) = do
    maybe_glyphcs <- mapM glyph_to_glyphc glyphs
    let missing = [Symbol.glyph_font glyph
            | (glyph, Nothing) <- zip glyphs maybe_glyphcs]
    if not (null missing)
        -- These should all be Just since glyph_to_glyphc never returns Nothing
        -- for a Nothing font.
        then return (Maybe.catMaybes missing)
        else do
            let glyphcs = Maybe.catMaybes maybe_glyphcs
            FFI.withText (mangle_name name) $ \namep -> withArrayLen glyphcs $
                \len glyphsp ->
                    c_insert_symbol namep (fromBool absolute_y)
                        glyphsp (FFI.c_int len)
            return []

foreign import ccall "insert_symbol"
    c_insert_symbol :: CString -> CInt -> Ptr GlyphC -> CInt -> IO ()

mangle_name :: Text -> Text
mangle_name name = case Config.platform of
    -- Fltk on X puts a character at the start of the name to encode the style,
    -- and space for no style.  Fltk on OS X doesn't do this.
    Config.Linux -> " " <> name
    Config.Mac -> name

glyph_to_glyphc :: Symbol.Glyph -> IO (Maybe GlyphC)
glyph_to_glyphc (Symbol.Glyph text maybe_font size (x, y) rotation) = do
    cfont <- maybe (return (#const Config::font))
        (\f -> withCString f c_get_font) maybe_font
    return $ if cfont == (#const SymbolTable::font_not_found)
        then Nothing
        else Just $ GlyphC text cfont size x y rotation

foreign import ccall "get_font" c_get_font :: CString -> IO CFont


type CFont = CInt

data GlyphC = GlyphC Text.Text CFont Int Double Double Int
    deriving (Show)

instance Storable GlyphC where
    sizeOf _ = #size SymbolTable::Glyph
    alignment _ = alignment (0 :: CDouble)
    peek = error "GlyphC peek"
    poke glyphp (GlyphC text font size align_x align_y rotate) = do
        encoded <- FFI.newCString0 text
        (#poke SymbolTable::Glyph, utf8) glyphp encoded
        (#poke SymbolTable::Glyph, font) glyphp font
        (#poke SymbolTable::Glyph, size) glyphp (FFI.c_int size)
        (#poke SymbolTable::Glyph, align_x) glyphp (FFI.c_double align_x)
        (#poke SymbolTable::Glyph, align_y) glyphp (FFI.c_double align_y)
        (#poke SymbolTable::Glyph, rotate) glyphp (FFI.c_int rotate)
