-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.SymbolC (getFonts, insert) where
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
getFonts :: IO [Symbol.Font]
getFonts = do
    fontspp <- cGetFonts
    fontsp <- peekArray0 nullPtr fontspp
    fonts <- mapM peekCString fontsp
    mapM_ free fontsp
    free fontspp
    return fonts

foreign import ccall "get_fonts" cGetFonts :: IO (Ptr CString)


-- | Insert the given symbol into the symbol map.  Return any missing Fonts.
-- If the return is non-null, the symbol wasn't inserted.
insert :: Symbol.Symbol -> IO [Symbol.Font]
insert (Symbol.Symbol name absoluteY glyphs) = do
    maybeGlyphcs <- mapM glyphToGlyphc glyphs
    let missing = [Symbol.glyphFont glyph
            | (glyph, Nothing) <- zip glyphs maybeGlyphcs]
    if not (null missing)
        -- These should all be Just since glyph_to_glyphc never returns Nothing
        -- for a Nothing font.
        then return (Maybe.catMaybes missing)
        else do
            let glyphcs = Maybe.catMaybes maybeGlyphcs
            FFI.withText (mangleName name) $ \namep -> withArrayLen glyphcs $
                \len glyphsp ->
                    cInsertSymbol namep (fromBool absoluteY)
                        glyphsp (FFI.cInt len)
            return []

foreign import ccall "insert_symbol"
    cInsertSymbol :: CString -> CInt -> Ptr GlyphC -> CInt -> IO ()

mangleName :: Text -> Text
mangleName name = case Config.platform of
    -- Fltk on X puts a character at the start of the name to encode the style,
    -- and space for no style.  Fltk on OS X doesn't do this.
    Config.Linux -> " " <> name
    Config.Mac -> name

glyphToGlyphc :: Symbol.Glyph -> IO (Maybe GlyphC)
glyphToGlyphc (Symbol.Glyph text maybeFont size (x, y) rotation) = do
    cfont <- maybe (return (#const Config::font))
        (\f -> withCString f cGetFont) maybeFont
    return $ if cfont == (#const SymbolTable::font_not_found)
        then Nothing
        else Just $ GlyphC text cfont size x y rotation

foreign import ccall "get_font" cGetFont :: CString -> IO CFont


type CFont = CInt

data GlyphC = GlyphC Text.Text CFont Int Double Double Int
    deriving (Show)

instance Storable GlyphC where
    sizeOf _ = #size SymbolTable::Glyph
    alignment _ = alignment (0 :: CDouble)
    peek = error "GlyphC peek"
    poke glyphp (GlyphC text font size alignX alignY rotate) = do
        encoded <- FFI.newCString0 text
        (#poke SymbolTable::Glyph, utf8) glyphp encoded
        (#poke SymbolTable::Glyph, font) glyphp font
        (#poke SymbolTable::Glyph, size) glyphp (FFI.cInt size)
        (#poke SymbolTable::Glyph, align_x) glyphp (FFI.cDouble alignX)
        (#poke SymbolTable::Glyph, align_y) glyphp (FFI.cDouble alignY)
        (#poke SymbolTable::Glyph, rotate) glyphp (FFI.cInt rotate)
