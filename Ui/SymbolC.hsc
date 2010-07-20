{-# LANGUAGE ForeignFunctionInterface #-}
module Ui.SymbolC (get_fonts, insert_symbol) where
import qualified Codec.Binary.UTF8.String as UTF8.String
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import Foreign
import Foreign.C

import qualified Util.Num as Num

import qualified Ui.Util as Util
import qualified Ui.Symbol as Symbol


#include "c_interface.h"
-- See comment in BlockC.hsc.
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


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


-- | Insert the given symbol into the symbol map.  Return any missing Fonts.  If
-- the return is non-null, the symbol wasn't inserted.
insert_symbol :: Symbol.Symbol -> IO [Symbol.Font]
insert_symbol (Symbol.Symbol name align glyphs) = do
    let (bx, by) = maybe (0, 0) id align
    maybe_glyphcs <- mapM glyph_to_glyphc glyphs
    let missing = [Symbol.glyph_font glyph
            | (glyph, Nothing) <- zip glyphs maybe_glyphcs]
    if not (null missing)
        -- These should all be Just since glyph_to_glyphc never returns Nothing
        -- for a Nothing font.
        then return (Maybe.catMaybes missing)
        else do
            let glyphcs = Maybe.catMaybes maybe_glyphcs
            withCString name $ \namep -> withArrayLen glyphcs $ \len glyphsp ->
                c_insert_symbol namep (Num.d2c bx) (Num.d2c by) glyphsp
                    (Util.c_int len)
            return []

foreign import ccall "insert_symbol"
    c_insert_symbol :: CString -> CDouble -> CDouble -> Ptr GlyphC -> CInt
        -> IO ()

glyph_to_glyphc :: Symbol.Glyph -> IO (Maybe GlyphC)
glyph_to_glyphc (Symbol.Glyph chars maybe_font size (x, y)) = do
    cfont <- maybe (return (#const SymbolTable::font_default))
        (\f -> withCString f c_get_font) maybe_font
    return $ if cfont == (#const SymbolTable::font_not_found)
        then Nothing
        else Just $ GlyphC chars cfont size x y

foreign import ccall "get_font" c_get_font :: CString -> IO CFont


type CFont = CInt

data GlyphC = GlyphC String CFont Int Double Double deriving (Show)

instance Storable GlyphC where
    sizeOf _ = #size SymbolTable::Glyph
    alignment _ = #{alignment SymbolTable::Glyph}
    poke glyphp (GlyphC str font size align_x align_y) = do
        encoded <- newCString (encode_utf8 str)
        (#poke SymbolTable::Glyph, utf8) glyphp encoded
        (#poke SymbolTable::Glyph, font) glyphp font
        (#poke SymbolTable::Glyph, size) glyphp size
        (#poke SymbolTable::Glyph, align_x) glyphp align_x
        (#poke SymbolTable::Glyph, align_y) glyphp align_y

-- | This is less efficient than the one in Ui.Event, but a lot less scary.
encode_utf8 :: String -> String
encode_utf8 = map (Char.chr . fromIntegral) . UTF8.String.encode
