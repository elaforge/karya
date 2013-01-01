{- | A ruler is also a Track, and can be mixed with them freely.  Rulers have
    Marks at various positions.  Marks have width, color, translucency, and an
    optional name.  They also have a display at zoom value.  Marks are only
    displayed if the zoom level is >= the display at zoom.
-}
module Ui.RulerC (with_ruler, no_ruler) where
import qualified Data.Map as Map
import Foreign
import Foreign.C

import qualified Ui.Ruler as Ruler
import qualified Ui.Util as Util
import Types


with_ruler :: Ruler.Ruler
    -> (Ptr Ruler.Ruler -> Ptr Ruler.Marklist -> CInt -> IO a) -> IO a
with_ruler ruler f =
    with ruler $ \rulerp -> withArrayLen marklists $ \len mlists ->
        f rulerp mlists (Util.c_int len)
    where marklists = Map.elems (Ruler.ruler_marklists ruler)

no_ruler :: (Ptr Ruler.Ruler -> Ptr Ruler.Marklist -> CInt -> IO a) -> IO a
no_ruler f = f nullPtr nullPtr 0


-- Storable

#include "Ui/c_interface.h"
-- See comment in BlockC.hsc.
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable Ruler.Marklist where
    sizeOf _ = #size Marklist
    alignment _ = #{alignment Marklist}
    peek = error "Marklist peek unimplemented"
    poke = poke_marklist

poke_marklist marklistp (Ruler.Marklist marks) = do
    (#poke Marklist, length) marklistp (Util.c_int (Map.size marks))
    (#poke Marklist, marks) marklistp
        =<< newArray (map PosMark (Map.toAscList marks))

newtype PosMark = PosMark (ScoreTime, Ruler.Mark) deriving (Show)

instance Storable PosMark where
    sizeOf _ = #size PosMark
    alignment _ = #{alignment PosMark}
    peek = error "PosMark peek unimplemented"
    poke posmarkp (PosMark (pos, mark)) = do
        (#poke PosMark, pos) posmarkp pos
        (#poke PosMark, mark) posmarkp mark

instance Storable Ruler.Ruler where
    sizeOf _ = #size RulerConfig
    alignment _ = #{alignment RulerConfig}
    peek = error "Ruler peek unimplemented"
    poke = poke_ruler

-- Doesn't poke the marklists, since those are passed separately, since the
-- real RulerConfig uses an STL vector which has to be serialized in c++.
poke_ruler :: Ptr Ruler.Ruler -> Ruler.Ruler -> IO ()
poke_ruler rulerp (Ruler.Ruler mlists bg show_names align_to_bottom) = do
    (#poke RulerConfig, bg) rulerp bg
    (#poke RulerConfig, show_names) rulerp show_names
    -- The haskell layer no longer differentiates between ruler track rulers
    -- and event track overlay rulers, so these are hardcoded.  This way the
    -- fltk layer doesn't have to know anything about that and simply does
    -- what it's told.
    (#poke RulerConfig, use_alpha) rulerp (Util.c_bool True)
    (#poke RulerConfig, full_width) rulerp (Util.c_bool True)
    (#poke RulerConfig, align_to_bottom) rulerp (Util.c_bool align_to_bottom)
    (#poke RulerConfig, last_mark_pos) rulerp
        (last_mark_pos (Map.elems mlists))
    where last_mark_pos mlists = maximum (0 : map Ruler.marklist_end mlists)

instance Storable Ruler.Mark where
    sizeOf _ = #size Mark
    alignment _ = #{alignment Mark}
    peek = error "Mark peek unimplemented"
    poke = poke_mark

poke_mark :: Ptr Ruler.Mark -> Ruler.Mark -> IO ()
poke_mark markp (Ruler.Mark
    { Ruler.mark_rank = rank
    , Ruler.mark_width = width
    , Ruler.mark_color = color
    , Ruler.mark_name = name
    , Ruler.mark_name_zoom_level = name_zoom_level
    , Ruler.mark_zoom_level = zoom_level
    }) = do
        -- Must be freed by the caller.
        namep <- if null name then return nullPtr else newCString name
        (#poke Mark, rank) markp (Util.c_int rank)
        (#poke Mark, width) markp (Util.c_int width)
        (#poke Mark, color) markp color
        (#poke Mark, name) markp namep
        (#poke Mark, name_zoom_level) markp (Util.c_double name_zoom_level)
        (#poke Mark, zoom_level) markp (Util.c_double zoom_level)
