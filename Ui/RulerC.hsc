{-# OPTIONS_GHC -XEmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-
A ruler is also a Track, and can be mixed with them freely.  Rulers have Marks
at various positions.  Marks have width, color, translucency, and an optional
name.  They also have a display at zoom value.  Marks are only displayed if
the zoom level is >= the display at zoom.
-}

module Ui.RulerC where
import Control.Monad
import Foreign
import Foreign.C

import qualified Util.Log as Log
import Ui.Types
import qualified Ui.Util as Util

import qualified Ui.Ruler as Ruler


with_ruler :: Ruler.Ruler
    -> (Ptr Ruler.Ruler -> Ptr Ruler.Marklist -> CInt -> IO a)
    -> IO a
with_ruler ruler f = do
    with ruler $ \rulerp -> withArrayLen marklists $ \len mlists ->
        f rulerp mlists (Util.c_int len)
    where marklists = map snd (Ruler.ruler_marklists ruler)

-- typedef int (*FindMarks)(TrackPos *start_pos, TrackPos *end_pos,
--         TrackPos **ret_tps, Mark **ret_marks);
type FindMarks = Ptr TrackPos -> Ptr TrackPos -> Ptr (Ptr TrackPos)
    -> Ptr (Ptr Ruler.Mark) -> IO Int

cb_find_marks :: Ruler.Marklist -> FindMarks
cb_find_marks marklist startp endp ret_tps ret_marks = do
    start <- peek startp
    end <- peek endp
    let (bwd, fwd) = Ruler.at marklist start
        (until_end, rest) = break ((>=end) . fst) fwd
        -- Give extra marks, one before start and one after end, so that marks
        -- scrolled half-off are still displayed.
        marks = take 1 bwd ++ until_end ++ take 1 rest
    when (not (null marks)) $ do
        -- Calling c++ is responsible for freeing this.
        tp_array <- newArray (map fst marks)
        mark_array <- newArray (map snd marks)
        poke ret_tps tp_array
        poke ret_marks mark_array
    -- putStrLn $ "find marks: " ++ show (length marks)
    return (length marks)

make_find_marks marklist = do
    cb <- c_make_find_marks (cb_find_marks marklist)
    Log.debug $ "make find marks callback: " ++ show cb
    return cb

foreign import ccall "wrapper"
    c_make_find_marks :: FindMarks -> IO (FunPtr FindMarks)

-- Storable

#include "c_interface.h"

instance Storable Ruler.Marklist where
    sizeOf _ = #size Marklist
    alignment _ = undefined
    poke = poke_marklist

poke_marklist marklistp marklist = do
    find_marks <- make_find_marks marklist
    (#poke Marklist, find_marks) marklistp find_marks

instance Storable Ruler.Ruler where
    sizeOf _ = #size RulerConfig
    alignment _ = undefined
    peek = undefined
    poke = poke_ruler

-- Doesn't poke the marklists, since those are passed separately, since the
-- real RulerConfig uses an STL vector which has to be serialized in c++.
poke_ruler rulerp (Ruler.Ruler mlists bg show_names use_alpha full_width) = do
    (#poke RulerConfig, bg) rulerp bg
    (#poke RulerConfig, show_names) rulerp show_names
    (#poke RulerConfig, use_alpha) rulerp use_alpha
    (#poke RulerConfig, full_width) rulerp full_width
    (#poke RulerConfig, last_mark_pos) rulerp (last_mark_pos (map snd mlists))

last_mark_pos mlists = maximum (TrackPos 0 : map Ruler.last_pos mlists)

instance Storable Ruler.Mark where
    sizeOf _ = #size Mark
    alignment _ = undefined
    peek = undefined
    poke = poke_mark

poke_mark markp (Ruler.Mark
    { Ruler.mark_rank = rank
    , Ruler.mark_width = width
    , Ruler.mark_color = color
    , Ruler.mark_name = name
    , Ruler.mark_name_zoom_level = name_zoom_level
    , Ruler.mark_zoom_level = zoom_level
    }) = do
        -- Must be freed by the caller, OverlayRuler::draw_marklists.
        namep <- if null name then return nullPtr else newCString name
        (#poke Mark, rank) markp (Util.c_int rank)
        (#poke Mark, width) markp (Util.c_int width)
        (#poke Mark, color) markp color
        (#poke Mark, name) markp namep
        (#poke Mark, name_zoom_level) markp
            (Util.c_double name_zoom_level)
        (#poke Mark, zoom_level) markp (Util.c_double zoom_level)
