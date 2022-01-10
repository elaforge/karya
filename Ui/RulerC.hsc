-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | A ruler is also a Track, and can be mixed with them freely.  Rulers have
    Marks at various positions.  Marks have width, color, translucency, and an
    optional name.  They also have a display at zoom value.  Marks are only
    displayed if the zoom level is >= the display at zoom.
-}
module Ui.RulerC (with_ruler, no_ruler) where
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map

import qualified Util.FFI as FFI
import qualified Util.TimeVector as TimeVector

import qualified Ui.Meter.Mark as Mark
import qualified Ui.Ruler as Ruler

import           ForeignC
import           Types


with_ruler :: Ruler.Ruler
    -> (Ptr Ruler.Ruler -> Ptr (Ptr Mark.Marklist) -> CInt -> IO a) -> IO a
with_ruler ruler f =
    with ruler $ \rulerp -> with_marklists marklists $ \len mlists ->
        f rulerp mlists (FFI.c_int len)
    where marklists = map snd $ Map.elems $ Ruler.ruler_marklists ruler

no_ruler :: (Ptr Ruler.Ruler -> Ptr (Ptr Mark.Marklist) -> CInt -> IO a)
    -> IO a
no_ruler f = f nullPtr nullPtr 0

-- Storable

#include "Ui/c_interface.h"

with_marklists :: [Mark.Marklist] -> (Int -> Ptr (Ptr Mark.Marklist) -> IO a)
    -> IO a
with_marklists mlists f = do
    fptrs <- mapM marklist_fptr mlists
    FFI.withForeignPtrs fptrs $ \ptrs -> do
        mapM_ c_marklist_incref ptrs
        withArrayLen ptrs f

-- | Create and cache a new marklist pointer, or re-used the cached one.
marklist_fptr :: Mark.Marklist -> IO (ForeignPtr Mark.Marklist)
marklist_fptr mlist = MVar.modifyMVar (extract mlist) create
    where
    extract = (\(Mark.MarklistPtr a) -> a) . Mark.marklist_fptr
    create (Right fptr) = return (Right fptr, fptr)
    create (Left _) = do
        fptr <- create_marklist mlist
        return (Right fptr, fptr)

create_marklist :: Mark.Marklist -> IO (ForeignPtr Mark.Marklist)
create_marklist mlist = do
    marksp <- newArray $ map PosMark $ Mark.ascending 0 mlist
    mlistp <- c_create_marklist marksp $ FFI.c_int $
        TimeVector.length $ Mark.marklist_vec mlist
    newForeignPtr c_marklist_decref mlistp

foreign import ccall "create_marklist"
    c_create_marklist :: Ptr PosMark -> CInt -> IO (Ptr Mark.Marklist)
foreign import ccall "&marklist_decref"
    c_marklist_decref :: FunPtr (Ptr Mark.Marklist -> IO ())
foreign import ccall "marklist_incref"
    c_marklist_incref :: Ptr Mark.Marklist -> IO ()

newtype PosMark = PosMark (ScoreTime, Mark.Mark) deriving (Show)

instance CStorable PosMark where
    sizeOf _ = #size PosMark
    alignment _ = alignment (0 :: CDouble)
    peek = error "PosMark peek unimplemented"
    poke posmarkp (PosMark (pos, mark)) = do
        (#poke PosMark, pos) posmarkp pos
        (#poke PosMark, mark) posmarkp mark

instance CStorable Ruler.Ruler where
    sizeOf _ = #size RulerConfig
    alignment _ = alignment (0 :: CDouble)
    peek = error "Ruler peek unimplemented"
    poke = poke_ruler

-- Doesn't poke the marklists, since those are passed separately, since the
-- real RulerConfig uses an STL vector which has to be serialized in c++.
poke_ruler :: Ptr Ruler.Ruler -> Ruler.Ruler -> IO ()
poke_ruler rulerp ruler@(Ruler.Ruler _ bg show_names align_to_bottom) = do
    (#poke RulerConfig, bg) rulerp bg
    (#poke RulerConfig, show_names) rulerp (FFI.c_bool show_names)
    -- The haskell layer no longer differentiates between ruler track rulers
    -- and event track overlay rulers, so these are hardcoded.  This way the
    -- fltk layer doesn't have to know anything about that and simply does
    -- what it's told.
    (#poke RulerConfig, use_alpha) rulerp (FFI.c_bool True)
    (#poke RulerConfig, full_width) rulerp (FFI.c_bool True)
    (#poke RulerConfig, align_to_bottom) rulerp (FFI.c_bool align_to_bottom)
    (#poke RulerConfig, last_mark_pos) rulerp (Ruler.time_end ruler)

instance CStorable Mark.Mark where
    sizeOf _ = #size Mark
    alignment _ = alignment (0 :: CDouble)
    peek = error "Mark peek unimplemented"
    poke = poke_mark

poke_mark :: Ptr Mark.Mark -> Mark.Mark -> IO ()
poke_mark markp (Mark.Mark
    { mark_rank = rank
    , mark_width = width
    , mark_color = color
    , mark_name = name
    , mark_name_zoom_level = name_zoom_level
    , mark_zoom_level = zoom_level
    }) = do
        -- Must be freed by the caller.
        namep <- FFI.newCStringNull0 name
        (#poke Mark, rank) markp (FFI.c_int (fromEnum rank))
        (#poke Mark, width) markp (FFI.c_int width)
        (#poke Mark, color) markp color
        (#poke Mark, name) markp namep
        (#poke Mark, name_zoom_level) markp (FFI.c_double name_zoom_level)
        (#poke Mark, zoom_level) markp (FFI.c_double zoom_level)
