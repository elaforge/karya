-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | A ruler is also a Track, and can be mixed with them freely.  Rulers have
    Marks at various positions.  Marks have width, color, translucency, and an
    optional name.  They also have a display at zoom value.  Marks are only
    displayed if the zoom level is >= the display at zoom.
-}
module Ui.RulerC (withRuler, noRuler) where
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map

import qualified Util.FFI as FFI
import qualified Util.TimeVector as TimeVector

import qualified Ui.Meter.Mark as Mark
import qualified Ui.Ruler as Ruler

import           ForeignC
import           Types


withRuler :: Ruler.Ruler
    -> (Ptr Ruler.Ruler -> Ptr (Ptr Mark.Marklist) -> CInt -> IO a) -> IO a
withRuler ruler f =
    with ruler $ \rulerp -> withMarklists marklists $ \len mlists ->
        f rulerp mlists (FFI.cInt len)
    where marklists = map snd $ Map.elems $ Ruler.rulerMarklists ruler

noRuler :: (Ptr Ruler.Ruler -> Ptr (Ptr Mark.Marklist) -> CInt -> IO a)
    -> IO a
noRuler f = f nullPtr nullPtr 0

-- Storable

#include "Ui/c_interface.h"

withMarklists :: [Mark.Marklist] -> (Int -> Ptr (Ptr Mark.Marklist) -> IO a)
    -> IO a
withMarklists mlists f = do
    fptrs <- mapM marklistFptr mlists
    FFI.withForeignPtrs fptrs $ \ptrs -> do
        mapM_ cMarklistIncref ptrs
        withArrayLen ptrs f

-- | Create and cache a new marklist pointer, or re-used the cached one.
marklistFptr :: Mark.Marklist -> IO (ForeignPtr Mark.Marklist)
marklistFptr mlist = MVar.modifyMVar (extract mlist) create
    where
    extract = (\(Mark.MarklistPtr a) -> a) . Mark.marklistFptr
    create (Right fptr) = return (Right fptr, fptr)
    create (Left _) = do
        fptr <- createMarklist mlist
        return (Right fptr, fptr)

createMarklist :: Mark.Marklist -> IO (ForeignPtr Mark.Marklist)
createMarklist mlist = do
    marksp <- newArray $ map PosMark $ Mark.ascending 0 mlist
    mlistp <- cCreateMarklist marksp $ FFI.cInt $
        TimeVector.length $ Mark.marklistVec mlist
    newForeignPtr cMarklistDecref mlistp

foreign import ccall "create_marklist"
    cCreateMarklist :: Ptr PosMark -> CInt -> IO (Ptr Mark.Marklist)
foreign import ccall "&marklist_decref"
    cMarklistDecref :: FunPtr (Ptr Mark.Marklist -> IO ())
foreign import ccall "marklist_incref"
    cMarklistIncref :: Ptr Mark.Marklist -> IO ()

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
    poke = pokeRuler

-- Doesn't poke the marklists, since those are passed separately, since the
-- real RulerConfig uses an STL vector which has to be serialized in c++.
pokeRuler :: Ptr Ruler.Ruler -> Ruler.Ruler -> IO ()
pokeRuler rulerp ruler@(Ruler.Ruler _ bg showNames alignToBottom) = do
    (#poke RulerConfig, bg) rulerp bg
    (#poke RulerConfig, show_names) rulerp (FFI.cBool showNames)
    -- The haskell layer no longer differentiates between ruler track rulers
    -- and event track overlay rulers, so these are hardcoded.  This way the
    -- fltk layer doesn't have to know anything about that and simply does
    -- what it's told.
    (#poke RulerConfig, use_alpha) rulerp (FFI.cBool True)
    (#poke RulerConfig, full_width) rulerp (FFI.cBool True)
    (#poke RulerConfig, align_to_bottom) rulerp (FFI.cBool alignToBottom)
    (#poke RulerConfig, last_mark_pos) rulerp (Ruler.timeEnd ruler)

instance CStorable Mark.Mark where
    sizeOf _ = #size Mark
    alignment _ = alignment (0 :: CDouble)
    peek = error "Mark peek unimplemented"
    poke = pokeMark

pokeMark :: Ptr Mark.Mark -> Mark.Mark -> IO ()
pokeMark markp (Mark.Mark
    { markRank = rank
    , markWidth = width
    , markColor = color
    , markName = name
    , markNameZoomLevel = nameZoomLevel
    , markZoomLevel = zoomLevel
    }) = do
        -- Must be freed by the caller.
        namep <- FFI.newCStringNull0 name
        (#poke Mark, rank) markp (FFI.cInt (fromEnum rank))
        (#poke Mark, width) markp (FFI.cInt width)
        (#poke Mark, color) markp color
        (#poke Mark, name) markp namep
        (#poke Mark, name_zoom_level) markp (FFI.cDouble nameZoomLevel)
        (#poke Mark, zoom_level) markp (FFI.cDouble zoomLevel)
