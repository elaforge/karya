{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -XEmptyDataDecls #-}
{-
A ruler is also a Track, and can be mixed with them freely.  Rulers have Marks
at various positions.  Marks have width, color, translucency, and an optional
name.  They also have a display at zoom value.  Marks are only displayed if
the zoom level is >= the display at zoom.
-}

module Interface.Ruler where
import Foreign
import Foreign.C

import Interface.Types
import qualified Interface.Util as Util
import qualified Interface.Color as Color

data CRulerTrackModel
newtype Marklist = Marklist [(TrackPos, Mark)] deriving (Eq, Show)
data Ruler = Ruler (ForeignPtr CRulerTrackModel) (Color.Color, [Marklist])

data Mark = Mark
    { mark_rank :: Int
    , mark_width :: Int
    , mark_color :: Color.Color
    , mark_name :: String
    , mark_name_zoom_level :: Double
    , mark_zoom_level :: Double
    } deriving (Eq, Show)


-- Marshalling

create :: Color.Color -> [Marklist] -> IO Ruler
create bg marklists = with bg $ \bgp -> do
    -- Do mlist marshalling here, but I may later let clients do that so
    -- mlists can be shared at the c level.
    mlistfps <- mapM marshal_marklist marklists
    rulerp <- Util.withForeignPtrs mlistfps $ \ps -> withArray ps $ \mlistp ->
        c_ruler_track_model_new bgp (Util.c_int (length marklists)) mlistp
    rulerfp <- newForeignPtr c_ruler_track_model_destroy rulerp
    return $ Ruler rulerfp (bg, marklists)

foreign import ccall unsafe "ruler_track_model_new"
    c_ruler_track_model_new :: Ptr Color.Color -> CInt
        -> Ptr (Ptr CMarklist) -> IO (Ptr CRulerTrackModel)
foreign import ccall unsafe "&ruler_track_model_destroy"
    c_ruler_track_model_destroy :: FunPtr (Ptr CRulerTrackModel -> IO ())

-- Storable instance

#include "c_interface.h"

newtype MarkMarshal = MarkMarshal (TrackPos, Mark)
data CMarklist

marshal_marklist :: Marklist -> IO (ForeignPtr CMarklist)
marshal_marklist (Marklist marks) = do
    marksp <- newArray (map MarkMarshal marks)
    marklistp <- c_marklist_new (Util.c_int (length marks)) marksp
    newForeignPtr c_marklist_destroy marklistp

-- 'marklist_new' is responsible for freeing all storage in 'marks'.
foreign import ccall unsafe "marklist_new"
    c_marklist_new :: CInt -> Ptr MarkMarshal -> IO (Ptr CMarklist)
foreign import ccall unsafe "&marklist_destroy"
    c_marklist_destroy :: FunPtr (Ptr CMarklist -> IO ())

instance Storable MarkMarshal where
    sizeOf _ = #size MarkMarshal
    alignment _ = 1
    peek = undefined
    poke = poke_mark

poke_mark markp (MarkMarshal (pos, Mark
    { mark_rank = rank
    , mark_width = width
    , mark_color = color
    , mark_name = name
    , mark_name_zoom_level = name_zoom_level
    , mark_zoom_level = zoom_level
    })) = do
        namep <- newCString name
        (#poke MarkMarshal, pos) markp pos
        (#poke MarkMarshal, rank) markp (Util.c_int rank)
        (#poke MarkMarshal, width) markp (Util.c_int width)
        (#poke MarkMarshal, color) markp color
        (#poke MarkMarshal, name) markp namep
        (#poke MarkMarshal, name_zoom_level) markp
            (Util.c_double name_zoom_level)
        (#poke MarkMarshal, zoom_level) markp (Util.c_double zoom_level)
