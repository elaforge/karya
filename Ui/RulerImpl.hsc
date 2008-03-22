{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -XEmptyDataDecls #-}
{-
A ruler is also a Track, and can be mixed with them freely.  Rulers have Marks
at various positions.  Marks have width, color, translucency, and an optional
name.  They also have a display at zoom value.  Marks are only displayed if
the zoom level is >= the display at zoom.
-}

module Ui.RulerImpl where
import qualified Data.Array.IArray as IArray
import Data.Array.IArray ((!))
import Foreign
import Foreign.C

import qualified Util.Array
import qualified Util.Seq as Seq
import qualified Util.Test as Test
import Ui.Types
import qualified Ui.Util as Util
import qualified Ui.Color as Color

data CRulerTrackModel
data Ruler = Ruler
    { ruler_p :: ForeignPtr CRulerTrackModel
    , ruler_config :: Config
    } deriving (Eq, Show)

data Config = Config
    { config_marklists :: [Marklist]
    , config_bg :: Color.Color
    , config_show_names :: Bool
    , config_use_alpha :: Bool
    , config_full_width :: Bool
    } deriving (Eq, Show)


-- Marshalling

create :: Config -> IO Ruler
create config@(Config marklists bg show_names use_alpha full_width)
    = with bg $ \bgp -> do
        let mlistfps = map (\(Marklist _ ptr) -> ptr) marklists
        rulerp <- Util.withForeignPtrs mlistfps $ \ps ->
            withArray ps $ \mlistp ->
                c_ruler_track_model_new bgp (Util.c_int (length marklists))
                    mlistp (fromBool show_names) (fromBool use_alpha)
                    (fromBool full_width)
        rulerfp <- newForeignPtr c_ruler_track_model_destroy rulerp
        return $ Ruler rulerfp config

foreign import ccall unsafe "ruler_track_model_new"
    c_ruler_track_model_new :: Ptr Color.Color -> CInt -> Ptr (Ptr CMarklist)
        -> CInt -> CInt -> CInt -> IO (Ptr CRulerTrackModel)
foreign import ccall unsafe "&ruler_track_model_destroy"
    c_ruler_track_model_destroy :: FunPtr (Ptr CRulerTrackModel -> IO ())


-- * Marklists

data Marklist
    = Marklist (IArray.Array Int (TrackPos, Mark)) (ForeignPtr CMarklist)
    deriving (Eq)
instance Show Marklist where
    show (Marklist a ptr) = "Ruler.create_marklist " ++ show (IArray.elems a)

data Mark = Mark
    { mark_rank :: Int
    , mark_width :: Int
    , mark_color :: Color.Color
    , mark_name :: String
    , mark_name_zoom_level :: Double
    , mark_zoom_level :: Double
    } deriving (Eq, Show)
null_mark = Mark 0 0 Color.black "" 0 0

-- 'create_marklist' has an IO type because I store a pointer to the marshalled
-- C version.  This means I can create a marklist once and share it among
-- many rulers and event tracks, but it also means creating a ruler config
-- is no longer a pure operation.
create_marklist :: [(TrackPos, Mark)] -> IO Marklist
create_marklist posmarks = do
    marksp <- newArray (map MarkMarshal posmarks)
    marklistp <- c_marklist_new (Util.c_int (length posmarks)) marksp
    fptr <- newForeignPtr c_marklist_destroy marklistp
    return (Marklist
        (IArray.listArray (0, length posmarks - 1) (Seq.sortOn fst posmarks))
        fptr)

-- | Marks starting at the first mark >= the given pos, to the end.
forward :: Marklist -> TrackPos -> [(TrackPos, Mark)]
forward (Marklist a _) pos = map (a!) [i..snd (IArray.bounds a)]
    where i = Util.Array.bsearch_on fst a pos

-- | Like 'forward', but don't include a mark equal to @pos@.
forward_from marklist pos
    | not (null marks) && fst (head marks) == pos = tail marks
    | otherwise = marks
    where marks = forward marklist pos

-- | Marks starting at the first mark <= the given pos, to the beginning.
backward :: Marklist -> TrackPos -> [(TrackPos, Mark)]
backward (Marklist a _) pos = map (a IArray.!) ixs2
    where
    i = min (snd (IArray.bounds a)) (Util.Array.bsearch_on fst a pos)
    ixs = [i, i-1 .. 0]
    ixs2 = if not (null ixs) && pos < fst (a ! head ixs) then tail ixs else ixs

-- | Like 'backward', but don't include a mark equal to @pos@.
backward_from marklist pos
    | not (null marks) && fst (head marks) == pos = tail marks
    | otherwise = marks
    where marks = backward marklist pos


-- Storable

#include "c_interface.h"

newtype MarkMarshal = MarkMarshal (TrackPos, Mark)
data CMarklist

-- 'marklist_new' is responsible for freeing all storage in 'marks'.
foreign import ccall unsafe "marklist_new"
    c_marklist_new :: CInt -> Ptr MarkMarshal -> IO (Ptr CMarklist)
foreign import ccall unsafe "&marklist_destroy"
    c_marklist_destroy :: FunPtr (Ptr CMarklist -> IO ())

instance Storable MarkMarshal where
    sizeOf _ = #size MarkMarshal
    alignment _ = undefined
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


-- test

test_fwd_bwd = do
    mlist <- create_marklist (zip pos (repeat null_mark))
    let eq f pos expected
            = Test.check_equal (map fst (f mlist (TrackPos pos))) expected
    -- before, equal to, after
    eq forward 0 pos
    eq forward_from 0 pos
    eq forward 10 pos
    eq forward_from 10 (tail pos)
    eq forward 15 (tail pos)
    eq forward 35 []

    -- before, equal to, after
    eq backward 0 []
    eq backward_from 0 []
    eq backward 10 (take 1 pos)
    eq backward_from 10 []
    eq backward 15 (take 1 pos)
    eq backward 35 (reverse pos)
    where
    pos = map TrackPos [10, 20, 30]
