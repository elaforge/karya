-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveDataTypeable #-}
-- | This holds the 'view_id_to_ptr' global variable.  Only very low level
-- modules should import this.
module Ui.PtrMap (
    -- * error
    FltkException(..), throw
    -- * views
    , CView, modify, get_map, get, lookup, lookup_id
    , view_exists
) where
import Prelude hiding (lookup)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Typeable as Typeable

import qualified System.IO.Unsafe as Unsafe

import ForeignC (Ptr)
import Types


-- * error

newtype FltkException = FltkException String deriving (Typeable.Typeable)
instance Exception.Exception FltkException
instance Show FltkException where
    show (FltkException msg) = "FltkException: " ++ msg

throw :: String -> IO a
throw = Exception.throwIO . FltkException

-- * views

-- | Phantom type for block view ptrs.
data CView

-- | Global map of view IDs to their windows.  This is global mutable state,
-- but I don't feel too bad about it because the underlying window system
-- state is also global mutable state, and this is just a handle on that.
{-# NOINLINE view_id_to_ptr #-}
view_id_to_ptr :: MVar.MVar (Map.Map ViewId (Ptr CView))
view_id_to_ptr = Unsafe.unsafePerformIO (MVar.newMVar Map.empty)

modify :: (Map.Map ViewId (Ptr CView) -> IO (Map.Map ViewId (Ptr CView)))
    -> IO ()
modify = MVar.modifyMVar_ view_id_to_ptr

get_map :: IO (Map.Map ViewId (Ptr CView))
get_map = MVar.readMVar view_id_to_ptr

get :: ViewId -> IO (Ptr CView)
get view_id = do
    ptr_map <- MVar.readMVar view_id_to_ptr
    case Map.lookup view_id ptr_map of
        Nothing -> throw $ show view_id ++ " not in displayed view list: "
            ++ show (Map.assocs ptr_map)
        Just viewp -> return viewp

lookup :: ViewId -> IO (Maybe (Ptr CView))
lookup view_id = do
    ptr_map <- MVar.readMVar view_id_to_ptr
    return $ Map.lookup view_id ptr_map

-- | Nothing indicates that the UI returned a view ptr I didn't know I had.
-- It's rare, but it can happen if I close a window but a msg about it is still
-- in the queue.
lookup_id :: Ptr CView -> IO (Maybe ViewId)
lookup_id viewp = do
    ptr_map <- MVar.readMVar view_id_to_ptr
    return $ fst <$> List.find ((==viewp) . snd) (Map.assocs ptr_map)

view_exists :: ViewId -> IO Bool
view_exists = fmap Maybe.isJust . lookup
