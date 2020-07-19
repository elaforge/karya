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
    -- * keymap
    , lookup_keymap, set_keymap
) where
import           Prelude hiding (lookup)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Typeable as Typeable

import           ForeignC (Ptr)
import qualified System.IO.Unsafe as Unsafe

import qualified Ui.KeymapT as KeymapT

import           Global
import           Types


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
{-# NOINLINE global_windows #-}
global_windows :: MVar.MVar Windows
global_windows = Unsafe.unsafePerformIO $ MVar.newMVar $ Windows
    { _blocks = Map.empty
    , _keymap = Nothing
    }

data Windows = Windows {
    _blocks :: Map ViewId (Ptr CView)
    , _keymap :: Maybe (Ptr KeymapT.CWindow)
    }

modify :: (Map ViewId (Ptr CView) -> IO (Map ViewId (Ptr CView))) -> IO ()
modify modify = MVar.modifyMVar_ global_windows $ \windows -> do
    blocks <- modify (_blocks windows)
    return $ windows { _blocks = blocks }

get_map :: IO (Map ViewId (Ptr CView))
get_map = _blocks <$> MVar.readMVar global_windows

get :: ViewId -> IO (Ptr CView)
get view_id = do
    blocks <- _blocks <$> MVar.readMVar global_windows
    case Map.lookup view_id blocks of
        Nothing -> throw $ show view_id ++ " not in displayed view list: "
            ++ show (Map.assocs blocks)
        Just viewp -> return viewp

lookup :: ViewId -> IO (Maybe (Ptr CView))
lookup view_id = do
    blocks <- _blocks <$> MVar.readMVar global_windows
    return $ Map.lookup view_id blocks

-- | Nothing indicates that the UI returned a view ptr I didn't know I had.
-- It's rare, but it can happen if I close a window but a msg about it is still
-- in the queue.
lookup_id :: Ptr CView -> IO (Maybe ViewId)
lookup_id viewp = do
    blocks <- _blocks <$> MVar.readMVar global_windows
    return $ fst <$> List.find ((==viewp) . snd) (Map.toList blocks)

view_exists :: ViewId -> IO Bool
view_exists = fmap Maybe.isJust . lookup

-- * keymap

lookup_keymap :: IO (Maybe (Ptr KeymapT.CWindow))
lookup_keymap = _keymap <$> MVar.readMVar global_windows

set_keymap :: Maybe (Ptr KeymapT.CWindow) -> IO ()
set_keymap keymap = MVar.modifyMVar_ global_windows $ \windows ->
    return $ windows { _keymap = keymap }
