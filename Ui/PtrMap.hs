-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This holds the 'view_id_to_ptr' global variable.  Only very low level
-- modules should import this.
module Ui.PtrMap (
    -- * error
    FltkException(..), throw
    -- * views
    , CView, modify, getMap, get, lookup, lookupId
    , viewExists
    -- * keycaps
    , lookupKeycaps, setKeycaps
) where
import           Prelude hiding (lookup)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import           ForeignC (Ptr)
import qualified System.IO.Unsafe as Unsafe

import qualified Ui.KeycapsT as KeycapsT

import           Global
import           Types


-- * error

newtype FltkException = FltkException String
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
{-# NOINLINE globalWindows #-}
globalWindows :: MVar.MVar Windows
globalWindows = Unsafe.unsafePerformIO $ MVar.newMVar $ Windows
    { _blocks = Map.empty
    , _keycaps = Nothing
    }

data Windows = Windows
    { _blocks :: Map ViewId (Ptr CView)
    , _keycaps :: Maybe (Ptr KeycapsT.CWindow)
    }

modify :: (Map ViewId (Ptr CView) -> IO (Map ViewId (Ptr CView))) -> IO ()
modify modify = MVar.modifyMVar_ globalWindows $ \windows -> do
    blocks <- modify (_blocks windows)
    pure $ windows { _blocks = blocks }

getMap :: IO (Map ViewId (Ptr CView))
getMap = _blocks <$> MVar.readMVar globalWindows

get :: ViewId -> IO (Ptr CView)
get viewId = do
    blocks <- _blocks <$> MVar.readMVar globalWindows
    case Map.lookup viewId blocks of
        Nothing -> throw $ show viewId ++ " not in displayed view list: "
            ++ show (Map.assocs blocks)
        Just viewp -> pure viewp

lookup :: ViewId -> IO (Maybe (Ptr CView))
lookup viewId = do
    blocks <- _blocks <$> MVar.readMVar globalWindows
    pure $ Map.lookup viewId blocks

-- | Nothing indicates that the UI returned a view ptr I didn't know I had.
-- It's rare, but it can happen if I close a window but a msg about it is still
-- in the queue.
lookupId :: Ptr CView -> IO (Maybe ViewId)
lookupId viewp = do
    blocks <- _blocks <$> MVar.readMVar globalWindows
    pure $ fst <$> List.find ((==viewp) . snd) (Map.toList blocks)

viewExists :: ViewId -> IO Bool
viewExists = fmap Maybe.isJust . lookup

-- * keycaps

lookupKeycaps :: IO (Maybe (Ptr KeycapsT.CWindow))
lookupKeycaps = _keycaps <$> MVar.readMVar globalWindows

setKeycaps :: Maybe (Ptr KeycapsT.CWindow) -> IO ()
setKeycaps keycaps = MVar.modifyMVar_ globalWindows $ \windows ->
    pure $ windows { _keycaps = keycaps }
