-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Binding to libsamplerate.
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Util.Audio.SampleRateC (
    State, Quality(..), new, delete, setRatio
    , Input(..), Output(..)
    , process
    , Exception(..)
    -- * SavedState
    , SavedState(..)
    , getState, putState
    , getRatio
) where
import qualified Control.Exception as Exception
import Control.Monad (when)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as Unsafe

import qualified Foreign
import Foreign (alloca, peek)
import qualified Foreign.C as C
import qualified GHC.Stack as Stack
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack


type Channels = Int

new :: Quality -> Channels -> IO State
new quality channels = Foreign.alloca $ \errp -> do
    State state <- src_new quality channels errp
    when (state == Foreign.nullPtr) $
        throw "new" =<< Foreign.peek errp
    return $ State state

delete :: State -> IO ()
delete state = src_delete state >> return ()

setRatio :: State -> Double -> IO ()
setRatio state ratio = check "setRatio" =<< src_set_ratio state ratio

-- | This corresponds to the input part of SRC_DATA.
data Input = Input {
    data_in :: Foreign.Ptr C.CFloat
    , data_out :: Foreign.Ptr C.CFloat
    , input_frames :: Integer
    , output_frames :: Integer
    , src_ratio :: Double
    , end_of_input :: Bool
    } deriving (Eq, Show)

-- | This corresponds to the output part of SRC_DATA.
data Output = Output {
    input_frames_used :: !Integer
    , output_frames_generated :: !Integer
    } deriving (Eq, Show)

process :: State -> Input -> IO Output
process state (Input {..}) =
    Foreign.allocaBytes {#sizeof SRC_DATA #} $ \datap -> do
        {#set SRC_DATA.data_in      #} datap data_in
        {#set SRC_DATA.data_out     #} datap data_out
        {#set SRC_DATA.input_frames #} datap $ fromIntegral input_frames
        {#set SRC_DATA.output_frames #} datap $ fromIntegral output_frames
        {#set SRC_DATA.src_ratio    #} datap $ realToFrac src_ratio
        {#set SRC_DATA.end_of_input #} datap $ Foreign.fromBool end_of_input
        check "process" =<< src_process state (Data datap)
        Output
            <$> (fromIntegral <$> {#get SRC_DATA.input_frames_used #} datap)
            <*> (fromIntegral <$> {#get SRC_DATA.output_frames_gen #} datap)

-- * errors

newtype Exception = Exception Text.Text deriving (Eq, Show)
instance Exception.Exception Exception

throw :: Stack.HasCallStack => Text.Text -> C.CInt -> IO ()
throw caller code = do
    errp <- src_strerror code
    err <- if errp == Foreign.nullPtr
        then return "src_strerror returned null"
        else C.peekCString errp
    CallStack.throw Exception $ caller <> ": " <> Text.pack err

check :: Stack.HasCallStack => Text.Text -> Int -> IO ()
check caller code
    | code == 0 = return ()
    | otherwise = throw caller (fromIntegral code)

-- * bindings

#include <samplerate.h>

{#pointer *SRC_STATE as State newtype #}
{#pointer *SRC_DATA as Data newtype #}

{#enum define Quality
    { SRC_SINC_BEST_QUALITY as SincBestQuality
    , SRC_SINC_MEDIUM_QUALITY as SincMediumQuality
    , SRC_SINC_FASTEST as SincFastest
    , SRC_ZERO_ORDER_HOLD as ZeroOrderHold
    , SRC_LINEAR as Linear
    } deriving (Eq, Show, Bounded)
#}

{#fun src_new { `Quality', `Int', id `Foreign.Ptr C.CInt' } -> `State' #}
{#fun src_delete { `State' } -> `State' #}
{#fun src_set_ratio { `State', `Double' } -> `Int' #}
{#fun src_strerror { id `C.CInt' } -> `C.CString' id #}
{#fun src_process { `State', `Data' } -> `Int' #}

type CSize = {#type size_t #}
{#pointer *SRC_STATE_FLAT as StateFlat newtype #}

data SavedState = SavedState !ByteString.ByteString !ByteString.ByteString
    deriving (Eq, Show)

-- | The second ByteString in SavedState is done without copying, so use it
-- before doing anything with State!
--
-- This is only in my fork of libsamplerate, referenced in default.nix.
getState :: State -> IO SavedState
getState state = do
    let size1 = {#sizeof SRC_STATE_FLAT #}
    ptr <- Foreign.mallocBytes size1
    (size2, state2p) <- src_get_state state (StateFlat ptr)

    -- This is safe because I allocated the memory.
    state1 <- Unsafe.unsafePackMallocCStringLen (Foreign.castPtr ptr, size1)
    -- This is unsafe, because I'm reusing libsamplerate's memory, and
    -- libsamplerate will definitely mutate it on the next 'process' call.
    -- See TODO non-copying state:.
    state2 <- Unsafe.unsafePackCStringLen
        (Foreign.castPtr state2p, fromIntegral size2)
    -- Safe version:
    -- state2 <- ByteString.packCStringLen
    --     (Foreign.castPtr state2p, fromIntegral size2)

    return $ SavedState state1 state2

-- void src_get_state(SRC_STATE *state, SRC_STATE_FLAT *saved1,
--   size_t *size, void **private)
{#fun src_get_state
    { `State'
    , `StateFlat'
    , alloca- `CSize' peek*
    , alloca- `Foreign.Ptr ()' peek*
    } -> `()'
#}

putState :: Quality -> State -> SavedState -> IO Bool
putState quality state (SavedState state1 state2) =
    Unsafe.unsafeUseAsCString state1 $ \state1p ->
    Unsafe.unsafeUseAsCStringLen state2 $ \(state2p, size2) ->
        src_put_state quality state (StateFlat (Foreign.castPtr state1p))
            (fromIntegral size2) (Foreign.castPtr state2p)

-- int src_put_state(
--     int converter_type,
--     SRC_STATE *state, SRC_STATE_FLAT *saved1, size_t size, void *private) ;
{#fun src_put_state
    { `Quality', `State', `StateFlat', id `CSize', id `Foreign.Ptr ()'
    } -> `Bool'
#}

getRatio :: State -> IO Double
getRatio (State state) = realToFrac <$> {#get SRC_PRIVATE.last_ratio #} state
