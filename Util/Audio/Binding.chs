-- Temporary copy of from https://github.com/mtolly/conduit-audio, which is
-- Copyright (c)2016, Michael Tolly

{- |
A binding to the <http://www.mega-nerd.com/SRC/api_full.html full API> of @libsamplerate@.
Errors are turned into Haskell exceptions of type 'SRCError'.
The @SRC_DATA@ struct is split into two Haskell types
for the input ('DataIn') and output ('DataOut') parts.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module Util.Audio.Binding
( new, delete, process, reset, setRatio
, Callback, callbackNew, makeCallback, callbackRead
, State, DataIn(..), DataOut(..), ConverterType(..), SRCError(..)
) where

import Foreign hiding (new)
import Foreign.C
import Control.Monad (when)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Typeable (Typeable)
import Control.Exception (Exception, throwIO)

#include <samplerate.h>

inThisModule :: String -> String
inThisModule = ("Data.Conduit.Audio.SampleRate.Binding." ++)

{#pointer *SRC_STATE as State newtype #}
{#pointer *SRC_DATA  as Data  newtype #}

{#context prefix="src_"#}

-- SRC_STATE* src_new (int converter_type, int channels, int *error) ;
{#fun new as newRaw
  { convTypeToC `ConverterType'
  , `Int'
  , id `Ptr CInt'
  } -> `State' #}

-- SRC_STATE* src_delete (SRC_STATE *state) ;
{#fun delete as deleteRaw
  { `State'
  } -> `State' #}

-- int src_process (SRC_STATE *state, SRC_DATA *data) ;
{#fun process as processRaw
  { `State'
  , `Data'
  } -> `Int' #}

-- int src_reset (SRC_STATE *state) ;
{#fun reset as resetRaw
  { `State'
  } -> `Int' #}

-- int src_set_ratio (SRC_STATE *state, double new_ratio) ;
{#fun set_ratio as setRatioRaw
  { `State'
  , `Double'
  } -> `Int' #}

{#enum define ConverterType
  { SRC_SINC_BEST_QUALITY as SincBestQuality
  , SRC_SINC_MEDIUM_QUALITY as SincMediumQuality
  , SRC_SINC_FASTEST as SincFastest
  , SRC_ZERO_ORDER_HOLD as ZeroOrderHold
  , SRC_LINEAR as Linear
  } deriving (Eq, Ord, Show, Read, Bounded) #}

convTypeToC :: ConverterType -> CInt
convTypeToC = fromIntegral . fromEnum

-- const char* src_strerror (int error) ;
{#fun strerror as ^
  { id `CInt'
  } -> `CString' id #}

sampleRateError :: (Integral i) => String -> i -> IO ()
sampleRateError _  0 = return ()
sampleRateError fn i = do
  ps <- strerror $ fromIntegral i
  s <- if ps == nullPtr
    then return "strerror returned NULL"
    else peekCString ps
  throwIO $ SRCError (inThisModule fn) (fromIntegral i) s

-- | @libsamplerate@ functions may throw this as an exception.
-- Contains the function that caused the error, the numeric error code,
-- and a human-readable message.
data SRCError = SRCError String Int String
  deriving (Eq, Ord, Show, Read, Typeable)

instance Exception SRCError

new
  :: ConverterType
  -> Int -- ^ channels
  -> IO State
new ctype chans = alloca $ \perr -> do
  state@(State pstate) <- newRaw ctype chans perr
  when (pstate == nullPtr) $ peek perr >>= sampleRateError "new"
  return state

{-
typedef struct
{   float  *data_in, *data_out ;

    long   input_frames, output_frames ;
    long   input_frames_used, output_frames_gen ;

    int    end_of_input ;

    double src_ratio ;
} SRC_DATA ;
-}

data DataIn = DataIn
  { data_in       :: Ptr CFloat
  , data_out      :: Ptr CFloat
  , input_frames  :: Integer
  , output_frames :: Integer
  , src_ratio     :: Double
  , end_of_input  :: Bool
  } deriving (Eq, Ord, Show)

data DataOut = DataOut
  { input_frames_used :: Integer
  , output_frames_gen :: Integer
  } deriving (Eq, Ord, Show)

process :: State -> DataIn -> IO DataOut
process state input = allocaBytes {#sizeof SRC_DATA#} $ \pdata -> do
  let sdata = Data pdata
  {#set SRC_DATA.data_in       #} sdata $ data_in                      input
  {#set SRC_DATA.data_out      #} sdata $ data_out                     input
  {#set SRC_DATA.input_frames  #} sdata $ fromIntegral $ input_frames  input
  {#set SRC_DATA.output_frames #} sdata $ fromIntegral $ output_frames input
  {#set SRC_DATA.src_ratio     #} sdata $ realToFrac   $ src_ratio     input
  {#set SRC_DATA.end_of_input  #} sdata $ fromBool     $ end_of_input  input
  processRaw state sdata >>= sampleRateError "process"
  DataOut
    <$> fmap fromIntegral ({#get SRC_DATA.input_frames_used #} sdata)
    <*> fmap fromIntegral ({#get SRC_DATA.output_frames_gen #} sdata)

delete :: State -> IO ()
delete state = do
  State p <- deleteRaw state
  when (p /= nullPtr) $ throwIO $
    SRCError (inThisModule "delete") 0 "delete returned non-null pointer"

reset :: State -> IO ()
reset state = resetRaw state >>= sampleRateError "reset"

setRatio :: State -> Double -> IO ()
setRatio state r = setRatioRaw state r >>= sampleRateError "setRatio"


-- * callback API

-- typedef long (*src_callback_t) (void *cb_data, float **data) ;
type Callback = Ptr () -- ^ extra argument, ignored
  -> Ptr (Ptr CFloat) -- ^ update this to point to sample buffer
  -> IO CLong -- ^ number of frames supplied

-- SRC_STATE* src_callback_new(
--     src_callback_t func, int converter_type,
--     int channels,
--     int *error, void* cb_data) ;
{#fun callback_new as callbackNewRaw
  { id `FunPtr Callback'
  , convTypeToC `ConverterType'
  , `Int' -- channels
  , id `Ptr CInt' -- error
  , `Ptr ()'
  } -> `State' #}

callbackNew :: FunPtr Callback -> ConverterType -> Int -> IO State
callbackNew callback ctype chans = alloca $ \perr -> do
  state@(State pstate) <- callbackNewRaw callback ctype chans perr nullPtr
  when (pstate == nullPtr) $ peek perr >>= sampleRateError "callbackNew"
  return state

foreign import ccall "wrapper"
  makeCallbackRaw :: Callback -> IO (FunPtr Callback)

-- | This allocates memory, you must free it with Foreign.freeHaskellFunPtr!
makeCallback :: IO (Ptr CFloat, CLong) -> IO (FunPtr Callback)
makeCallback getBuffer = makeCallbackRaw $ \_vp bufferpp -> do
  (bufferp, frames) <- getBuffer
  poke bufferpp bufferp
  return frames

callbackRead :: State -> CDouble -> CLong -> Ptr CFloat -> IO CLong
callbackRead state ratio genFrames outp = do
  frames <- callbackReadRaw state ratio genFrames outp
  when (frames == 0) $ do
    err <- errorRaw state
    when (err /= 0) $ sampleRateError "callbackRead" err
  return frames

-- long src_callback_read (SRC_STATE *state, double src_ratio,
--                        long frames, float *data) ;
{#fun callback_read as callbackReadRaw
  { id `State'
  , id `CDouble' -- ^ ratio
  , id `CLong' -- ^ make frames to generate
  , id `Ptr CFloat' -- ^ output buffer
  } -> `CLong' #} -- ^ frames generated

-- int src_error (SRC_STATE *state) ;
{#fun error as errorRaw { id `State' } -> `CInt' #}
