{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- | Storable instances for the signal values.  The Storable instances are
-- used both by storablevector and when the signals are copied to C, so they
-- have to produce structs as expected by C.
module Perform.SignalStorable where
import Foreign
import qualified Perform.RealTime as RealTime


#include "Ui/c_interface.h"
-- See comment in BlockC.hsc.
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


instance Storable (RealTime.RealTime, Double) where
    sizeOf _ = #size ControlSample
    alignment _ = #{alignment ControlSample}
    poke sp (time, val) = do
        (#poke ControlSample, time) sp time
        (#poke ControlSample, val) sp val
    peek sp = do
        time <- (#peek ControlSample, time) sp
        val <- (#peek ControlSample, val) sp
        return (time, val)

instance Storable (Sample Double) where
    sizeOf _ = #size ControlSample
    alignment _ = #{alignment ControlSample}
    poke sp (Sample time val) = do
        (#poke ControlSample, time) sp time
        (#poke ControlSample, val) sp val
    peek sp = do
        time <- (#peek ControlSample, time) sp
        val <- (#peek ControlSample, val) sp
        return $ Sample time val

data Sample y = Sample {
    sx :: {-# UNPACK #-} !RealTime.RealTime
    , sy :: !y
    } deriving (Read, Show, Eq)
