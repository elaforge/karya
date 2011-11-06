{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- | Storable instances for the signal values.  The Storable instances are
-- used both by storablevector and when the signals are copied to C, so they
-- have to produce structs as expected by C.
module Perform.SignalStorable where
import Foreign

import qualified Perform.SignalBase as SignalBase


#include "Ui/c_interface.h"
-- See comment in BlockC.hsc.
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


instance Storable (SignalBase.X, (Float, Float, Float)) where
    sizeOf _ = #size PitchSample
    alignment _ = #{alignment PitchSample}
    poke sp (pos, (from, to, at)) = do
        (#poke PitchSample, time) sp pos
        (#poke PitchSample, from) sp from
        (#poke PitchSample, to) sp to
        (#poke PitchSample, at) sp at
    peek sp = do
        time <- (#peek PitchSample, time) sp
        from <- (#peek PitchSample, from) sp
        to <- (#peek PitchSample, to) sp
        at <- (#peek PitchSample, at) sp
        return (time, (from, to, at))

instance Storable (SignalBase.X, Double) where
    sizeOf _ = #size ControlSample
    alignment _ = #{alignment ControlSample}
    poke sp (time, val) = do
        (#poke ControlSample, time) sp time
        (#poke ControlSample, val) sp val
    peek sp = do
        time <- (#peek ControlSample, time) sp
        val <- (#peek ControlSample, val) sp
        return (time, val)
