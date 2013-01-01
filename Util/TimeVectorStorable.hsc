{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- | Storable instances for unboxed TimeVector values, declared separately to
-- avoid an hsc dependence for TimeVector.
--
-- The Storable instances are used both by vector and when the signals are
-- copied to C, so they have to produce structs as expected by C.
module Util.TimeVectorStorable where
import Foreign
import qualified Util.ForeignC as C
import qualified Ui.Util as Util
import qualified Perform.RealTime as RealTime


#include "Ui/c_interface.h"
-- See comment in BlockC.hsc.
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

type X = RealTime.RealTime

instance Storable (Sample Double) where
    sizeOf _ = #size ControlSample
    alignment _ = #{alignment ControlSample}
    poke sp (Sample time val) = do
        (#poke ControlSample, time) sp time
        (#poke ControlSample, val) sp (Util.c_double val)
    peek sp = do
        time <- (#peek ControlSample, time) sp
        val <- (#peek ControlSample, val) sp
        return $ Sample time (Util.hs_double val)

instance C.CStorable (Sample Double) where
    sizeOf = sizeOf
    alignment = alignment
    peek = peek
    poke = poke

data Sample y = Sample {
    sx :: {-# UNPACK #-} !X
    , sy :: !y
    } deriving (Read, Show, Eq)
