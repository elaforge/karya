-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- | Storable instances for unboxed TimeVector values, declared separately to
-- avoid an hsc dependence for TimeVector.
--
-- The Storable instances are used both by vector and when the signals are
-- copied to C, so they have to produce structs as expected by C.
module Util.TimeVectorStorable where
import qualified Data.Aeson as Aeson
import Foreign

import qualified Util.CUtil as CUtil
import qualified Util.ForeignC as C
import qualified Util.Serialize as Serialize

import qualified Perform.RealTime as RealTime


#include "Ui/c_interface.h"

data Sample y = Sample {
    sx :: {-# UNPACK #-} !X
    , sy :: !y
    } deriving (Show, Eq)

type X = RealTime.RealTime

instance Storable (Sample Double) where
    sizeOf _ = #size ControlSample
    alignment _ = alignment (0 :: C.CDouble)
    poke sp (Sample time val) = do
        (#poke ControlSample, time) sp time
        (#poke ControlSample, val) sp (CUtil.c_double val)
    peek sp = do
        time <- (#peek ControlSample, time) sp
        val <- (#peek ControlSample, val) sp
        return $ Sample time (CUtil.hs_double val)

-- TODO I think this is necessary all-vector implementations in Util.Segment
-- instance (Storable a, Storable b) => Storable (a, b) where
--     -- vector uses undefined internally, so I can't look at the arguments.
--     -- Proxy or explicit type arguments wouldn't have this problem!
--     sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: b)
--     alignment _ = alignment (undefined :: a)
--     poke p (a, b) = do
--         poke (castPtr p) a
--         poke (castPtr (p `plusPtr` sizeOf a)) b
--     peek p = do
--         a <- peek (castPtr p)
--         b <- peek (castPtr (p `plusPtr` sizeOf a))
--         return (a, b)

instance C.CStorable (Sample Double) where
    sizeOf = sizeOf
    alignment = alignment
    peek = peek
    poke = poke

instance (Serialize.Serialize y) => Serialize.Serialize (Sample y) where
    put (Sample a b) = Serialize.put a >> Serialize.put b
    get = Serialize.get >>= \a -> Serialize.get >>= \b -> return $ Sample a b

instance Aeson.ToJSON (Sample Double) where
    toJSON (Sample x y) = Aeson.toJSON (x, y)
instance Aeson.FromJSON (Sample Double) where
    parseJSON = fmap (uncurry Sample) . Aeson.parseJSON
