-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | 'Signal' implementation.
module Synth.Shared.Signal where
import qualified Data.Aeson as Aeson
import qualified Data.Vector.Storable as Vector
import qualified Foreign

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import Global


-- | A time series signal.  It should be sorted by 'x'.  There is implicit
-- linear interpolation between each sample, so a discontinuity requires two
-- samples with the same 'x'.
--
-- TODO This is different from karya's Signal, which doesn't interpolate.  The
-- reason is that I didn't want to have doubled samples for common
-- discontinuities.  But I need interpolation here because it's audio-level and
-- otherwise I get clicks, while karya emits MIDI which relies on the
-- synthesizer doing the same interpolation, just with a hardcoded latency.
-- But then there is a problem, how do I turn a karya Signal into a synth
-- signal?  Maybe I should switch karya back to interpolation?  Or maybe I just
-- have to be careful to make karya emit doubled samples for explicit
-- discontinuities.  Or I could emulate MIDI and do a fixed latency
-- interpolation.
newtype Signal = Signal { vector :: Vector }
    deriving (Eq, Show, Aeson.FromJSON, Aeson.ToJSON, Serialize.Serialize,
        Pretty.Pretty)
    -- TODO I should be able to get a more efficient deserialize with
    -- vector-mmap

data Sample = Sample {
    sx :: {-# UNPACK #-} !X
    , sy :: {-# UNPACK #-} !Y
    } deriving (Eq, Show)

instance Serialize.Serialize Sample where
    get = Sample <$> Serialize.get <*> Serialize.get
    put (Sample x y) = Serialize.put x *> Serialize.put y

instance Pretty.Pretty Sample where
    format (Sample x y) = Pretty.format x <> Pretty.char ':' <> Pretty.format y

type Vector = Vector.Vector Sample
type X = Double
type Y = Double

instance Foreign.Storable Sample where
    alignment _ = Foreign.alignment (0 :: Double)
    sizeOf _ = Foreign.sizeOf (0 :: Double) * 2
    peek sp = Sample <$> Foreign.peekElemOff p 0 <*> Foreign.peekElemOff p 1
        where p = Foreign.castPtr sp
    poke sp (Sample x y) =
        Foreign.pokeElemOff p 0 x *> Foreign.pokeElemOff p 1 y
        where p = Foreign.castPtr sp

instance Aeson.ToJSON Sample where
    toJSON (Sample x y) = Aeson.toJSON (x, y)
instance Aeson.FromJSON Sample where
    parseJSON = fmap (uncurry Sample) . Aeson.parseJSON

empty :: Signal
empty = Signal mempty

constant :: Y -> Signal
constant y = Signal $ Vector.singleton (Sample 0 y)

fromList :: [(X, Y)] -> Signal
fromList = Signal . Vector.fromList . map (uncurry Sample)

toList :: Signal -> [(X, Y)]
toList = map toPair . Vector.toList . vector
    where toPair (Sample x y) = (x, y)

at :: X -> Signal -> Y
at x sig = interpolate vec (highestIndex x vec)
    where
    vec = vector sig
    interpolate vec i
        | Vector.null vec = 0
        | i + 1 >= Vector.length vec = y0
        | i < 0 = 0
        | otherwise = yAt x0 y0 x1 y1 x
        where
        Sample x0 y0 = Vector.unsafeIndex vec i
        Sample x1 y1 = Vector.unsafeIndex vec (i+1)

apply :: (Vector -> Vector) -> Signal -> Signal
apply f (Signal v) = Signal (f v)

mapY :: (Y -> Y) -> Signal -> Signal
mapY f = apply $ Vector.map $ \(Sample x y) -> Sample x (f y)


-- This has some overlaps with Util.TimeVector, but as long as there isn't much
-- it should be ok.  Meanwhile, these types are simpler because they are
-- monomorphic.

-- | Return the highest index of the given X.  So the next value is
-- guaranteed to have a higher x, if it exists.  Return -1 if @x@ is before
-- the first element.  'eta' is added to @x@, so a sample that's
-- almost the same will still be considered a match.
highestIndex :: X -> Vector -> Int
highestIndex x vec
    | Vector.null vec = -1
    | otherwise = i - 1
        where i = bsearchAbove (x + eta) vec

-- | This gets the index of the value *after* @x@.
bsearchAbove :: X -> Vector -> Int
bsearchAbove x vec = go 0 (Vector.length vec)
    where
    go low high
        | low == high = low
        | x >= sx (Vector.unsafeIndex vec mid) = go (mid+1) high
        | otherwise = go low mid
        where mid = (low + high) `div` 2

-- | Eta for comparison.  Since X is seconds, this amount of time is
-- definitely unnoticeable.
eta :: X
eta = 0.0000000000004

-- | Given a line defined by the two points, find the y at the given x.
-- Crashes if called on a vertical line (y0==y1).  Yeah, it's inconsistent
-- with 'x_at'.
yAt :: X -> Y -> X -> Y -> X -> Y
yAt x0 y0 x1 y1 x
    | x0 == x1 = error $ "yAt on vertical line: "
        ++ show ((x0, y0), (x1, y1), x)
    | otherwise = (y1 - y0) / (x1 - x0) * (x - x0) + y0
