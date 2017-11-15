-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ness.Global where
import qualified Data.Text as Text

import Global


type SamplingRate = Int

class Render a where
    render :: a -> Text

instance Render Double where
    render n = fromMaybe t $ Text.stripSuffix ".0" t
        where t = showt n
instance Render Int where render = showt
instance Render Bool where render b = if b then "1" else "0"

type Meters = Double
type Seconds = Double
type Newtons = Double
type Kg = Double
type Velocity = Double -- m/s
-- | From -1 to 1.
type Pan = Double
-- | Normalized linear position, from 0 to 1.
type Location = Double
type Frequency = Double -- hz

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
