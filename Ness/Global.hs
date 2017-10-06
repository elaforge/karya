module Ness.Global where
import qualified Data.Text as Text

import Global


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
-- | From -1 to 1.
type Pan = Double
type Location = Double -- from 0 to 1
type Frequency = Double -- hz

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
