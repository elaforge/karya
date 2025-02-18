-- Copyright 2025 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Re-export a minimal version of the optics API.
--
-- I'd like to be able to write @(a #= x) . (b #= y)@ without parentheses,
-- but since (.) already binds the strongest at 9, I can't make (#=) stronger.
-- Besides, I already want (#=) to bind loosely so I can write @x#y #= 1+2@.
-- I would need a version of (.) at 0, but that's too much trouble.
module Util.Lens (
    Lens, lens
    , (#)
    , (#$), (#=), (%=)
    , (<#>)
    -- * data
    , map
) where
import           Prelude hiding (map)
import qualified Data.Map as Map
import qualified Optics.Getter as Getter
import qualified Optics.Lens as Lens
import qualified Optics.Optic as Optic
import qualified Optics.Setter as Setter


-- r is the record, a is the field
type Lens r a = Lens.Lens' r a

lens :: (r -> a) -> (r -> a -> r) -> Lens r a
lens get modify = Lens.lens get modify

(#) :: Lens r a -> Lens a b -> Lens r b
(#) = (Optic.%%)
infixl 9 #

-- | Get: @bval = a#b $# record@
(#$) :: Lens r a -> r -> a
(#$) = Getter.view
infixr 1 #$

-- | Set: @a#b #= 42 record@
(#=) :: Lens r a -> a -> r -> r
(#=) = Setter.set
infix 1 #=

-- | Modify: @a#b %= (+1) record@
(%=) :: Lens r a -> (a -> a) -> r -> r
(%=) = Setter.over
infix 1 %=

-- | Use like @a#b <#> State.get@.
(<#>) :: Functor f => Lens a b -> f a -> f b
(<#>) = fmap . Getter.view
infixl 4 <#> -- same as <$>

-- * data

-- | Modify the map at the given key, or delete if Nothing.
map :: Ord k => k -> Lens (Map.Map k a) (Maybe a)
map k = lens (Map.lookup k) (\m ma -> Map.alter (const ma) k m)
