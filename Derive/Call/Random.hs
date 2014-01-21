-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for randomized scores.
module Derive.Call.Random where
import Util.Control
import qualified Derive.Call as Call
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, many1)
import qualified Derive.TrackLang as TrackLang


-- * note calls

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [("alt", c_alternate)]
    [("omit", c_omit)]

control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.call_maps
    [("alt", c_alternate)]
    [("omit", c_omit)]

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.call_maps
    [("alt", c_alternate)]
    [("omit", c_omit)]

c_omit :: (Derive.Callable d) => Derive.Transformer d
c_omit = Derive.transformer "omit" Tags.random
    "Omit the derived call a certain percentage of the time."
    $ Sig.callt
    (defaulted "omit" 0.5
        "Chance, from 0 to 1, that the transformed note will be omitted."
    ) $ \omit _args deriver -> ifM (Util.chance omit) (return mempty) deriver

c_alternate :: (Derive.Callable d) => Derive.Generator d
c_alternate = Derive.make_call "alternate" Tags.random
    ("Pick one of several expressions and evaluate it.\
    \ They have to be strings since calls themselves are not first class."
    ) $ Sig.call (many1 "expr" "Expression to evaluate.") $
    \exprs args -> Call.reapply_string args =<< Util.pick exprs


-- * val calls

val_calls :: Derive.ValCallMap
val_calls = Derive.make_calls
    [ ("pick", c_pick) -- or ?
    , ("range", c_range) -- or -?
    ]

c_pick :: Derive.ValCall
c_pick = Derive.val_call "pick" Tags.random
    "Pick one of the arguments randomly." $
        Sig.call (many1 "val" "Value of any type.") $ \vals _ ->
            Util.pick (vals :: NonEmpty TrackLang.Val)

c_range :: Derive.ValCall
c_range = Derive.val_call "range" Tags.random
    "Pick a random number within a range." $ Sig.call ((,)
    <$> defaulted "low" 0 "Bottom of range, inclusive."
    <*> defaulted "high" 1 "Top of range, inclusive."
    ) $ \(low, high) _args -> Util.random_in low high :: Derive.Deriver Double
