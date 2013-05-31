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

note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("omit", c_omit)
    , ("alt", c_alternate)
    ]

control_calls :: Derive.ControlCallMap
control_calls = Derive.make_calls
    [ ("omit", c_omit)
    , ("alt", c_alternate)
    ]

pitch_calls :: Derive.PitchCallMap
pitch_calls = Derive.make_calls
    [ ("omit", c_omit)
    , ("alt", c_alternate)
    ]

c_omit :: (Derive.Derived d) => Derive.Call d
c_omit = Derive.transformer "omit" Tags.random
    "Omit the derived call a certain percentage of the time."
    $ Sig.callt
    (defaulted "omit" 0.5
        "Chance, from 0 to 1, that the transformed note will be omitted."
    ) $ \omit _args deriver -> ifM (Util.chance omit) (return mempty) deriver

c_alternate :: (Derive.Derived d) => Derive.Call d
c_alternate = Derive.stream_generator "alternate" Tags.random
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
            Util.pick vals

c_range :: Derive.ValCall
c_range = Derive.val_call "range" Tags.random
    "Pick a random number within a range." $ Sig.call ((,)
    <$> defaulted "low" 0 "Bottom of range, inclusive."
    <*> defaulted "high" 1 "Top of range, inclusive."
    ) $ \(low, high) _args -> TrackLang.num <$> Util.random_in low high
