-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls for randomized scores.
module Derive.Call.Random where
import Util.Control
import qualified Util.Seq as Seq
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang


-- * note calls

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [("alt", c_alternate), ("alt-t", c_alternate_tracks)]
    [("omit", c_omit)]

control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.call_maps
    [("alt", c_alternate)]
    [("omit", c_omit)]

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.call_maps
    [("alt", c_alternate)]
    [("omit", c_omit)]

c_omit :: Derive.Callable d => Derive.Transformer d
c_omit = Derive.transformer Module.prelude "omit" Tags.random
    "Omit the derived call a certain percentage of the time."
    $ Sig.callt
    (Sig.defaulted "chance" 0.5
        "Chance, from 0 to 1, that the transformed note will be omitted."
    ) $ \omit _args deriver -> ifM (Util.chance omit) (return mempty) deriver

c_alternate :: Derive.Callable d => Derive.Generator d
c_alternate = Derive.make_call Module.prelude "alternate" Tags.random
    ("Pick one of several expressions and evaluate it.\
    \ They have to be strings since calls themselves are not first class."
    ) $ Sig.call (Sig.many1 "expr" "Expression to evaluate.") $
    \exprs args -> Call.reapply_string args =<< Util.pick exprs

c_alternate_tracks :: Derive.Generator Derive.Note
c_alternate_tracks = Derive.make_call Module.prelude "alternate-tracks"
    (Tags.random <> Tags.subs) "Evaluate notes from one of the sub-tracks."
    $ Sig.call
    (Sig.many "weight" "Likelihood to choose each child track. Each number is\
        \ a relative weight, and tracks without a number default to 1. It's\
        \ an error to have more numbers than tracks.") $
    \weights args -> do
        subs <- Sub.sub_events args
        let err =  "more weights than tracks: " <> show (length weights)
                <> " > " <> show (length subs) <> " tracks"
        sub_weights <- mapM (pair err) $ Seq.padded_zip subs weights
        Sub.place . pick_weighted sub_weights [] =<< Util.random
    where
    pair _ (Seq.Both sub weight) = return (sub, weight)
    pair _ (Seq.First sub) = return (sub, 1)
    pair err (Seq.Second _) = Derive.throw err

pick_weighted :: [(a, Double)] -> a -> Double -> a
pick_weighted weights deflt rnd_ = go 0 weights
    where
    rnd = rnd_ * sum (map snd weights)
    go _ [] = deflt
    go _ [(a, _)] = a
    go collect ((a, weight) : weights)
        | collect + weight > rnd = a
        | otherwise = go (collect + weight) weights


-- * val calls

val_calls :: Derive.CallMap Derive.ValCall
val_calls = Derive.call_map
    [ ("pick", c_pick) -- or ?
    , ("range", c_range) -- or -?
    ]

c_pick :: Derive.ValCall
c_pick = Derive.val_call Module.prelude "pick" Tags.random
    "Pick one of the arguments randomly." $
        Sig.call (Sig.many1 "val" "Value of any type.") $ \vals _ ->
            Util.pick (vals :: NonEmpty TrackLang.Val)

c_range :: Derive.ValCall
c_range = Derive.val_call Module.prelude "range" Tags.random
    "Pick a random number within a range." $ Sig.call ((,)
    <$> Sig.defaulted "low" 0 "Bottom of range, inclusive."
    <*> Sig.defaulted "high" 1 "Top of range, inclusive."
    ) $ \(low, high) _args -> Util.random_in low high :: Derive.Deriver Double
