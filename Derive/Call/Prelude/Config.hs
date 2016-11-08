-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls that configure other calls.
module Derive.Call.Prelude.Config where
import qualified Data.List.NonEmpty as NonEmpty

import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Flags as Flags
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import Global


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("add-flag", c_add_flag)
    , ("h", c_hold)
    , ("infer-dur", c_infer_dur)
    ]

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.transformer_call_map
    [ ("h", c_hold)
    ]

control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.transformer_call_map
    [ ("h", c_hold)
    ]

c_add_flag :: Derive.Transformer Derive.Note
c_add_flag = Derive.transformer Module.prelude "add-flag" Tags.postproc
    "Add the given flags to transformed events.\
    \ Mostly for debugging and testing."
    $ Sig.callt (Sig.many1 "flag" "Add these flags.") $ \flags _args ->
        fmap $ Post.emap1_ $ Score.add_flags $ mconcatMap Flags.flag $
            NonEmpty.toList flags

c_hold :: Derive.Taggable d => Derive.Transformer d
c_hold = Make.environ Module.prelude EnvKey.hold "'Derive.EnvKey.hold'"
    (Sig.defaulted "time" (Typecheck.real 0.25) "Hold this long.")
    Typecheck._real

c_infer_dur :: Derive.Transformer Derive.Note
c_infer_dur = Derive.transformer Module.prelude "infer-dur" Tags.postproc
    "Add 'Derive.Flags.infer_duration' to the events."
    $ Sig.call0t $ \_args ->
        fmap $ Post.emap1_ $ Score.add_flags Flags.infer_duration
