-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Per-patch calls.
module Synth.Faust.Code where
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig

import qualified Perform.Signal as Signal

import Types


note_terminate :: ScoreT.Control -> RealTime -> Derive.Transformer Derive.Note
note_terminate control decay = Derive.transformer Module.instrument
    "note-terminate" (Tags.postproc <> Tags.inst)
    "Terminate a note with a control change."
    $ Sig.callt (
    Sig.defaulted "value" (0.1 :: Double) "Move to this value."
    ) $ \value _args -> fmap (Post.emap1_ (apply decay value))
    where
    apply decay value event = Score.modify_signal control
        (modify decay value (Score.event_end event)) event
    modify decay value end = Signal.sig_multiply
        (Signal.from_pairs
            [(Signal.beginning, 1), (end, 1), (end + decay, value)])
