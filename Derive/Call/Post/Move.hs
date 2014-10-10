-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Postprocs that change note start and duration.
module Derive.Call.Post.Move where
import Util.Control
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("apply-start-offset", c_apply_start_offset)
    ]

-- | Previously I applied the @%start-s@ and @%start-t@ controls in the note
-- generator, but I wound up with notes getting out of sync with their
-- controls.  Even if I apply the controls before inversion, it still doesn't
-- work other calls, like say block calls, and I can't apply the controls
-- before the block call
c_apply_start_offset :: Derive.Transformer Derive.Note
c_apply_start_offset =
    Derive.transformer Module.prelude "apply-start-offset" Tags.postproc
    ("Apply the " <> ShowVal.doc_val Environ.start_offset_val <> " env var.\
     \ This is set by note deriver from the "
     <> ShowVal.doc_val Controls.start_s <> " and "
     <> ShowVal.doc_val Controls.start_t <> " controls, so if you want those\
     \ controls to have an effect, you have to use this postproc."
    ) $ Sig.call0t $ \_args deriver -> Post.emap1 apply_start_offset <$> deriver

apply_start_offset :: Score.Event -> Score.Event
apply_start_offset event =
    case TrackLang.maybe_val Environ.start_offset_val env of
        Nothing -> event
        Just offset -> Score.move_start Note.min_duration offset event
    where env = Score.event_environ event

-- TODO fancier version that won't move past neighbors:
-- Divide up by instrument and hand, then move each event by the offset control
-- inside it, but not so that it overlaps a neighbor.
