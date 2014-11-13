-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls that use 'Environ.track_voice' to infer things about the track.
-- For example, you can automatically have the first and second tracks get
-- @hand=l@ and @hand=r@, respectively.  If you use the local definitions
-- file to bind @>pno = infer-hands@, then every @>pno@ track will both set the
-- instruments and the hand.  Only explicitly named instruments are counted,
-- so you can still have multiple tracks for one hand, if they are named @>@.
--
-- TODO this is experimental, because I'm not sure I like it.  It means
-- parts can change just by inserting a new note track, which seems
-- non-obvious.
module Derive.Call.InferTrackVoice where
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Sig as Sig

import Global


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("infer-hands", c_infer_hands)
    ]

c_infer_hands :: Derive.Taggable d => Derive.Transformer d
c_infer_hands = Derive.transformer Module.prelude "infer-hands" mempty
    "Infer `hand=l` and `hand=r` for the two tracks explicitly named with the\
    \ same instrument. >2 tracks is an error. This only sets `hand` if it\
    \ isn't already set."
    $ Sig.call0t $ \_ deriver ->
        ifM (Derive.is_val_set Environ.hand) deriver $ do
            voice <- Derive.lookup_val Environ.track_voice
            case voice :: Maybe Int of
                Just 0 -> Derive.with_val Environ.hand ("l" :: Text) deriver
                Just 1 -> Derive.with_val Environ.hand ("r" :: Text) deriver
                Just n -> Derive.throw $
                    "expected <=2 track-voices, got " <> show n
                Nothing -> deriver
