-- | Utility functions for writing instrument calls.
module Derive.Instrument.Util where
import Util.Control
import qualified Util.Pretty as Pretty
import qualified Derive.Call as Call
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang


-- | Make a call that simply calls the default note call with the given attrs.
attrs_note :: Score.Attributes -> Derive.NoteCall
attrs_note attrs =
    Derive.stream_generator ("attrs_note " ++ Pretty.pretty attrs) $
    \args -> CallSig.call0 args $ Util.add_attrs attrs $
        Call.reapply_call args (TrackLang.call "" [])

note_call :: String -> (Derive.EventDeriver -> Derive.EventDeriver)
    -> Derive.NoteCall
note_call name transform =
    Derive.stream_generator name $ \args -> CallSig.call0 args $ transform $
        Call.reapply_call args (TrackLang.call "" [])

-- | Make a note and add the attribute if it's 0 duration.
note0_attrs :: Score.Attributes -> Derive.NoteCall
note0_attrs attrs = postproc_note $ \evt ->
    if Score.event_duration evt /= 0 then evt else evt
        { Score.event_attributes = attrs <> Score.event_attributes evt }

-- | Just like the default note call, except apply a function to the output.
postproc_note :: (Score.Event -> Score.Event) -> Derive.NoteCall
postproc_note f = Derive.Call "note"
    (Just (apply . Note.note_generate)) (Just Note.note_transform)
    where
    apply d = do
        events <- d
        return $ map (fmap f) events
