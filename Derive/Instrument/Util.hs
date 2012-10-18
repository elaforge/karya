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
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang


-- | Make a call that simply calls the default note call with the given attrs.
attrs_note :: Score.Attributes -> Derive.NoteCall
attrs_note attrs =
    Derive.stream_generator ("attrs_note " ++ Pretty.pretty attrs)
        "Invoke the default note call with the given attrs." $
    CallSig.call0g $ \args ->
    Util.add_attrs attrs $ Call.reapply_call args (TrackLang.call "" [])

note_call :: String -> (Derive.EventDeriver -> Derive.EventDeriver)
    -> Derive.NoteCall
note_call name transform = Derive.stream_generator name
    "Invoke the default note call with a certain transform." $
    CallSig.call0g $ \args -> Note.when_under_inversion args transform $
        Call.reapply_call args (TrackLang.call "" [])

-- | Make a note and add the attribute if it's 0 duration.
note0_attrs :: Score.Attributes -> Derive.NoteCall
note0_attrs attrs = postproc_note ("note0 " ++ ShowVal.show_val attrs)
    "Invoke the default note call, but add attrs if it has a zero duration." $
    \evt -> if Score.event_duration evt /= 0 then evt
        else Score.modify_attributes (attrs<>) evt

-- | Just like the default note call, except apply a function to the output.
postproc_note :: String -> String -> (Score.Event -> Score.Event)
    -> Derive.NoteCall
postproc_note name doc f = postproc_generator name doc Note.c_note apply
    where
    apply d = do
        events <- d
        return $ map (fmap f) events

-- | Transform the generator of an existing call by applying a function to it.
-- It gets a new name and the documentation is prepended to the documentation
-- of the original call.
postproc_generator :: String -> String -> Derive.Call d
    -> (Derive.LogsDeriver d -> Derive.LogsDeriver d)
    -> Derive.Call d
postproc_generator name doc call f = Derive.Call
    { Derive.call_name = name
    , Derive.call_generator = postproc <$> Derive.call_generator call
    , Derive.call_transformer = Derive.call_transformer call
    }
    where
    postproc (Derive.GeneratorCall func (Derive.CallDoc gdoc args_doc)) =
        Derive.GeneratorCall (f . func)
        (Derive.CallDoc (doc ++ "\n" ++ gdoc) args_doc)
