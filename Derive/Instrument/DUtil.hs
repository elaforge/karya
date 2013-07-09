-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions for instrument cmds.  This is called DUtil because there is also
-- "Cmd.Instrument.CUtil" and they are usually imported together.
--
-- I need a better name than \"Util\" for everything.
module Derive.Instrument.DUtil where
import Util.Control
import qualified Derive.Call as Call
import qualified Derive.Call.Articulation as Articulation
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified App.MidiInst as MidiInst


-- | Make a call that simply calls the default note call with the given attrs.
attrs_note :: Score.Attributes -> Derive.NoteCall
attrs_note attrs =
    Derive.stream_generator ("attrs_note " <> ShowVal.show_val attrs)
        Tags.attr
        "Invoke the default note call with the given attrs." $
    Sig.call0 $ \args ->
    Util.add_attrs attrs $ Call.reapply_call args (TrackLang.call "" [])

-- | This re-applies the default note call and wraps it in a transformer.
-- However, it's more direct and probably clearer to directly create
-- a transformed note call via 'Note.transformed_note' or 'Note.note_call'.
note_call :: Text -> (Derive.EventDeriver -> Derive.EventDeriver)
    -> Derive.NoteCall
note_call name transform = Derive.stream_generator name mempty
    "Invoke the default note call with a certain transform." $
    Sig.call0 $ \args -> Note.when_under_inversion args transform $
        Call.reapply_call args (TrackLang.call "" [])

-- | Make a note and add the attribute if it's 0 duration.
note0_attrs :: Score.Attributes -> Derive.NoteCall
note0_attrs attrs = postproc_note ("note0 " <> ShowVal.show_val attrs)
    "Invoke the default note call, but add attrs if it has a zero duration." $
    \evt -> if Score.event_duration evt /= 0 then evt
        else Score.modify_attributes (attrs<>) evt

-- | Just like the default note call, except apply a function to the output.
postproc_note :: Text -> Text -> (Score.Event -> Score.Event)
    -> Derive.NoteCall
postproc_note name doc f = postproc_generator name doc Note.c_note apply
    where apply d = map (fmap f) <$> d

-- | Transform the generator of an existing call by applying a function to it.
-- It gets a new name and the documentation is prepended to the documentation
-- of the original call.
postproc_generator :: Text -> Text -> Derive.Call d
    -> (Derive.LogsDeriver d -> Derive.LogsDeriver d)
    -> Derive.Call d
postproc_generator name doc call f = Derive.Call
    { Derive.call_name = name
    , Derive.call_generator = postproc <$> Derive.call_generator call
    , Derive.call_transformer = Derive.call_transformer call
    }
    where
    postproc (Derive.GeneratorCall func (Derive.CallDoc tags gdoc args_doc)) =
        Derive.GeneratorCall (f . func)
        (Derive.CallDoc tags (doc <> "\n" <> gdoc) args_doc)

-- * code

-- | Instruments that support legato samples should replace the @(@ call
-- with an attribute-setting one, and disable the default note call's +legato
-- behaviour.
legato_samples :: [(Text, Derive.NoteCall)]
legato_samples =
        ("(", Articulation.c_attr_legato)
        : MidiInst.null_call note_call
    where
    note_call = Note.note_call "" "" mempty $ Note.default_note $
        Note.use_attributes { Note.config_legato = False }
