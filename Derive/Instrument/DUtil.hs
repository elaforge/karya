-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions for instrument cmds.  This is called DUtil because there is also
-- "Cmd.Instrument.CUtil" and they are usually imported together.
--
-- I need a better name than \"Util\" for everything.
module Derive.Instrument.DUtil where
import Util.Control
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig


-- | Make a call that simply calls the default note call with the given attrs.
attrs_note :: Score.Attributes -> Derive.Generator Derive.Note
attrs_note attrs =
    Derive.make_call ("attrs_note " <> ShowVal.show_val attrs)
        Tags.attr
        "Invoke the default note call with the given attrs." $
    Sig.call0 $ \args ->
    Util.add_attrs attrs $ Call.reapply_call args "" []

-- | This re-applies the default note call and wraps it in a transformer.
-- However, it's more direct and probably clearer to directly create
-- a transformed note call via 'Note.transformed_note' or 'Note.note_call'.
note_call :: Text -> (Derive.EventDeriver -> Derive.EventDeriver)
    -> Derive.Generator Derive.Note
note_call name transform = Derive.make_call name mempty
    "Invoke the default note call with a certain transform." $
    Sig.call0 $ \args -> Sub.when_under_inversion args transform $
        Call.reapply_call args "" []

zero_duration :: Text -> (Derive.EventDeriver -> Derive.EventDeriver)
    -> Derive.Generator Derive.Note
zero_duration doc transform = Note.transformed_note
    ("A normal note, but modified when it has zero duration: " <> doc) mempty $
    \args deriver -> (if Args.duration args == 0 then transform else id) deriver

-- | Just like the default note call, except apply a function to the output.
postproc_note :: Text -> Text -> (Score.Event -> Score.Event)
    -> Derive.Generator Derive.Note
postproc_note name doc f = postproc_generator name doc Note.c_note apply
    where apply d = map (fmap f) <$> d

-- | Transform an existing call by applying a function to it.  It gets a new
-- name and the documentation is prepended to the documentation of the original
-- call.
postproc_generator :: Text -> Text -> Derive.Call (a -> b) -> (b -> c)
    -> Derive.Call (a -> c)
postproc_generator name new_doc (Derive.Call _ old_doc func) f =
    Derive.Call name (append_doc new_doc old_doc) (f . func)
    where
    append_doc text (Derive.CallDoc tags doc args) =
        Derive.CallDoc tags (doc <> "\n" <> text) args
