-- | Calls that just apply attributes to the notes.  There is already general
-- purpose syntax for this, e.g. @attr = +x@ or @n +x@, and instruments may
-- supply special calls for their attributes, but there are several attributes
-- which look nice with their own calls and are used by many instruments.
--
-- TODO There are too many ways to apply attributes to notes, and they work
-- in inconsistent ways.
module Derive.Call.Attribute where
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import qualified Derive.Derive as Derive
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("o", attributed_note Attrs.harmonic)
    , ("m", attributed_note Attrs.mute)
    , ("{", attributed_note Attrs.legato)
    ]


attributed_note :: Attrs.Attributes -> Derive.NoteCall
attributed_note attrs = Derive.Call
    { Derive.call_name = "note with " ++ ShowVal.show_val attrs
    , Derive.call_generator = Just $ Derive.generator_call
        ("Apply attributes to notes. When applied as a note transformer\
        \ (i.e. it has notes in child tracks) it applies its attributes to\
        \ those notes. Otherwise, it applies its attributes to the null note\
        \ call."
        ) generator
    , Derive.call_transformer = Just $ Derive.transformer_call
        "Apply attributes to the transformed deriver." transformer
    }
    where
    generator = CallSig.call0g $ \args -> case Note.sub_events args of
        [] -> add_attrs $ Call.reapply_call args (TrackLang.call "" [])
        subs -> Note.place (Note.map_events add_attrs (concat subs))
    transformer = CallSig.call0t $ \_ deriver -> add_attrs deriver
    add_attrs = Util.add_attrs attrs
