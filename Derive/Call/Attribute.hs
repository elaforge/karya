-- | Calls that just apply attributes to the notes.  There is already general
-- purpose syntax for this, e.g. @attr = +x@ or @n +x@, and instruments may
-- supply special calls for their attributes, but there are several attributes
-- which look nice with their own calls and are used by many instruments.
--
-- TODO There are too many ways to apply attributes to notes, and they work
-- in inconsistent ways.
module Derive.Call.Attribute where
import Util.Control
import qualified Util.Seq as Seq
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.ParseBs as ParseBs
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang


lookup_attr :: Derive.LookupCall Derive.NoteCall
lookup_attr = Derive.pattern_lookup "attribute starting with `+`" doc $
    \(TrackLang.Symbol sym) -> parse_symbol sym
    where
    parse_symbol sym@(c:_)
        | c == '+' || c == '=' = case ParseBs.parse_val sym of
            Right (TrackLang.VRelativeAttrs rel) -> return $ Just $ call rel
            _ -> return Nothing
    parse_symbol _ = return Nothing
    call rel = transform_notes ("relative attrs: " ++ ShowVal.show_val rel)
        "Doc unused." "Doc unused."
        (Util.with_attrs (TrackLang.apply_attr rel))
    doc = Derive.extract_doc $ attributed_note (Score.attr "example-attr")

note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("o", attributed_note Attrs.harm)
    , ("m", attributed_note Attrs.mute)
    , (".", attributed_note Attrs.staccato)
    , ("(", c_legato)
    , ("{", attributed_note Attrs.porta)
    ]

attributed_note :: Attrs.Attributes -> Derive.NoteCall
attributed_note attrs =
    transform_notes ("note with " ++ ShowVal.show_val attrs)
        ("Apply attributes to notes. When applied as a note transformer\
        \ (i.e. it has notes in child tracks) it applies its attributes to\
        \ those notes. Otherwise, it applies its attributes to the null note\
        \ call.")
        "Apply attributes to the transformed deriver."
        (Util.add_attrs attrs)

transform_notes :: String -> String -> String
    -> (Derive.EventDeriver -> Derive.EventDeriver) -> Derive.NoteCall
transform_notes name generator_doc transform_doc transform = Derive.Call
    { Derive.call_name = name
    , Derive.call_generator = Just $
        Derive.generator_call Tags.attr generator_doc generator
    , Derive.call_transformer = Just $
        Derive.transformer_call (Tags.attr <> Tags.subs) transform_doc
            transformer
    }
    where
    generator = Sig.call0 $ \args -> Note.sub_events args >>= \x -> case x of
        [] -> transform $ Note.inverting Util.placed_note args
        subs -> Note.place $ Note.map_events transform (concat subs)
    transformer = Sig.call0t $ \_args deriver -> transform deriver

c_legato :: Derive.NoteCall
c_legato = Derive.stream_generator "legato" (Tags.attr <> Tags.subs <> Tags.ly)
    ("Play the transformed notes legato.  This sets `+legato` on all notes\
    \ except the last one. The default note deriver will respond to `+legato`\
    \ and " <> ShowVal.doc_val Score.c_legato_overlap <> "."
    ) $ Sig.call0 $ init_attr Attrs.legato

c_legato_ly :: Derive.NoteCall
c_legato_ly = Derive.stream_generator "legato-ly" (Tags.subs <> Tags.ly)
    "Add a lilypond slur." $ Sig.call0 $
        Lily.notes_around_ly (Lily.SuffixFirst, "(") (Lily.SuffixLast, ")")

-- | Like 'c_legato', but apply the attribute to all notes instead of all but
-- the last.  This is when the instrument itself responds to legato, e.g. with
-- a keyswitch for transition samples, rather than the call responding by
-- lengthening notes.
--
-- If you use this, you should definitely turn off 'Note.config_legato'.
c_legato_all :: Derive.NoteCall
c_legato_all = attributed_note Attrs.legato

-- | Apply the attributes to the init of the sub-events, i.e. every one but the
-- last.
init_attr :: Score.Attributes -> Derive.PassedArgs d -> Derive.EventDeriver
init_attr attr = Note.place . concatMap add <=< Note.sub_events
    where
    add notes = case Seq.viewr notes of
        (notes, Just last) ->
            Note.map_events (Util.add_attrs attr) notes ++ [last]
        _ -> []
