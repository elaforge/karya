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
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.ParseBs as ParseBs
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal


lookup_attr :: Derive.LookupCall Derive.NoteCall
lookup_attr = Derive.pattern_lookup "attribute starting with `+`" doc $
    \(TrackLang.Symbol sym) -> parse_symbol sym
    where
    parse_symbol sym@(c:_)
        | c == '+' || c == '=' = case ParseBs.parse_val sym of
            Right (TrackLang.VRelativeAttrs rel) -> return $ Just $ call rel
            _ -> return Nothing
    parse_symbol _ = return Nothing
    call rel = Make.transform_notes
        ("relative attrs: " ++ ShowVal.show_val rel)
        Tags.attr "Doc unused." Sig.no_args
        (\() -> Util.with_attrs (TrackLang.apply_attr rel))
    doc = Derive.extract_doc $ Make.attributed_note (Score.attr "example-attr")

note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("o", Make.attributed_note Attrs.harm)
    , ("m", Make.attributed_note Attrs.mute)
    , (".", Make.attributed_note Attrs.staccato)
    , ("(", c_legato)
    , ("{", Make.attributed_note Attrs.porta)
    , ("detach", c_detach)
    ]

-- * legato

c_legato :: Derive.NoteCall
c_legato = Derive.stream_generator "legato" (Tags.attr <> Tags.subs <> Tags.ly)
    ("Play the transformed notes legato.  This sets `+legato` on all notes\
    \ except the last one. The default note deriver will respond to `+legato`\
    \ and " <> ShowVal.doc_val Score.c_legato_overlap <> "."
    ) $ Sig.call0 $ init_attr Attrs.legato

c_ly_slur :: Derive.NoteCall
c_ly_slur = Derive.stream_generator "ly-slur" (Tags.subs <> Tags.ly_only)
    "Add a lilypond slur." $ Sig.call0 $
        Lily.notes_around_ly (Lily.SuffixFirst, "(") (Lily.SuffixLast, ")")

c_ly_slur_up :: Derive.NoteCall
c_ly_slur_up = Derive.stream_generator "ly-slur-up" (Tags.subs <> Tags.ly_only)
    "Add a lilypond slur, forced to be above." $ Sig.call0 $
        Lily.notes_around_ly (Lily.SuffixFirst, "^(") (Lily.SuffixLast, ")")

c_ly_slur_down :: Derive.NoteCall
c_ly_slur_down = Derive.stream_generator "ly-slur-down"
    (Tags.subs <> Tags.ly_only)
    "Add a lilypond slur, forced to be below." $ Sig.call0 $
        Lily.notes_around_ly (Lily.SuffixFirst, "_(") (Lily.SuffixLast, ")")

-- | Like 'c_legato', but apply the attribute to all notes instead of all but
-- the last.  This is when the instrument itself responds to legato, e.g. with
-- a keyswitch for transition samples, rather than the call responding by
-- lengthening notes.
--
-- If you use this, you should definitely turn off 'Note.config_legato'.
c_legato_all :: Derive.NoteCall
c_legato_all = Make.attributed_note Attrs.legato

-- | Apply the attributes to the init of the sub-events, i.e. every one but the
-- last.
init_attr :: Score.Attributes -> Derive.PassedArgs d -> Derive.EventDeriver
init_attr attr = Note.place . concatMap add <=< Note.sub_events
    where
    add notes = case Seq.viewr notes of
        (notes, Just last) ->
            Note.map_events (Util.add_attrs attr) notes ++ [last]
        _ -> []

-- * misc

c_detach :: Derive.NoteCall
c_detach = Make.transform_notes "detach" mempty
    ("Detach the notes slightly, by setting "
        <> ShowVal.show_val Score.c_sustain_abs <> ".")
    (Sig.defaulted "time" 0.15 "Set control to `-time`.") $ \time ->
        Derive.with_control Score.c_sustain_abs
            (Score.untyped (Signal.constant (-time)))
    where
