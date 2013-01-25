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
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.ParseBs as ParseBs
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import Types


lookup_attr :: Derive.LookupCall Derive.NoteCall
lookup_attr = Derive.pattern_lookup "attribute starting with `+`" doc $
    \(TrackLang.Symbol sym) -> parse_symbol sym
    where
    parse_symbol sym@('+':_) = case ParseBs.parse_val sym of
        Right (TrackLang.VRelativeAttr (TrackLang.RelativeAttr
            (TrackLang.Add, attr))) ->
                return $ Just $ attributed_note (Score.attr attr)
        _ -> return Nothing
    parse_symbol _ = return Nothing
    doc = Derive.extract_doc $ attributed_note (Score.attr "example-attr")

note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("o", attributed_note Attrs.harmonic)
    , ("m", attributed_note Attrs.mute)
    , ("marc", attributed_note Attrs.marcato)
    , (".", attributed_note Attrs.staccato)
    , ("(", c_legato)
    , ("{", c_portamento)
    ]

attributed_note :: Attrs.Attributes -> Derive.NoteCall
attributed_note attrs =
    transform_notes ("note with " ++ ShowVal.show_val attrs)
        (Util.add_attrs attrs)
        ("Apply attributes to notes. When applied as a note transformer\
        \ (i.e. it has notes in child tracks) it applies its attributes to\
        \ those notes. Otherwise, it applies its attributes to the null note\
        \ call.")
        "Apply attributes to the transformed deriver."

transform_notes :: String -> (Derive.EventDeriver -> Derive.EventDeriver)
    -> String -> String -> Derive.NoteCall
transform_notes name transform generator_doc transform_doc =
    Derive.Call
        { Derive.call_name = name
        , Derive.call_generator = Just $
            Derive.generator_call generator_doc generator
        , Derive.call_transformer = Just $
            Derive.transformer_call transform_doc transformer
        }
    where
    generator = Sig.call0 $ \args -> case Note.sub_events args of
        [] -> transform $ Note.inverting Util.placed_note args
        subs -> Note.place (Note.map_events transform (concat subs))
    transformer = Sig.call0t $ \_args deriver -> transform deriver

c_legato :: Derive.NoteCall
c_legato = Derive.stream_generator "legato"
    ("Play the transformed notes legato.  This sets `+legato` on all notes\
    \ except the last one. The default note deriver will respond to `+legato`\
    \ and " <> ShowVal.doc_val Score.c_legato_overlap <> "."
    ) $ Sig.call0 $ \args ->
    Lily.notes_around (Lily.Suffix "(") (Lily.Suffix ")") args $
        init_attr Attrs.legato args

-- | Apply the attributes to the init of the sub-events, i.e. every one but the
-- last.
init_attr :: Score.Attributes -> Derive.PassedArgs d -> Derive.EventDeriver
init_attr attr = Note.place . concatMap add . Note.sub_events
    where
    add notes = case Seq.viewr notes of
        (notes, Just last) ->
            Note.map_events (Util.add_attrs attr) notes ++ [last]
        _ -> []

extend_duration :: RealTime -> [Score.Event] -> Score.Event -> [Score.Event]
    -> Score.Event
extend_duration _ _ cur [] = cur
extend_duration overlap _prev cur (next:_) = Score.set_duration dur cur
    where dur = Score.event_start next - Score.event_start cur + overlap

c_portamento :: Derive.NoteCall
c_portamento = Derive.stream_generator "portamento"
    "Add `+porta` to all notes except the last one." $
    Sig.call0 $ \args ->
    Lily.notes_around (Lily.Suffix "(") (Lily.Suffix ")") args $
        init_attr Attrs.porta args
