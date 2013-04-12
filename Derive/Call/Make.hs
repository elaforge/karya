-- | This is like "Derive.Call.Util", but higher level.  It has templates for
-- creating calls.
module Derive.Call.Make where
import Util.Control
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang


attributed_note :: Score.Attributes -> Derive.NoteCall
attributed_note attrs = transform_notes
    ("note with " <> ShowVal.show_val attrs) Tags.attr
    "Add attributes to the notes." Sig.no_args (\() -> Util.add_attrs attrs)

environ_note :: (TrackLang.Typecheck a) => Text -> Tags.Tags -> Text
    -> TrackLang.ValName -> a -> Derive.NoteCall
environ_note name tags doc key val =
    transform_notes name tags doc Sig.no_args (\() -> Derive.with_val key val)

transform_notes :: Text -> Tags.Tags -> Text -> Sig.Parser a
    -> (a -> Derive.EventDeriver -> Derive.EventDeriver) -> Derive.NoteCall
transform_notes name tags transform_doc sig transform = Derive.Call
    { Derive.call_name = name
    , Derive.call_generator = Just $
        Derive.generator_call (tags <> Tags.subs) generator_doc generator
    , Derive.call_transformer = Just $
        Derive.transformer_call tags transform_doc transformer
    }
    where
    generator_doc = "If there are notes in child tracks, apply the\
        \ transformation to them. Otherwise apply transformation to the null\
        \ note call."
    generator = Sig.call sig $ \params args ->
        Note.sub_events args >>= \x -> case x of
            [] -> transform params $ Note.inverting Util.placed_note args
            subs -> Note.place $
                Note.map_events (transform params) (concat subs)
    transformer = Sig.callt sig $ \params _args deriver ->
        transform params deriver
