-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleContexts #-}
-- | This is like "Derive.Call.Util", but higher level.  It has templates for
-- creating calls.
module Derive.Call.Make where
import Util.Control
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang


-- | Bundle a generator and transformer together, so I can define them
-- together.  The rationale is described in 'Derive.CallMaps'.
type Calls d = (Derive.Generator d, Derive.Transformer d)

call_maps :: [(TrackLang.CallId, Calls d)]
    -> [(TrackLang.CallId, Derive.Generator d)]
    -> [(TrackLang.CallId, Derive.Transformer d)] -> Derive.CallMaps d
call_maps calls generators transformers =
    Derive.call_maps (gs ++ generators) (ts ++ transformers)
    where
    gs = zip (map fst calls) (map (fst . snd) calls)
    ts = zip (map fst calls) (map (snd . snd) calls)

attributed_note :: Module.Module -> Score.Attributes -> Calls Derive.Note
attributed_note module_ attrs = transform_notes module_
    ("note with " <> ShowVal.show_val attrs) Tags.attr
    "Add attributes to the notes." Sig.no_args (\() -> Util.add_attrs attrs)

environ_note :: (TrackLang.Typecheck a) => Module.Module -> Text -> Tags.Tags
    -> Text -> TrackLang.ValName -> a -> Calls Derive.Note
environ_note module_ name tags doc key val =
    transform_notes module_ name tags doc Sig.no_args $
        \() -> Derive.with_val key val

-- | The generator either derives subs or derives a new Util.note if there are
-- no subs, and then applies the transform.  The transformer call just applies
-- the transform.
transform_notes :: Module.Module -> Text -> Tags.Tags -> Text -> Sig.Parser a
    -> (a -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Calls Derive.Note
transform_notes module_ name tags transform_doc sig transform =
    (generator, transformer)
    where
    generator = Derive.make_call module_ name (tags <> Tags.subs) generator_doc
        $ Sig.call sig $ \params args -> Sub.sub_events args >>= \x -> case x of
            [] -> transform params $ Sub.inverting Util.placed_note args
            subs -> Sub.place $ map (fmap (transform params)) (concat subs)
    generator_doc = "If there are notes in child tracks, apply the\
        \ transformation to them. Otherwise apply transformation to the null\
        \ note call."
    transformer = Derive.transformer module_ name tags transform_doc $
        Sig.callt sig $ \params _args deriver -> transform params deriver

transform_notes_subevents :: Module.Module -> Text -> Tags.Tags -> Sig.Parser a
    -> (a -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
transform_notes_subevents module_ name tags sig transform =
    Derive.make_call module_ name (tags <> Tags.subs) generator_doc generator
    where
    generator_doc = "If there are notes in child tracks, apply the\
        \ transformation to them. Otherwise apply transformation to the null\
        \ note call."
    generator = Sig.call sig $ \params args ->
        Sub.sub_events args >>= \x -> case x of
            [] -> transform params $ Sub.inverting Util.placed_note args
            subs -> Sub.place $ map (fmap (transform params)) (concat subs)

-- | Create a transformer that just sets an environ value.  This is higher
-- level and more concise than using the @=@ transformer.
with_environ :: (TrackLang.Typecheck val, Derive.Taggable d) =>
    Module.Module -> Text -> Sig.Parser a -> (a -> val) -> Derive.Transformer d
with_environ module_ name sig extract = Derive.transformer module_ name mempty
        ("Set `" <> name <> "` environ variable.")
    $ Sig.callt sig $ \val _args ->
        Derive.with_val (TrackLang.Symbol name) (extract val)


-- * val calls

-- | Make a new ValCall from an existing one, by mapping over its output.
modify_vcall :: Derive.ValCall -> Module.Module -> Text -> Text
    -> (TrackLang.Val -> TrackLang.Val) -> Derive.ValCall
modify_vcall vcall module_ name doc f = vcall
    { Derive.vcall_name = name
    , Derive.vcall_doc = Derive.CallDoc
        { Derive.cdoc_tags = Derive.cdoc_tags (Derive.vcall_doc vcall)
        , Derive.cdoc_module = module_
        , Derive.cdoc_doc = doc
        , Derive.cdoc_args = Derive.cdoc_args (Derive.vcall_doc vcall)
        }
    , Derive.vcall_call = fmap f . Derive.vcall_call vcall
    }
