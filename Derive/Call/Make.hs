-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleContexts #-}
-- | This is like "Derive.Call", but higher level.  It has templates for
-- creating calls.
module Derive.Call.Make where
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Signal as Signal
import Global


-- | Bundle a generator and transformer together, so I can define them
-- together.  The rationale is described in 'Derive.CallMaps'.
type Calls d = (Derive.Generator d, Derive.Transformer d)

call_maps :: [(BaseTypes.CallId, Calls d)]
    -> [(BaseTypes.CallId, Derive.Generator d)]
    -> [(BaseTypes.CallId, Derive.Transformer d)] -> Derive.CallMaps d
call_maps calls generators transformers =
    Derive.call_maps (gs ++ generators) (ts ++ transformers)
    where
    gs = zip (map fst calls) (map (fst . snd) calls)
    ts = zip (map fst calls) (map (snd . snd) calls)

-- | This is a specialization of 'transform_notes' that adds Attributes.
attributed_note :: Module.Module -> Score.Attributes -> Calls Derive.Note
attributed_note module_ attrs = transform_notes module_
    ("note with " <> ShowVal.show_val attrs) Tags.attr
    "Add attributes to the notes." Sig.no_args (\() -> Call.add_attrs attrs)

-- | This is a specialization of 'transform_notes' that sets an environ value.
environ_note :: (Typecheck.Typecheck a, Typecheck.ToVal a) =>
    Module.Module -> Text -> Tags.Tags -> Text -> Env.Key -> a
    -> Calls Derive.Note
environ_note module_ name tags doc key val =
    transform_notes module_ name tags doc Sig.no_args $
        \() -> Derive.with_val key val

-- | This is a specialization of 'transform_notes' that sets a control.
control_note :: Module.Module -> Text -> Score.Control -> Signal.Y
    -> Calls Derive.Note
control_note module_ name control val = transform_notes module_ name mempty
    ("Note with `" <> ShowVal.show_val control <> " = "
        <> ShowVal.show_val val <> "`.")
    Sig.no_args $ \() ->
        Derive.with_constant_control control (Score.untyped val)

-- | The generator either derives subs or derives a new Call.note if there are
-- no subs, and then applies the transform.  The transformer call just applies
-- the transform.
transform_notes :: Module.Module -> Text -> Tags.Tags -> Text -> Sig.Parser a
    -> (a -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Calls Derive.Note
transform_notes module_ name tags transform_doc sig transform =
    (generator, transformer)
    where
    generator = Derive.generator module_ name (tags <> Tags.subs)
        (transform_doc <> "\n" <> generator_doc)
        $ Sig.call sig $ \params args -> Sub.sub_events args >>= \x -> case x of
            [] -> transform params $ Sub.inverting Call.placed_note args
            subs -> Sub.derive $ map (fmap (transform params)) (concat subs)
    generator_doc = "If there are notes in child tracks, apply the\
        \ transformation to them. Otherwise apply the transformation to the\
        \ null note call."
    transformer = Derive.transformer module_ name tags transform_doc $
        Sig.callt sig $ \params _args deriver -> transform params deriver

-- | Create a transformer that just sets an environ value.  This is higher
-- level and more concise than using the @=@ transformer.
with_environ ::
    (Typecheck.Typecheck val, Typecheck.ToVal val, Derive.Taggable d)
    => Module.Module -> BaseTypes.Key -> Text -> Sig.Parser a -> (a -> val)
    -> Derive.Transformer d
with_environ module_ key key_doc sig extract =
    Derive.transformer module_ (BaseTypes.unsym key) mempty
        ("Set the " <> key_doc <> " environ variable.")
    $ Sig.callt sig $ \val _args ->
        Derive.with_val key (extract val)


-- * val calls

-- | Make a new ValCall from an existing one, by mapping over its output.
modify_vcall :: Derive.ValCall -> Module.Module -> Text -> Text
    -> (BaseTypes.Val -> BaseTypes.Val) -> Derive.ValCall
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
