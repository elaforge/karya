-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleContexts #-}
-- | This is like "Derive.Call", but higher level.  It has templates for
-- creating calls.
module Derive.Call.Make where
import qualified Util.Doc as Doc
import qualified Util.TextUtil as TextUtil
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.Expr as Expr
import qualified Derive.Flags as Flags
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Signal as Signal
import Global


-- | Bundle a generator and transformer together, so I can define them
-- together.  The rationale is described in 'Derive.CallMaps'.
type Calls d = (Derive.Generator d, Derive.Transformer d)

-- TODO use a real record
generator :: Calls d -> Derive.Generator d
generator = fst

transformer :: Calls d -> Derive.Transformer d
transformer = snd

call_maps :: [(Expr.Symbol, Calls d)] -> Derive.CallMaps d
call_maps calls = Derive.call_maps gs ts
    where
    gs = zip (map fst calls) (map (fst . snd) calls)
    ts = zip (map fst calls) (map (snd . snd) calls)

-- | This is a specialization of 'transform_notes' that adds Attributes.
attributed_note :: Module.Module -> Attrs.Attributes -> Calls Derive.Note
attributed_note module_ attrs = transform_notes module_
    (Derive.CallName $ "note with " <> ShowVal.show_val attrs) Tags.attr
    "Add attributes to the notes." Sig.no_args
    (\() -> Call.add_attributes attrs)

-- | This is a specialization of 'transform_notes' that sets an environ value.
environ_note :: Typecheck.ToVal a => Module.Module -> Derive.CallName
    -> Tags.Tags -> Doc.Doc -> Env.Key -> a -> Calls Derive.Note
environ_note module_ name tags doc key val =
    transform_notes module_ name tags doc Sig.no_args $
        \() -> Derive.with_val key val

-- | This is a specialization of 'transform_notes' that sets a control.
control_note :: Module.Module -> Derive.CallName -> Score.Control -> Signal.Y
    -> Calls Derive.Note
control_note module_ name control val = transform_notes module_ name mempty
    ("Note with " <> Doc.literal (ShowVal.show_val control <> " = "
        <> ShowVal.show_val val) <> ".")
    Sig.no_args $ \() -> Derive.with_constant_control control val

-- | The generator either derives subs or derives a new Call.note if there are
-- no subs, and then applies the transform.  The transformer call just applies
-- the transform.
transform_notes :: Module.Module -> Derive.CallName -> Tags.Tags -> Doc.Doc
    -> Sig.Parser a -> (a -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Calls Derive.Note
transform_notes module_ name tags transform_doc sig transform =
    (generator, transformer)
    where
    generator = Derive.generator module_ name (tags <> Tags.subs)
        (transform_doc <> "\n" <> generator_doc) $
        Sig.call sig $ \params args -> Sub.sub_events args >>= \x -> case x of
            [] -> transform params $ Sub.inverting Call.placed_note args
            subs -> mconcat $ map (transform params . Sub.derive) subs
    generator_doc = "If there are notes in child tracks, apply the\
        \ transformation to them. Otherwise apply the transformation to the\
        \ null note call."
    transformer = Derive.transformer module_ name tags transform_doc $
        Sig.callt sig $ \params _args deriver -> transform params deriver

-- | Create a transformer that just sets an environ value.  This is higher
-- level and more concise than using the @=@ transformer.
environ :: (Typecheck.ToVal val, Derive.Taggable d) =>
    Module.Module -> Env.Key -> Doc.Doc -> Sig.Parser a
    -> (a -> val) -> Derive.Transformer d
environ module_ key key_doc sig extract =
    Derive.transformer module_ (Derive.CallName key) mempty
        ("Set the " <> key_doc <> " environ variable.")
    $ Sig.callt sig $ \val _args ->
        Derive.with_val key (extract val)

-- | Make a call that sets an environ key to a specific value.
environ_val :: (ShowVal.ShowVal a, Typecheck.ToVal a, Derive.Taggable d) =>
    Module.Module -> Derive.CallName -> Env.Key -> a -> Doc.Doc
    -> Derive.Transformer d
environ_val module_ name key val extra_doc =
    Derive.transformer module_ name mempty (TextUtil.join2 doc extra_doc) $
        Sig.call0t $ \_args -> Derive.with_val key val
    where
    doc = Doc.literal (ShowVal.show_val key <> " = " <> ShowVal.show_val val)
        <> "."

-- | Make a call that adds a flag.
add_flag :: Module.Module -> Derive.CallName -> Doc.Doc -> Flags.Flags
    -> Derive.Transformer Score.Event
add_flag module_ name doc flags =
    Derive.transformer module_ name Tags.postproc doc $
    Sig.call0t $ \_args -> fmap $ Post.emap1_ $ Score.add_flags flags

-- * modify

-- | Make a modified version of an existing call.  Args are the same.
modify_generator :: Module.Module -> Derive.CallName -> Doc.Doc
    -> (Derive.Deriver (Stream.Stream a) -> Derive.Deriver (Stream.Stream a))
    -> Derive.Generator a -> Derive.Generator a
modify_generator module_ name doc transform =
    modify_call module_ name doc $ \gfunc -> gfunc
        { Derive.gfunc_f = transform . Derive.gfunc_f gfunc }

-- | Like 'modify_generator', but inherit metadata from the original call.
modify_generator_ :: Doc.Doc
    -> (Derive.Deriver (Stream.Stream a) -> Derive.Deriver (Stream.Stream a))
    -> Derive.Generator a -> Derive.Generator a
modify_generator_ doc_prefix transform call =
    modify_generator (Derive.cdoc_module cdoc) (Derive.call_name call)
        (doc_prefix <> "\n" <> Derive.cdoc_doc cdoc) transform call
    where cdoc = Derive.call_doc call

-- | Make a modified version of an existing call.  Args are the same.
modify_transformer :: Module.Module -> Derive.CallName -> Doc.Doc
    -> (Derive.Deriver (Stream.Stream a) -> Derive.Deriver (Stream.Stream a))
    -> Derive.Transformer a -> Derive.Transformer a
modify_transformer module_ name doc transform =
    modify_call module_ name doc (fmap transform .)

modify_transformer_ :: Doc.Doc
    -> (Derive.Deriver (Stream.Stream a) -> Derive.Deriver (Stream.Stream a))
    -> Derive.Transformer a -> Derive.Transformer a
modify_transformer_ doc_prefix transform call =
    modify_transformer (Derive.cdoc_module cdoc) (Derive.call_name call)
        (TextUtil.joinWith "\n" doc_prefix (Derive.cdoc_doc cdoc))
        transform call
    where cdoc = Derive.call_doc call

-- | Modify a generator transformer pair, inheriting metadata.
modify_calls_ :: Doc.Doc
    -> (Derive.Deriver (Stream.Stream a) -> Derive.Deriver (Stream.Stream a))
    -> Calls a -> Calls a
modify_calls_ doc_prefix transform (gen, trans) =
    ( modify_generator_ doc_prefix transform gen
    , modify_transformer_ doc_prefix transform trans
    )

modify_call :: Module.Module -> Derive.CallName -> Doc.Doc
    -> (a -> b) -> Derive.Call a -> Derive.Call b
modify_call module_ name doc modify call = Derive.Call
    { call_name = name
    , call_doc = Derive.CallDoc
        { cdoc_tags = Derive.cdoc_tags (Derive.call_doc call)
        , cdoc_module = module_
        , cdoc_doc = doc
        , cdoc_args = Derive.cdoc_args (Derive.call_doc call)
        }
    , call_func = modify $ Derive.call_func call
    }

-- * val calls

constant_val :: (Typecheck.ToVal a, ShowVal.ShowVal a) =>
    Module.Module -> Derive.CallName -> Doc.Doc -> a -> Derive.ValCall
constant_val module_ name doc val = Derive.val_call module_  name mempty
    (TextUtil.joinWith "\n" doc ("Constant: " <> ShowVal.doc val)) $
    Sig.call0 $ \_args -> return val

-- | Make a new ValCall from an existing one, by mapping over its output.
modify_vcall :: Derive.ValCall -> Module.Module -> Derive.CallName -> Doc.Doc
    -> (BaseTypes.Val -> BaseTypes.Val) -> Derive.ValCall
modify_vcall vcall module_ name doc f = Derive.ValCall
    { vcall_name = name
    , vcall_doc = Derive.CallDoc
        { cdoc_tags = Derive.cdoc_tags (Derive.vcall_doc vcall)
        , cdoc_module = module_
        , cdoc_doc = doc
        , cdoc_args = Derive.cdoc_args (Derive.vcall_doc vcall)
        }
    , vcall_call = fmap f . Derive.vcall_call vcall
    }
