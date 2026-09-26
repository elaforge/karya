-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleContexts #-}
-- | This is like "Derive.Call", but higher level.  It has templates for
-- creating calls.
module Derive.Call.Make where
import qualified Util.Doc as Doc
import qualified Util.Texts as Texts
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.Flags as Flags
import qualified Derive.Library as Library
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Signal as Signal


-- | This is a specialization of 'transform_notes' that adds Attributes.
attributedNote :: Module.Module -> Attrs.Attributes
    -> Library.Calls Derive.Note
attributedNote module_ attrs = transformNotes module_
    (Derive.CallName $ "note with " <> ShowVal.showVal attrs) Tags.attr
    "Add attributes to the notes." Sig.noArgs
    (\() -> Call.addAttributes attrs)

-- | This is a specialization of 'transform_notes' that sets an environ value.
environNote :: Typecheck.ToVal a => Module.Module -> Derive.CallName
    -> Tags.Tags -> Doc.Doc -> Env.Key -> a -> Library.Calls Derive.Note
environNote module_ name tags doc key val =
    transformNotes module_ name tags doc Sig.noArgs $
        \() -> Derive.withVal key val

-- | This is a specialization of 'transform_notes' that sets a control.
controlNote :: Module.Module -> Derive.CallName -> ScoreT.Control -> Signal.Y
    -> Library.Calls Derive.Note
controlNote module_ name control val = transformNotes module_ name mempty
    ("Note with " <> Doc.literal (ShowVal.showVal control <> " = "
        <> ShowVal.showVal val) <> ".")
    Sig.noArgs $ \() -> Derive.withConstantControl control val

-- | 'transform_notes_args' without the PassedArgs.
transformNotes :: Module.Module -> Derive.CallName -> Tags.Tags -> Doc.Doc
    -> Sig.Parser a -> (a -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Library.Calls Derive.Note
transformNotes module_ name tags transformDoc sig transform =
    transformNotesArgs module_ name tags transformDoc sig
        (\params _args deriver -> transform params deriver)

-- | The generator either derives subs or derives a new Call.note if there are
-- no subs, and then applies the transform.  The transformer call just applies
-- the transform.
transformNotesArgs :: Module.Module -> Derive.CallName -> Tags.Tags
    -> Doc.Doc -> Sig.Parser a
    -> (a -> Derive.PassedArgs Score.Event -> Derive.NoteDeriver
        -> Derive.NoteDeriver)
    -> Library.Calls Derive.Note
transformNotesArgs module_ name tags transformDoc sig transform =
    Library.Calls generator transformer
    where
    generator = Derive.generator module_ name (tags <> Tags.subs)
        (transformDoc <> "\n" <> generatorDoc) $
        Sig.call sig $ \params args -> Sub.subEvents args >>= \case
            [] -> transform params args $ Sub.inverting Call.placedNote args
            subs -> mconcat $ map (transform params args . Sub.derive) subs
    generatorDoc = "If there are notes in child tracks, apply the\
        \ transformation to them. Otherwise apply the transformation to the\
        \ null note call."
    transformer = Derive.transformer module_ name tags transformDoc $
        Sig.callt sig $ \params args deriver -> transform params args deriver

-- | Create a transformer that just sets an environ value.  This is higher
-- level and more concise than using the @=@ transformer.
environ :: (Typecheck.ToVal val, Derive.Taggable d) =>
    Module.Module -> Env.Key -> Doc.Doc -> Sig.Parser a
    -> (a -> val) -> Derive.Transformer d
environ module_ key keyDoc sig extract =
    Derive.transformer module_ (Derive.CallName key) mempty
        ("Set the " <> keyDoc <> " environ variable.")
    $ Sig.callt sig $ \val _args ->
        Derive.withVal key (extract val)

-- | Make a call that sets an environ key to a specific value.
environVal :: (ShowVal.ShowVal a, Typecheck.ToVal a, Derive.Taggable d) =>
    Module.Module -> Derive.CallName -> Env.Key -> a -> Doc.Doc
    -> Derive.Transformer d
environVal module_ name key val extraDoc =
    Derive.transformer module_ name mempty (Texts.unwords2 doc extraDoc) $
        Sig.call0t $ \_args -> Derive.withVal key val
    where
    doc = Doc.literal (ShowVal.showVal key <> " = " <> ShowVal.showVal val)
        <> "."

-- | Make a call that adds a flag.
addFlag :: Module.Module -> Derive.CallName -> Doc.Doc -> Flags.Flags
    -> Derive.Transformer Score.Event
addFlag module_ name doc flags =
    Derive.transformer module_ name Tags.postproc doc $
    Sig.call0t $ \_args -> fmap $ Post.emap1_ $ Score.addFlags flags

-- * modify

-- | Make a modified version of an existing call.  Args are the same.
modifyGenerator :: Module.Module -> Derive.CallName -> Doc.Doc
    -> Derive.TransformerF a -> Derive.Generator a -> Derive.Generator a
modifyGenerator module_ name doc transform =
    modifyCall module_ name doc $ \gfunc -> gfunc
        { Derive.gfuncF = \args -> transform args (Derive.gfuncF gfunc args)
        }

-- | Like 'modify_generator', but inherit metadata from the original call.
modifyGenerator_ :: Doc.Doc
    -> Derive.TransformerF a -> Derive.Generator a -> Derive.Generator a
modifyGenerator_ docPrefix transform call =
    modifyGenerator (Derive.cdocModule cdoc) (Derive.callName call)
        (Texts.unlines2 docPrefix (Derive.cdocDoc cdoc))
        transform call
    where cdoc = Derive.callDoc call

-- | Make a modified version of an existing call.  Args are the same.
modifyTransformer :: Module.Module -> Derive.CallName -> Doc.Doc
    -> Derive.TransformerF a -> Derive.Transformer a -> Derive.Transformer a
modifyTransformer module_ name doc transform =
    modifyCall module_ name doc $ \tfunc args -> transform args . tfunc args

modifyTransformer_ :: Doc.Doc
    -> Derive.TransformerF a -> Derive.Transformer a -> Derive.Transformer a
modifyTransformer_ docPrefix transform call =
    modifyTransformer (Derive.cdocModule cdoc) (Derive.callName call)
        (Texts.unlines2 docPrefix (Derive.cdocDoc cdoc))
        transform call
    where cdoc = Derive.callDoc call

-- | Modify a generator transformer pair, inheriting metadata.
modifyCalls_ :: Doc.Doc
    -> Derive.TransformerF a
    -> Library.Calls a -> Library.Calls a
modifyCalls_ docPrefix transform (Library.Calls gen trans) = Library.Calls
    { generator = modifyGenerator_ docPrefix transform gen
    , transformer = modifyTransformer_ docPrefix transform trans
    }

modifyCall :: Module.Module -> Derive.CallName -> Doc.Doc
    -> (a -> b) -> Derive.Call a -> Derive.Call b
modifyCall module_ name doc modify call = Derive.Call
    { callName = name
    , callDoc = Derive.CallDoc
        { cdocTags = Derive.cdocTags (Derive.callDoc call)
        , cdocModule = module_
        , cdocDoc = doc
        , cdocArgs = Derive.cdocArgs (Derive.callDoc call)
        }
    , callFunc = modify $ Derive.callFunc call
    }

-- * val calls

constantVal :: (Typecheck.ToVal a, ShowVal.ShowVal a) =>
    Module.Module -> Derive.CallName -> Doc.Doc -> a -> Derive.ValCall
constantVal module_ name doc val = Derive.valCall module_  name mempty
    (Texts.unlines2 doc ("Constant: " <> ShowVal.doc val)) $
    Sig.call0 $ \_args -> pure val

-- | Make a new ValCall from an existing one, by mapping over its output.
modifyVcall :: Derive.ValCall -> Module.Module -> Derive.CallName -> Doc.Doc
    -> (DeriveT.Val -> DeriveT.Val) -> Derive.ValCall
modifyVcall vcall module_ name doc f = Derive.Call
    { callName = name
    , callDoc = Derive.CallDoc
        { cdocTags = Derive.cdocTags (Derive.callDoc vcall)
        , cdocModule = module_
        , cdocDoc = doc
        , cdocArgs = Derive.cdocArgs (Derive.callDoc vcall)
        }
    , callFunc = fmap f . Derive.callFunc vcall
    }
