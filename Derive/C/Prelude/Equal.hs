-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export 'c_equal' call, which implements @=@.
--
-- The equal call is heavily overloaded because I want to reuse the nice infix
-- syntax.  Unfortunately it results in lots of cryptic prefixes.  Is it worth
-- it?
module Derive.C.Prelude.Equal (
    library, cEqual
    , transformExpr
) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck
import qualified Derive.ValType as ValType

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Id as Id

import           Global


-- * note

library :: Library.Library
library = mconcat
    [ Library.generators [("=", cEqualGenerator)]
    , Library.polyTransformers
        [ ("=", cEqual)
        , ("default-merge", cDefaultMerge)
        ]
    ]

-- * util

-- | Parse an expression containing only equal calls and turn it into a
-- transformer.  'Eval.eval_transform_expr' is more general, but only
-- transforms Streams, because most transforms work with a stream.  So by
-- having a more restrictive input, this can have a more general output:
-- contravariance I guess?
transformExpr :: Text -> Derive.Deriver a -> Derive.Deriver a
transformExpr expr deriver = do
    parsed <- Derive.requireRight id $ Parse.parseExpr expr
    transforms <- if isEmptyExpr parsed
        then pure []
        else mapM apply $ NonEmpty.toList parsed
    foldr ($) deriver transforms
    where
    apply expr = do
        (lhs, rhs) <- Derive.require
            ("expected an assignment: " <> ShowVal.showVal expr)
            (equalExpr expr)
        Derive.requireRight id $ parseEqual Set lhs rhs

isEmptyExpr :: Expr.Expr a -> Bool
isEmptyExpr (Expr.Call "" [] :| []) = True
isEmptyExpr _ = False

equalExpr :: DeriveT.Call -> Maybe (Text, DeriveT.Val)
equalExpr (Expr.Call (Expr.Symbol "=")
        [Expr.Literal (DeriveT.VStr (Expr.Str lhs)), Expr.Literal val]) =
    Just (lhs, val)
equalExpr _ = Nothing

-- * implementation

cEqual :: Derive.CallableExpr d => Derive.Transformer d
cEqual = Derive.transformer Module.prelude "equal" Tags.subs equalDoc $
    Sig.callt equalArgs $ \(lhs, rhs, merge) _args deriver -> do
        transform <- Derive.requireRight id $ parseEqual merge lhs rhs
        transform deriver

cEqualGenerator :: Derive.Generator Derive.Note
cEqualGenerator = Derive.generator Module.prelude "equal" Tags.subs
    "Similar to the transformer, this will evaluate the notes below in\
    \ a transformed environ." $
    Sig.call equalArgs $ \(lhs, rhs, merge) args -> do
        transform <- Derive.requireRight id $ parseEqual merge lhs rhs
        transform . Sub.derive . concat =<< Sub.subEvents args

equalArgs :: Sig.Parser (Text, DeriveT.Val, Merge)
equalArgs = (,,)
    <$> Sig.requiredEnv "lhs" Sig.None
        "Assign to this. This looks like a Str, but\
        \ can actualy contain any characters except `=`, due to the special\
        \ infix parsing for `=`. Symbolic prefixes determine what is\
        \ assigned, and the valid types for the rhs."
    <*> Sig.requiredEnv "rhs" Sig.None "Source of the assignment."
    <*> (parseMerge <$> Sig.defaulted "merge" ("set" :: Text) mergeDoc)

mergeDoc :: Doc.Doc
mergeDoc = "Merge operator. This can be `default` to use the default for the\
    \ control, `set` to replace the old signal, or one of the operators from\
    \ 'Derive.Deriver.Monad.mergers': "
    <> Doc.commas (map ShowVal.doc (Map.keys Derive.mergers)) <> "."
    <> " There are also symbolic aliases, to support `=+` syntax: "
    <> Doc.pretty symbolToMerge

data Merge = Default | Set | Merge Expr.Symbol deriving (Show)

instance ShowVal.ShowVal Merge where
    showVal Default = "default"
    showVal Set = "set"
    showVal (Merge sym) = ShowVal.showVal sym

parseMerge :: Expr.Symbol -> Merge
parseMerge name
    | name == "set" = Set
    | name == "default" = Default
    | otherwise = Merge $ Map.findWithDefault name name symbolToMerge

symbolToMerge :: Map Expr.Symbol Expr.Symbol
symbolToMerge = Map.fromList
    [ ("+", "add")
    , ("-", "sub")
    , ("*", "mul")
    , ("@", "scale")
    ]

equalDoc :: Doc.Doc
equalDoc =
    "Evaluate the deriver with a value set. Special parser support means this\
    \ can be called infix.  The arguments can take many forms to set different\
    \ kinds of values.\
    \\nSet an environ value by setting a plain symbol or unset it by assigning\
    \ to `_`: `x = 42` or `x = _`.\
    \\nPrefixes:\
    \\n- `>>` - Create a note transformer, whose name will be prefixed with\
    \ `>`.  This is used to set an instrument transformer, which can apply\
    \ a transformer when an instrument is set by the title of a note track, as\
    \ implemented by by `note-track`.\
    \\n- `-val` - Bind a val call.\
    \\n- `^note`, `*pitch`, `.control` - Bind the respective call type\
    \ generator.\
    \\n- `^-note`, `*-pitch`, `.-control` - As above, but bind transformer.\
    \\n- `>alias = >inst` - alias instrument name\
    \\nE.g.: set note generator: `^phrase = some-block`,\
    \ note transformer: `^-mute = +mute+loose`,\
    \ control transfomrer: `.-i = t`, val call: `-4c = (5c)`.\
    \\nYou can bypass all this cryptic prefix garbage by using a `ky` file.\
    \ It has more space available so it can use a more readable syntax.\
    \\nIf you bind a call to a quoted expression, this creates a new\
    \ call: `^abc = \"(a b c)` will create a `abc` call, which is a macro for\
    \ `a b c`. The created call does not take arguments.\
    \\nSet the default pitch signal with `#`, e.g. `# = (4c)` or `# = 45nn`.\
    \ Control signal assignment also supports the same merge functions as the\
    \ control track: `a = .5 add` or `a = %b add`.  However, the second example\
    \ throws an error if `%b` is a ControlFunction. `a = .5 default` will\
    \ combine with `a`'s default merge function. Assigning to `_` unsets the\
    \ control, and any ControlFunction.\
    \\nThe `=` operator can be suffixed with symbol, which will become the last\
    \ argument, so `%x=+1` becomes `%x = 1 '+'`.  Note that the order\
    \ is backwards from the usual `+=`, which is ultimately because the first\
    \ word can have almost any character except space and `=`. Also, `x=-1` is\
    \ ambiguous, and parsed as `x =- 1`."
    -- Previously > was for binding note calls, but that was taken by
    -- instrument aliasing.  ^ at least looks like a rotated >.

parseEqual :: Merge -> Text -> DeriveT.Val
    -> Either Text (Derive.Deriver a -> Derive.Deriver a)
parseEqual Set lhs rhs
    -- Assign to call.
    | Just new <- Text.stripPrefix "^" lhs = Right $
        overrideCall new rhs
            (Derive.sGenerator#Derive.sNote)
            (Derive.sTransformer#Derive.sNote)
    | Just new <- Text.stripPrefix ">>" lhs = Right $
        overrideTransformer (">" <> new) rhs
            (Derive.sTransformer#Derive.sNote)
    | Just new <- Text.stripPrefix "*" lhs = Right $
        overrideCall new rhs
            (Derive.sGenerator#Derive.sPitch)
            (Derive.sTransformer#Derive.sPitch)
    | Just new <- Text.stripPrefix "." lhs = Right $
        overrideCall new rhs
            (Derive.sGenerator#Derive.sControl)
            (Derive.sTransformer#Derive.sControl)
    | Just new <- Text.stripPrefix "-" lhs = Right $ overrideValCall new rhs
parseEqual Set lhs rhs
    -- Create instrument alias.
    | Just new <- Text.stripPrefix ">" lhs = case rhs of
        DeriveT.VStr (Expr.Str inst) -> Right $
            Derive.withInstrumentAlias (ScoreT.Instrument new)
                (ScoreT.Instrument inst)
        _ -> Left $ "aliasing an instrument expected an instrument rhs, got "
            <> pretty (ValType.specificTypeOf rhs)
-- TODO should I centralize the parsing of #?  Or is equal the only place that
-- needs this notation where # is state_pitch?  I used to parse a VPControlRef
parseEqual Set "#" rhs = case rhs of
    DeriveT.VPSignal sig -> Right $ Derive.withPitch sig
    -- I could also convert a signal, but not sure it's useful.
    DeriveT.VSignal (ScoreT.Typed ScoreT.Nn sig)
        | Just nn <- Signal.constantVal sig ->
            Right $ Derive.withPitch
                (PSignal.constant (PSignal.nnPitch (Pitch.nn nn)))
    DeriveT.VNotGiven -> Right $ Derive.withPitch mempty
    DeriveT.VPControlRef ref -> Right $ \deriver -> do
        sig <- Typecheck.resolvePitchRef ref
        Derive.withPitch sig deriver
    _ -> Left $ "binding a pitch signal expected a pitch, pitch"
        <> " signal, or nn, but got " <> pretty (ValType.specificTypeOf rhs)
parseEqual Set lhs rhs
    | not (Id.validSymbol lhs) = Left $
        "tried to assign to invalid symbol name: " <> ShowVal.showVal lhs
    | otherwise = Right $ Derive.withVal lhs rhs
-- if rhs is a signal or number, then merge is ok
parseEqual merge lhs rhs = case rhs of
    DeriveT.VSignal sig -> Right $ mergeSignal sig
    _ -> Left $ "merge is only supported for numeric types, tried to merge "
        <> pretty (ValType.specificTypeOf rhs) <> " with "
        <> ShowVal.showVal merge
    where
    mergeSignal sig deriver = do
        merger <- getMerger control merge
        Derive.withMergedControl merger control sig deriver
    control = ScoreT.Control lhs

-- | Unlike 'Derive.MergeDefault', the default is Derive.Set.
getMerger :: ScoreT.Control -> Merge -> Derive.Deriver Derive.Merger
getMerger control = \case
    Set -> pure Derive.Set
    Default -> Derive.getDefaultMerger control
    Merge merge -> Derive.getControlMerge merge

-- | Look up a call with the given Symbol and add it as an override to the
-- scope given by the lenses.  I wanted to pass just one lens, but apparently
-- they're not sufficiently polymorphic.
overrideCall :: (Derive.CallableExpr d1, Derive.CallableExpr d2)
    => Text -> DeriveT.Val
    -> Lens Derive.Scopes (Derive.ScopePriority (Derive.Generator d1))
    -> Lens Derive.Scopes (Derive.ScopePriority (Derive.Transformer d2))
    -> Derive.Deriver a -> Derive.Deriver a
overrideCall lhs rhs generator transformer deriver
    | Just stripped <- Text.stripPrefix "-" lhs =
        overrideTransformer stripped rhs transformer deriver
    | otherwise = overrideGeneratorScope
        =<< resolveSource quotedGenerator rhs
    where
    overrideGeneratorScope call = Derive.withScopes add deriver
        where
        add = generator %= Derive.addPriority Derive.PrioOverride
            (Derive.singleCall (Expr.Symbol lhs) call)

-- | Make an expression into a transformer and stick it into the
-- 'Derive.PrioOverride' slot.
overrideTransformer :: Derive.CallableExpr d => Text -> DeriveT.Val
    -> Lens Derive.Scopes (Derive.ScopePriority (Derive.Transformer d))
    -> Derive.Deriver a -> Derive.Deriver a
overrideTransformer lhs rhs transformer deriver =
    overrideScope =<< resolveSource quotedTransformer rhs
    where
    overrideScope call = Derive.withScopes
        (transformer %= Derive.addPriority Derive.PrioOverride
            (Derive.singleCall (Expr.Symbol lhs) call))
        deriver

overrideValCall :: Text -> DeriveT.Val -> Derive.Deriver a
    -> Derive.Deriver a
overrideValCall lhs rhs deriver = do
    call <- resolveSource quotedValCall rhs
    let add = Derive.sVal %= Derive.addPriority Derive.PrioOverride
            (Derive.singleCall (Expr.Symbol lhs) call)
    Derive.withScopes add deriver

-- | A VQuoted becomes a call, a Str is expected to name a call, and
-- everything else is turned into a Str via ShowVal.  This will cause
-- a parse error for un-showable Vals, but what else is new?
resolveSource :: Derive.Callable call => (DeriveT.Quoted -> call)
    -> DeriveT.Val -> Derive.Deriver call
resolveSource makeCall rhs = case rhs of
    DeriveT.VQuoted quoted -> pure $ makeCall quoted
    DeriveT.VStr (Expr.Str sym) -> getCall (Expr.Symbol sym)
    _ -> getCall (Expr.Symbol (ShowVal.showVal rhs))

getCall :: forall call. Derive.Callable call => Expr.Symbol
    -> Derive.Deriver call
getCall sym = maybe (Derive.throw $ Eval.unknownSymbol name sym) pure
    =<< Derive.lookupCall sym
    where name = Derive.callableName (Proxy :: Proxy call)


-- * quoted

-- | Create a new call from a quoted expression.  This is flirting with
-- function definiion, but is really just macro expansion, with all the
-- variable capture problems implied.  But since the only variables I have are
-- calls maybe it's not so bad.
quotedGenerator :: Derive.CallableExpr d => DeriveT.Quoted
    -> Derive.Generator d
quotedGenerator quoted@(DeriveT.Quoted expr) =
    Derive.generator quotedModule "quoted-call" mempty
    ("Created from expression: " <> ShowVal.doc quoted)
    $ Sig.call0 $ \args -> Eval.evalExpr True (Args.context args) expr

quotedTransformer :: Derive.CallableExpr d
    => DeriveT.Quoted -> Derive.Transformer d
quotedTransformer quoted@(DeriveT.Quoted expr) =
    Derive.transformer quotedModule "quoted-call" mempty
    ("Created from expression: " <> ShowVal.doc quoted)
    $ Sig.call0t $ \args deriver ->
        Eval.evalTransformers (Args.context args) (NonEmpty.toList expr)
            deriver

quotedValCall :: DeriveT.Quoted -> Derive.ValCall
quotedValCall quoted = Derive.valCall quotedModule "quoted-call" mempty
    ("Created from expression: " <> ShowVal.doc quoted)
    $ Sig.call0 $ \args -> do
        call <- case quoted of
            DeriveT.Quoted (call :| []) -> pure $ Expr.ValCall call
            _ -> Derive.throw $
                "expected a val call, but got a full expression: "
                <> ShowVal.showVal quoted
        Eval.eval (Args.context args) call

-- | Pseudo-module for val calls generated from a quoted expression.
quotedModule :: Module.Module
quotedModule = "quoted"


-- * other

cDefaultMerge :: Derive.Taggable d => Derive.Transformer d
cDefaultMerge = Derive.transformer Module.prelude "default-merge" mempty
    "Set the default merge operators for controls. These apply when the\
    \ control track doesn't have an explicit operator."
    $ Sig.callt ((,)
    <$> (parseMerge <$> Sig.required "merge" mergeDoc)
    <*> (NonEmpty.toList <$> Sig.many1 "control" "Control names.")
    ) $ \(merge, controls) _args deriver -> do
        mergers <- mapM (\c -> getMerger c merge) controls
        Internal.withDefaultMerge (Map.fromList (zip controls mergers))
            deriver
