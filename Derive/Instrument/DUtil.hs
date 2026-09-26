-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions for instrument cmds.  This is called DUtil because there is also
-- "Cmd.Instrument.CUtil" and they are usually imported together.
--
-- I need a better name than \"Util\" for everything.
module Derive.Instrument.DUtil where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.Call as Call
import qualified Derive.Call.GraceUtil as GraceUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Flags as Flags
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import           Global
import           Types


generator :: Derive.CallName -> Doc.Doc
    -> Derive.WithArgDoc (Derive.GeneratorF d)
    -> Derive.Generator d
generator name = Derive.generator Module.instrument name mempty

generator0 :: Derive.Taggable d => Derive.CallName -> Doc.Doc
    -> (Derive.PassedArgs d -> Derive.Deriver (Stream.Stream d))
    -> Derive.Generator d
generator0 name doc call = generator name doc (Sig.call0 call)

transformer :: Derive.CallName -> Doc.Doc
    -> Derive.WithArgDoc (Derive.TransformerF d) -> Derive.Transformer d
transformer name doc =
    Derive.transformer Module.instrument name mempty doc

transformer0 :: Derive.Taggable d => Derive.CallName -> Doc.Doc
    -> Derive.TransformerF d -> Derive.Transformer d
transformer0 name doc call =
    Derive.transformer Module.instrument name mempty doc (Sig.call0t call)

-- | Make a call that simply calls the default note call with the given attrs.
attributesNote :: Attrs.Attributes -> Derive.Generator Derive.Note
attributesNote attrs =
    generator0 (Derive.CallName $ "attributes_note " <> ShowVal.showVal attrs)
        "Invoke the default note call with the given attrs." $ \args ->
    Call.addAttributes attrs (Call.reapplyNote args)

zeroDurationTransform :: Doc.Doc
    -> (Derive.NoteArgs -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
zeroDurationTransform doc transform = Note.transformedNote
    ("A normal note, but modified when it has zero duration: " <> doc)
    mempty $ \args deriver ->
        ifM (isZeroDuration args) (transform args deriver) deriver

-- | Create a generator that has a different implementation for zero and
-- non-zero duration.
zeroDuration :: Derive.CallName -> Doc.Doc
    -> (Derive.NoteArgs -> Derive.NoteDeriver)
    -> (Derive.NoteArgs -> Derive.NoteDeriver) -> Derive.Generator Derive.Note
zeroDuration name doc zero nonZero = generator0 name doc $ \args ->
    ifM (isZeroDuration args) (zero args) (nonZero args)

isZeroDuration :: Derive.PassedArgs a -> Derive.Deriver Bool
isZeroDuration args
    | Args.duration args == 0 = do
        -- It turns out it's hard to figure out if a note has zero
        -- duration, and isn't just an infer-duration note.
        stack <- Internal.getStack
        environ <- Derive.getEnviron
        zero <- (==0) <$> Args.realDuration args
        let flags = Note.noteFlags zero stack environ
        pure $ zero && not (flags `Flags.has` Flags.inferDuration)
    | otherwise = pure False

-- | Just like the default note call, except apply a function to the output.
postprocNote :: Derive.CallName -> Doc.Doc -> (Score.Event -> Score.Event)
    -> Derive.Generator Derive.Note
postprocNote name doc f = postprocGenerator name doc Note.cNote apply
    where apply d = fmap f <$> d

-- | Transform an existing call by applying a function to it.  It gets a new
-- name and the documentation is prepended to the documentation of the original
-- call.
postprocGenerator :: Derive.CallName -> Doc.Doc -> Derive.Generator d
    -> (Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d))
    -> Derive.Generator d
postprocGenerator name newDoc (Derive.Call _ oldDoc func) f = Derive.Call
    { callName = name
    , callDoc = appendDoc newDoc oldDoc
    , callFunc = func { Derive.gfuncF = f . Derive.gfuncF func }
    }
    where
    appendDoc text (Derive.CallDoc tags module_ doc args) =
        Derive.CallDoc tags module_ (doc <> "\n" <> text) args

multipleCalls :: [(Expr.Symbol, [Expr.Symbol])]
    -> [(Expr.Symbol, Derive.Generator Derive.Note)]
multipleCalls calls = do
    (call, subcalls) <- calls
    let name = Text.intercalate "-" [sym | Expr.Symbol sym <- subcalls]
    pure (call, multipleCall (Derive.CallName name) subcalls)

-- | Create a call that just dispatches to other calls.
multipleCall :: Derive.CallName -> [Expr.Symbol]
    -> Derive.Generator Derive.Note
multipleCall name calls = generator0 name
    -- I intentionally omit the calls from the doc string, so they will
    -- combine in the call doc.  Presumably the calls are apparent from the
    -- name.
    "Dispatch to multiple calls." $ \args ->
        mconcatMap (Eval.reapplyGenerator args) calls

-- | The grace note falls either before or after the beat.
data Placement = Before | After deriving (Show, Eq)

doubledCall :: Expr.Symbol -> Derive.CallName -> Placement -> RealTime
    -> Signal.Y -> Derive.Generator Derive.Note
doubledCall callee name place defaultTime defaultDynScale = generator name
    ("Doubled call. The grace note falls "
        <> (if place == Before then "before" else "after") <> " the beat.")
    $ Sig.call ((,)
    <$> Sig.defaulted "time" (Typecheck.real defaultTime)
        "Time between the strokes."
    <*> Sig.defaulted "dyn" defaultDynScale "Dyn scale for the grace note."
    ) $ \(Typecheck.DefaultReal time, dynScale) -> Sub.inverting $ \args -> do
        dyn <- Call.dynamic =<< Args.realStart args
        let withDyn = Call.withDynamic (dyn * dynScale)
        let note = Eval.reapplyGeneratorNormalized args callee
        notes <- GraceUtil.repeatNotes note 2 time
            (Typecheck.Normalized $ if place == Before then 0 else 1) args
        case notes of
            [first, second]
                | place == Before -> Sub.derive [withDyn <$> first, second]
                | otherwise -> Sub.derive [first, withDyn <$> second]
            -- Shouldn't happen, because I passed 2 to repeat_notes.
            _ -> Derive.throw "expected 2 notes"

-- * composite

compositeDoc :: Doc.Doc
compositeDoc = "Composite instrument calls create notes for multiple\
    \ instruments, splitting pitch and control signals among them. The\
    \ composite instrument itself doesn't wind up in the output, so it\
    \ should have an empty allocation."

-- | A composite patch corresponds to multiple underlying patches.
--
-- This is useful for instruments with multiple pitches, e.g. a drum with
-- a keymap for strokes as well as a tuned pitch, or a pitched instrument with
-- a secondary pitch as a resonance.
data Composite = Composite
    -- | Dispatch to this call.
    { cCall :: !Expr.Symbol
    -- | And this instrument.
    , cInstrument :: !ScoreT.Instrument
    , cPitch :: !Pitch
    , cControls :: !Controls
    } deriving (Show)

instance Pretty Composite where
    pretty (Composite call inst pitch controls) = Text.unwords
        [ pretty inst <> ":", pretty call, ppitch
        , showControls controls
        ]
        where
        ppitch = case pitch of
            NoPitch -> "(no pitch)"
            Pitch control -> ShowVal.showVal control

data Pitch = NoPitch | Pitch ScoreT.PControl deriving (Show)

-- | Assigning a control to a composite actually means the other composite
-- parts will *not* get it.  This is because derivers naturally inherit the
-- entire environment, and it seems better to list what it should have, and not
-- exhaustively list all it gets.
--
-- So if this is empty it gets all controls, and if it's not, it still gets the
-- named controls but other composites don't.
type Controls = Set ScoreT.Control

showControls :: Controls -> Text
showControls cs
    | Set.null cs = "(all)"
    | otherwise = pretty cs

controlsDoc :: Controls -> Doc.Doc
controlsDoc = Doc.Doc . showControls

redirectPitch :: Derive.CallName -> Expr.Symbol -> Controls
    -> Expr.Symbol -> Controls -> Derive.Generator Derive.Note
redirectPitch name pitchedCall pitchedControls unpitchedCall
        unpitchedControls =
    generator name ("A composite instrument splits pitch and controls to\
        \ separate instruments.\n" <> compositeDoc)
    $ Sig.call ((,)
    <$> Sig.requiredEnviron "pitched" Sig.Prefixed
        ("This instrument gets the pitch signal and controls: "
            <> controlsDoc pitchedControls)
    <*> Sig.requiredEnviron "unpitched" Sig.Prefixed
        ("This instrument gets controls: " <> controlsDoc unpitchedControls)
    ) $ \(pitched, unpitched) -> Sub.inverting $ \args -> compositeCall args
        [ Composite pitchedCall pitched (Pitch ScoreT.defaultPitch)
            pitchedControls
        , Composite unpitchedCall unpitched NoPitch unpitchedControls
        ]

doublePitch :: Derive.CallName -> Controls -> ScoreT.PControl -> Controls
    -> Derive.Generator Derive.Note
doublePitch name baseControls pcontrol secondaryControls =
    generator name ("A composite instrument that has two pitch signals.\n"
        <> compositeDoc)
    $ Sig.call ((,)
    <$> Sig.requiredEnviron "base-inst" Sig.Prefixed
        ("Instrument that gets `#`, and controls: "
            <> controlsDoc baseControls)
    <*> Sig.requiredEnviron "second-inst" Sig.Prefixed
        ("Instrument that gets " <> ShowVal.doc pcontrol
            <> ", and controls: " <> controlsDoc secondaryControls)
    ) $ \(inst1, inst2) -> Sub.inverting $ \args -> compositeCall args
        [ Composite "" inst1 (Pitch ScoreT.defaultPitch) baseControls
        , Composite "" inst2 (Pitch pcontrol) secondaryControls
        ]

-- | See 'Composite'.
compositeCall :: Derive.NoteArgs -> [Composite] -> Derive.NoteDeriver
compositeCall args composites = mconcatMap (split args) composites
    where
    split args (Composite call inst pitch controls) =
        Derive.withInstrument inst $ withPitch pitch $
        replaceControls controls $
        Eval.reapplyGenerator args call
    withPitch p deriver = case p of
        NoPitch -> Derive.withPitch mempty deriver
        Pitch control
            | control == ScoreT.defaultPitch -> deriver
            | otherwise -> do
                mbPsig <- Derive.lookupPitchSignal control
                Derive.withPitch (fromMaybe mempty mbPsig) deriver
    replaceControls controls deriver = do
        -- This is not so much add the desired controls, because they're all
        -- ready there, but remove the rest.
        let notMine = usedControls `Set.difference` controls
        Derive.removeControls (Set.toList notMine) deriver
    usedControls = mconcatMap cControls composites


-- * control vals

constantPitch :: Derive.Generator Derive.Note
constantPitch = constantControls True mempty

constantControls :: Bool -> Set ScoreT.Control -> Derive.Generator Derive.Note
constantControls constantPitch controls =
    Note.transformedNote doc mempty $ \args ->
        (if constantPitch then setConstantPitch args else id)
        . setConstantControls controls args
    where
    doc = mconcat $ concat
        [ ["Notes have a constant pitch, sampled at attack time."
            | constantPitch]
        , [ "These controls are sampled at attack time, which means they work\
                \ with ControlFunctions: "
                <> Doc.commas (map Doc.pretty (Set.toList controls)) <> ".\n"
            | not (Set.null controls)
          ]
        ]

setConstantControls :: Set ScoreT.Control -> Derive.PassedArgs b
    -> Derive.Deriver a -> Derive.Deriver a
setConstantControls controls args deriver
    | Set.null controls = deriver
    | otherwise = do
        vals <- Derive.controlsAt =<< Args.realStart args
        let sampled = ScoreT.untyped . Signal.constant <$>
                Map.filterWithKey (\k _ -> k `Set.member` controls) vals
        Derive.withControls (Map.toList sampled) deriver

setConstantPitch :: Derive.PassedArgs b -> Derive.Deriver a
    -> Derive.Deriver a
setConstantPitch args deriver = do
    pitch <- Derive.pitchAt =<< Args.realStart args
    case pitch of
        Nothing -> deriver
        Just pitch -> Derive.withConstantPitch pitch $
            setConstantControls transposers args deriver
            where
            transposers = PSignal.pscaleTransposers (PSignal.pitchScale pitch)

-- * postproc

elementFrom :: (Typecheck.Typecheck old, Typecheck.ToVal new) => EnvKey.Key
    -> (old -> Either Log.Msg new) -> Cmd.InstrumentPostproc
elementFrom key = moveVal key EnvKey.element

elementFromId :: EnvKey.Key -> Cmd.InstrumentPostproc
elementFromId key =
    moveVal key EnvKey.element (\v -> Right (v :: DeriveT.Val))

-- | Move an environ val from one key to another.  This is meant to be put in
-- 'Cmd.Cmd.inst_postproc', because doing it in the note call may be too early.
moveVal :: (Typecheck.Typecheck old, Typecheck.ToVal new) => EnvKey.Key
    -> EnvKey.Key -> (old -> Either Log.Msg new) -> Cmd.InstrumentPostproc
moveVal oldKey newKey convert event =
    case Env.checkedVal2 oldKey (Score.eventEnviron event) of
        Nothing -> (event, [])
        Just (Left err) -> (event, [msg])
            where
            msg = Log.msg Log.Warn (Just (Score.eventStack event)) $
                "postproc: " <> err
        Just (Right old) -> case convert old of
            Left msg ->
                ( event
                , [msg { Log.msgStack = Just $ Score.eventStack event }]
                )
            Right new ->
                (Score.modifyEnviron (Env.insertVal newKey new) event, [])

withSymbolicPitch :: Cmd.InstrumentPostproc
withSymbolicPitch = whenEnv "symbolic-pitch" (Just True) addSymbolicPitch

addSymbolicPitch :: Cmd.InstrumentPostproc
addSymbolicPitch =
    addSymbolicPitchConvert (first pretty . PSignal.pitchNote)

addSymbolicPitchConvert :: (PSignal.Transposed -> Either Text Pitch.Note)
    -> Cmd.InstrumentPostproc
addSymbolicPitchConvert convert event = Log.runId $
    case Score.transposedAt (Score.eventStart event) event of
        Nothing -> do
            Log.warn $ "no symbolic pitch for " <> Score.shortEvent event
            pure event
        Just pitch -> case convert pitch of
            Left err -> do
                Log.warn $ "converting symbolic pitch for "
                    <> Score.shortEvent event <> ": " <> err
                pure event
            Right note ->
                pure $ setEnviron EnvKey.element (Pitch.noteText note) event

setEnviron :: Typecheck.ToVal key => EnvKey.Key -> key -> Score.Event
    -> Score.Event
setEnviron key val =
    Score.modifyVal key (const (Typecheck.toVal val))

whenEnv :: (Eq val, Typecheck.Typecheck val) => EnvKey.Key -> Maybe val
    -> Cmd.InstrumentPostproc -> Cmd.InstrumentPostproc
whenEnv key val postproc event
    | val == Env.maybeVal key (Score.eventEnviron event) = postproc event
    | otherwise = (event, [])
