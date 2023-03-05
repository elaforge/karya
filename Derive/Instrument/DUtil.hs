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
attributes_note :: Attrs.Attributes -> Derive.Generator Derive.Note
attributes_note attrs =
    generator0 (Derive.CallName $ "attributes_note " <> ShowVal.show_val attrs)
        "Invoke the default note call with the given attrs." $ \args ->
    Call.add_attributes attrs (Call.reapply_note args)

zero_duration_transform :: Doc.Doc
    -> (Derive.NoteArgs -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
zero_duration_transform doc transform = Note.transformed_note
    ("A normal note, but modified when it has zero duration: " <> doc)
    mempty $ \args deriver ->
        ifM (is_zero_duration args) (transform args deriver) deriver

-- | Create a generator that has a different implementation for zero and
-- non-zero duration.
zero_duration :: Derive.CallName -> Doc.Doc
    -> (Derive.NoteArgs -> Derive.NoteDeriver)
    -> (Derive.NoteArgs -> Derive.NoteDeriver) -> Derive.Generator Derive.Note
zero_duration name doc zero non_zero = generator0 name doc $ \args ->
    ifM (is_zero_duration args) (zero args) (non_zero args)

is_zero_duration :: Derive.PassedArgs a -> Derive.Deriver Bool
is_zero_duration args
    | Args.duration args == 0 = do
        -- It turns out it's hard to figure out if a note has zero
        -- duration, and isn't just an infer-duration note.
        stack <- Internal.get_stack
        environ <- Derive.get_environ
        zero <- (==0) <$> Args.real_duration args
        let flags = Note.note_flags zero stack environ
        return $ zero && not (flags `Flags.has` Flags.infer_duration)
    | otherwise = return False

-- | Just like the default note call, except apply a function to the output.
postproc_note :: Derive.CallName -> Doc.Doc -> (Score.Event -> Score.Event)
    -> Derive.Generator Derive.Note
postproc_note name doc f = postproc_generator name doc Note.c_note apply
    where apply d = fmap f <$> d

-- | Transform an existing call by applying a function to it.  It gets a new
-- name and the documentation is prepended to the documentation of the original
-- call.
postproc_generator :: Derive.CallName -> Doc.Doc -> Derive.Generator d
    -> (Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d))
    -> Derive.Generator d
postproc_generator name new_doc (Derive.Call _ old_doc func) f = Derive.Call
    { call_name = name
    , call_doc = append_doc new_doc old_doc
    , call_func = func { Derive.gfunc_f = f . Derive.gfunc_f func }
    }
    where
    append_doc text (Derive.CallDoc tags module_ doc args) =
        Derive.CallDoc tags module_ (doc <> "\n" <> text) args

multiple_calls :: [(Expr.Symbol, [Expr.Symbol])]
    -> [(Expr.Symbol, Derive.Generator Derive.Note)]
multiple_calls calls =
    [ (call, multiple_call (Derive.sym_to_call_name call) subcalls)
    | (call, subcalls) <- calls
    ]

-- | Create a call that just dispatches to other calls.
multiple_call :: Derive.CallName -> [Expr.Symbol]
    -> Derive.Generator Derive.Note
multiple_call name calls = generator0 name
    -- I intentionally omit the calls from the doc string, so they will
    -- combine in the call doc.  Presumably the calls are apparent from the
    -- name.
    "Dispatch to multiple calls." $ \args ->
        mconcatMap (Eval.reapply_generator args) calls

-- | The grace note falls either before or after the beat.
data Placement = Before | After deriving (Show, Eq)

doubled_call :: Expr.Symbol -> Derive.CallName -> Placement -> RealTime
    -> Signal.Y -> Derive.Generator Derive.Note
doubled_call callee name place default_time default_dyn_scale = generator name
    ("Doubled call. The grace note falls "
        <> (if place == Before then "before" else "after") <> " the beat.")
    $ Sig.call ((,)
    <$> Sig.defaulted "time" (Typecheck.real default_time)
        "Time between the strokes."
    <*> Sig.defaulted "dyn" default_dyn_scale "Dyn scale for the grace note."
    ) $ \(Typecheck.DefaultReal time, dyn_scale) -> Sub.inverting $ \args -> do
        dyn <- Call.dynamic =<< Args.real_start args
        let with_dyn = Call.with_dynamic (dyn * dyn_scale)
        let note = Eval.reapply_generator_normalized args callee
        notes <- GraceUtil.repeat_notes note 2 time
            (Typecheck.Normalized $ if place == Before then 0 else 1) args
        case notes of
            [first, second]
                | place == Before -> Sub.derive [with_dyn <$> first, second]
                | otherwise -> Sub.derive [first, with_dyn <$> second]
            -- Shouldn't happen, because I passed 2 to repeat_notes.
            _ -> Derive.throw "expected 2 notes"

-- * composite

composite_doc :: Doc.Doc
composite_doc = "Composite instrument calls create notes for multiple\
    \ instruments, splitting pitch and control signals among them. The\
    \ composite instrument itself doesn't wind up in the output, so it\
    \ should have an empty allocation."

-- | A composite patch corresponds to multiple underlying patches.
--
-- This is useful for instruments with multiple pitches, e.g. a drum with
-- a keymap for strokes as well as a tuned pitch, or a pitched instrument with
-- a secondary pitch as a resonance.
data Composite = Composite {
    -- | Dispatch to this call.
    c_call :: !Expr.Symbol
    -- | And this instrument.
    , c_instrument :: !ScoreT.Instrument
    , c_pitch :: !Pitch
    , c_controls :: !Controls
    } deriving (Show)

instance Pretty Composite where
    pretty (Composite call inst pitch controls) = Text.unwords
        [ pretty inst <> ":", pretty call, ppitch
        , show_controls controls
        ]
        where
        ppitch = case pitch of
            NoPitch -> "(no pitch)"
            Pitch control -> ShowVal.show_val control

data Pitch = NoPitch | Pitch ScoreT.PControl deriving (Show)

-- | Assigning a control to a composite actually means the other composite
-- parts will *not* get it.  This is because derivers naturally inherit the
-- entire environment, and it seems better to list what it should have, and not
-- exhaustively list all it gets.
--
-- So if this is empty it gets all controls, and if it's not, it still gets the
-- named controls but other composites don't.
type Controls = Set ScoreT.Control

show_controls :: Controls -> Text
show_controls cs
    | Set.null cs = "(all)"
    | otherwise = pretty cs

controls_doc :: Controls -> Doc.Doc
controls_doc = Doc.Doc . show_controls

redirect_pitch :: Derive.CallName -> Expr.Symbol -> Controls
    -> Expr.Symbol -> Controls -> Derive.Generator Derive.Note
redirect_pitch name pitched_call pitched_controls unpitched_call
        unpitched_controls =
    generator name ("A composite instrument splits pitch and controls to\
        \ separate instruments.\n" <> composite_doc)
    $ Sig.call ((,)
    <$> Sig.required_environ "pitched" Sig.Prefixed
        ("This instrument gets the pitch signal and controls: "
            <> controls_doc pitched_controls)
    <*> Sig.required_environ "unpitched" Sig.Prefixed
        ("This instrument gets controls: " <> controls_doc unpitched_controls)
    ) $ \(pitched, unpitched) -> Sub.inverting $ \args -> composite_call args
        [ Composite pitched_call pitched (Pitch ScoreT.default_pitch)
            pitched_controls
        , Composite unpitched_call unpitched NoPitch unpitched_controls
        ]

double_pitch :: Derive.CallName -> Controls -> ScoreT.PControl -> Controls
    -> Derive.Generator Derive.Note
double_pitch name base_controls pcontrol secondary_controls =
    generator name ("A composite instrument that has two pitch signals.\n"
        <> composite_doc)
    $ Sig.call ((,)
    <$> Sig.required_environ "base-inst" Sig.Prefixed
        ("Instrument that gets `#`, and controls: "
            <> controls_doc base_controls)
    <*> Sig.required_environ "second-inst" Sig.Prefixed
        ("Instrument that gets " <> ShowVal.doc pcontrol
            <> ", and controls: " <> controls_doc secondary_controls)
    ) $ \(inst1, inst2) -> Sub.inverting $ \args -> composite_call args
        [ Composite "" inst1 (Pitch ScoreT.default_pitch) base_controls
        , Composite "" inst2 (Pitch pcontrol) secondary_controls
        ]

-- | See 'Composite'.
composite_call :: Derive.NoteArgs -> [Composite] -> Derive.NoteDeriver
composite_call args composites = mconcatMap (split args) composites
    where
    split args (Composite call inst pitch controls) =
        Derive.with_instrument inst $ with_pitch pitch $
        replace_controls controls $
        Eval.reapply_generator args call
    with_pitch p deriver = case p of
        NoPitch -> Derive.with_pitch mempty deriver
        Pitch control
            | control == ScoreT.default_pitch -> deriver
            | otherwise -> do
                mb_psig <- Derive.lookup_pitch_signal control
                Derive.with_pitch (fromMaybe mempty mb_psig) deriver
    replace_controls controls deriver = do
        -- This is not so much add the desired controls, because they're all
        -- ready there, but remove the rest.
        let not_mine = used_controls `Set.difference` controls
        Derive.remove_controls (Set.toList not_mine) deriver
    used_controls = mconcatMap c_controls composites


-- * control vals

constant_pitch :: Derive.Generator Derive.Note
constant_pitch = constant_controls True mempty

constant_controls :: Bool -> Set ScoreT.Control -> Derive.Generator Derive.Note
constant_controls constant_pitch controls =
    Note.transformed_note doc mempty $ \args ->
        (if constant_pitch then set_constant_pitch args else id)
        . set_constant_controls controls args
    where
    doc = mconcat $ concat
        [ ["Notes have a constant pitch, sampled at attack time."
            | constant_pitch]
        , [ "These controls are sampled at attack time, which means they work\
                \ with ControlFunctions: "
                <> Doc.commas (map Doc.pretty (Set.toList controls)) <> ".\n"
            | not (Set.null controls)
          ]
        ]

set_constant_controls :: Set ScoreT.Control -> Derive.PassedArgs b
    -> Derive.Deriver a -> Derive.Deriver a
set_constant_controls controls args deriver
    | Set.null controls = deriver
    | otherwise = do
        vals <- Derive.controls_at =<< Args.real_start args
        let sampled = ScoreT.untyped . Signal.constant <$>
                Map.filterWithKey (\k _ -> k `Set.member` controls) vals
        Derive.with_controls (Map.toList sampled) deriver

set_constant_pitch :: Derive.PassedArgs b -> Derive.Deriver a
    -> Derive.Deriver a
set_constant_pitch args deriver = do
    pitch <- Derive.pitch_at =<< Args.real_start args
    case pitch of
        Nothing -> deriver
        Just pitch -> Derive.with_constant_pitch pitch $
            set_constant_controls transposers args deriver
            where
            transposers = PSignal.pscale_transposers (PSignal.pitch_scale pitch)

-- * postproc

element_from :: (Typecheck.Typecheck old, Typecheck.ToVal new) => EnvKey.Key
    -> (old -> Either Log.Msg new) -> Cmd.InstrumentPostproc
element_from key = move_val key EnvKey.element

element_from_id :: EnvKey.Key -> Cmd.InstrumentPostproc
element_from_id key =
    move_val key EnvKey.element (\v -> Right (v :: DeriveT.Val))

-- | Move an environ val from one key to another.  This is meant to be put in
-- 'Cmd.Cmd.inst_postproc', because doing it in the note call may be too early.
move_val :: (Typecheck.Typecheck old, Typecheck.ToVal new) => EnvKey.Key
    -> EnvKey.Key -> (old -> Either Log.Msg new) -> Cmd.InstrumentPostproc
move_val old_key new_key convert event =
    case Env.checked_val2 old_key (Score.event_environ event) of
        Nothing -> (event, [])
        Just (Left err) -> (event, [msg])
            where
            msg = Log.msg Log.Warn (Just (Score.event_stack event)) $
                "postproc: " <> err
        Just (Right old) -> case convert old of
            Left msg ->
                ( event
                , [msg { Log.msg_stack = Just $ Score.event_stack event }]
                )
            Right new ->
                (Score.modify_environ (Env.insert_val new_key new) event, [])

with_symbolic_pitch :: Cmd.InstrumentPostproc
with_symbolic_pitch = when_env "symbolic-pitch" (Just True) add_symbolic_pitch

add_symbolic_pitch :: Cmd.InstrumentPostproc
add_symbolic_pitch =
    add_symbolic_pitch_convert (first pretty . PSignal.pitch_note)

add_symbolic_pitch_convert :: (PSignal.Transposed -> Either Text Pitch.Note)
    -> Cmd.InstrumentPostproc
add_symbolic_pitch_convert convert event = Log.run_id $
    case Score.transposed_at (Score.event_start event) event of
        Nothing -> do
            Log.warn $ "no symbolic pitch for " <> Score.short_event event
            return event
        Just pitch -> case convert pitch of
            Left err -> do
                Log.warn $ "converting symbolic pitch for "
                    <> Score.short_event event <> ": " <> err
                return event
            Right note ->
                return $ set_environ EnvKey.element (Pitch.note_text note) event

set_environ :: Typecheck.ToVal key => EnvKey.Key -> key -> Score.Event
    -> Score.Event
set_environ key val =
    Score.modify_environ_key key (const (Typecheck.to_val val))

when_env :: (Eq val, Typecheck.Typecheck val) => EnvKey.Key -> Maybe val
    -> Cmd.InstrumentPostproc -> Cmd.InstrumentPostproc
when_env key val postproc event
    | val == Env.maybe_val key (Score.event_environ event) = postproc event
    | otherwise = (event, [])
