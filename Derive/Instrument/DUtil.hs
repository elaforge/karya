-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions for instrument cmds.  This is called DUtil because there is also
-- "Cmd.Instrument.CUtil" and they are usually imported together.
--
-- I need a better name than \"Util\" for everything.
module Derive.Instrument.DUtil where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Europe.Grace as Grace
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Call.Sub as Sub
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.Flags as Flags
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global
import Types


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
        environ <- Internal.get_environ
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

multiple_calls :: [(BaseTypes.CallId, [BaseTypes.CallId])]
    -> [(BaseTypes.CallId, Derive.Generator Derive.Note)]
multiple_calls calls =
    [ (call, multiple_call (Derive.sym_to_call_name call) subcalls)
    | (call, subcalls) <- calls
    ]

-- | Create a call that just dispatches to other calls.
multiple_call :: Derive.CallName -> [BaseTypes.CallId]
    -> Derive.Generator Derive.Note
multiple_call name calls = generator0 name
    -- I intentionally omit the calls from the doc string, so they will
    -- combine in the call doc.  Presumably the calls are apparent from the
    -- name.
    "Dispatch to multiple calls." $ \args ->
        mconcatMap (Eval.reapply_generator args) calls

-- | The grace note falls either before or after the beat.
data Placement = Before | After deriving (Show, Eq)

doubled_call :: BaseTypes.CallId -> Derive.CallName -> Placement -> RealTime
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
        notes <- Grace.repeat_notes note 2 time
            (if place == Before then 0 else 1) args
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

-- | A composite patch corresponds to multiple underlying MIDI patches.
--
-- This is useful for instruments with multiple pitches, e.g. a drum with
-- a keymap for strokes as well as a tuned pitch, or a pitched instrument with
-- a secondary pitch as a resonance.
data Composite = Composite {
    -- | Dispatch to this call.
    c_call :: !BaseTypes.CallId
    -- | And this instrument.
    , c_instrument :: !Score.Instrument
    , c_pitch :: !Pitch
    , c_controls :: !Controls
    } deriving (Show)

instance Pretty.Pretty Composite where
    pretty (Composite call inst pitch controls) = Text.unwords
        [ pretty inst <> ":", pretty call, ppitch
        , maybe "(all)" pretty controls
        ]
        where
        ppitch = case pitch of
            NoPitch -> "(no pitch)"
            Pitch control -> ShowVal.show_val control

data Pitch = NoPitch | Pitch Score.PControl deriving (Show)
-- | If Nothing, then this Composite gets all the controls that are not given
-- to other instruments.  Otherwise, it only gets the named ones.
type Controls = Maybe (Set.Set Score.Control)

show_controls :: Controls -> Doc.Doc
show_controls = Doc.Doc . maybe "(all)" pretty

redirect_pitch :: Derive.CallName -> BaseTypes.CallId -> Controls
    -> BaseTypes.CallId -> Controls -> Derive.Generator Derive.Note
redirect_pitch name pitched_call pitched_controls unpitched_call
        unpitched_controls =
    generator name ("A composite instrument splits pitch and controls to\
        \ separate instruments.\n" <> composite_doc)
    $ Sig.call ((,)
    <$> Sig.required_environ "pitched" Sig.Prefixed
        ("This instrument gets the pitch signal and controls: "
            <> show_controls pitched_controls)
    <*> Sig.required_environ "unpitched" Sig.Prefixed
        ("This instrument gets controls: " <> show_controls unpitched_controls)
    ) $ \(pitched, unpitched) -> Sub.inverting $ \args -> composite_call args
        [ Composite pitched_call pitched (Pitch Score.default_pitch)
            pitched_controls
        , Composite unpitched_call unpitched NoPitch unpitched_controls
        ]

double_pitch :: Derive.CallName -> Controls -> Score.PControl -> Controls
    -> Derive.Generator Derive.Note
double_pitch name base_controls pcontrol secondary_controls =
    generator name ("A composite instrument that has two pitch signals.\n"
        <> composite_doc)
    $ Sig.call ((,)
    <$> Sig.required_environ "base-inst" Sig.Prefixed
        ("Instrument that gets `#`, and controls: "
            <> show_controls base_controls)
    <*> Sig.required_environ "second-inst" Sig.Prefixed
        ("Instrument that gets " <> ShowVal.doc pcontrol
            <> ", and controls: " <> show_controls secondary_controls)
    ) $ \(inst1, inst2) -> Sub.inverting $ \args -> composite_call args
        [ Composite "" inst1 (Pitch Score.default_pitch) base_controls
        , Composite "" inst2 (Pitch pcontrol) secondary_controls
        ]

composite_call :: Derive.NoteArgs -> [Composite] -> Derive.NoteDeriver
composite_call args composites = mconcatMap (split args) composites
    where
    allocated = mconcatMap (fromMaybe mempty . c_controls) composites
    split args (Composite call inst pitch controls) =
        Derive.with_instrument inst $ with_pitch pitch $
        with_controls controls $ Eval.reapply_generator args call
    with_pitch p deriver = case p of
        NoPitch -> Derive.with_pitch mempty deriver
        Pitch control
            | control == Score.default_pitch -> deriver
            | otherwise -> do
                sig <- Derive.get_named_pitch control
                Derive.with_pitch (fromMaybe mempty sig) deriver
    with_controls controls deriver = do
        cmap <- Derive.get_controls
        cfuncs <- Derive.get_control_functions
        let strip = Map.filterWithKey $ \control _ -> maybe
                (Set.notMember control allocated) (Set.member control) controls
        Derive.with_control_maps (strip cmap) (strip cfuncs) deriver

-- * default pitch

c_set_pitch_sargam :: Derive.Taggable d => Derive.Transformer d
c_set_pitch_sargam = transformer "set-pitch-sargam"
    "Set the pitch to a constant. Understands sargam style pitches like `2s`\
    \ or 4m`."
    $ Sig.callt (
        Sig.defaulted "pitch" (Right (Left (Pitch.pitch 0 0))) "Pitch."
    ) $ \pitch args deriver -> do
        pitch <- sargam_pitch (Args.start args) pitch
        Derive.with_constant_pitch pitch deriver

sargam_pitch :: ScoreTime -> Either Text (Either Pitch.Pitch PSignal.Pitch)
    -> Derive.Deriver PSignal.Pitch
sargam_pitch start p = case p of
    Left sargam -> do
        pitch <- Derive.require ("can't parse sargam: " <> sargam) $
            parse_sargam sargam
        Call.eval_pitch start pitch
    Right (Left pitch) -> Call.eval_pitch start pitch
    Right (Right pitch) -> return pitch

-- | TODO: write and use Parse.p_int for negative octave
parse_sargam :: Text -> Maybe Pitch.Pitch
parse_sargam t = do
    [oct_c, swaram_c] <- return $ Text.unpack t
    oct <- Num.readDigit oct_c
    swaram <- List.findIndex (==swaram_c) "srgmpdn"
    return $ Pitch.pitch oct swaram

c_set_default_pitch :: Derive.Taggable d => Pitch.Pitch -> Derive.Transformer d
c_set_default_pitch pitch = transformer "set-default-pitch"
    "Set the pitch to a constant if there is no pitch in scope."
    $ Sig.callt (Sig.defaulted "pitch" (Left pitch) "Pitch.")
    $ \pitch args deriver -> with_default_pitch (Args.start args) pitch deriver

-- | If there is no pitch in scope at the given time, set the given pitch.
with_default_pitch :: ScoreTime -> Either Pitch.Pitch PSignal.Pitch
    -> Derive.Deriver a -> Derive.Deriver a
with_default_pitch start default_pitch deriver = do
    maybe_pitch <- Derive.pitch_at =<< Derive.real start
    case maybe_pitch of
        Just _ -> deriver
        Nothing -> case default_pitch of
            Left pitch -> do
                pitch <- Call.eval_pitch start pitch
                Derive.with_constant_pitch pitch deriver
            Right pitch -> Derive.with_constant_pitch pitch deriver
