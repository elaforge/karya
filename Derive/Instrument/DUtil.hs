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

import qualified Util.Pretty as Pretty
import qualified Derive.Args as Args
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
import qualified Derive.TrackLang as TrackLang
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import Global
import Types


generator :: Text -> Text
    -> Derive.WithArgDoc (Derive.GeneratorF d)
    -> Derive.Generator d
generator name = Derive.generator Module.instrument name mempty

generator0 :: Derive.Taggable d => Text -> Text
    -> (Derive.PassedArgs d -> Derive.Deriver (Stream.Stream d))
    -> Derive.Generator d
generator0 name doc call = generator name doc (Sig.call0 call)

transformer :: Text -> Text -> Derive.WithArgDoc (Derive.TransformerF d)
    -> Derive.Transformer d
transformer name doc =
    Derive.transformer Module.instrument name mempty doc

transformer0 :: Derive.Taggable d => Text -> Text -> Derive.TransformerF d
    -> Derive.Transformer d
transformer0 name doc call =
    Derive.transformer Module.instrument name mempty doc (Sig.call0t call)

-- | Make a call that simply calls the default note call with the given attrs.
attrs_note :: Score.Attributes -> Derive.Generator Derive.Note
attrs_note attrs =
    generator0 ("attrs_note " <> ShowVal.show_val attrs)
        "Invoke the default note call with the given attrs." $ \args ->
    Call.add_attrs attrs (Call.note_here args)

zero_duration_transform :: Text
    -> (Derive.NoteArgs -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
zero_duration_transform doc transform = Note.transformed_note
    ("A normal note, but modified when it has zero duration: " <> doc)
    mempty $ \args deriver ->
        ifM (is_zero_duration args) (transform args deriver) deriver

zero_duration :: Text -> Text -> (Derive.NoteArgs -> Derive.NoteDeriver)
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
postproc_note :: Text -> Text -> (Score.Event -> Score.Event)
    -> Derive.Generator Derive.Note
postproc_note name doc f = postproc_generator name doc Note.c_note apply
    where apply d = fmap f <$> d

-- | Transform an existing call by applying a function to it.  It gets a new
-- name and the documentation is prepended to the documentation of the original
-- call.
postproc_generator :: Text -> Text -> Derive.Generator d
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

multiple_calls :: [(TrackLang.CallId, [TrackLang.CallId])]
    -> [(TrackLang.CallId, Derive.Generator Derive.Note)]
multiple_calls calls =
    [(call, multiple_call (TrackLang.unsym call) subcalls)
        | (call, subcalls) <- calls]

-- | Create a call that just dispatches to other calls.
multiple_call :: Text -> [TrackLang.CallId] -> Derive.Generator Derive.Note
multiple_call name calls = generator0 name
    -- I intentionally omit the calls from the doc string, so they will
    -- combine in the call doc.  Presumably the calls are apparent from the
    -- name.
    "Dispatch to multiple calls." $ \args ->
        mconcatMap (Eval.reapply_generator args) calls

double_calls :: [(TrackLang.CallId, TrackLang.CallId)]
    -- ^ (call_name, repeated_call)
    -> [(TrackLang.CallId, Derive.Generator Derive.Note)]
double_calls = map (second double_call)

double_call :: TrackLang.CallId -> Derive.Generator Derive.Note
double_call repeated = generator "double"
    "Doubled call. This is a specialization of `roll`."
    $ Sig.call ((,)
    <$> Sig.defaulted "time" Grace.default_grace_dur "Time between the strokes."
    <*> Sig.defaulted "dyn" 0.5 "Dyn scale for grace notes."
    ) $ \(Typecheck.DefaultReal time, dyn) args ->
        Grace.repeat_notes (Eval.reapply_generator_normalized args repeated)
            1 time dyn args

-- * composite

composite_doc :: Text
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
    c_call :: !TrackLang.CallId
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

show_controls :: Controls -> Text
show_controls = maybe "(all)" pretty

redirect_pitch :: Text -> TrackLang.CallId -> Controls -> TrackLang.CallId
    -> Controls -> Derive.Generator Derive.Note
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
        [ Composite pitched_call pitched (Pitch Score.default_pitch) pitched_controls
        , Composite unpitched_call unpitched NoPitch unpitched_controls
        ]

double_pitch :: Text -> Controls -> Score.PControl -> Controls
    -> Derive.Generator Derive.Note
double_pitch name base_controls pcontrol secondary_controls =
    generator name ("A composite instrument that has two pitch signals.\n"
        <> composite_doc)
    $ Sig.call ((,)
    <$> Sig.required_environ "base-inst" Sig.Prefixed
        ("Instrument that gets `#`, and controls: "
            <> show_controls base_controls)
    <*> Sig.required_environ "second-inst" Sig.Prefixed
        ("Instrument that gets " <> ShowVal.doc_val pcontrol
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
                sig <- Derive.get_pitch control
                Derive.with_pitch (fromMaybe mempty sig) deriver
    with_controls controls deriver = do
        cmap <- Derive.get_controls
        cfuncs <- Derive.get_control_functions
        let strip = Map.filterWithKey $ \control _ -> maybe
                (Set.notMember control allocated) (Set.member control) controls
        Derive.with_control_maps (strip cmap) (strip cfuncs) deriver

-- * default pitch

c_set_default_pitch :: Derive.Taggable d => Pitch.Pitch -> Derive.Transformer d
c_set_default_pitch pitch = transformer "set-default-pitch"
    "Set the pitch to a constant if if there is no pitch in scope."
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
                (_, to_note, _) <- Call.get_pitch_functions
                note <- Derive.require
                    ("scale has no for default: " <> pretty pitch)
                    (to_note pitch)
                pitch <- Call.eval_note start note
                Derive.with_constant_pitch pitch deriver
            Right pitch -> Derive.with_constant_pitch pitch deriver
