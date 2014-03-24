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

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Europe.Grace as Grace
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang


-- | Make a call that simply calls the default note call with the given attrs.
attrs_note :: Score.Attributes -> Derive.Generator Derive.Note
attrs_note attrs =
    Derive.make_call Module.instrument ("attrs_note " <> ShowVal.show_val attrs)
        Tags.attr
        "Invoke the default note call with the given attrs." $
    Sig.call0 $ \args -> Util.add_attrs attrs (Util.note_here args)

-- | This re-applies the default note call and wraps it in a transformer.
-- However, it's more direct and probably clearer to directly create
-- a transformed note call via 'Note.transformed_note' or 'Note.note_call'.
note_call :: Text -> (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
note_call name transform = Derive.make_call Module.instrument name mempty
    "Invoke the default note call with a certain transform." $
    Sig.call0 $ \args -> Sub.when_under_inversion args transform $
        Util.note_here args

zero_duration :: Text -> (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
zero_duration doc transform = Note.transformed_note
    ("A normal note, but modified when it has zero duration: " <> doc) mempty $
    \args deriver -> (if Args.duration args == 0 then transform else id) deriver

-- | Just like the default note call, except apply a function to the output.
postproc_note :: Text -> Text -> (Score.Event -> Score.Event)
    -> Derive.Generator Derive.Note
postproc_note name doc f = postproc_generator name doc Note.c_note apply
    where apply d = map (fmap f) <$> d

-- | Transform an existing call by applying a function to it.  It gets a new
-- name and the documentation is prepended to the documentation of the original
-- call.
postproc_generator :: Text -> Text -> Derive.Call (a -> b) -> (b -> c)
    -> Derive.Call (a -> c)
postproc_generator name new_doc (Derive.Call _ old_doc func) f =
    Derive.Call name (append_doc new_doc old_doc) (f . func)
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
multiple_call name calls = Derive.make_call Module.instrument name Tags.inst
    -- I intentionally omit the calls from the doc string, so they will
    -- combine in the call doc.  Presumably the calls are apparent from the
    -- name.
    "Dispatch to multiple calls." $ Sig.call0 $ \args ->
        mconcat $ map (Call.reapply_gen args) calls

double_calls :: [(TrackLang.CallId, TrackLang.CallId)]
    -- ^ (call_name, repeated_call)
    -> [(TrackLang.CallId, Derive.Generator Derive.Note)]
double_calls = map (second double_call)

double_call :: TrackLang.CallId -> Derive.Generator Derive.Note
double_call repeated = Derive.make_call Module.instrument "double" Tags.inst
    "Doubled call. This is a specialization of `roll`."
    $ Sig.call ((,)
    <$> Sig.defaulted "time" Grace.default_grace_dur "Time between the strokes."
    <*> Sig.defaulted "dyn" 0.5 "Dyn scale for grace notes."
    ) $ \(TrackLang.DefaultReal time, dyn) args ->
        Grace.repeat_notes (Call.reapply_gen_normalized args repeated)
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
    pretty (Composite call inst pitch controls) = unwords
        [pretty inst <> ":", pretty call, ppitch, maybe "(all)" pretty controls]
        where
        ppitch = case pitch of
            NoPitch -> "(no pitch)"
            Pitch control -> untxt $ ShowVal.show_val (Score.PControl control)

data Pitch = NoPitch | Pitch (Maybe Score.Control) deriving (Show)
-- | If Nothing, then this Composite gets all the controls that are not given
-- to other instruments.  Otherwise, it only gets the named ones.
type Controls = Maybe (Set.Set Score.Control)

show_controls :: Controls -> Text
show_controls = maybe "(all)" prettyt

redirect_pitch :: Text -> TrackLang.CallId -> Controls -> TrackLang.CallId
    -> Controls -> Derive.Generator Derive.Note
redirect_pitch name pitched_call pitched_controls unpitched_call
        unpitched_controls =
    Derive.make_call Module.instrument name Tags.inst
        ("A composite instrument splits pitch and controls to separate\
        \ instruments.\n" <> composite_doc)
    $ Sig.call ((,)
    <$> Sig.required_environ "pitched" Sig.Prefixed
        ("This instrument gets the pitch signal and controls: "
            <> show_controls pitched_controls)
    <*> Sig.required_environ "unpitched" Sig.Prefixed
        ("This instrument gets controls: " <> show_controls unpitched_controls)
    ) $ \(pitched, unpitched) -> Sub.inverting $ \args -> composite_call args
        [ Composite pitched_call pitched (Pitch Nothing) pitched_controls
        , Composite unpitched_call unpitched NoPitch unpitched_controls
        ]

double_pitch :: Text -> Controls -> Score.Control -> Controls
    -> Derive.Generator Derive.Note
double_pitch name base_controls pcontrol secondary_controls =
    Derive.make_call Module.instrument name Tags.inst
        ("A composite instrument that has two pitch signals.\n"
        <> composite_doc)
    $ Sig.call ((,)
    <$> Sig.required_environ "base-inst" Sig.Prefixed
        ("Instrument that gets `#`, and controls: "
            <> show_controls base_controls)
    <*> Sig.required_environ "second-inst" Sig.Prefixed
        ("Instrument that gets "
            <> ShowVal.doc_val (Score.PControl (Just pcontrol))
            <> ", and controls: " <> show_controls secondary_controls)
    ) $ \(inst1, inst2) -> Sub.inverting $ \args -> composite_call args
        [ Composite "" inst1 (Pitch Nothing) base_controls
        , Composite "" inst2 (Pitch (Just pcontrol)) secondary_controls
        ]

composite_call :: Derive.NoteArgs -> [Composite] -> Derive.NoteDeriver
composite_call args composites = mconcat $ map (split args) composites
    where
    allocated = mconcat $ map (fromMaybe mempty . c_controls) composites
    split args (Composite call inst pitch controls) =
        Derive.with_instrument inst $ with_pitch pitch $
        with_controls controls $ Call.reapply_gen args call
    with_pitch p deriver = case p of
        NoPitch -> Derive.with_pitch Nothing mempty deriver
        Pitch Nothing -> deriver
        Pitch (Just control) -> do
            sig <- Derive.get_named_pitch control
            Derive.with_pitch Nothing (fromMaybe mempty sig) deriver
    with_controls controls deriver = do
        cmap <- Derive.get_controls
        cfuncs <- Derive.get_control_functions
        let strip = Map.filterWithKey $ \control _ -> maybe
                (Set.notMember control allocated) (Set.member control) controls
        Derive.with_control_maps (strip cmap) (strip cfuncs) deriver
