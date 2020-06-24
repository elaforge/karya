-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Audio Modeling's SWAM.
module User.Elaforge.Instrument.Swam (synth) where
import qualified Data.Map as Map

import qualified Util.Doc as Doc
import qualified Util.Seq as Seq
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Articulation as Articulation
import qualified Derive.Call as Call
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Instrument.InstTypes as InstTypes
import qualified Midi.CC as CC
import qualified Perform.Midi.Patch as Patch
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import           Global


{-
    "Random bow" and "Random finger" = 0.  It leads to inconsistent harmonics,
    and I can randomize myself if I want.

    "Manual Trem / BowC KS" = BowCh.  Otherwise keyswitch tremolo is on note
    attack per note.  Though I don't use that yet.

    "Expr. Trigger Mode" = "off". It's unclear what this actually does, but it
    seems to have no affect for me.  It implies that without it, the note will
    be played even when there is no pressure, but of course it won't.  Off
    because I do my own attack, and it implies it will make an attack for me
    when on.

    Attack = Expression
    PortTime = CC

    Automatic tremolo uses +trem to turn it on, then %trem-speed to configure
    it.  It's convenient but lacks control.

    If I want exact tremolo speed or accents or something, I have to do it
    manually.  Just calling 'trem' doesn't reverse bow direction.  I probably
    have to set the gesture=bipolar to force bow changes.

    Things I'd like:

    - Better documentation: what is "dynamic transitions"?  What is "env attack
    speed" and how excatly does the automatic env generation work?

    - Explicit control over string selection per note, to avoid all the
    automatic string selection heuristics.

    - More harmonics.

    - Harmonic gliss, e.g. hold a note, then change pitch+pitch bend to affect
    the touch point.

    - More extreme sul pont / sul tasto, from on the bridge to middle of the
    string.

    - Custom open strings.
-}

synth :: MidiInst.Synth
synth =
    MidiInst.synth "swam" "Audio Modeling SWAM" $
        MidiInst.synth_controls [] patches
    where
    patches = map (uncurry string)
        [ ("violin", [NN.g3, NN.d4, NN.a4, NN.e4])
        , ("viola", [NN.c3, NN.g3, NN.d4, NN.a4])
        , ("cello", [NN.c2, NN.g2, NN.d3, NN.a3])
        , ("bass", [NN.e1, NN.a1, NN.d2, NN.g2])
        ]

{-
    - mono-sx - damp "from" string according to interval.

    - mono - never damp "from" string.

    - double - Play two notes at once.  It selects the strings
    automatically, but can't do legato.

    - double-hold - Like double, but use str-select.  Can do legato on one
    string.  The second played string is assumed to be the moving one, so you
    have to tweak start times based on who is moving, and even so movements on
    the lower string often stop the upper string.

    - auto - try to detect, but add latency.
-}

string :: InstTypes.Name -> [Pitch.NoteNumber] -> MidiInst.Patch
string name open_strings = MidiInst.pressure $
    MidiInst.code #= code $
    MidiInst.environ EnvKey.open_strings open_strings $
    MidiInst.patch#Patch.mode_map #= modes $
    MidiInst.patch#Patch.attribute_map #= keyswitches $
    -- defaults apply after the bipolar conversion
    MidiInst.control_defaults [(bow_force, 0.5), (bow_pos, 0.5)] $
    MidiInst.named_patch (-24, 24) name controls
    where
    code = MidiInst.note_calls
        [ MidiInst.both "o" c_harmonic
        , MidiInst.both "harsh" c_harsh
        , MidiInst.both "spic" c_spiccato
        , control_call "pont" "Sul ponticello." bow_pos (-1)
        , control_call "tasto" "Sul tasto." bow_pos 1
        , control_call "flaut" "Flautando." bow_force (-1)
        , MidiInst.transformer "bow" c_bow
        , MidiInst.transformer "`downbow`" (c_bow_direction (pure Down))
        , MidiInst.transformer "`upbow`" (c_bow_direction (pure Up))
        , MidiInst.generator "damp" c_damp
        ]
        <> MidiInst.postproc ((,[]) . postproc)
    controls = mode_controls ++
        [ (CC.mod, Controls.vib)
        , (CC.vib_speed, Controls.vib_speed)
        , (CC.pan, Controls.pan)
        -- Set to >0 to turn legato into portamento.  The VST defaults to 0 is
        -- slow, 1 is fast, but I reversed it so the default 0 gets noraml
        -- legato.
        , (5, "port-time")
        -- When there's a string crossing on a portamento, this is ratio of
        -- string 1 to string 2.
        , (14, "port-split")
        , (15, bow_force)
        , (16, bow_pos)
        , (17, "bow-noise")
        , (20, "trem-speed")
        -- <64 or >=64
        -- called sustain, but I use that name for something else
        , (64, Controls.pedal)
        ]
    -- CC breakpoints are <=42, <=84, >=85
    keyswitches = Patch.cc_keyswitches_permute
        [ (32, [(mempty, 10), (Attrs.pizz, 60), (Attrs.legno, 100)])
        , (39, [(mempty, 10), (Attrs.harm, 60), (Attrs.harm<>Attrs.third, 100)])
        , (41, [(mempty, 10), (Attrs.trem, 60), (Attrs.attr "trem-fast", 100)])
        , (65, [(mempty, 0), (Attrs.mute, 127)]) -- con sord
        ]
    (modes, mode_controls) = Patch.cc_mode_map
        [ ("gesture", 33, [("expr", 10), ("bipolar", 60), ("bowing", 100)])
        -- Which strings to select for double-stops mode.
        , ("str-select", 34, [("4-3", 10), ("3-2", 80), ("2-1", 100)])
        , ("poly-mode", 35, [("mono-sx", 10), ("mono", 40),
            ("double", 60), ("double-hold", 80), ("auto", 120)])
        , ("fingering", 36, [("mid", 10), ("bridge", 60), ("nut", 100)])
        , ("bow-lift", 37, [("t", 10), ("f", 80)])
        , ("bow-start", 38, [("d", 10), ("u", 80)])
        ]

bow_force :: ScoreT.Control
bow_force = "bow-force"

bow_pos :: ScoreT.Control
bow_pos = "bow-pos"

postproc :: Score.Event -> Score.Event
postproc = bipolar_controls [bow_force, bow_pos] . bipolar_expression

bipolar_controls :: [ScoreT.Control] -> Score.Event -> Score.Event
bipolar_controls controls event
    | null sigs = event
    | otherwise = event
        { Score.event_controls = sigs <> Score.event_controls event }
    where
    sigs = Map.fromList $ map (second normalize) $ Seq.map_maybe_snd id $
        Seq.key_on_snd (\c -> Map.lookup c (Score.event_controls event))
            controls

-- | When gesture=bipolar, the expression control is 0--62 for downbow, 64-127
-- for upbow.
bipolar_expression :: Score.Event -> Score.Event
bipolar_expression = when_val "gesture" ("bipolar" :: Text) $
    bipolar_controls [Controls.dynamic]

when_val :: (Typecheck.Typecheck val, Eq val) => EnvKey.Key -> val
    -> (Score.Event -> Score.Event) -> Score.Event -> Score.Event
when_val key val modify event =
    case Env.maybe_val key (Score.event_environ event) of
        Just v | v == val -> modify event
        _ -> event

-- Normalize -1--1 to 0--1.
normalize :: ScoreT.Typed Signal.Control -> ScoreT.Typed Signal.Control
normalize = fmap (Signal.scalar_divide 2 . Signal.scalar_add 1)

-- * calls

-- Up and Down mean set gesture=bipolar, and leave dyn alone or invert it.
-- Alternate means each non-overlapping note gets inverted direction
c_bow :: Derive.Transformer Derive.Note
c_bow = c_bow_direction (Sig.defaulted "dir" Alternate "Bow direction.")

c_bow_direction :: Sig.Parser BowDirection -> Derive.Transformer Derive.Note
c_bow_direction sig = Derive.transformer Module.instrument "bow" mempty
    "Set bow direction, either to up or down, or alternate. Alternate means\
    \ the bow changes as soon as there is a non-overlapping note." $
    Sig.callt sig $
    \dir _args -> bow dir . Derive.with_val "gesture" ("bipolar" :: Text)
    where
    bow dir deriver = case dir of
        Down -> Post.emap1_ invert_dyn <$> deriver
        Up -> deriver
        Alternate -> snd . Post.emap1 alternate_bowing Call.Down
            . Post.prev_by Score.event_instrument <$> deriver

alternate_bowing :: Call.UpDown -> (Maybe Score.Event, Score.Event)
    -> (Call.UpDown, Score.Event)
alternate_bowing dir (prev, event)
    | maybe True (Score.events_overlap event) prev = (dir, set dir event)
    | otherwise = (rev dir, set (rev dir) event)
    where
    rev Call.Up = Call.Down
    rev Call.Down = Call.Up
    set Call.Down = invert_dyn
    set Call.Up = id

invert_dyn :: Score.Event -> Score.Event
invert_dyn = Score.modify_control Controls.dynamic (Signal.scalar_multiply (-1))

data BowDirection = Down | Up | Alternate
    deriving (Eq, Show, Enum, Bounded)

instance Typecheck.Typecheck BowDirection
instance Typecheck.ToVal BowDirection
instance ShowVal.ShowVal BowDirection

control_call :: Expr.Symbol -> Doc.Doc -> ScoreT.Control
    -> Signal.Y -> MidiInst.Call Derive.Note
control_call name doc control val = MidiInst.both name $
    Make.transform_notes Module.instrument (sym_to_name name) mempty doc
    (Sig.defaulted "val" val "How much.") $
    \val -> fmap $ Post.emap1_ $ Score.modify_control control $
        Signal.sig_add (Signal.constant val)
    where
    sym_to_name (Expr.Symbol a) = Derive.CallName a

c_harsh :: Library.Calls Derive.Note
c_harsh = Make.transform_notes Module.instrument "harsh" mempty
    "Harsh attack." ((,)
    <$> Sig.defaulted "val" 1 "How much bow pressure."
    <*> Sig.defaulted "dur" 0.15 "How long."
    ) $ \(val, dur) -> fmap $ Post.emap1_ (attack val dur)
    where
    attack val dur event =
        Score.modify_control bow_force (Signal.sig_add sig) event
        where
        start = Score.event_start event
        sig = Signal.from_pairs [(start, val), (start+dur, 0)]

c_spiccato :: Library.Calls Derive.Note
c_spiccato = Make.transform_notes Module.instrument "spic" mempty "Spiccato."
    (Sig.defaulted "dur" (Sig.typed_control "spic-dur" 0.05 ScoreT.Real)
        "How long."
    ) $ \dur deriver -> do
        events <- Derive.with_val "bow-lift" (ShowVal.show_val True) deriver
        -- This is a lot of work to make spic-dur be a signal, but it seems not
        -- actually that useful, since if I want variable durations I can just
        -- use sus-set directly.
        durs <- Post.duration_control Typecheck.Real dur events
        return $ Post.emap1_ (uncurry Score.set_duration)
            (Stream.zip durs events)

c_harmonic :: Library.Calls Derive.Note
c_harmonic = Make.transform_notes Module.instrument "harmonic"
    (Tags.attr <> Tags.ly)
    "Harmonic, with lilypond for natural and artificial harmonic notation."
    ((,)
    <$> Sig.defaulted "n" 2 "Which harmonic. SWAM only supports 2 and 3."
    <*> Articulation.lily_harmonic_sig
    ) $ \(harm, lily_args) deriver -> Ly.when_lilypond
        (Articulation.lily_harmonic lily_args (htype harm) deriver)
        (harmonic harm deriver)
    where
    harmonic (h :: Int) deriver = case h of
        2 -> Call.add_attributes Attrs.harm $
            Call.add_constant Controls.nn (-12) deriver
        3 -> Call.add_attributes (Attrs.harm <> Attrs.third) $
            Call.add_constant Controls.nn (-19) deriver
        h -> Derive.throw $ "only 2nd and 3rd harmonics supported: " <> showt h
    -- TODO this doesn't look at open strings, so it will produce lilypond that
    -- doesn't correspond to what the synth plays.  Or maybe it's that the
    -- synth will happily play impossible things, but the notation tries to
    -- be realistic.
    htype 2 = Articulation.Natural
    htype _ = Articulation.Artificial

-- | Emit a short note with reduced dyn, bow-lift=f.
c_damp :: Derive.Generator Derive.Note
c_damp = Derive.generator Module.instrument "damp" mempty
    "Emit a damped stroke." $
    Sig.call ((,)
    <$> Sig.defaulted "dur" (Typecheck.real 0.05) "Duration."
    <*> Sig.defaulted "dyn" 0.5 "Dynamic scale."
    ) $ \(dur, dyn) -> Sub.inverting $ \args -> do
        dur <- Derive.score dur
        Derive.place (Args.start args) dur $
            Derive.with_val "bow-lift" (ShowVal.show_val False) $
            Call.multiply_dynamic dyn Call.note
