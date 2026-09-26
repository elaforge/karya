-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Audio Modeling's SWAM.
module User.Elaforge.Instrument.Swam (synth) where
import qualified Data.Map as Map

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
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

import qualified Instrument.InstT as InstT
import qualified Midi.CC as CC
import qualified Perform.Midi.Patch as Patch
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import           Global
import           Types


{-
    "Random bow" and "Random finger" = 0.  It leads to inconsistent harmonics,
    and I can randomize myself if I want.

    "Manual Trem / BowC KS" = BowCh.  Otherwise keyswitch tremolo is on note
    attack per note.  Though I don't use that yet.

    "Expr. Trigger Mode" = "off". It's unclear what this actually does, but it
    seems to have no effect for me.  It implies that without it, the note will
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
        MidiInst.synthControls [] patches
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

string :: InstT.Name -> [Pitch.NoteNumber] -> MidiInst.Patch
string name openStrings = MidiInst.pressure $
    MidiInst.code #= code $
    MidiInst.environ EnvKey.openStrings openStrings $
    MidiInst.patch#Patch.modeMap #= modes $
    MidiInst.patch#Patch.attributeMap #= keyswitches $
    -- defaults apply after the bipolar conversion
    MidiInst.controlDefaults [(bowForce, 0.5), (bowPos, 0.5)] $
    MidiInst.namedPatch (-24, 24) name controls
    where
    code = MidiInst.noteCalls
        [ MidiInst.both "o" cHarmonic
        , MidiInst.both "harsh" cHarsh
        , MidiInst.both "spic" cSpiccato
        , controlCall "pont" "Sul ponticello." bowPos (-1)
        , controlCall "tasto" "Sul tasto." bowPos 1
        , controlCall "flaut" "Flautando." bowForce (-1)
        , MidiInst.transformer "bow" cBow
        , MidiInst.transformer "`downbow`" (cBowDirection (pure Down))
        , MidiInst.transformer "`upbow`" (cBowDirection (pure Up))
        , MidiInst.generator "damp" cDamp
        ]
        <> MidiInst.postproc ((,[]) . postproc)
    controls = modeControls ++
        [ (CC.mod, Controls.vib)
        , (CC.vibSpeed, Controls.vibSpeed)
        , (CC.pan, Controls.pan)
        -- Set to >0 to turn legato into portamento.  The VST defaults to 0 is
        -- slow, 1 is fast, but I reversed it so the default 0 gets noraml
        -- legato.
        , (5, "port-time")
        -- When there's a string crossing on a portamento, this is ratio of
        -- string 1 to string 2.
        , (14, "port-split")
        , (15, bowForce)
        , (16, bowPos)
        , (17, "bow-noise")
        , (20, "trem-speed")
        -- <64 or >=64
        -- called sustain, but I use that name for something else
        , (64, Controls.pedal)
        ]
    -- CC breakpoints are <=42, <=84, >=85
    keyswitches = Patch.ccKeyswitchesPermute
        [ (32, [(mempty, 10), (Attrs.pizz, 60), (Attrs.legno, 100)])
        , (39, [(mempty, 10), (Attrs.harm, 60), (Attrs.harm<>Attrs.third, 100)])
        , (41, [(mempty, 10), (Attrs.trem, 60), (Attrs.attr "trem-fast", 100)])
        , (65, [(mempty, 0), (Attrs.mute, 127)]) -- con sord
        ]
    (modes, modeControls) = Patch.ccModeMap
        [ ("gesture", 33, [("expr", 10), ("bipolar", 60), ("bowing", 100)])
        -- Which strings to select for double-stops mode.
        , ("str-select", 34, [("4-3", 10), ("3-2", 80), ("2-1", 100)])
        , ("poly-mode", 35, [("mono-sx", 10), ("mono", 40),
            ("double", 60), ("double-hold", 80), ("auto", 120)])
        , ("fingering", 36, [("mid", 10), ("bridge", 60), ("nut", 100)])
        , ("bow-lift", 37, [("t", 10), ("f", 80)])
        , ("bow-start", 38, [("d", 10), ("u", 80)])
        ]

bowForce :: ScoreT.Control
bowForce = "bow-force"

bowPos :: ScoreT.Control
bowPos = "bow-pos"

postproc :: Score.Event -> Score.Event
postproc = bipolarControls [bowForce, bowPos] . bipolarExpression

bipolarControls :: [ScoreT.Control] -> Score.Event -> Score.Event
bipolarControls controls event
    | null sigs = event
    | otherwise = Score.modifyEnviron (Env.fromControls sigs <>) event
    where
    sigs = Map.fromList $ map (second normalize) $ Lists.mapMaybeSnd id $
        Lists.keyOnSnd (\c -> Map.lookup c (Score.eventControls event))
            controls

-- | When gesture=bipolar, the expression control is 0--62 for downbow, 64-127
-- for upbow.
bipolarExpression :: Score.Event -> Score.Event
bipolarExpression = whenVal "gesture" ("bipolar" :: Text) $
    bipolarControls [Controls.dynamic]

whenVal :: (Typecheck.Typecheck val, Eq val) => EnvKey.Key -> val
    -> (Score.Event -> Score.Event) -> Score.Event -> Score.Event
whenVal key val modify event =
    case Env.maybeVal key (Score.eventEnviron event) of
        Just v | v == val -> modify event
        _ -> event

-- Normalize -1--1 to 0--1.
normalize :: ScoreT.Typed Signal.Control -> ScoreT.Typed Signal.Control
normalize = fmap (Signal.scalarDivide 2 . Signal.scalarAdd 1)

-- * calls

-- Up and Down mean set gesture=bipolar, and leave dyn alone or invert it.
-- Alternate means each non-overlapping note gets inverted direction
cBow :: Derive.Transformer Derive.Note
cBow = cBowDirection (Sig.defaulted "dir" Alternate "Bow direction.")

cBowDirection :: Sig.Parser BowDirection -> Derive.Transformer Derive.Note
cBowDirection sig = Derive.transformer Module.instrument "bow" mempty
    "Set bow direction, either to up or down, or alternate. Alternate means\
    \ the bow changes as soon as there is a non-overlapping note." $
    Sig.callt sig $
    \dir _args -> bow dir . Derive.withVal "gesture" ("bipolar" :: Text)
    where
    bow dir deriver = case dir of
        Down -> Post.emap1_ invertDyn <$> deriver
        Up -> deriver
        Alternate -> snd . Post.emap1 alternateBowing Call.Down
            . Post.prevBy Score.eventInstrument <$> deriver

alternateBowing :: Call.UpDown -> (Maybe Score.Event, Score.Event)
    -> (Call.UpDown, Score.Event)
alternateBowing dir (prev, event)
    | maybe True (Score.eventsOverlap event) prev = (dir, set dir event)
    | otherwise = (rev dir, set (rev dir) event)
    where
    rev Call.Up = Call.Down
    rev Call.Down = Call.Up
    set Call.Down = invertDyn
    set Call.Up = id

invertDyn :: Score.Event -> Score.Event
invertDyn = Score.modifyControlVals Controls.dynamic negate

data BowDirection = Down | Up | Alternate
    deriving (Eq, Show, Enum, Bounded)

instance Typecheck.Typecheck BowDirection
instance Typecheck.ToVal BowDirection
instance ShowVal.ShowVal BowDirection

controlCall :: Expr.Symbol -> Doc.Doc -> ScoreT.Control
    -> Signal.Y -> MidiInst.Call Derive.Note
controlCall name doc control val = MidiInst.both name $
    Make.transformNotes Module.instrument (symToName name) mempty doc
    (Sig.defaulted "val" val "How much.") $
    \val -> fmap $ Post.emap1_ $ Score.modifyControlVals control (+val)
    where
    symToName (Expr.Symbol a) = Derive.CallName a

cHarsh :: Library.Calls Derive.Note
cHarsh = Make.transformNotes Module.instrument "harsh" mempty
    "Harsh attack." ((,)
    <$> Sig.defaulted "val" (1 :: Double) "How much bow pressure."
    <*> Sig.defaulted "dur" (0.15 :: Double) "How long."
    ) $ \(val, dur) -> fmap $ Post.emap1_ (attack val dur)
    where
    attack val dur event =
        Score.modifySignal bowForce (Signal.sigAdd sig) event
        where
        start = Score.eventStart event
        sig = Signal.fromPairs [(start, val), (start+dur, 0)]

cSpiccato :: Library.Calls Derive.Note
cSpiccato = Make.transformNotes Module.instrument "spic" mempty "Spiccato."
    (Sig.defaulted "dur" (0.05 :: RealTime) "How long."
    ) $ \(Typecheck.RealTimeFunctionT durT dur) deriver -> do
        events <- Derive.withVal "bow-lift" (ShowVal.showVal True) deriver
        durs <- Post.durationControl durT dur events
        pure $ Post.emap1_ (uncurry Score.setDuration)
            (Stream.zip durs events)

cHarmonic :: Library.Calls Derive.Note
cHarmonic = Make.transformNotes Module.instrument "harmonic"
    (Tags.attr <> Tags.ly)
    "Harmonic, with lilypond for natural and artificial harmonic notation."
    ((,)
    <$> Sig.defaulted "n" (2 :: Double)
        "Which harmonic. SWAM only supports 2 and 3."
    <*> Articulation.lilyHarmonicSig
    ) $ \(harm, lilyArgs) deriver -> Ly.whenLilypond
        (Articulation.lilyHarmonic lilyArgs (htype harm) deriver)
        (harmonic harm deriver)
    where
    harmonic (h :: Int) deriver = case h of
        2 -> Call.addAttributes Attrs.harm $
            Call.addConstant Controls.nn (-12) deriver
        3 -> Call.addAttributes (Attrs.harm <> Attrs.third) $
            Call.addConstant Controls.nn (-19) deriver
        h -> Derive.throw $ "only 2nd and 3rd harmonics supported: " <> showt h
    -- TODO this doesn't look at open strings, so it will produce lilypond that
    -- doesn't correspond to what the synth plays.  Or maybe it's that the
    -- synth will happily play impossible things, but the notation tries to
    -- be realistic.
    htype 2 = Articulation.Natural
    htype _ = Articulation.Artificial

-- | Emit a short note with reduced dyn, bow-lift=f.
cDamp :: Derive.Generator Derive.Note
cDamp = Derive.generator Module.instrument "damp" mempty
    "Emit a damped stroke." $
    Sig.call ((,)
    <$> Sig.defaulted "dur" (Typecheck.real 0.05) "Duration."
    <*> Sig.defaulted "dyn" (0.5 :: Double) "Dynamic scale."
    ) $ \(Typecheck.DefaultReal dur, dyn) -> Sub.inverting $ \args -> do
        dur <- Derive.score dur
        Derive.place (Args.start args) dur $
            Derive.withVal "bow-lift" (ShowVal.showVal False) $
            Call.multiplyDynamic dyn Call.note
