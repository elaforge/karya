-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Kendang sunda patches for "User.Elaforge.Instrument.Kontakt".
module User.Elaforge.Instrument.Kontakt.KendangSunda (
    patches, write_ksp, pitch_control
    , resolve_errors
) where
import qualified Midi.Key as Key
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig

import qualified User.Elaforge.Instrument.Kontakt.Util as Util
import Global


patches :: [MidiInst.Patch]
patches =
    [ MidiInst.code #= code $ CUtil.pitched_drum_patch pitched_notes $
        patch "kendang-sunda"
    ]
    where
    patch name = MidiInst.named_patch (-24, 24) name [(4, pitch_control)]
    notes = map fst pitched_notes
    code = MidiInst.cmd (CUtil.drum_cmd CUtil.MidiThru notes)
        <> MidiInst.note_generators
            (replace_det $ CUtil.drum_calls Nothing (Just tuning_control) notes)

replace_det :: [(Expr.Symbol, Derive.Generator Derive.Note)]
    -> [(Expr.Symbol, Derive.Generator Derive.Note)]
replace_det = (calls++) . filter ((`notElem` map fst calls) . fst)
    where
    calls =
        [ ("o", c_dong)
        , ("e", c_det Nothing)
        , ("e_", c_det (Just Low))
        , ("e-", c_det (Just Middle))
        , ("e^", c_det (Just High))
        ]

-- | Just like the default, except force 'pitch_control' to 0.
c_dong :: Derive.Generator Derive.Note
c_dong = CUtil.drum_call (Just tuning_control) dyn dong $
    Derive.with_constant_control pitch_control 0
    where dyn = 1

data Pitch = Low | Middle | High deriving (Show)

-- | Like the default, except set @+dong@ and force 'pitch_control' if Pitch is
-- given.  Of course, I could also set +det+low etc. and it would work with
-- integration.  Ok, so don't change the attrs, but set the control and have
-- separate defaults.
c_det :: Maybe Pitch -> Derive.Generator Derive.Note
c_det vague_pitch = Derive.generator Module.instrument name Tags.attr doc $
    Sig.call (
        Sig.defaulted "pitch" (Sig.control pitch_control pitch_default)
            "0 is the open pitch, and 1 is the highest pitch."
    ) $ \pitch args -> do
        -- The pitch control may already be in the environ, but not if it was
        -- given as a literal or default arg only.
        pitch <- Call.control_at pitch =<< Args.real_start args
        Call.add_attributes attrs $ CUtil.tuning_control args tuning_control $
            Derive.with_constant_control pitch_control pitch $
            Note.default_note Note.no_duration_attributes args
    where
    doc = "This takes a pitch argument and is split into separate calls so\
        \ there can be separate defaults for low, middle, and high."
    name = case vague_pitch of
        Nothing -> "det"
        Just Low -> "det-low"
        Just Middle -> "det-mid"
        Just High -> "det-high"
    pitch_default = case vague_pitch of
        Nothing -> 0.5
        Just Low -> 0.25
        Just Middle -> 0.5
        Just High -> 0.75
    attrs = det <> case vague_pitch of
        Nothing -> mempty
        Just Low -> Attrs.low
        Just Middle -> Attrs.middle
        Just High -> Attrs.high

tuning_control :: Score.Control
tuning_control = "kendang-tune"

pitch_control :: Score.Control
pitch_control = "pitch"

pitched_notes :: CUtil.PitchedNotes
(pitched_notes, resolve_errors) = CUtil.resolve_strokes 0.3 keymap strokes

strokes :: [(Char, Expr.Symbol, Attrs.Attributes, Drums.Group)]
stops :: [(Drums.Group, [Drums.Group])]
(stops, strokes) = (,) stops
    -- TODO paired strokes:
    -- bang = dong + pak, plang = tong + pak, blang = dong + peung
    -- plak = tak + phak
    --
    -- with panggul
    -- indung, kiri
    [ ('a', "-",    tak <> soft,        left_closed)
    , ('z', "+",    tak,                left_closed)
    , ('s', ".",    dong <> soft,       left_open)
    , ('x', "o",    dong,               left_open)
    , ('c', "i",    ting,               left_semiclosed)
    -- This uses the %pitch control.
    , ('v', "e",    det,                left_semiclosed)
    -- These are hardcoded to a certain pitch.
    , ('b', "e_",   det <> Attrs.low,   left_semiclosed)
    , ('n', "e-",   det <> Attrs.middle,left_semiclosed)
    , ('m', "e^",   det <> Attrs.high,  left_semiclosed)
    -- kulanter gede
    , (',', "u",    tung,               kulanter_gede_open)
    -- indung, kanan
    , ('1', "^",    phak <> soft,       right_closed)
    , ('q', "P",    phak,               right_closed)
    , ('2', "'",    phak1 <> soft,      right_closed)
    , ('w', "T",    phak1,              right_closed)
    , ('e', "I",    ping,               right_open)
    , ('r', "O",    pong,               right_open)
    -- kulanter leutik
    , ('t', "K",    pak,                kulanter_leutik_closed)
    , ('y', "E",    peung,              kulanter_leutik_open)
    ]
    where
    soft = Attrs.soft
    stops =
        [ (left_closed, [left_open])
        -- TODO this should be (left_semiclosed, [left_open, left_semiclosed]),
        -- but the ksp doesn't understand cc switching.
        , (left_open, [left_open])
        , (right_closed, [right_open])
        , (kulanter_leutik_closed, [kulanter_leutik_open])
        ]
    left_open = "left-open"
    -- TODO this should be its own group, but the ksp doesn't understand cc
    -- switching.
    left_semiclosed = "left-open"
    left_closed = "left-closed"

    right_open = "right-open"
    right_closed = "right-closed"
    kulanter_gede_open = "kulanter-gede-open"
    kulanter_leutik_closed = "kulanter-leutik-closed"
    kulanter_leutik_open = "kulanter-leutik-open"

keymap :: Map Attrs.Attributes CUtil.KeyswitchRange
keymap = CUtil.make_keymap2 Nothing 8 6 12 Key.c4
    -- indung, left
    [ [dong, det, det <> Attrs.low, det <> Attrs.middle, det <> Attrs.high]
    , [ting]
    , [tak] -- closed

    -- indung, right
    , [phak]
    , [phak1] -- phak with one finger
    , [ping]
    , [pong]
    -- kulanter gede
    , [tung]
    -- kulanter leutik
    , [pak]
    , [peung]
    ]

write_ksp :: IO ()
write_ksp = mapM_ (uncurry Util.write)
    [ ("kendang-sunda.ksp",
        Util.drum_mute_ksp "kendang sunda" pitched_notes stops)
    ]

-- indung, kiri

-- | Open left hand stroke, low pitch.
dong :: Attrs.Attributes
dong = Attrs.attr "dong"

-- | Open left hand stroke, high pitch.
det :: Attrs.Attributes
det = Attrs.attr "det"

-- | Right side harmonic played on the left hand.
ting :: Attrs.Attributes
ting = Attrs.attr "ting"

-- | Closed left hand stroke.  This isn't an official name.
tak :: Attrs.Attributes
tak = Attrs.attr "tak"

-- indung, kanan

-- | Open right hand stroke.
ping :: Attrs.Attributes
ping = Attrs.attr "ping"

-- | Open right hand rim.
pong :: Attrs.Attributes
pong = Attrs.attr "pong"

-- | Closed right hand stroke.
phak :: Attrs.Attributes
phak = Attrs.attr "phak"

-- | Phak with one finger.
phak1 :: Attrs.Attributes
phak1 = Attrs.attr "phak1"

-- | Kulanter gede, open stroke.
tung :: Attrs.Attributes
tung = Attrs.attr "tung"

-- | Kulanter leutik, closed stroke.
pak :: Attrs.Attributes
pak = Attrs.attr "pak"

-- | Kulanter leutik, open stroke.
peung :: Attrs.Attributes
peung = Attrs.attr "peung"
