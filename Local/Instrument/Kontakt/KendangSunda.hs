-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Kendang sunda patches for "Local.Instrument.Kontakt".
module Local.Instrument.Kontakt.KendangSunda (
    patches, write_ksp, pitch_control
) where
import qualified Data.Map as Map

import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.NN as NN
import qualified Local.Instrument.Kontakt.Util as Util
import Global


patches :: [MidiInst.Patch]
patches =
    [ (CUtil.pitched_drum_patch pitched_notes $ patch "kendang-sunda", code)
    ]
    where
    patch name = MidiInst.patch (-24, 24) name [(4, pitch_control)]
    notes = map fst pitched_notes
    code = MidiInst.cmd (CUtil.drum_cmd notes)
        <> MidiInst.note_generators
            (replace_det $ CUtil.drum_calls (Just tuning_control) notes)

replace_det :: [(TrackLang.CallId, Derive.Generator Derive.Note)]
    -> [(TrackLang.CallId, Derive.Generator Derive.Note)]
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
    Derive.with_constant_control pitch_control (Score.untyped 0)
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
        Call.add_attrs attrs $ CUtil.tuning_control args tuning_control $
            Derive.with_constant_control pitch_control (Score.untyped pitch) $
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
        Just Low -> 0.15
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
(pitched_notes, _unmapped_notes) = CUtil.resolve_strokes 0.3 keymap strokes

strokes :: [(Char, BaseTypes.CallId, Score.Attributes, Drums.Group)]
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
    , ('c', "i",    ting,               right_open)
    -- This uses the %pitch control.
    , ('v', "e",    det,                left_closed)
    -- These are hardcoded to a certain pitch.
    , ('b', "e_",   det <> Attrs.low,   left_closed)
    , ('n', "e-",   det <> Attrs.middle,left_closed)
    , ('m', "e^",   det <> Attrs.high,  left_closed)
    -- kulanter gede
    , ('v', "u",    tung,               kulanter_gede_open)
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
        , (right_closed, [right_open])
        , (kulanter_leutik_closed, [kulanter_leutik_open])
        ]
    left_open = "left-open"
    left_closed = "left-closed"
    right_open = "right-open"
    right_closed = "right-closed"
    kulanter_gede_open = "kulanter-gede-open"
    kulanter_leutik_closed = "kulanter-leutik-closed"
    kulanter_leutik_open = "kulanter-leutik-open"

keymap :: Map.Map Score.Attributes CUtil.KeyswitchRange
keymap = CUtil.make_keymap2 Nothing 8 6 12 NN.c4
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
dong :: Score.Attributes
dong = Score.attr "dong"

-- | Open left hand stroke, high pitch.
det :: Score.Attributes
det = Score.attr "det"

-- | Right side harmonic played on the left hand.
ting :: Score.Attributes
ting = Score.attr "ting"

-- | Closed left hand stroke.  This isn't an official name.
tak :: Score.Attributes
tak = Score.attr "tak"

-- indung, kanan

-- | Open right hand stroke.
ping :: Score.Attributes
ping = Score.attr "ping"

-- | Open right hand rim.
pong :: Score.Attributes
pong = Score.attr "pong"

-- | Closed right hand stroke.
phak :: Score.Attributes
phak = Score.attr "phak"

-- | Phak with one finger.
phak1 :: Score.Attributes
phak1 = Score.attr "phak1"

-- | Kulanter gede, open stroke.
tung :: Score.Attributes
tung = Score.attr "tung"

-- | Kulanter leutik, closed stroke.
pak :: Score.Attributes
pak = Score.attr "pak"

-- | Kulanter leutik, open stroke.
peung :: Score.Attributes
peung = Score.attr "peung"
