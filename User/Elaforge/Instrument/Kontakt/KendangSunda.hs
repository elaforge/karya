-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Kendang sunda patches for "User.Elaforge.Instrument.Kontakt".
module User.Elaforge.Instrument.Kontakt.KendangSunda (
    patches, writeKsp, pitchControl
    , resolveErrors
) where
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst

import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig

import qualified Midi.Key as Key
import qualified User.Elaforge.Instrument.Kontakt.Util as Util

import           Global


patches :: [MidiInst.Patch]
patches =
    [ MidiInst.code #= code $ CUtil.pitchedDrumPatch pitchedStrokes $
        patch "kendang-sunda"
    ]
    where
    patch name = MidiInst.namedPatch (-24, 24) name [(4, pitchControl)]
    strokes = map fst pitchedStrokes
    code = MidiInst.cmd (CUtil.drumCmd [] CUtil.MidiThru strokes)
        <> MidiInst.noteGenerators
            (replaceDet $ CUtil.drumCalls (map (,callConfig) strokes))

replaceDet :: [(Expr.Symbol, Derive.Generator Derive.Note)]
    -> [(Expr.Symbol, Derive.Generator Derive.Note)]
replaceDet = (calls++) . filter ((`notElem` map fst calls) . fst)
    where
    calls =
        [ ("o", cDong)
        , ("e", cDet Nothing)
        , ("e_", cDet (Just Low))
        , ("e-", cDet (Just Middle))
        , ("e^", cDet (Just High))
        ]

-- | Just like the default, except force 'pitch_control' to 0.
cDong :: Derive.Generator Derive.Note
cDong = CUtil.drumCall config "o" dong
    where
    config = callConfig
        { CUtil._transform = Derive.withConstantControl pitchControl 0 }

callConfig :: CUtil.CallConfig
callConfig = CUtil.callConfig { CUtil._tuningControl = Just tuningControl }

data Pitch = Low | Middle | High deriving (Show)

-- | Like the default, except set @+dong@ and force 'pitch_control' if Pitch is
-- given.  Of course, I could also set +det+low etc. and it would work with
-- integration.  Ok, so don't change the attrs, but set the control and have
-- separate defaults.
cDet :: Maybe Pitch -> Derive.Generator Derive.Note
cDet vaguePitch = Derive.generator Module.instrument name Tags.attr doc $
    Sig.call (
        Sig.defaultedEnv "pitch" Derive.Unprefixed pitchDefault
            "0 is the open pitch, and 1 is the highest pitch."
    ) $ \pitch args -> Call.addAttributes attrs $
        CUtil.applyTuningControl args tuningControl $
        Derive.withConstantControl pitchControl pitch $
        Note.defaultNote Note.noDurationAttributes args
    where
    doc = "This takes a pitch argument and is split into separate calls so\
        \ there can be separate defaults for low, middle, and high."
    name = case vaguePitch of
        Nothing -> "det"
        Just Low -> "det-low"
        Just Middle -> "det-mid"
        Just High -> "det-high"
    pitchDefault :: Double
    pitchDefault = case vaguePitch of
        Nothing -> 0.5
        Just Low -> 0.25
        Just Middle -> 0.5
        Just High -> 0.75
    attrs = det <> case vaguePitch of
        Nothing -> mempty
        Just Low -> Attrs.low
        Just Middle -> Attrs.middle
        Just High -> Attrs.high

tuningControl :: ScoreT.Control
tuningControl = "kendang-tune"

pitchControl :: ScoreT.Control
pitchControl = "pitch"

pitchedStrokes :: CUtil.PitchedStrokes
(pitchedStrokes, resolveErrors) = CUtil.resolveStrokes 0.3 keymap strokes

strokes :: [(Char, Expr.Symbol, Attrs.Attributes, Drums.Group)]
stops :: [(Drums.Group, [Drums.Group])]
(stops, strokes) = (,) stops
    -- TODO paired strokes:
    -- bang = dong + pak, plang = tong + pak, blang = dong + peung
    -- plak = tak + phak
    --
    -- with panggul
    -- indung, kiri
    [ ('a', "-",    tak <> soft,        leftClosed)
    , ('z', "+",    tak,                leftClosed)
    , ('s', ".",    dong <> soft,       leftOpen)
    , ('x', "o",    dong,               leftOpen)
    , ('c', "i",    ting,               leftSemiclosed)
    -- This uses the %pitch control.
    , ('v', "e",    det,                leftSemiclosed)
    -- These are hardcoded to a certain pitch.
    , ('b', "e_",   det <> Attrs.low,   leftSemiclosed)
    , ('n', "e-",   det <> Attrs.middle,leftSemiclosed)
    , ('m', "e^",   det <> Attrs.high,  leftSemiclosed)
    -- kulanter gede
    , (',', "u",    tung,               kulanterGedeOpen)
    -- indung, kanan
    , ('1', "^",    phak <> soft,       rightClosed)
    , ('q', "P",    phak,               rightClosed)
    , ('2', "'",    phak1 <> soft,      rightClosed)
    , ('w', "T",    phak1,              rightClosed)
    , ('e', "I",    ping,               rightOpen)
    , ('r', "O",    pong,               rightOpen)
    -- kulanter leutik
    , ('t', "K",    pak,                kulanterLeutikClosed)
    , ('y', "E",    peung,              kulanterLeutikOpen)
    ]
    where
    soft = Attrs.soft
    stops =
        [ (leftClosed, [leftOpen])
        -- TODO this should be (left_semiclosed, [left_open, left_semiclosed]),
        -- but the ksp doesn't understand cc switching.
        , (leftOpen, [leftOpen])
        , (rightClosed, [rightOpen])
        , (kulanterLeutikClosed, [kulanterLeutikOpen])
        ]
    leftOpen = "left-open"
    -- TODO this should be its own group, but the ksp doesn't understand cc
    -- switching.
    leftSemiclosed = "left-open"
    leftClosed = "left-closed"

    rightOpen = "right-open"
    rightClosed = "right-closed"
    kulanterGedeOpen = "kulanter-gede-open"
    kulanterLeutikClosed = "kulanter-leutik-closed"
    kulanterLeutikOpen = "kulanter-leutik-open"

keymap :: Map Attrs.Attributes CUtil.KeyswitchRange
keymap = CUtil.makeKeymap2 Nothing 8 6 12 Key.c4
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

writeKsp :: IO ()
writeKsp = mapM_ (uncurry Util.write)
    [ ("kendang-sunda.ksp",
        Util.drumMuteKsp "kendang sunda" pitchedStrokes stops)
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
