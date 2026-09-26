-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Pakhawaj patch.  The notation is based on mridangam notation.
module User.Elaforge.Instrument.Kontakt.Pakhawaj where
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Instrument.Mridangam as Mridangam

import qualified Derive.Attrs as Attrs
import qualified Derive.C.India.Pakhawaj as Pakhawaj
import Derive.C.India.Pakhawaj (Stroke(..))
import qualified Derive.Expr as Expr

import qualified Midi.Key2 as Key2
import qualified Perform.NN as NN
import qualified User.Elaforge.Instrument.Kontakt.Util as Util

import Global


patches :: [MidiInst.Patch]
patches = [MidiInst.code #= code $ patch "pakhawaj" pitchedStrokes]
    where
    patch name strokes = CUtil.pitchedDrumPatch strokes $
        MidiInst.namedPatch (-24, 24) name []
    code = Mridangam.makeCode CUtil.MidiThru pitchedAttributes NN.c4 Nothing
        allStrokes bothCalls

pitchedStrokes :: CUtil.PitchedStrokes
(pitchedStrokes, _pitchedStrokes) = CUtil.drumPitchedStrokes allStrokes $
    CUtil.makeCcKeymap Key2.c_1 12 Key2.fs2
        [ [attr Ka]
        , [attr Ge]
        , [attr Tet]
        , [attr Te]
        , [attr Ne]
        , [attr Na, nam]
        , [din]
        , [attr Ta]
        , [attr Di]
        ]
    where attr = Pakhawaj.strokeToAttribute

-- | Create calls for all simultaneous left and right hand combinations, and
-- key bindings for a few common ones.
bothCalls :: [(Expr.Symbol, [Expr.Symbol], Maybe Char)]
bothCalls = Mridangam.makeBoth leftStrokes rightStrokes specialNames
    [ ("D", 'c')
    , ("T", 'f')
    , ("E", 'v')
    ]
    where
    specialNames =
        [ ("D", ["o", "u"]) -- dha
        , ("E", ["o", "k"]) -- dhet
        , ("T", ["+", "k"]) -- thet
        ]

writeKsp :: IO ()
writeKsp = mapM_ (uncurry Util.write)
    [ ("pakhawaj.ksp.txt", Util.drumMuteKsp "pakhawaj" pitchedStrokes stops)
    ]

-- | The symbols follow the same scheme as mridangam.
leftStrokes, rightStrokes :: [Drums.Stroke]
stops :: Drums.Stops
(leftStrokes, rightStrokes, stops) = (leftStrokes, rightStrokes, stops)
    where
    leftStrokes = concat
        [ group lClosed
            [ n 'a' "-" (attr Ka) 0.5
            , n 'z' "+" (attr Ka) 1
            ]
        , group lOpen
            [ n 's' "." (attr Ge) 0.5
            , n 'x' "o" (attr Ge) 1
            ]
        ]
    rightStrokes = concat
        [ group rClosed
            [ n '1' "l" (attr Tet) 0.5
            , n 'q' "k" (attr Tet) 1
            , n '2' "p" (attr Ne)  0.75
            , n 'w' "t" (attr Te)  1
            ]
        , group rSyahi
            [ n '3' "m" nam 1
            , n 'e' "n" (attr Na) 1
            , n 'r' "d" din 1
            ]
        , group rSyahiOpen
            [ n 't' "u" (attr Ta) 1
            ]
        , group rDheem [n 'y' "i" (attr Di) 1]
        ]

    stops =
        [ (lClosed, [lOpen])
        , (rClosed, [rSyahi, rSyahiOpen, rDheem])
        , (rSyahi, [rSyahiOpen, rDheem])
        , (rSyahiOpen, [rDheem])
        ]
    rClosed = "r-closed"
    rSyahi = "r-syahi"
    rSyahiOpen = "r-syahi_open"
    rDheem = "r-dheem"
    lClosed = "l-closed"
    lOpen = "l-open"
    group name = map $ \n -> n { Drums._group = name }
    n = Drums.strokeDyn
    attr = Pakhawaj.strokeToAttribute

pitchedAttributes :: [Attrs.Attributes]
pitchedAttributes = map Pakhawaj.strokeToAttribute [Ge, Na, Ta, Di]

allStrokes :: [Drums.Stroke]
allStrokes = leftStrokes ++ rightStrokes

-- mridangam strokes
din = Attrs.attr "din"
nam = Attrs.attr "nam"
