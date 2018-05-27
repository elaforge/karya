-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ness.Guitar.GConvert_test where
import qualified Data.List as List

import Util.Test
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Synth.Shared.Signal as Signal
import Global
import qualified Ness.Guitar as Guitar
import qualified Ness.Guitar.GConvert as GConvert
import qualified Ness.Guitar.Patch as Patch

import Types


test_collectFingers = do
    let f = fmap (bimap (map e_note) (map e_finger)) . GConvert.collectFingers
            . map mknote
    equal (f []) $ Right ([], [])
    pprint (f [("3e", 0, 1, [(0, NN.e4)]), ("3e", 1, 1, [(1, NN.b3)])])

e_note n = (Guitar.nString n, Guitar.nStart n)
e_finger f = (Guitar.fString f, Guitar.fMovement f)

mknote :: (Text, RealTime, RealTime, [(RealTime, Pitch.NoteNumber)])
    -> GConvert.Note
mknote (str, start, dur, pitch) = GConvert.Note
    { _instrument = inst
    , _string = fromMaybe (error $ "no string: " <> show str) $
        List.find ((==str) . Guitar.sName) $ Guitar.iStrings inst
    , _start = start
    , _duration = dur
    , _pitch = Signal.from_pairs $ map (second Pitch.nn_to_double) pitch
    , _finger = Signal.constant 1
    , _dynamic = 1
    , _location = 0.8
    }
    where inst = getInstrument "g12-1"

getInstrument :: Text -> Guitar.Instrument
getInstrument name = fromMaybe (error (show name)) $
    List.find ((==name) . Guitar.iName) Patch.instruments
