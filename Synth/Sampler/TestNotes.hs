-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Hand edit notes for testing.
module Synth.Sampler.TestNotes where
import qualified Data.Map as Map

import qualified Derive.Attrs as Attrs
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal
import Global


write :: IO Bool
write = Note.serialize "sampler.notes" notes

notes :: [Note.Note]
notes = map make
    [ (0,   nn NN.c4,       env [(0, 0.15), (0.25, 0.15), (0.3, 0)], ["open"])
    , (0.5, nn NN.d4,       vel 0.35,   ["open"])
    , (1,   pcurve [(1, NN.a3), (1.25, NN.ds4)], vel 1, ["open"])
    , (2,   Nothing,        vel 1,      ["cek"])
    , (2.1, Nothing,        vel 0.5,    ["cek"])
    ]
    where
    env = Signal.from_pairs
    vel = Signal.constant
    nn = Just . Signal.constant . Pitch.nn_to_double
    pcurve = Just . Signal.from_pairs . map (second Pitch.nn_to_double)
    make (start, pitch, dyn, attrs) = Note.Note
        { patch = "test"
        , instrument = "test"
        , element = ""
        , start = start
        , duration = 0 -- the sampler uses envelope, not duration
        , controls = Map.fromList $
            (Control.dynamic, dyn)
            : maybe [] ((:[]) . (Control.pitch,)) pitch
        , attributes = Attrs.attrs attrs
        }
