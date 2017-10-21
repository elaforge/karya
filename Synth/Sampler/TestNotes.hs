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


write :: IO ()
write = Note.serialize "sampler.notes" notes

notes :: [Note.Note]
notes = map make
    [ (0,   Just NN.c4, 0.15,   ["open"])
    , (0.5, Just NN.d4, 0.35,   ["open"])
    , (1,   Just NN.ds4, 1,     ["open"])
    , (2,   Nothing,    1,      ["cek"])
    , (2.05, Nothing,   1,      ["cek"])
    ]
    where
    make (start, pitch, dyn, attrs) = Note.Note
        { instrument = "inst"
        , patch = "test"
        , start = start
        , duration = 0 -- the sampler uses envelope, not duration
        , controls = Map.fromList $
            (Control.dynamic, Signal.constant dyn)
            : case pitch of
                Nothing -> []
                Just nn ->
                    [(Control.pitch, Signal.constant (Pitch.nn_to_double nn))]
        , attributes = Attrs.attrs attrs
        }
