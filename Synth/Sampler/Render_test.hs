-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Render_test where
import qualified Data.List as List
import qualified System.Directory as Directory
import System.FilePath ((</>))

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.Resample as Resample
import Util.Test
import qualified Util.Test.Testing as Testing

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Sampler.Render as Render
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Types


test_write_incremental = do
    dir <- Testing.tmp_dir "write"
    let dur = AUtil.toSeconds chunkSize
    let write = Render.write_ chunkSize Resample.Linear dir

    -- no notes produces no output
    io_equal (write []) (Right (0, 0))
    io_equal (Directory.listDirectory (dir </> Checkpoint.cacheDir)) []

    -- One failed note counts as a checkpoint, but no output.
    io_equal (write [mkNote "no-such-patch" 0 dur NN.c4]) (Right (1, 1))
    io_equal (listWavs dir) []

chunkSize :: Audio.Frame
chunkSize = 8


-- * shared

mkNote :: Note.PatchName -> RealTime -> RealTime -> Pitch.NoteNumber
    -> Note.Note
mkNote patch start dur nn = Note.setHash $
    Note.withControl Control.volume (Signal.constant 1) $
    Note.withControl Control.dynamic (Signal.constant 1) $
    Note.withPitch nn $ Note.note patch "inst" start dur

listWavs :: FilePath -> IO [FilePath]
listWavs = fmap (List.sort . filter (".wav" `List.isSuffixOf`))
    . Directory.listDirectory
