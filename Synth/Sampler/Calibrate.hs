-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Calibrate where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Map as Map
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.Audio.Resample as Resample
import qualified Util.Lists as Lists

import qualified Derive.Attrs as Attrs
import qualified Perform.RealTime as RealTime
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Sampler.RenderSample as RenderSample
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global
import           Synth.Types


type Axis = Text

pitch, art, dyn, tuning, var :: Axis
pitch = "pitch"
art = "art"
dyn = "dyn"
tuning = "tuning"
var = "var"

-- * render

-- create notes with an even dyn spread

data By = Attr | Pitch | Dyn | Var
    deriving (Show, Read)

sequence :: By -> Note.PatchName -> RealTime -> [Attrs.Attributes]
    -> [Note.Element] -> Signal.Y -> Signal.Y -> [Note.Note]
sequence by patch dur attrs pitches variations dynamics =
    zipWith setStart (Lists.range_ 0 dur) $ case by of
        -- Basic order is [attr, pitch, dyn, var] but the comparison axis
        -- goes to the end.  The order is in perceptual "size" but is somewhat
        -- arbitrary.
        Attr ->
            [ make pitch dyn var attr
            | pitch <- pitches
            , var <- vars
            , dyn <- dyns
            , attr <- attrs
            ]
        Dyn ->
            [ make pitch dyn var attr
            | attr <- attrs
            , pitch <- pitches
            , var <- vars
            , dyn <- dyns
            ]
        Pitch ->
            [ make pitch dyn var attr
            | attr <- attrs
            , dyn <- dyns
            , var <- vars
            , pitch <- pitches
            ]
        Var ->
            [ make pitch dyn var attr
            | attr <- attrs
            , pitch <- pitches
            , dyn <- dyns
            , var <- vars
            ]
    where
    vars = Lists.range 0 1 (1 / (variations-1))
    dyns = Lists.range 0 1 (1 / (dynamics-1))
    setStart start note = note { Note.start = start }
    make element dyn var attr = (Note.note patch "inst" 0 dur)
        { Note.element = element
        , Note.attributes = attr
        , Note.duration = dur
        , Note.controls = Map.fromList
            [ (Control.dynamic, Signal.constant dyn)
            , (Control.variation, Signal.constant var)
            ]
        }

renderSequence :: FilePath -> RealTime -> [(Sample.SamplePath, Util.Dyn)]
    -> IO ()
renderSequence outWav dur fnameDyns = renderDirect outWav Nothing samples
    where
    samples = zip (Lists.range_ 0 dur) (map makeSample fnameDyns)
    makeSample (fname, dyn) = (Sample.make fname)
        { Sample.envelope =
            Signal.from_pairs [(0, dyn), (dur - decay, dyn), (dur, 0)]
        }
    decay = 0.15

renderStarts :: FilePath -> FilePath -> [Sample.Sample] -> IO ()
renderStarts sampleDir outDir samples = do
    putStrLn $ "==> " <> filename
    exist <- mapM (Directory.doesFileExist . (sampleDir</>) . Sample.filename)
        samples
    if all id exist
        then renderDirect filename (Just 1) $
            map ((0,) . Sample.modifyFilename (sampleDir</>)) samples
        else putStrLn "*** missing"
    where
    filename = outDir </> replace '/' '-'
        (FilePath.dropExtension (Sample.filename (head samples))) ++ ".wav"
    replace a b = map (\c -> if c == a then b else c)

renderDirect :: FilePath -> Maybe Audio.Seconds -> [(RealTime, Sample.Sample)]
    -> IO ()
renderDirect filename dur samples = do
    audios <- mapM render samples
    Resource.runResourceT $
        Audio.File.write AUtil.outputFormat filename $
        maybe id Audio.takeS dur $ Audio.mix audios
    where
    render (offset, sample) =
        (Audio.takeS (RealTime.to_seconds offset) Audio.silence <>) <$>
            RenderSample.render config 0 sample
    config = Resample.Config
        { _quality = Resample.SincFastest
        , _state = Nothing
        , _notifyState = const $ return ()
        , _blockSize = Config.chunkSize
        , _now = 0
        , _name = filename
        }
