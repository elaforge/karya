-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Calibrate where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.Audio.Resample as Resample
import qualified Util.Seq as Seq

import qualified Derive.Attrs as Attrs
import qualified Perform.RealTime as RealTime
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.RenderSample as RenderSample
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global
import           Synth.Types


type Axis = Text


select :: Eq b => [(Axis, b)] -> [(a, Map Axis b)] -> [(a, Map Axis b)]
select tags = filter (hasTags . snd)
    where
    hasTags m = all (\(k, v) -> Map.lookup k m == Just v) tags

orderBy :: Ord b => [Axis] -> [(a, Map Axis b)] -> [a]
orderBy axes samples = map snd $ List.sortOn fst
    [ (map (`Map.lookup` tags) axes, val)
    | (val, tags) <- samples
    ]

pitch, art, dyn, tuning, var :: Axis
pitch = "pitch"
art = "art"
dyn = "dyn"
tuning = "tuning"
var = "var"

-- * render

-- create notes with an even dyn spread

data By = Attr | Pitch | Dyn
    deriving (Show, Read)

sequence :: By -> Note.PatchName -> RealTime -> [Attrs.Attributes]
    -> [Note.Element] -> Signal.Y -> Signal.Y -> [Note.Note]
sequence by patch dur attrs pitches variations dynamics =
    zipWith setStart (Seq.range_ 0 dur) $ case by of
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
            , var <- vars
            , dyn <- dyns
            , pitch <- pitches
            ]
    where
    vars = Seq.range 0 1 (1 / (variations-1))
    dyns = Seq.range 0 1 (1 / (dynamics-1))
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

renderSequence :: FilePath -> RealTime -> [Sample.SamplePath] -> IO ()
renderSequence outDir dur fnames = do
    renderDirect (outDir </> "out.wav") Nothing samples
    where
    samples = zip (Seq.range_ 0 dur) (map makeSample fnames)
    makeSample fname = (Sample.make fname)
        { Sample.envelope =
            Signal.from_pairs [(0, 1), (dur - decay, 1), (dur, 0)]
        }
    decay = 0.15

renderStarts :: FilePath -> [Sample.Sample] -> IO ()
renderStarts outDir samples = do
    putStrLn $ "==> " <> filename
    exist <- mapM (Directory.doesFileExist . (patchDir</>) . Sample.filename)
        samples
    if all id exist
        then renderDirect filename (Just 1) $
            map ((0,) . Sample.modifyFilename (patchDir</>)) samples
        else putStrLn "*** missing"
    where
    filename = outDir </> replace '/' '-'
            (FilePath.dropExtension (Sample.filename (head samples)))
            ++ ".wav"
    patchDir = "../data/sampler/wayang"
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
