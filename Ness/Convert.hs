module Ness.Convert where
import qualified Data.Map as Map
import qualified System.FilePath as FilePath

import qualified Util.PPrint as PPrint
import qualified Util.Seq as Seq
import qualified Synth.Shared.Note as Note
import Global
import Ness.Global
import qualified Ness.Guitar as Guitar
import qualified Ness.Guitar.Convert as Guitar.Convert
import qualified Ness.Instruments as Instruments
import Ness.Instruments (Instrument(..), Performance(..))
import qualified Ness.Multiplate as Multiplate
import qualified Ness.Multiplate.Convert as Multiplate.Convert
import qualified Ness.Util as Util


srate :: SamplingRate
srate = 11000

blockFile :: String -> FilePath
blockFile b = "im/ness-notes/ness-" ++ b

printPerformance :: String -> IO ()
printPerformance block =
    mapM_ scoreOf =<< either errorIO return =<< loadConvert block
    where
    scoreOf (Guitar _ s) = PPrint.pprint s
    scoreOf (Multiplate _ s) = PPrint.pprint s

run :: String -> IO ()
run block = do
    performances <- either errorIO return =<< loadConvert block
    Util.submitInstruments "convert" (FilePath.takeFileName (blockFile block))
        (map nameScore performances)
    where
    nameScore p =
        (untxt $ Instruments.performanceName p, renderPerformance srate p)


-- * implementation

type Error = Text

loadConvert :: String -> IO (Either Error [Performance])
loadConvert b = convert <$> load (blockFile b)

renderPerformance :: SamplingRate -> Performance -> (Text, Text)
renderPerformance sr (Guitar i s) = Guitar.renderAll sr (i, s)
renderPerformance sr (Multiplate i s) = Multiplate.renderAll sr (i, s)

convert :: [Note.Note] -> Either Error [Performance]
convert notes = do
    insts <- forM notes $ \n -> tryJust ("no patch: " <> pretty n) $
        Map.lookup (Note.patch n) Instruments.instruments
    concatMapM (uncurry convertBackend) $
        Seq.keyed_group_sort fst (zip insts notes)

convertBackend :: Instrument -> [(Instrument, Note.Note)]
    -> Either Error [Performance]
convertBackend (IGuitar _) =
    fmap (map (uncurry Guitar)) . Guitar.Convert.convert . extract
    where extract notes = [(inst, note) | (IGuitar inst, note) <- notes]
convertBackend (IMultiplate _) =
    fmap (map (uncurry Multiplate)) . Multiplate.Convert.convert . extract
    where
    extract notes = [(inst, note) | (IMultiplate inst, note) <- notes]

load :: FilePath -> IO [Note.Note]
load fname = either (errorIO . pretty) return =<< Note.unserialize fname
