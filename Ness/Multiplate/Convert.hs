module Ness.Multiplate.Convert where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.FilePath as FilePath

import qualified Util.PPrint as PPrint
import qualified Util.Seq as Seq
import qualified Perform.RealTime as RealTime
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import Global
import Ness.Global
import qualified Ness.Multiplate as Multiplate
import qualified Ness.Multiplate.Patch as Patch
import qualified Ness.Util as Util


srate :: SamplingRate
srate = 11000

run :: String -> IO ()
run block = do
    scores <- either errorIO return =<< loadConvert block
    Util.submitInstruments Multiplate.renderAll srate "multiplate"
        (FilePath.takeFileName (blockFile block))
        [(untxt $ Multiplate.iName i, (i, s)) | (i, s) <- scores]

-- | Scale dynamic=1 to this.
maxAmp :: Newtons
maxAmp = 1000


-- * implementation

convert :: [Note.Note]
    -> Either Error [(Multiplate.Instrument, Multiplate.Score)]
convert notes = do
    let getInstrument n = tryJust ("no patch: " <> pretty n) $
            Map.lookup (Note.patch n) Patch.patches
    insts <- mapM getInstrument notes
    strikes <- mapM (uncurry convertNote) (zip insts notes)
    let instScores = map (second makeScore) $ Seq.group_fst (zip insts strikes)
    let verifyErrors = do
            (i, s) <- instScores
            let errors = Multiplate.verify i s
            guard $ not (null errors)
            return $ Multiplate.iName i <> ": " <> Text.intercalate "; " errors
    unless (null verifyErrors) $
        Left $ Text.unlines verifyErrors
    return instScores

makeScore :: [Multiplate.Strike] -> Multiplate.Score
makeScore strikes = Multiplate.Score
    { sDecay = 6
    , sStrikes = strikes
    }

convertNote :: Multiplate.Instrument -> Note.Note
    -> Either Error Multiplate.Strike
convertNote inst note = first ((pretty note <> ": ")<>) $ do
    let object = Note.element note
    unless (object `elem` map Multiplate.pName (Multiplate.iPlates inst)) $
        Left $ "no object: " <> object
    let get c = tryJust ("no " <> pretty c) $ Note.initialControl c note
    dyn <- get Control.dynamic
    (x, y) <- (,) <$> get Patch.c_x <*> get Patch.c_y
    dur <- get Patch.c_duration
    return $ Multiplate.Strike
        { sObject = object
        , sStart = RealTime.to_seconds $ Note.start note
        , sDuration = dur
        , sPosition = (x, y)
        , sForce = dyn * maxAmp
        }

-- * util

type Error = Text

loadConvert :: String
    -> IO (Either Error [(Multiplate.Instrument, Multiplate.Score)])
loadConvert b = convert <$> load (blockFile b)

load :: FilePath -> IO [Note.Note]
load fname = either (errorIO . pretty) return =<< Note.unserialize fname

blockFile :: String -> FilePath
blockFile b = "im/ness-notes/ness-" ++ b

printScore :: String -> IO ()
printScore block = mapM_ (PPrint.pprint . snd)
    =<< either errorIO return =<< loadConvert block
