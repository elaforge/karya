-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Various samples from Sonic Couture's Balinese gamelan.
module Synth.Sampler.Patch.ScGamelan (patches) where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Typeable as Typeable

import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified Text.Read as Read

import qualified Util.Lists as Lists
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Derive.Attrs as Attrs
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Code as Code
import qualified Synth.Sampler.Patch.Lib.Drum as Drum
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Signal as Signal

import           Global


patches :: [Patch.Patch]
patches = [gongPatch]
    -- CUtil.simple_drum CUtil.MidiThru Nothing gong_strokes (sc_patch "gong")
    -- : CUtil.simple_drum CUtil.MidiThru Nothing kempli_kajar_notes
    --     (sc_patch "kempli")
    -- : reyong_ks (ranged_patch Legong.reyong_range "reyong")
    -- : ranged_patch Legong.trompong_range "trompong"
    -- : concat
    -- [ pasang True (range_of Legong.jegog) "jegog"
    -- , pasang True (range_of Legong.calung) "calung"
    -- , pasang True (range_of Legong.penyacah) "penyacah"
    -- , tunggal False Legong.ugal_range "ugal"
    -- , pasang False (range_of Legong.pemade) "pemade"
    -- , pasang False (range_of Legong.kantilan) "kantilan"
    -- ]

gongPatch :: Patch.Patch
gongPatch = Drum.patch dir "sc-gong" gongStrokeMap gongConvertMap configOf
    where
    configOf (Just gong) = CUtil.call_config
        { CUtil._natural_nn = Just $ gongNn gong
        , CUtil._transform = Code.withVariation
        }
    -- Drum.strokeMapTable shouldn't put Nothings in there.
    configOf Nothing = error "no gong for attrs!?"
    dir = baseDir </> gongsDir

gongStrokeMap :: Drum.StrokeMap Gong
gongStrokeMap = Drum.strokeMapSimple []
    [ ('z', "O", gong <> wadon, GongWadon, open)
    , ('x', "o", gong <> lanang, GongLanang, open)
    , ('q', "p", kempur, Kempur, open)
    , ('w', "m", kemong, Kemong, open)
    ]
    where open = ""

gongConvertMap :: Drum.ConvertMap Gong
gongConvertMap = Drum.ConvertMap
    { dynRange = (0.9, 1.1)
    , naturalNn = Just gongNn
    , muteTime = Nothing
    , getFilename = gongFilename
    , allFilenames = gongAllFilenames
    }

{-
t0 = mapM_ Text.IO.putStrLn $ Texts.columns 2 $ map head $
    showDyns Kempur 0.05 0.5 1.5
t1 = mapM_ Text.IO.putStrLn $ Texts.columns 2 $ map head $
    showDyns2 Kempur 0.05 0.5 1.5

showDyns gong step minDyn maxDyn =
    map (\dyn -> map (normal dyn) $ gongVariations dyn gong) $
        Lists.rangeEnd 0 1 step
    where
    normal dyn (fname, (low, high)) =
        [ pp dyn, pp $ dynToVel dyn, txt fname
        , pp $ Num.scale minDyn maxDyn $ Num.normalize low high dyn
        ]

showDyns2 gong step minDyn maxDyn =
    map (\dyn -> map (normal dyn) $ gongVariations dyn gong) $
        Lists.rangeEnd 0 1 step
    where
    normal dyn (fname, (low, high)) =
        [ pp dyn, pp $ dynToVel dyn, txt fname
        , pp $ Util.dynamicAutoScale (minDyn, maxDyn) (low, high) dyn
        ]

pp :: Pretty a => a -> Text
pp = pretty
-}

gongFilename :: Gong -> Signal.Y -> Signal.Y
    -> (Maybe FilePath, Maybe (Signal.Y, Signal.Y))
gongFilename gong dyn var =
    case Util.pickVariation (gongVariations dyn gong) var of
        Nothing -> (Nothing, Nothing)
        Just (fname, range) -> (Just fname, Just range)

baseDir :: FilePath
baseDir = "sc-gamelan"

gongsDir :: FilePath
gongsDir = "gongs"

gong = Attrs.attr "gong"
kemong = Attrs.attr "kemong"
kempur = Attrs.attr "kempur"
wadon = Attrs.attr "wadon"
lanang = Attrs.attr "lanang"

gongVariations :: Signal.Y -> Gong -> [(FilePath, (Signal.Y, Signal.Y))]
gongVariations dyn = map toDyn . select . gongSamples
    where
    toDyn ((low, high), fname) =
        (fname, (Util.velToDyn low, Util.velToDyn high))
    select = takeWhile ((<=vel) . fst . fst) . dropWhile ((<vel). snd . fst)
    vel = Util.dynToVel dyn

data Gong = GongWadon | GongLanang | Kempur | Kemong
    deriving (Eq, Ord, Show, Typeable.Typeable, Enum, Bounded)

gongNn :: Gong -> Pitch.NoteNumber
gongNn = \case
    GongWadon -> NN.as1 + 0.25
    GongLanang -> NN.b1 + 0.75
    Kempur -> NN.gs2 + 0.24
    Kemong -> NN.as3 - 0.09

-- * make gongSamples

groupSamples :: [((String, Char, Int), FilePath)]
    -> [(Gong, [((Int, Int), FilePath)])]
groupSamples =
    map (fmap group) .  map (bimap parseGong Lists.groupFst)
        . Lists.groupFst . map shuffle
    where
    shuffle ((inst, var, maxVel), fname) = (inst, (var, (maxVel, fname)))
    group :: [(Char, [(Int, FilePath)])] -> [((Int, Int), FilePath)]
    group = Lists.sortOn (fst . fst) . concatMap (groupRanges . snd)

groupRanges :: [(Int, a)] -> [((Int, Int), a)]
groupRanges = go 0 . Lists.sortOn fst
    where
    go low ((high, a) : xs) = ((low, high), a) : go (high+1) xs
    go _ [] = []

parseGong :: String -> Gong
parseGong n = Map.findWithDefault (error $ "no gong: " <> show n) n toGong
    where
    toGong :: Map String Gong
    toGong = Map.fromList
        [ ("Gong Wadon", GongWadon)
        , ("Gong Lanang", GongLanang)
        , ("Kempur", Kempur)
        , ("Klentong", Kemong)
        ]

makeFileList :: FilePath -> IO [(Gong, [((Int, Int), FilePath)])]
makeFileList inst = do
    fnames <- Directory.listDirectory $
        Config.unsafeSamplerRoot </> baseDir </> inst
    return $ groupSamples $ Lists.keyOn parseFilename fnames

-- Gongs: $instName-{A,B,C}$maxVel.flac
parseFilename :: FilePath -> (String, Char, Int)
parseFilename fname = fromMaybe (error $ "no parse: " <> show fname) $
    case Lists.split "-" fname of
        [inst, rest] -> case Lists.split "." rest of
            [var : maxVel, "flac"] -> (inst, var,) <$> Read.readMaybe maxVel
            _ -> Nothing
        _ -> Nothing

-- | Generate 'gongSamples'.
_makeGongSamples :: IO ()
_makeGongSamples =
    mapM_ putStrLn . Drum.enumFunction "gongSamples" =<< makeFileList gongsDir

gongAllFilenames :: Set FilePath
gongAllFilenames = Util.assertLength 97 $ Set.fromList
    [fname | art <- Util.enumAll , (_, fname) <- gongSamples art]

-- * generated

gongSamples :: Gong -> [((Int, Int), FilePath)]
gongSamples = \case
    GongLanang ->
        [ ((0, 20), "Gong Lanang-A020.flac")
        , ((0, 20), "Gong Lanang-B020.flac")
        , ((0, 34), "Gong Lanang-C034.flac")
        , ((21, 34), "Gong Lanang-A034.flac")
        , ((21, 36), "Gong Lanang-B036.flac")
        , ((35, 50), "Gong Lanang-A050.flac")
        , ((35, 50), "Gong Lanang-C050.flac")
        , ((37, 52), "Gong Lanang-B052.flac")
        , ((51, 70), "Gong Lanang-A070.flac")
        , ((51, 71), "Gong Lanang-C071.flac")
        , ((53, 68), "Gong Lanang-B068.flac")
        , ((69, 89), "Gong Lanang-B089.flac")
        , ((71, 87), "Gong Lanang-A087.flac")
        , ((72, 88), "Gong Lanang-C088.flac")
        , ((88, 103), "Gong Lanang-A103.flac")
        , ((89, 102), "Gong Lanang-C102.flac")
        , ((90, 106), "Gong Lanang-B106.flac")
        , ((103, 117), "Gong Lanang-C117.flac")
        , ((104, 117), "Gong Lanang-A117.flac")
        , ((107, 118), "Gong Lanang-B118.flac")
        , ((118, 127), "Gong Lanang-A127.flac")
        , ((118, 127), "Gong Lanang-C127.flac")
        , ((119, 127), "Gong Lanang-B127.flac")
        ]
    GongWadon ->
        [ ((0, 26), "Gong Wadon-A026.flac")
        , ((0, 27), "Gong Wadon-B027.flac")
        , ((0, 26), "Gong Wadon-C026.flac")
        , ((27, 38), "Gong Wadon-A038.flac")
        , ((27, 38), "Gong Wadon-C038.flac")
        , ((28, 42), "Gong Wadon-B042.flac")
        , ((39, 47), "Gong Wadon-A047.flac")
        , ((39, 48), "Gong Wadon-C048.flac")
        , ((43, 55), "Gong Wadon-B055.flac")
        , ((48, 58), "Gong Wadon-A058.flac")
        , ((49, 56), "Gong Wadon-C056.flac")
        , ((56, 65), "Gong Wadon-B065.flac")
        , ((57, 67), "Gong Wadon-C067.flac")
        , ((59, 67), "Gong Wadon-A067.flac")
        , ((66, 77), "Gong Wadon-B077.flac")
        , ((68, 77), "Gong Wadon-A077.flac")
        , ((68, 77), "Gong Wadon-C077.flac")
        , ((78, 87), "Gong Wadon-A087.flac")
        , ((78, 86), "Gong Wadon-B086.flac")
        , ((78, 87), "Gong Wadon-C087.flac")
        , ((87, 96), "Gong Wadon-B096.flac")
        , ((88, 98), "Gong Wadon-A098.flac")
        , ((88, 97), "Gong Wadon-C097.flac")
        , ((97, 107), "Gong Wadon-B107.flac")
        , ((98, 107), "Gong Wadon-C107.flac")
        , ((99, 108), "Gong Wadon-A108.flac")
        , ((108, 115), "Gong Wadon-B115.flac")
        , ((108, 116), "Gong Wadon-C116.flac")
        , ((109, 117), "Gong Wadon-A117.flac")
        , ((116, 122), "Gong Wadon-B122.flac")
        , ((117, 122), "Gong Wadon-C122.flac")
        , ((118, 123), "Gong Wadon-A123.flac")
        , ((123, 127), "Gong Wadon-B127.flac")
        , ((123, 127), "Gong Wadon-C127.flac")
        , ((124, 127), "Gong Wadon-A127.flac")
        ]
    Kempur ->
        [ ((0, 36), "Kempur-A036.flac")
        , ((0, 36), "Kempur-B036.flac")
        , ((0, 36), "Kempur-C036.flac")
        , ((37, 55), "Kempur-A055.flac")
        , ((37, 54), "Kempur-B054.flac")
        , ((37, 58), "Kempur-C058.flac")
        , ((55, 68), "Kempur-B068.flac")
        , ((56, 72), "Kempur-A072.flac")
        , ((59, 72), "Kempur-C072.flac")
        , ((69, 81), "Kempur-B081.flac")
        , ((73, 89), "Kempur-A089.flac")
        , ((73, 82), "Kempur-C082.flac")
        , ((82, 94), "Kempur-B094.flac")
        , ((83, 94), "Kempur-C094.flac")
        , ((90, 106), "Kempur-A106.flac")
        , ((95, 106), "Kempur-B106.flac")
        , ((95, 108), "Kempur-C108.flac")
        , ((107, 119), "Kempur-A119.flac")
        , ((107, 118), "Kempur-B118.flac")
        , ((109, 119), "Kempur-C119.flac")
        , ((119, 127), "Kempur-B127.flac")
        , ((120, 127), "Kempur-A127.flac")
        , ((120, 127), "Kempur-C127.flac")
        ]
    Kemong ->
        [ ((0, 15), "Klentong-A015.flac")
        , ((0, 27), "Klentong-B027.flac")
        , ((0, 18), "Klentong-C018.flac")
        , ((16, 39), "Klentong-A039.flac")
        , ((19, 46), "Klentong-C046.flac")
        , ((28, 54), "Klentong-B054.flac")
        , ((40, 63), "Klentong-A063.flac")
        , ((47, 70), "Klentong-C070.flac")
        , ((55, 77), "Klentong-B077.flac")
        , ((64, 87), "Klentong-A087.flac")
        , ((71, 94), "Klentong-C094.flac")
        , ((78, 102), "Klentong-B102.flac")
        , ((88, 111), "Klentong-A111.flac")
        , ((95, 127), "Klentong-C127.flac")
        , ((103, 127), "Klentong-B127.flac")
        , ((112, 127), "Klentong-A127.flac")
        ]
