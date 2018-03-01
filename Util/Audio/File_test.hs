module Util.Audio.File_test where
import qualified Control.Monad.Trans.Resource as Resource

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.Audio.Resample as Resample


copy = do
    copy "in.wav" "out.wav"

resample out = Resource.runResourceT $ write out $
    Resample.resample Resample.SincBestQuality 2 $
    File.read44k "g1.wav"

mix out = Resource.runResourceT $ write out $ Audio.mix
    [ (0, File.read44k "g1.wav")
    , (11000, File.read44k "g1.wav")
    ]

write = File.write File.wavFormat

copy :: FilePath -> FilePath -> IO ()
copy input output = Resource.runResourceT $
    File.write File.wavFormat output $ File.read44k input
