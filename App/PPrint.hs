-- | Standalone pretty printer for debugging.
import qualified Util.PPrint as PPrint

main :: IO ()
main = PPrint.pprints =<< getContents
