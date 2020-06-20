-- | Local build config.  This is re-exported by Local, courtesy of
-- tools/setup-*
module User.Elaforge.ShakeConfig where
import qualified Shake.C as C

import           Shake.Config


localConfig :: Config
localConfig = defaultConfig
    { enableEkg = False
    , enableEventLog = True
    , enableIm = True
    -- , extraDefines = ["-DHACKED_FLTK"]
    , fltkConfig = "/usr/local/src/fltk/fltk-config"
    , rubberband = C.ExternalLibrary
        { C.libLink = ["/nix/store/c9yl3lash921jkfg0n4nr9k5v7s4k30h-rubberband-1.8.2/lib/librubberband.a"]
        , C.libCompile = ["-I/nix/store/c9yl3lash921jkfg0n4nr9k5v7s4k30h-rubberband-1.8.2/include"]
        }
    }
