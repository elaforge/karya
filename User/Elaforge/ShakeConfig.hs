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
    -- TODO: normally this would come out of NIX_CFLAGS and NIX_LDFLAGS, but I
    -- haven't migrated to consistently using tools/nix-enter yet.
    , rubberband = C.ExternalLibrary
        { C.libLink = ["/nix/store/hkbzvc64rinnwhi2wa0f26485m5s384f-rubberband-1.8.2/lib/librubberband.a"]
        , C.libCompile = ["-I/nix/store/hkbzvc64rinnwhi2wa0f26485m5s384f-rubberband-1.8.2/include"]
        }
    }
