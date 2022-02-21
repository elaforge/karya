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
    , libsamplerate = C.ExternalLibrary
        { C.libLink = ["/usr/local/src/libsamplerate/src/.libs/libsamplerate.a"]
        , C.libCompile = ["-I/usr/local/src/libsamplerate"]
        }
    -- TODO: normally this would come out of NIX_CFLAGS and NIX_LDFLAGS, but I
    -- haven't migrated to consistently using tools/nix-enter yet.

    , rubberband = C.ExternalLibrary
        { C.libLink = ["/nix/store/zmj1p295p8zaqzk86c39ppi0k2jygiqn-rubberband-2.0.0/lib/librubberband.a"]
        , C.libCompile = ["-I/nix/store/zmj1p295p8zaqzk86c39ppi0k2jygiqn-rubberband-2.0.0/include"]
        }

    -- TODO: only for the nix build, I should use the nix generated ShakeConfig
    -- , extraFrameworkPaths = ["/nix/store/rrxzrwa47dfxz7mm2wz873mkw4ryrigz-apple-framework-CoreFoundation/Library/Frameworks"]
    }
