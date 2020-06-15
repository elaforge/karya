-- | Local build config.  This is re-exported by Local, courtesy of
-- tools/setup-*
module User.Elaforge.ShakeConfig where
import Shake.Config

localConfig = defaultConfig
    { enableEkg = False
    , enableEventLog = True
    , enableIm = True
    -- , extraDefines = ["-DHACKED_FLTK"]
    , fltkConfig = "/usr/local/src/fltk/fltk-config"
    }
