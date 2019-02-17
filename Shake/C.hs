-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Specify C targets and dependencies.
--
-- This is beginning of me trying to modularize build rules.
module Shake.C where
import qualified Development.Shake as Shake


type Flag = String

data ExternalLibrary = ExternalLibrary {
    -- | Add to the link line, e.g. ["-L/path/to/lib", "-lthing"] or
    -- ["/path/to/libthing.a"].
    libLink :: [Flag]
    -- | Add to the compile line, e.g. [-I/path/to/include].
    , libCompile :: [Flag]
    } deriving (Show)

library :: String -> ExternalLibrary
library name = ExternalLibrary
    { libLink = ["-l" <> name]
    , libCompile = []
    }

data Binary config = Binary {
    binName :: String
    -- | Object files required, relative to build/<mode>/obj.
    , binObjs :: [FilePath]
    , binCompile :: config -> [Flag]
    , binLink :: config -> [Flag]
    , binLibraries :: config -> [ExternalLibrary]
    -- | Run this after building, with a complete path to the binary.
    , binPostproc :: FilePath -> Shake.Action ()
    }
    -- TODO I'd much prefer to have all fields be plain data, not functions,
    -- but they sometimes need access to dynamic config, such as paths
    -- discovered at runtime.  I could just create the Binary from a config,
    -- but then all the fields are dependent on having the config.
    --
    -- A way around this would be to allow variables in Flag, like
    -- "-I${prefix}/include", which are then resolved at runtime.

{- | Describe a C++ binary target.  Unlike haskell binary targets, this has all
    the binary's obj file dependencies explicitly listed.  This is because
    C source files import separate include files, so I can't infer all the
    dependencies just by chasing imports, unless I want to assume that each
    name.h has a corresponding name.cc.  In any case, I have relatively little
    C++ and it changes rarely, so I don't mind a hardcoded list.  An explicit
    list of deps means I can also give compile flags per source file, instead
    of having a global list of flags that applies to all sources.
-}
binary :: String -> [FilePath] -> Binary config
binary name objs = Binary
    { binName = name
    , binObjs = objs
    , binCompile = const []
    , binLink = const []
    , binLibraries = const []
    , binPostproc = const $ return ()
    }

binCompileFlags :: Binary config -> config -> [Flag]
binCompileFlags bin config =
    binCompile bin config ++ concatMap libCompile (binLibraries bin config)

binLinkFlags :: Binary config -> config -> [Flag]
binLinkFlags bin config =
    binLink bin config ++ concatMap libLink (binLibraries bin config)
