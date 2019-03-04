-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This module tries to avoid confusing relative, absolute, and canonical
-- paths at the type level.  This is different from the hackage @paths@
-- package, because I have Relative to AppDir, and I distinguish Canonical.
--
-- I used to have a separate Absolute path, but the conversions get awkward.
module App.Path (
    AppDir(..), get_app_dir
    , Relative, relative, unrelative, (</>)
    , to_absolute, get_absolute
    -- * Canonical
    , Canonical, make_canonical, canonical, to_path
    , drop_prefix
) where
import qualified Data.String as String
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

import qualified Util.Seq as Seq


-- | All paths should be relative to this one.
-- I may later change this to an env var, a flag, or just leave it hardcoded.
get_app_dir :: IO AppDir
get_app_dir = AppDir <$> Directory.getCurrentDirectory

newtype AppDir = AppDir FilePath
    deriving (Eq, Show)

-- | Paths which are intended to be relative to the app dir get this type,
-- so it's harder to accidentally use them directly.
newtype Relative = Relative FilePath
    deriving (Eq, Show, String.IsString)

relative :: FilePath -> Relative
relative = Relative

-- | Normally you should use 'to_absolute', but sometimes I need the Realtive
-- path as a string.
unrelative :: Relative -> FilePath
unrelative (Relative path) = path

(</>) :: Relative -> Relative -> Relative
Relative a </> Relative b = Relative (a FilePath.</> b)

to_absolute :: AppDir -> Relative -> FilePath
to_absolute (AppDir app_dir) (Relative path) = app_dir FilePath.</> path

get_absolute :: Relative -> IO FilePath
get_absolute dir = to_absolute <$> get_app_dir <*> pure dir

-- * Canonical

-- | This is a path that is absolute and has had all the symlinks squeezed out.
-- The only reason I have this is that I want to strip the global save dir
-- prefix to get a shorter save file name.  Save filenames come from the user
-- and likely involve symlinks.
newtype Canonical = Canonical FilePath
    deriving (Eq, Show)

-- | For tests.
make_canonical :: FilePath -> Canonical
make_canonical = Canonical

canonical :: FilePath -> IO Canonical
canonical = fmap Canonical . Directory.canonicalizePath

to_path :: Canonical -> FilePath
to_path (Canonical path) = path

drop_prefix :: Canonical -> Canonical -> FilePath
drop_prefix (Canonical prefix) (Canonical path) =
    dropWhile (=='/') $ fst $ Seq.drop_prefix prefix path
