-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module App.Path (
    AppDir(..), get_app_dir
    , Relative, relative, unrelative, (</>)
    , absolute, get_absolute
) where
import qualified Data.String as String
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath


-- | All paths should be relative to this one.
-- I may later change this to an env var, a flag, or just leave it hardcoded.
get_app_dir :: IO AppDir
get_app_dir = AppDir <$> Directory.getCurrentDirectory

newtype AppDir = AppDir FilePath
    deriving (Eq, Show)

-- | Paths which are intended to be relative to the app dir get this type,
-- so it's harder to accidentally use them directly.
newtype Relative = Relative FilePath deriving (Eq, Show, String.IsString)

relative :: FilePath -> Relative
relative = Relative

-- | Normally you should use 'absolute', but sometimes I need the Realtive path
-- as a string.
unrelative :: Relative -> FilePath
unrelative (Relative path) = path

(</>) :: Relative -> Relative -> Relative
Relative a </> Relative b = Relative (a FilePath.</> b)

absolute :: AppDir -> Relative -> FilePath
absolute (AppDir app_dir) (Relative path) = app_dir FilePath.</> path

get_absolute :: Relative -> IO FilePath
get_absolute dir = absolute <$> get_app_dir <*> pure dir
