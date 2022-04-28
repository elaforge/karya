-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is a re-export module for parsing.
module Util.P (
    module Util.P
    , module Text.Megaparsec, module Text.Megaparsec.Char
) where
import           Prelude hiding (takeWhile)
import           Control.Monad (void)
import           Data.Text (Text)
import qualified Data.Void as Void

import           Text.Megaparsec
import           Text.Megaparsec.Char


type Parser a = Parsec Void.Void Text a

noneOfC :: Ord e => [Char] -> ParsecT e Text m Char
noneOfC cs = satisfy (`notElem` cs)

oneOfC :: Ord e => [Char] -> ParsecT e Text m Char
oneOfC cs = satisfy (`elem` cs)

takeWhile :: MonadParsec e s m => (Token s -> Bool) -> m (Tokens s)
takeWhile = takeWhileP Nothing

skipWhile :: MonadParsec e s m => (Token s -> Bool) -> m ()
skipWhile = void . takeWhileP Nothing

takeWhile1 :: MonadParsec e s m => (Token s -> Bool) -> m (Tokens s)
takeWhile1 = takeWhile1P Nothing

skipWhile1 :: MonadParsec e s m => (Token s -> Bool) -> m ()
skipWhile1 = void . takeWhile1P Nothing
