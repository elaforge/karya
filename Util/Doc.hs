-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for the 'Doc' type.
module Util.Doc (
    Doc(..), pretty, literal, commas
) where
import qualified Data.String as String
import           Data.Text (Text)

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import qualified Util.Texts as Texts


-- | This is for documentation text.  It can contain some simple markdown-like
-- formatting, which may be either be printed directly, or formatted via
-- 'html_doc'.
newtype Doc = Doc Text
    deriving (Eq, Ord, Show, Pretty.Pretty, Semigroup, Monoid, String.IsString,
        Serialize.Serialize)

instance Texts.Textlike Doc where
    toText (Doc t) = t
    fromText = Doc

-- | This probably doesn't belong here, but it's useful in the same contexts as
-- 'Doc'.
pretty :: Pretty.Pretty a => a -> Doc
pretty = literal . Pretty.pretty

literal :: Text -> Doc
literal text = Doc $ "`" <> text <> "`"

commas :: [Doc] -> Doc
commas = Texts.join ", "
