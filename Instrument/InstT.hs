-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Types for "Derive.Inst", split apart to reduce dependencies.
module Instrument.InstT where
import qualified Data.Text as Text

import qualified Util.Serialize as Serialize
import Global


-- | This is an instrument name qualified by synth name.  It should uniquely
-- address a single instrument.  It's different from
-- a 'Derive.ScoreTypes.Instrument', which addresses a particular instantiation
-- of an instrument in a particular score.
data Qualified = Qualified SynthName Name deriving (Show, Eq, Ord)

synth :: Qualified -> SynthName
synth (Qualified synth _) = synth

name :: Qualified -> Name
name (Qualified _ name) = name

instance Pretty Qualified where pretty = show_qualified

instance Serialize.Serialize Qualified where
    put (Qualified a b) = Serialize.put a >> Serialize.put b
    get = Qualified <$> Serialize.get <*> Serialize.get

-- | Short but unabbreviated lowercase name with no spaces.  It should
-- follow 'Ui.Id.valid_symbol'.
type SynthName = Text

-- | A name uniquely addresses this instrument within a synth.  It should also
-- follow 'Ui.Id.valid_symbol'.
type Name = Text

parse_qualified :: Text -> Qualified
parse_qualified text = Qualified pre (Text.drop 1 post)
    where (pre, post) = Text.break (=='/') text

show_qualified :: Qualified -> Text
show_qualified (Qualified synth name) = synth <> "/" <> name
