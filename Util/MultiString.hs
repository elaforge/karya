-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Quasi-quoter for multi-line strings.
module Util.MultiString where
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as Quote


s :: Quote.QuasiQuoter
s = Quote.QuasiQuoter
    { quoteExp = return . TH.LitE . TH.StringL
    , quotePat = const $ fail "MultiString.s is for expressions only"
    , quoteType = const $ fail "MultiString.s is for expressions only"
    , quoteDec = const $ fail "MultiString.s is for expressions only"
    }
