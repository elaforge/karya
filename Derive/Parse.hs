{- | Utilities to help parse event text.

I wound up not using parsec for call arg parsing, so this module is a little
weedy.
-}
module Derive.Parse where
import Control.Monad
import qualified Text.ParserCombinators.Parsec as Parsec

import qualified Util.Parse as Parse
import qualified Derive.Derive as Derive


-- | Parse the text of an event with the given parser @p@.
parse :: (Monad m) => Parsec.CharParser () a -> String -> Derive.DeriveT m a
parse parser text = case Parse.parse (Parse.p_rest parser) text of
    Left err -> Derive.throw err
    Right (val, rest) -> do
        unless (null rest) $
            Derive.warn $ "trailing junk: " ++ show rest
        return val
