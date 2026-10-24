-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Support for testing the GUI.

    There is a function 'Ui.BlockC.dump', which emits a sexpr-like set of
    key-value pairs representing its current state.  Tests can then check this
    dump for certain expected values.

    Example input: @key1 val1 key2 (subkey1 subval1)@

    Flattened output: @[("key1", "val1"), ("key2.subkey1", "subval1")]@
-}
module Ui.Dump where
import qualified Control.Applicative as Applicative
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.ParseA as ParseA

import           Global


type Dump = [(String, String)]

newtype Tree = Tree [(String, Val)] deriving (Show)
data Val = Val String | Sub Tree deriving (Show)

parse :: String -> Either Text Dump
parse = fmap flatten . ParseA.parse1 pTree . Text.pack

flatten :: Tree -> Dump
flatten (Tree pairs) = concatMap (go []) pairs
    where
    go prefix (key, Val val) = [(flattenKey (key:prefix), val)]
    go prefix (key, Sub (Tree subs)) = concatMap (go (key:prefix)) subs
    flattenKey = Lists.join "." . reverse

pTree :: A.Parser Tree
pTree = Tree <$> Applicative.many pPair

pPair :: A.Parser (String, Val)
pPair = (,) <$> ParseA.lexeme pWord <*> ParseA.lexeme (pSub <|> pVal)

pSub :: A.Parser Val
pSub = Sub <$> ParseA.between (A.char '(') (A.char ')') pTree

pVal :: A.Parser Val
pVal = Val <$> pWord

pWord :: A.Parser String
pWord = Text.unpack <$> (pStr <|> A.takeWhile1 (`notElem` (" ()" :: [Char])))

pStr :: A.Parser Text
pStr = ParseA.between (A.char '"') (A.char '"')
        (mconcat <$> Applicative.many str)
    where
    str = do
        chunk <- A.takeWhile (\c -> c /= '"' && c /= '\\')
        quoted <- A.option "" (A.string "\\\"" <|> A.string "\\\\")
        let res = chunk <> Text.drop 1 quoted
        if Text.null res then Applicative.empty else pure res
