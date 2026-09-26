-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Parse.Record where
import qualified Data.Char as Char
import qualified Data.Text as Text

import qualified Util.P as P
import qualified Util.Parse
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Parse as Parse
import qualified Derive.ShowVal as ShowVal

import           Global


type Parser a = P.Parser a

data RVal = Record Record | Val DeriveT.Val
    deriving (Show)

type Record = [(Text, RVal)]

list :: [DeriveT.Val] -> RVal
list = Val . DeriveT.VList

pRval :: Parser RVal
pRval = Record <$> pRecord
    <|> Val . DeriveT.VList <$> pList
    <|> Val <$> pVal

pVal :: Parser DeriveT.Val
pVal = Util.Parse.attoparse "val" (Parse.pVal Parse.UnquotedStrict)

type Indent = Int

unRval :: RVal -> [Text]
unRval = \case
    Record rec -> unRecord rec
    Val (DeriveT.VList vals) ->
        ["[" <> Text.intercalate ", " (map ShowVal.showVal vals) <> "]"]
    Val a -> [ShowVal.showVal a]

unRecord :: Record -> [Text]
unRecord = \case
    [] -> ["{}"]
    [(k, Val v)] ->
        [mconcat $ ["{", k, ": "] ++ unRval (Val v) ++ ["}"]]
    (k, v) : fields ->
        unField "{" (k, v) ++ concatMap (unField ",") fields ++ ["}"]
    where
    -- Some shenanigans to unwrap single val or single element records.
    -- It's probably inconsistent and broken, but only needs to work
    -- in one specific case.
    unField delim (k, v) = case v of
        Val v -> [mconcat $ key ++ [" "] ++ unRval (Val v)]
        Record [(k2, v2)] -> [mconcat $ key ++ [" "] ++ unRecord [(k2, v2)]]
        Record fields -> mconcat key : map (indent <>) (unRecord fields)
        where key = [delim, " ", k, ":"]
    indent = "    "

-- TODO use above if ShowVal is inconsistent?
unSingleQuoteString :: Text -> Text
unSingleQuoteString t = "'" <> Text.replace "'" "''" t <> "'"

pRecord :: Parser Record
pRecord =
    between '{' '}' $ P.sepBy (lexeme pEntry) (lexeme (P.char ','))
    where
    pEntry = (,) <$> (lexeme pWord <* lexeme (P.char ':')) <*> pRval

pWord :: Parser Text
pWord = P.takeWhile1 $ \c -> any ($c)
    [ Char.isAsciiLower, Char.isAsciiUpper, Char.isDigit
    , (`elem` ("-_" :: [Char]))
    ]

pList :: Parser [DeriveT.Val]
pList = between '[' ']' $ P.sepBy (lexeme pVal) (lexeme (P.char ','))

-- * util

between :: Char -> Char -> Parser a -> Parser a
between pre post = P.between (lexeme (P.char pre)) (lexeme (P.char post))

-- * from Derive.Parse

lexeme :: Parser a -> Parser a
lexeme = (<* spaces)

spaces :: Parser ()
spaces = P.skipMany $
    ("--" *> P.skipWhile (/='\n') *> P.skipWhile (=='\n'))
    <|> P.skipSome (P.satisfy $ \c -> c == ' ' || c == '\n')
