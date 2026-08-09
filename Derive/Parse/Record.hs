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

p_rval :: Parser RVal
p_rval = Record <$> p_record
    <|> Val . DeriveT.VList <$> p_list
    <|> Val <$> p_val

p_val :: Parser DeriveT.Val
p_val = Util.Parse.attoparse "val" (Parse.p_val Parse.UnquotedStrict)

type Indent = Int

un_rval :: RVal -> [Text]
un_rval = \case
    Record rec -> un_record rec
    Val (DeriveT.VList vals) ->
        ["[" <> Text.intercalate ", " (map ShowVal.show_val vals) <> "]"]
    Val a -> [ShowVal.show_val a]

un_record :: Record -> [Text]
un_record = \case
    [] -> ["{}"]
    [(k, Val v)] ->
        [mconcat $ ["{", k, ": "] ++ un_rval (Val v) ++ ["}"]]
    (k, v) : fields ->
        un_field "{" (k, v) ++ concatMap (un_field ",") fields ++ ["}"]
    where
    -- Some shenanigans to unwrap single val or single element records.
    -- It's probably inconsistent and broken, but only needs to work
    -- in one specific case.
    un_field delim (k, v) = case v of
        Val v -> [mconcat $ key ++ [" "] ++ un_rval (Val v)]
        Record [(k2, v2)] -> [mconcat $ key ++ [" "] ++ un_record [(k2, v2)]]
        Record fields -> mconcat key : map (indent <>) (un_record fields)
        where key = [delim, " ", k, ":"]
    indent = "    "

-- TODO use above if ShowVal is inconsistent?
un_single_quote_string :: Text -> Text
un_single_quote_string t = "'" <> Text.replace "'" "''" t <> "'"

p_record :: Parser Record
p_record =
    between '{' '}' $ P.sepBy (lexeme p_entry) (lexeme (P.char ','))
    where
    p_entry = (,) <$> (lexeme p_word <* lexeme (P.char ':')) <*> p_rval

p_word :: Parser Text
p_word = P.takeWhile1 $ \c -> any ($c)
    [ Char.isAsciiLower, Char.isAsciiUpper, Char.isDigit
    , (`elem` ("-_" :: [Char]))
    ]

p_list :: Parser [DeriveT.Val]
p_list = between '[' ']' $ P.sepBy (lexeme p_val) (lexeme (P.char ','))

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
