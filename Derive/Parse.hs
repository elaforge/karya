-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Tracklang parsers.  Many of the parsers in here should be inverses of
-- the 'ShowVal.ShowVal' class.
module Derive.Parse (
    commentRemove, comment
    , parseExpr
    , parseVal, parseAttrs, parseNum, parseCall
    , lex1, lex, splitPipeline, joinPipeline
    , unparsedCall
    , UnquotedString(..)

    -- * parsers
    , lexeme, pPipe, pExpr, pPcontrolRef, pIdentifier, pSymbol
    -- * expand macros
    , expandMacros
    -- * for Parse.Ky
    , pVal
    , pUnparsedExpr, pNullCall, pSingleQuoteString
    , pEqualGeneric
    , emptyLine, spaces
#ifdef TESTING
    , module Derive.Parse
#endif
) where
import           Prelude hiding (lex)
import qualified Control.Applicative as A (many)
import qualified Control.Applicative as Applicative
import qualified Data.Attoparsec.Text as A
import           Data.Attoparsec.Text ((<?>))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

import qualified System.IO.Unsafe as Unsafe

import qualified Util.Lists as Lists
import qualified Util.ParseA as ParseA

import qualified Derive.Attrs as Attrs
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Symbols as Symbols

import qualified Perform.Signal as Signal
import qualified Ui.Id as Id

import           Global


-- | The comment for tracklang is --, but if an event starts with this comment,
-- the whole event is ignored.  A normal comment would mean the event exists,
-- but has no expression.
commentRemove :: Text
commentRemove = "--|"

comment :: Text
comment = "--"

{- | Hacky memo table for 'parse_expr'.  There are many small exprs, and
    most of them are the the same, so we can save a lot of parsing time,
    and probably memory too since they will be interned.

    It would be more elegant and probably faster to put the memo directly in
    the Ui.Event, but this would introduce a giant dependency loop from
    low level Ui.Event up to DeriveT.Val.  At the least, it would go
    Ui.Event -> Events.Events -> Ui.State -> Ui.M -> Cmd.M, which means either
    a type parameter would have to propagate that far, or a SOURCE import would
    include that much.
-}
{-# NOINLINE memoTable #-}
memoTable :: IORef.IORef (HashMap Text (Either Text DeriveT.Expr))
memoTable = Unsafe.unsafePerformIO (IORef.newIORef mempty)

parseExpr :: Text -> Either Text DeriveT.Expr
parseExpr str
    -- micro-optimize, but probably irrelevant
    | Text.null str = Right $ Expr.call0 "" :| []
    | otherwise = case HashMap.lookup str table of
        Just expr -> expr
        Nothing -> write (HashMap.insert str expr) `seq` expr
            where expr = parseExprRaw str
    where
    -- With concurrent access this can duplicate work, but since it's a cache
    -- that should be fine.  Also I don't do a strict modify, it probably makes
    -- no difference but the theory is to spend minimal time adding the entry,
    -- and the next call will force the thunk.
    table = Unsafe.unsafePerformIO $ IORef.readIORef memoTable
    write = Unsafe.unsafePerformIO . IORef.modifyIORef memoTable

{-# SCC parseExprRaw #-}
parseExprRaw :: Text -> Either Text DeriveT.Expr
parseExprRaw = parse (pExpr True)

-- | Parse a single Val.
{-# SCC parseVal #-}
parseVal :: Text -> Either Text DeriveT.Val
parseVal = ParseA.parse1 (lexeme (pVal UnquotedLoose))

-- | Parse attributes in the form +a+b.
parseAttrs :: String -> Either Text Attrs.Attributes
parseAttrs = parse pAttributes . Text.pack

-- | Parse a number or hex code, without a type suffix.
parseNum :: Text -> Either Text Signal.Y
parseNum = ParseA.parse1 (lexeme (pHex <|> pUntypedNum))

-- | Extract only the call part of the text.
parseCall :: Text -> Maybe Text
parseCall text = case parseExprRaw text of
    Right expr -> case NonEmpty.last expr of
        Expr.Call (Expr.Symbol call) _ -> Just call
    _ -> Nothing

parse :: A.Parser a -> Text -> Either Text a
parse p = ParseA.parse1 (spaces >> p)

-- * lex

-- | Lex out a single expression.  This isn't really a traditional lex, because
-- it will extract a whole parenthesized expression instead of a token.  Also,
-- this leaves on trailing whitespace, so you can concatenate the lexed out
-- words and get the original input back.
lex1 :: Text -> (Text, Text)
lex1 text = case parse ((,) <$> pLex1 <*> A.takeWhile (const True)) text of
    Right ((), rest) ->
        (Text.take (Text.length text - Text.length rest) text, rest)
    Left _ -> (text, "")

-- | Like 'lex1', but get all of them.
lex :: Text -> [Text]
lex text
    | Text.null pre = []
    | Text.null post = [pre]
    | otherwise = pre : lex post
    where
    (pre, post) = lex1 text

-- | Take an expression and lex it into words, where each sublist corresponds
-- to one expression in the pipeline.  Like 'lex1', this corresponds to call
-- name and arguments, not tokens.  The final word could be a comment.
--
-- This preserves trailing spaces on the words, because track editors use that
-- to infer edits in progress.
splitPipeline :: Text -> [[Text]]
splitPipeline =
    Lists.mapTail (drop 1) . Lists.splitBefore ((=="|") . Text.strip) . lex

joinPipeline :: [[Text]] -> Text
joinPipeline =
    mconcat . List.intercalate [" | "] . map (Lists.mapLast Text.stripEnd)

-- | This returns () on success and the caller will see how many chars were
-- consumed.  Attoparsec doesn't keep track of byte position, and always
-- backtracks.  I think this means I can't reuse 'p_term'.
pLex1 :: A.Parser ()
pLex1 =
    (str <|> parens <|> (pEqualOperator *> pure ()) <|> unparsed <|> pComment
            <|> word)
        *> (A.skipWhile isWhitespace)
    where
    str = pSingleQuoteString >> pure ()
    parens = do
        A.char '('
        A.many $ parens <|> str <|> (A.takeWhile1 contentChar >> pure ())
        A.char ')'
        pure ()
    word = A.skipWhile (\c -> c /= '(' && isWordChar c)
    contentChar c = c /= '(' && c /= ')' && c /= '\''
    unparsed = A.string (Expr.unsym unparsedCall)
        *> A.skipWhile (\c -> c /= '|' && c /= ')')
    pComment = A.string comment *> A.skipWhile (const True)

-- * expand macros

-- | Map the identifiers after a \"\@\" through the given function.  Used
-- to implement ID macros for the REPL.
--
-- A macro looks like \@valid-id-chars.
expandMacros :: (Text -> Text) -> Text -> Either Text Text
expandMacros replacement text
    | not $ "@" `Text.isInfixOf` text = Right text
    | otherwise = ParseA.parse1 (pMacros replacement) text

pMacros :: (Text -> Text) -> A.Parser Text
pMacros replace = do
    chunks <- A.many1 $ pMacro replace <|> pChunk
        <|> (("\""<>) . (<>"\"") <$> pHsString)
    pure $ mconcat chunks
    where
    pChunk = A.takeWhile1 (\c -> c /= '"' && c /= '@')

pMacro :: (Text -> Text) -> A.Parser Text
pMacro replacement = do
    A.char '@'
    replacement <$> (unbackslash <$> bareString)
    where
    -- Strip escaped quotes, because 'show' will turn it back into haskell
    -- and re-add them.  This will mess up all the other zillion backslash
    -- features in haskell strings, but I probably won't use those.  'p_str'
    -- would be simpler, but since it's for the REPL, I feel like haskell-ish
    -- strings will be less error-prone.
    unbackslash = txt . strip . untxt
        where
        strip ('\\':c:cs) = c : strip cs
        strip (c:cs) = c : strip cs
        strip [] = []
    bareString = A.takeWhile1 (\c -> Id.isIdChar c || c == '/')

pHsString :: A.Parser Text
pHsString =
    ParseA.between (A.char '"') (A.char '"') $ mconcat <$> A.many chunk
    where
    chunk = (A.char '\\' >> Text.cons '\\' <$> A.take 1)
        <|> A.takeWhile1 (\c -> c /= '"' && c /= '\\')

-- * toplevel parsers

pExpr :: Bool -> A.Parser DeriveT.Expr
pExpr toplevel = do
    -- It definitely matches at least one, because p_null_call always matches.
    c : cs <- A.sepBy1 (pToplevelCall toplevel) pPipe
    pure $ c :| cs

-- | A toplevel call has a few special syntactic forms, other than the plain
-- @call arg arg ...@ form parsed by 'p_call'.
pToplevelCall :: Bool -> A.Parser DeriveT.Call
pToplevelCall toplevel =
    pUnparsedExpr <|> pEqual <|> pCall toplevel <|> pNullCall

-- | Parse a 'unparsed_call'.
pUnparsedExpr :: A.Parser DeriveT.Call
pUnparsedExpr = do
    A.string $ Expr.unsym unparsedCall
    text <- A.takeWhile $ \c -> c /= '|' && c /= ')'
    let arg = Expr.Str $ Text.strip $ stripComment text
    pure $ Expr.Call unparsedCall [Expr.Literal $ DeriveT.VStr arg]
    where
    -- Normally comments are considered whitespace by 'spaces_to_eol'.  Normal
    -- tokenization is suppressed for 'unparsed_call' so that doesn't happen,
    -- but I still want to allow comments, for consistency.
    stripComment = fst . Text.breakOn comment

-- | This is a magic call name that suppresses normal parsing.  Instead, the
-- rest of the event expression is passed as a string.  The only characters
-- that can't be used are ) and |, so an unparsed call can still be included in
-- a sub expression.
unparsedCall :: Expr.Symbol
unparsedCall = "!"

pPipe :: A.Parser ()
pPipe = void $ lexeme (A.char '|')

pEqual :: A.Parser (Expr.Call DeriveT.Val)
pEqual = do
    (lhs, sym, rhs) <- pEqualGeneric (lexeme pTerm)
    pure $ Expr.Call Symbols.equal $
        literal lhs : rhs ++ maybe [] (:[]) (literal <$> sym)
    where literal = Expr.Literal . DeriveT.VStr

pEqualGeneric :: A.Parser a -> A.Parser (Expr.Str, Maybe Expr.Str, [a])
pEqualGeneric rhsTerm = do
    lhs <- (Expr.unstr <$> pStr) <|> (Expr.unsym <$> pSymbol True)
    spaces
    mbSym <- pEqualOperator
    spaces
    rhs <- A.many1 rhsTerm
    pure (Expr.Str lhs, Expr.Str . Text.singleton <$> mbSym, rhs)

pEqualOperator :: A.Parser (Maybe Char)
pEqualOperator = A.char '=' *> optional (A.satisfy (A.inClass mergeSymbols))

-- | Valid symbols after =.  This should correspond to the keys in
-- Equal.symbol_to_merge.  It could have more symbols, but then that syntax
-- becomes unavailable.  E.g. previously % was in the list, but then
-- @x=%control@ has to be written @x = %control@.
mergeSymbols :: [Char]
mergeSymbols = "-+*@"

pCall :: Bool -> A.Parser (Expr.Call DeriveT.Val)
pCall toplevel = Expr.Call
    <$> lexeme (pSymbol toplevel)
    <*> lexeme (A.sepBy pTerm spaces1)

pNullCall :: A.Parser (Expr.Call a)
pNullCall = pure (Expr.Call "" []) <?> "null call"

-- | Any word in call position is considered a Str.  This means that
-- you can have calls like @4@ and @>@, which are useful names for notes or
-- ornaments.
pSymbol :: Bool -- ^ A call at the top level can allow a ).
    -> A.Parser Expr.Symbol
pSymbol toplevel = Expr.Symbol <$> pWord toplevel

pWord :: Bool -> A.Parser Text
pWord toplevel =
    A.takeWhile1 (if toplevel then isToplevelWordChar else isWordChar)

pTerm :: A.Parser (Expr.Term DeriveT.Val)
pTerm = Expr.Literal <$> pVal UnquotedLoose <|> Expr.ValCall <$> pSubCall
    <?> "term"

pSubCall :: A.Parser (Expr.Call DeriveT.Val)
pSubCall = ParseA.between (A.char '(') (A.char ')') (pCall False)

pVal :: UnquotedString -> A.Parser DeriveT.Val
pVal unquoted =
    DeriveT.VAttributes <$> pAttributes
    <|> DeriveT.num <$> pHex
    <|> DeriveT.VSignal . fmap Signal.constant <$> pNum
    <|> DeriveT.VStr <$> pStr
    <|> DeriveT.VControlRef <$> pControlRef
    <|> DeriveT.VPControlRef <$> pPcontrolRef
    <|> DeriveT.VQuoted <$> pQuoted
    <|> (A.char '_' >> pure DeriveT.VNotGiven)
    <|> (A.char ';' >> pure DeriveT.VSeparator)
    <|> DeriveT.VStr <$> pUnquotedStr unquoted

pNum :: A.Parser (ScoreT.Typed Signal.Y)
pNum = do
    num <- pUntypedNum
    let suffix (typ, suf) = A.string suf >> pure typ
    typ <- A.choice $ map suffix codes
    pure $ ScoreT.Typed typ num
    where
    codes = zip ScoreT.allTypes $ map ScoreT.typeToCode ScoreT.allTypes

pUntypedNum :: A.Parser Signal.Y
pUntypedNum = pRatio <|> ParseA.pFloat
    -- It may seem as if this should include p_hex, but I don't want it
    -- for p_num, because none of the type codes make sense for a 0-1
    -- normalized value.

pRatio :: A.Parser Signal.Y
pRatio = do
    sign <- A.option '+' (A.satisfy (\c -> c == '+' || c == '-'))
    num <- ParseA.pNat
    A.char '/'
    denom <- ParseA.pNat
    pure $ (if sign == '-' then -1 else 1)
        * fromIntegral num / fromIntegral denom

-- | Parse numbers of the form @`0x`00@ or @0x00@, with an optional @-@ prefix
-- for negation.
pHex :: A.Parser Signal.Y
pHex = do
    sign <- A.option 1 (A.char '-' >> pure (-1))
    A.string ShowVal.hexPrefix <|> A.string "0x"
    let higit c = '0' <= c && c <= '9' || 'a' <= c && c <= 'f'
    c1 <- A.satisfy higit
    c2 <- A.satisfy higit
    pure $ fromIntegral (parseHex c1 c2) / 0xff * sign

parseHex :: Char -> Char -> Int
parseHex c1 c2 = higit c1 * 16 + higit c2
    where
    higit c
        | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
        | otherwise = fromEnum c - fromEnum 'a' + 10

-- | A string is anything between single quotes.  A single quote itself is
-- represented by two single quotes in a row.
pStr :: A.Parser Expr.Str
pStr = Expr.Str <$> pSingleQuoteString

pSingleQuoteString :: A.Parser Text
pSingleQuoteString = do
    chunks <- A.many1 $
        ParseA.between (A.char '\'') (A.char '\'') (A.takeTill (=='\''))
    pure $ Text.intercalate "'" chunks

-- There's no particular reason to restrict attrs to idents, but this will
-- force some standardization on the names.
pAttributes :: A.Parser Attrs.Attributes
pAttributes = A.char '+'
    *> (Attrs.attrs <$> A.sepBy (pIdentifier False "+") (A.char '+'))

pControlRef :: A.Parser DeriveT.ControlRef
pControlRef = do
    A.char '%'
    control <- ScoreT.Control <$> A.option "" (pIdentifier False ",")
    deflt <- ParseA.optional (A.char ',' >> pNum)
    pure $ DeriveT.Ref control (fmap Signal.constant <$> deflt)
    <?> "control"

-- | Unlike 'p_control_ref', this doesn't parse a comma and a default value,
-- because pitches don't have literals.  Instead, use the @#@ val call.
pPcontrolRef :: A.Parser DeriveT.PControlRef
pPcontrolRef = DeriveT.Ref <$> pPcontrol <*> pure Nothing
    where
    pPcontrol = do
        A.char '#'
        ScoreT.PControl <$> pIdentifier True ""
        <?> "pitch control"

pQuoted :: A.Parser DeriveT.Quoted
pQuoted = ParseA.between "\"(" ")" (DeriveT.Quoted <$> pExpr False)

data UnquotedString = UnquotedStrict | UnquotedLoose deriving (Eq, Show)

-- | Symbols can have anything in them but they have to start with a letter.
-- This means special literals can start with wacky characters and not be
-- ambiguous.
--
-- This should be a superset of what 'p_identifier' will accept, so if IDs use
-- 'p_identifier' and 'Id.valid_symbol', they will also be parseable without
-- quotes.
pUnquotedStr :: UnquotedString -> A.Parser Expr.Str
pUnquotedStr unquoted = do
    sym <- Text.cons
        <$> A.satisfy ShowVal.isUnquotedHead
        <*> A.takeWhile (case unquoted of
            UnquotedLoose -> isWordChar
            UnquotedStrict -> \c -> range 'a' 'z' c || range 'A' 'Z' c
                || range '0' '9' c || c == '-' || c == '.')
    -- If I have an unbalanced quote, it may parse as an unquoted string with
    -- a quote in it, which is confusing.  So let's outlaw that.
    when ("'" `Text.isInfixOf` sym) $
        fail $ "quote in unquoted string: " <> show sym
    pure $ Expr.Str sym
    where
    range x y c = x <= c && c <= y

-- | Identifiers are somewhat more strict than usual.  They must be lowercase,
-- and the only non-letter allowed is hyphen.  This means words must be
-- separated with hyphens, and leaves me free to give special meanings to
-- underscores or caps if I want.
--
-- @until@ gives additional chars that stop parsing, for idents that are
-- embedded in another lexeme.
pIdentifier :: Bool -> String -> A.Parser Text
pIdentifier nullOk until = do
    -- TODO attoparsec docs say it's faster to do the check manually, profile
    -- and see if it makes a difference.
    ident <- (if nullOk then A.takeWhile else A.takeWhile1)
        -- A.notInClass was buggy in old versions of attoparsec, and got
        -- turned into just an IntSet.
        -- (A.notInClass (until ++ " \n\t|=)")) -- buggy?
        (\c -> not $ c `elem` until || c == ' ' || c == '\n' || c == '\t'
            || c == '|' || c == '=' || c == ')')
        -- Newlines and tabs are forbidden from track and block titles and
        -- events, but can occur in ky files.
    -- This forces identifiers to be separated with spaces, except with | and
    -- =.  Otherwise @sym>inst@ is parsed as a call @sym >inst@, which I don't
    -- want to support.
    unless ((nullOk && Text.null ident) || Id.validSymbol ident) $
        fail $ "invalid chars in identifier, expected "
            <> untxt Id.symbolDescription <> ": " <> show ident
    pure ident

-- | A word is as permissive as possible, and is terminated by whitespace.
-- That's because this determines how calls are allowed to be named, and for
-- expressiveness it's nice to use symbols.  For example, the slur call is just
-- @(@.
--
-- At the toplevel, any character is allowed except @=@, which lets me write
-- 'p_equal' expressions without spaces.  In sub calls, @)@ is not allowed,
-- because then I couldn't tell where the sub call expression ends, e.g. @())@.
-- However, @(()@ is fine, even though it looks weird.
--
-- I could get rid of the toplevel distinction by not allowing ) in calls
-- even at the toplevel, but I have @ly-(@ and @ly-)@ calls and I kind of like
-- how those look.  I guess it's a crummy justification, but no need to change
-- it unless toplevel gives more more trouble.
isToplevelWordChar :: Char -> Bool
isToplevelWordChar c = ShowVal.isUnquotedBody c
    && c /= ';' -- This is so the ; separator can appear anywhere.
    -- TODO remove it when I remove VSeparator

-- | Anything except whitespace, "=);"
isWordChar :: Char -> Bool
isWordChar c = isToplevelWordChar c && c /= ')'

lexeme :: A.Parser a -> A.Parser a
lexeme = (<* spaces)

-- | Skip spaces, including a newline as long as the next line, skipping empty
-- lines, is indented.
spaces :: A.Parser ()
spaces = do
    spacesToEol
    A.option () $ do
        A.skip (=='\n')
        A.skipMany emptyLine
        -- The next non-empty line has to be indented.
        A.skip isWhitespace
        A.skipWhile isWhitespace

-- | Like 'spaces', but require a space at the beginning.
spaces1 :: A.Parser ()
spaces1 = A.char ' ' *> spaces

emptyLine :: A.Parser ()
emptyLine = spacesToEol >> A.skip (=='\n')

spacesToEol :: A.Parser ()
spacesToEol = do
    A.skipWhile isWhitespace
    comment <- A.option "" (A.string comment)
    unless (Text.null comment) $
        A.skipWhile (\c -> c /= '\n')

isWhitespace :: Char -> Bool
isWhitespace c = c == ' ' || c == '\t'

optional :: Applicative.Alternative f => f a -> f (Maybe a)
optional = A.option Nothing . fmap Just
