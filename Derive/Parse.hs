-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | TrackLang parsers using Text and Attoparsec.
module Derive.Parse (
    parse_expr, parse_control_title
    , parse_val, parse_attrs, parse_num, parse_call
    , lex1, lex, split_pipeline, join_pipeline

    -- * expand macros
    , expand_macros
    -- * ky file
    , Definitions(..), Definition
    , load_ky, find_mtime, parse_ky
#ifdef TESTING
    , p_equal, p_definition
    , split_sections
#endif
) where
import Prelude hiding (lex)
import qualified Control.Applicative as A (many)
import qualified Control.Monad.Error as Error
import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Text ((<?>))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time as Time
import qualified Data.Traversable as Traversable

import qualified System.Directory as Directory
import System.FilePath ((</>))

import qualified Util.File as File
import qualified Util.ParseText as ParseText
import qualified Util.Seq as Seq

import qualified Ui.Id as Id
import qualified Derive.Score as Score
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal
import Global


parse_expr :: Text -> Either Text TrackLang.Expr
parse_expr = parse (p_pipeline True)

-- | Parse a control track title.  The first expression in the composition is
-- parsed simply as a list of values, not a Call.  Control track titles don't
-- follow the normal calling process but pattern match directly on vals.
parse_control_title :: Text
    -> Either Text ([TrackLang.Val], [TrackLang.Call])
parse_control_title = ParseText.parse p_control_title

-- | Parse a single Val.
parse_val :: Text -> Either Text TrackLang.Val
parse_val = ParseText.parse (lexeme p_val)

-- | Parse attributes in the form +a+b.
parse_attrs :: String -> Either Text Score.Attributes
parse_attrs = parse p_attributes . Text.pack

-- | Parse a number or hex code, without a type suffix.
parse_num :: Text -> Either Text Signal.Y
parse_num = ParseText.parse (lexeme (p_hex <|> p_untyped_num))

-- | Extract only the call part of the text.
parse_call :: Text -> Maybe Text
parse_call text = case parse_expr text of
    Right expr -> case NonEmpty.last expr of
        TrackLang.Call (TrackLang.Symbol call) _ -> Just call
    _ -> Nothing

parse :: A.Parser a -> Text -> Either Text a
parse p = ParseText.parse (spaces >> p)

-- * lex

-- | Lex out a single expression.  This isn't really a traditional lex, because
-- it will extract a whole parenthesized expression instead of a token.
lex1 :: Text -> (Text, Text)
lex1 text = case parse ((,) <$> p_lex1 <*> A.takeWhile (const True)) text of
    Right ((), rest) ->
        (Text.take (Text.length text - Text.length rest) text, rest)
    Left _ -> (text, "")

-- | Like 'lex1', but get all of them.
lex :: Text -> [Text]
lex text
    | Text.null pre = []
    | Text.null post = [Text.stripEnd pre]
    | otherwise = Text.stripEnd pre : lex post
    where
    (pre, post) = lex1 text

-- | Take an expression and lex it into words, where each sublist corresponds
-- to one expression in the pipeline.
split_pipeline :: Text -> [[Text]]
split_pipeline = Seq.split_null ["|"] . lex

join_pipeline :: [[Text]] -> Text
join_pipeline = mconcat . List.intercalate [" | "]

-- | Attoparsec doesn't keep track of byte position, and always backtracks.
-- I think this means I can't reuse 'p_term'.
p_lex1 :: A.Parser ()
p_lex1 = (str <|> parens <|> word) >> spaces
    where
    str = p_single_quote_string >> return ()
    parens = do
        A.char '('
        A.many $ parens <|> str <|> (A.takeWhile1 content_char >> return ())
        A.char ')'
        return ()
    word = A.skipWhile (\c -> c /= '(' && is_word_char c)
    content_char c = c /= '(' && c /= ')' && c /= '\''

-- * expand macros

-- | Map the identifiers after a \"\@\" through the given function.  Used
-- to implement ID macros for the REPL.
expand_macros :: (Text -> Text) -> Text -> Either Text Text
expand_macros replacement text
    | not $ "@" `Text.isInfixOf` text = Right text
    | otherwise = ParseText.parse (p_macros replacement) text

p_macros :: (Text -> Text) -> A.Parser Text
p_macros replace = do
    chunks <- A.many1 $ p_macro replace <|> p_chunk <|> p_hs_string
    return $ mconcat chunks
    where
    p_chunk = A.takeWhile1 (\c -> c /= '"' && c /= '@')

p_macro :: (Text -> Text) -> A.Parser Text
p_macro replacement = do
    A.char '@'
    replacement <$> A.takeWhile1 (\c -> Id.is_id_char c || c == '/')

p_hs_string :: A.Parser Text
p_hs_string = fmap (\s -> "\"" <> s <> "\"") $
    ParseText.between (A.char '"') (A.char '"') $ mconcat <$> A.many chunk
    where
    chunk = (A.char '\\' >> Text.cons '\\' <$> A.take 1)
        <|> A.takeWhile1 (\c -> c /= '"' && c /= '\\')

-- * toplevel parsers

-- | See 'parse_control_title'.
p_control_title :: A.Parser ([TrackLang.Val], [TrackLang.Call])
p_control_title = do
    vals <- A.many (lexeme $ TrackLang.VSymbol <$> p_scale_id <|> p_val)
    expr <- A.option [] (p_pipe >> NonEmpty.toList <$> p_pipeline True)
    return (vals, expr)

p_pipeline :: Bool -> A.Parser TrackLang.Expr
p_pipeline toplevel = do
    -- It definitely matches at least one, because p_null_call always matches.
    c : cs <- A.sepBy1 (p_expr toplevel) p_pipe
    return $ c :| cs

p_expr :: Bool -> A.Parser TrackLang.Call
p_expr toplevel = A.try p_equal <|> A.try (p_call toplevel) <|> p_null_call

p_pipe :: A.Parser ()
p_pipe = void $ lexeme (A.char '|')

p_equal :: A.Parser TrackLang.Call
p_equal = do
    assignee <- p_call_symbol True
    spaces
    A.char '='
    spaces
    vals <- A.many1 p_term
    return $ TrackLang.Call TrackLang.c_equal $
        TrackLang.Literal (TrackLang.VSymbol assignee) : vals

p_call :: Bool -> A.Parser TrackLang.Call
p_call toplevel =
    TrackLang.Call <$> lexeme (p_call_symbol toplevel) <*> A.many p_term

p_null_call :: A.Parser TrackLang.Call
p_null_call = return (TrackLang.Call "" []) <?> "null call"

-- | Any word in call position is considered a Symbol.  This means that
-- you can have calls like @4@ and @>@, which are useful names for notes or
-- ornaments.
p_call_symbol :: Bool -- ^ A call at the top level can allow a ).
    -> A.Parser TrackLang.Symbol
p_call_symbol toplevel = TrackLang.Symbol <$> p_word toplevel

p_term :: A.Parser TrackLang.Term
p_term = lexeme $
    TrackLang.Literal <$> p_val <|> TrackLang.ValCall <$> p_sub_call

p_sub_call :: A.Parser TrackLang.Call
p_sub_call = ParseText.between (A.char '(') (A.char ')') (p_call False)

p_val :: A.Parser TrackLang.Val
p_val =
    TrackLang.VInstrument <$> p_instrument
    <|> TrackLang.VAttributes <$> p_attributes
    <|> TrackLang.VNum . Score.untyped <$> p_hex
    <|> TrackLang.VNum <$> p_num
    <|> TrackLang.VSymbol <$> p_string
    <|> TrackLang.VControlRef <$> p_control_ref
    <|> TrackLang.VPControlRef <$> p_pcontrol_ref
    <|> TrackLang.VQuoted <$> p_quoted
    <|> (A.char '_' >> return TrackLang.VNotGiven)
    <|> (A.char ';' >> return TrackLang.VSeparator)
    <|> TrackLang.VSymbol <$> p_symbol

p_num :: A.Parser Score.TypedVal
p_num = do
    num <- p_untyped_num
    let suffix (typ, suf) = A.string suf >> return typ
    typ <- A.choice $ map suffix codes
    return $ Score.Typed typ num
    where
    codes = zip ScoreTypes.all_types $
        map Score.type_to_code ScoreTypes.all_types

p_untyped_num :: A.Parser Signal.Y
p_untyped_num = p_ratio <|> ParseText.p_float

p_ratio :: A.Parser Signal.Y
p_ratio = do
    sign <- A.option '+' (A.satisfy (\c -> c == '+' || c == '-'))
    num <- ParseText.p_nat
    A.char '/'
    denom <- ParseText.p_nat
    return $ (if sign == '-' then -1 else 1)
        * fromIntegral num / fromIntegral denom

-- | Parse numbers of the form @`0x`00@ or @0x00@, with an optional @-@ prefix
-- for negation.
p_hex :: A.Parser Signal.Y
p_hex = do
    sign <- A.option 1 (A.char '-' >> return (-1))
    A.string ShowVal.hex_prefix <|> A.string "0x"
    let higit c = '0' <= c && c <= '9' || 'a' <= c && c <= 'f'
    c1 <- A.satisfy higit
    c2 <- A.satisfy higit
    return $ fromIntegral (parse_hex c1 c2) / 0xff * sign

parse_hex :: Char -> Char -> Int
parse_hex c1 c2 = higit c1 * 16 + higit c2
    where
    higit c
        | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
        | otherwise = fromEnum c - fromEnum 'a' + 10

-- | A string is anything between single quotes.  A single quote itself is
-- represented by two single quotes in a row.
p_string :: A.Parser TrackLang.Symbol
p_string = TrackLang.Symbol <$> p_single_quote_string

p_single_quote_string :: A.Parser Text
p_single_quote_string = do
    chunks <- A.many1 $
        ParseText.between (A.char '\'') (A.char '\'') (A.takeTill (=='\''))
    return $ Text.intercalate "'" chunks

-- There's no particular reason to restrict attrs to idents, but this will
-- force some standardization on the names.
p_attributes :: A.Parser Score.Attributes
p_attributes = A.char '+'
    *> (Score.attrs <$> A.sepBy (p_identifier False "+") (A.char '+'))

p_control_ref :: A.Parser TrackLang.ControlRef
p_control_ref = do
    A.char '%'
    control <- Score.unchecked_control <$> A.option "" (p_identifier False ",")
    deflt <- ParseText.optional (A.char ',' >> p_num)
    return $ case deflt of
        Nothing -> TrackLang.LiteralControl control
        Just val -> TrackLang.DefaultedControl control (Signal.constant <$> val)
    <?> "control"

-- | Unlike 'p_control_ref', this doesn't parse a comma and a default value,
-- because pitches don't have literals.  Instead, use the @pitch-control@ val
-- call.
p_pcontrol_ref :: A.Parser TrackLang.PControlRef
p_pcontrol_ref = do
    A.char '#'
    TrackLang.LiteralControl . Score.unchecked_pcontrol <$>
        A.option "" (p_identifier False "")
    <?> "pitch control"

p_quoted :: A.Parser TrackLang.Quoted
p_quoted =
    A.string "\"(" *> (TrackLang.Quoted <$> p_pipeline False) <* A.char ')'

-- | This is special syntax that's only allowed in control track titles.
p_scale_id :: A.Parser TrackLang.Symbol
p_scale_id = do
    A.char '*'
    TrackLang.Symbol . Text.cons '*' <$> A.option "" (p_identifier False "")
    <?> "scale id"

p_instrument :: A.Parser Score.Instrument
p_instrument =
    A.char '>' >> Score.Instrument <$> p_identifier True "" <?> "instrument"

-- | Symbols can have anything in them but they have to start with a letter.
-- This means special literals can start with wacky characters and not be
-- ambiguous.
--
-- They can also start with a *.  This is a special hack to support *scale
-- syntax in pitch track titles, but who knows, maybe it'll be useful in other
-- places too.
p_symbol :: A.Parser TrackLang.Symbol
p_symbol = do
    c <- A.satisfy $ \c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
        || c == '-' || c == '*'
    rest <- p_null_word
    return $ TrackLang.Symbol $ Text.cons c rest

-- | Identifiers are somewhat more strict than usual.  They must be lowercase,
-- and the only non-letter allowed is hyphen.  This means words must be
-- separated with hyphens, and leaves me free to give special meanings to
-- underscores or caps if I want.
--
-- @until@ gives additional chars that stop parsing, for idents that are
-- embedded in another lexeme.
p_identifier :: Bool -> String -> A.Parser Text
p_identifier null_ok until = do
    -- TODO attoparsec docs say it's faster to do the check manually, profile
    -- and see if it makes a difference.
    ident <- (if null_ok then A.takeWhile else A.takeWhile1)
        -- (A.notInClass (until ++ " \n\t|=)")) -- buggy?
        (\c -> not $ c `elem` until || c == ' ' || c == '\n' || c == '\t'
            || c == '|' || c == '=' || c == ')')
        -- Newlines and tabs are forbidden from track and block titles and
        -- events, but can occur in ky files.
    -- This forces identifiers to be separated with spaces, except with | and
    -- =.  Otherwise @sym>inst@ is parsed as a call @sym >inst@, which I don't
    -- want to support.
    unless ((null_ok && Text.null ident) || Id.valid ident) $
        fail $ "invalid chars in identifier, expected "
            <> untxt Id.valid_description <> ": " <> show ident
    return ident

p_word :: Bool -> A.Parser Text
p_word toplevel =
    A.takeWhile1 (if toplevel then is_toplevel_word_char else is_word_char)

p_null_word :: A.Parser Text
p_null_word = A.takeWhile is_word_char

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
-- how those look.  I guess it's a crummy justification, but not need to change
-- it unless toplevel gives more more trouble.
is_toplevel_word_char :: Char -> Bool
is_toplevel_word_char c = c /= ' ' && c /= '\t' && c /= '\n' && c /= '='
    && c /= ';' -- This is so the ; separator can appear anywhere.

is_word_char :: Char -> Bool
is_word_char c = is_toplevel_word_char c && c /= ')' && c /= ']'

lexeme :: A.Parser a -> A.Parser a
lexeme p = p <* spaces

-- | Skip spaces, including a newline as long as the next line, skipping empty
-- lines, is indented.
spaces :: A.Parser ()
spaces = do
    spaces_to_eol
    A.option () $ do
        A.skip (=='\n')
        A.skipMany empty_line
        -- The next non-empty line has to be indented.
        A.skip is_whitespace
        A.skipWhile is_whitespace

empty_line :: A.Parser ()
empty_line = spaces_to_eol >> A.skip (=='\n')

spaces_to_eol :: A.Parser ()
spaces_to_eol = do
    A.skipWhile is_whitespace
    comment <- A.option "" (A.string "--")
    unless (Text.null comment) $
        A.skipWhile (\c -> c /= '\n')

is_whitespace :: Char -> Bool
is_whitespace c = c == ' ' || c == '\t'

-- * definition file

-- | Load a ky file and all other files it imports.  'parse_ky' describes the
-- format of the ky file.
load_ky :: [FilePath] -> FilePath
    -> IO (Either Text (Definitions, [FilePath]))
    -- ^ (all_definitions, imported_files)
load_ky paths fname = fmap annotate . Error.runErrorT $ load Set.empty [fname]
    where
    load _ [] = return []
    load loaded (lib:libs)
        | lib `Set.member` loaded = return []
        | otherwise = do
            text <- find lib
            (imports, defs) <- lift $ parse_ky text
            ((defs, lib) :) <$>
                load (Set.insert lib loaded) (libs ++ imports)

    annotate (Left err) = Left $ txt fname <> ": " <> err
    annotate (Right results) = Right (mconcat defs, imports)
        where (defs, imports) = unzip results

    require msg = maybe (Error.throwError msg) return
    lift = either Error.throwError return
    read :: FilePath -> Error.ErrorT Text IO (Maybe Text)
    read = liftIO . File.ignoreEnoent . Text.IO.readFile
    find fname =
        require msg =<< firstJusts (map (\dir -> read (dir </> fname)) paths)
        where
        msg = "imported file not found: " <> txt fname <> " in "
            <> Text.intercalate ", " (map txt paths)
    -- TODO I warn about shadows, but maybe it should be normal in imports?

find_mtime :: [FilePath] -> FilePath -> IO (Either Text Time.UTCTime)
find_mtime paths fname =
    maybe (Left $ "file not found: " <> txt fname) Right <$>
        firstJusts (map (\dir -> get (dir </> fname)) paths)
    where get = File.ignoreEnoent . Directory.getModificationTime

-- | This is a mirror of 'Derive.Library', but with expressions instead of
-- calls.  (generators, transformers)
data Definitions = Definitions {
    def_note :: !([Definition], [Definition])
    , def_control :: !([Definition], [Definition])
    , def_pitch :: !([Definition], [Definition])
    , def_val :: ![Definition]
    } deriving (Show)

instance Monoid.Monoid Definitions where
    mempty = Definitions ([], []) ([], []) ([], []) []
    mappend (Definitions (a1, b1) (c1, d1) (e1, f1) g1)
            (Definitions (a2, b2) (c2, d2) (e2, f2) g2) =
        Definitions (a1<>a2, b1<>b2) (c1<>c2, d1<>d2) (e1<>e2, f1<>f2) (g1<>g2)

type Definition = (TrackLang.CallId, TrackLang.Expr)
type LineNumber = Int

{- | Parse a definitions file.  This file gives a way to define new calls
    in the tracklang language, which is less powerful but more concise than
    haskell.

    The syntax is a sequence of @import path/to/file@ lines followed by
    a sequence of sections.  A section is a @header:@ line followed by
    definitions.  The header determines the type of the calls defined after it,
    e.g.:

    > import 'somelib.ky'
    >
    > note generator:
    > x = y

    Valid headers are @val:@ or @(note|control|pitch) (generator|transformer):@.
    A line is continued if it is indented.

    This is similar to the "Derive.Call.Equal" call, but not quite the same.
    Firstly, it uses headers for the call type instead of equal's weirdo
    sigils.  Secondly, the syntax is different because the arguments to equal
    are evaluated in place, while a file is all quoted by nature.  E.g. a
    definition @x = a b c@ is equivalent to an equal @^x = \"(a b c)@.
    @x = a@ (no arguments) is equivalent to @^x = a@, in that @x@ can take the
    same arguments as @a@.
-}
parse_ky :: Text -> Either Text ([FilePath], Definitions)
parse_ky text = do
    let (imports, sections) = split_sections text
    let extra = Set.toList $
            Map.keysSet sections `Set.difference` Set.fromList headers
    unless (null extra) $
        Left $ "unknown sections: " <> Text.intercalate ", " extra
    imports <- ParseText.parse_lines 1 p_imports imports
    parsed <- Traversable.traverse parse_section sections
    let get header = Map.findWithDefault [] header parsed
        get2 kind = (get (kind <> " " <> generator),
            get (kind <> " " <> transformer))
    return $ (,) imports $ Definitions
        { def_note = get2 note
        , def_control = get2 control
        , def_pitch = get2 pitch
        , def_val = get val
        }
    where
    val = "val"
    note = "note"
    control = "control"
    pitch = "pitch"
    generator = "generator"
    transformer = "transformer"
    headers = val : [t1 <> " " <> t2 | t1 <- [note, control, pitch],
        t2 <- [generator, transformer]]
    parse_section [] = return []
    parse_section ((lineno, line0) : lines) =
        ParseText.parse_lines lineno p_section $
            Text.unlines (line0 : map snd lines)

split_sections :: Text -> (Text, Map.Map Text [(LineNumber, Text)])
split_sections =
    second (Map.fromListWith (flip (++)) . concatMap split_header)
        . split_imports . Seq.split_with is_header . zip [1..] . Text.lines
    where
    is_header = (":" `Text.isSuffixOf`) . snd
    split_imports [] = ("", [])
    split_imports ([] : sections) = ("", sections)
    split_imports (imports : sections) =
        (Text.unlines $ map snd imports, sections)
    strip_colon (_, header) = Text.take (Text.length header - 1) header
    split_header [] = []
    split_header (header : section) = [(strip_colon header, section)]

p_imports :: A.Parser [FilePath]
p_imports = A.skipMany empty_line *> A.many p_import <* A.skipMany empty_line
    where
    p_import = A.string "import" *> spaces *> (untxt <$> p_single_quote_string)
        <* spaces <* A.char '\n'

p_section :: A.Parser [Definition]
p_section =
    A.skipMany empty_line *> A.many p_definition <* A.skipMany empty_line

p_definition :: A.Parser Definition
p_definition = do
    assignee <- p_call_symbol True
    spaces
    A.skip (=='=')
    spaces
    expr <- p_pipeline True
    A.skipMany empty_line
    return (assignee, expr)
