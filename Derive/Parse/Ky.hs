-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Derive.Parse.Ky (
    Ky(..), Loaded(..)
    , Definitions(..), Definition
    , load_ky
    -- ** types
    , Expr(..), Call(..), Term(..), Var(..)
#ifdef TESTING
    , module Derive.Parse.Ky
#endif
) where
import           Control.Applicative (many)
import qualified Control.Monad.Except as Except
import qualified Data.Attoparsec.Text as A
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import           System.FilePath ((</>))

import qualified Util.Exceptions as Exceptions
import qualified Util.ParseText as ParseText
import qualified Util.Seq as Seq

import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse
import qualified Derive.Parse.Instruments as Instruments
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Symbols as Symbols

import qualified Ui.Id as Id

import           Global


-- | A parsed .ky file
data Ky a = Ky {
    ky_definitions :: Definitions
    , ky_imports :: [a]
    -- | Nothing if there is no instrument section.
    , ky_allocations :: Maybe [Instruments.Allocation]
    } deriving (Show)

instance Semigroup (Ky a) where
    Ky a1 b1 c1 <> Ky a2 b2 c2 = Ky (a1<>a2) (b1<>b2) (c1<>c2)
instance Monoid (Ky a) where
    mempty = Ky mempty mempty mempty
    mappend = (<>)

-- | Record a loaded .ky file, with its path and content.
-- A path of "" is used for the code directly in the UiConfig.
data Loaded = Loaded !FilePath !Text
    deriving (Eq, Show)
-- | A requested import.  Path to .ky file, .ky files it imports.
data Import = Import !FilePath !String
    deriving (Eq, Show)

-- | This is a mirror of 'Derive.Library', but with expressions instead of
-- calls.  (generators, transformers)
data Definitions = Definitions {
    def_note :: !([Definition], [Definition])
    , def_control :: !([Definition], [Definition])
    , def_pitch :: !([Definition], [Definition])
    , def_val :: ![Definition]
    , def_aliases :: ![(ScoreT.Instrument, ScoreT.Instrument)]
    } deriving (Show)

instance Semigroup Definitions where
    (<>)    (Definitions (a1, b1) (c1, d1) (e1, f1) g1 h1)
            (Definitions (a2, b2) (c2, d2) (e2, f2) g2 h2) =
        Definitions (a1<>a2, b1<>b2) (c1<>c2, d1<>d2) (e1<>e2, f1<>f2) (g1<>g2)
            (h1<>h2)
instance Monoid Definitions where
    mempty = Definitions ([], []) ([], []) ([], []) [] []
    mappend = (<>)

-- | (defining_file, (Symbol, Expr))
type Definition = (FilePath, (Expr.Symbol, Expr))

-- * parse ky

-- | Parse ky text and load and parse all the files it imports.  'parse_ky'
-- describes the format of the ky file.
load_ky :: [FilePath] -> Text -> IO (Either ParseText.Error (Ky Loaded))
load_ky paths content = Except.runExceptT $ do
    ky <- tryRight $ first (ParseText.prefix "<score>: ") $ parse_ky "" content
    kys <- load_ky_file paths Set.empty (ky_imports ky)
    return $ mconcat $ ky { ky_imports = [Loaded "" content] } : kys

load_ky_file :: [FilePath] -> Set FilePath -> [Import]
    -> Except.ExceptT ParseText.Error IO [Ky Loaded]
load_ky_file _ _ [] = return []
load_ky_file paths loaded (Import fname lib : imports)
    | lib `Set.member` loaded = return []
    | otherwise = do
        let prefix = txt lib <> ": "
        (fname, content) <- tryRight . first (ParseText.message . (prefix<>))
            =<< liftIO (find_ky paths fname lib)
        ky <- tryRight . first (ParseText.prefix prefix) $
            parse_ky fname content
        kys <- load_ky_file paths (Set.insert lib loaded)
            (imports ++ ky_imports ky)
        return $ ky { ky_imports = [Loaded fname content] } : kys

-- | Find the file in the given paths and return its filename and contents.
find_ky :: [FilePath] -> FilePath -> FilePath
    -> IO (Either Text (FilePath, Text))
find_ky paths from fname =
    catch_io (txt fname) $ justErr msg <$>
        firstJusts (map (\dir -> get (dir </> fname)) paths)
    where
    msg = "ky file not found: " <> txt fname
        <> (if from == "" then "" else " from " <> txt from)
        <> " (searched " <> Text.intercalate ", " (map txt paths) <> ")"
    get fn = Exceptions.ignoreEnoent $ (,) fn <$> Text.IO.readFile fn

-- | Catch any IO exceptions and put them in Left.
catch_io :: Text -> IO (Either Text a) -> IO (Either Text a)
catch_io prefix io =
    either (Left . ((prefix <> ": ") <>) . showt) id <$> Exceptions.tryIO io

{- | Parse a ky file.  This file gives a way to define new calls in the
    tracklang language, which is less powerful but more concise than haskell.

    The syntax is a sequence of @import 'path\/to\/file'@ lines followed by
    a sequence of sections.  A section is a @header:@ line followed by
    definitions.  The header determines the type of the calls defined after it,
    e.g.:

    > import 'somelib.ky'
    >
    > note generator:
    > x = y
    >
    > alias:
    > new-inst = source-inst

    Valid headers are @val:@, @(note|control|pitch) (generator|transformer):@,
    or @alias:@.  A line is continued if it is indented, and @--@ comments
    until the end of the line.

    This is similar to the "Derive.Call.Equal" call, but not quite the same:

    - It uses headers for the call type instead of equal's weird sigils.

    - The syntax is different because the arguments to equal are evaluated in
    place, while a file is all quoted by nature.  E.g. a definition @x = a b c@
    is equivalent to an equal @^x = \"(a b c)@.  @x = a@ (no arguments) is
    equivalent to @^x = a@, in that @x@ can take the same arguments as @a@.

    - Calls are defined as "Derive.Call.Macro"s, which means they can include
    $variables, which become arguments to the call.
-}
parse_ky :: FilePath -> Text -> Either ParseText.Error (Ky Import)
parse_ky fname text = do
    let (imports, sections) = split_sections $ strip_comments $ Text.lines text
    (instrument_section, sections) <- return
        ( Map.lookup instrument sections
        , Map.delete instrument sections
        )
    let extra = Set.toList $
            Map.keysSet sections `Set.difference` Set.fromList valid_headers
    unless (null extra) $
        Left $ ParseText.message $
            "unknown sections: " <> Text.intercalate ", " extra
    imports <- ParseText.parse p_imports imports
    parsed <- traverse parse_section sections
    let get header = Map.findWithDefault [] header parsed
        get2 kind =
            ( get (kind <> " " <> generator)
            , get (kind <> " " <> transformer)
            )
    aliases <- first (ParseText.Error Nothing) $ mapM parse_alias (get alias)
    allocs <- traverse (traverse parse_allocation) instrument_section
    let add_fname = map (fname,)
        add_fname2 = bimap add_fname add_fname
    return $ Ky
        { ky_definitions = Definitions
            { def_note = add_fname2 $ get2 note
            , def_control = add_fname2 $ get2 control
            , def_pitch = add_fname2 $ get2 pitch
            , def_val = add_fname $ get val
            , def_aliases = aliases
            }
        , ky_imports = map (Import fname) imports
        , ky_allocations = allocs
        }
    where
    val = "val"
    note = "note"
    control = "control"
    pitch = "pitch"
    generator = "generator"
    transformer = "transformer"
    alias = "alias"
    instrument = Instruments.instrument_section
    valid_headers = val : alias :
        [ t1 <> " " <> t2
        | t1 <- [note, control, pitch], t2 <- [generator, transformer]
        ]
    parse_section [] = return []
    parse_section ((lineno, line0) : lines) =
        first (ParseText.offset (lineno, 0)) $ ParseText.parse p_section $
            Text.unlines (line0 : map snd lines)
    strip_comments = filter (not . ("--" `Text.isPrefixOf`) . Text.stripStart)

parse_allocation :: (Int, Text) -> Either ParseText.Error Instruments.Allocation
parse_allocation (lineno, line) =
    first (ParseText.offset (lineno, 0)) $
        ParseText.parse Instruments.p_allocation line

-- | The alias section allows only @alias = inst@ definitions.
parse_alias :: (Expr.Symbol, Expr)
    -> Either Text (ScoreT.Instrument, ScoreT.Instrument)
parse_alias (lhs, rhs) = first (msg<>) $ case rhs of
    Expr (Call rhs [] :| [])
        | not (Id.valid_symbol (Expr.unsym lhs)) -> Left "lhs not a valid id"
        | not (Id.valid_symbol (Expr.unsym rhs)) -> Left "rhs not a valid id"
        | otherwise -> Right (convert lhs, convert rhs)
        where convert (Expr.Symbol a) = ScoreT.Instrument a
    _ -> Left "rhs not a symbol"
    where
    msg = "alias " <> ShowVal.show_val lhs <> " = " <> ShowVal.show_val rhs
        <> ": "

-- | Split sections into a Map with section name keys, and numbered lines.
split_sections :: [Text] -> (Text, Map Text [(Int, Text)])
split_sections =
    second (Map.fromListWith (flip (++)) . concatMap split_header)
        . split_imports . Seq.split_before is_header . zip [0..]
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
p_imports =
    A.skipMany Parse.empty_line *> many p_import <* A.skipMany Parse.empty_line
    where
    p_import = A.string "import" *> Parse.spaces
        *> (untxt <$> Parse.p_single_quote_string)
        <* Parse.spaces <* A.char '\n'

p_section :: A.Parser [(Expr.Symbol, Expr)]
p_section = A.skipMany Parse.empty_line *> many p_definition
    <* A.skipMany Parse.empty_line

p_definition :: A.Parser (Expr.Symbol, Expr)
p_definition = do
    assignee <- Parse.p_symbol True
    Parse.spaces
    A.skip (=='=')
    Parse.spaces
    expr <- p_expr_ky
    A.skipMany Parse.empty_line
    return (assignee, expr)

-- * types

-- | These are parallel to the 'Expr.Expr' types, except they add
-- 'VarTerm'.  The duplication is unfortunate, but as long as this remains
-- a simple AST it seems better than the various heavyweight techniques for
-- parameterizing an AST.
newtype Expr = Expr (NonEmpty Call)
    deriving (Show)
data Call = Call !Expr.Symbol ![Term]
    deriving (Show)
data Term = VarTerm !Var | ValCall !Call | Literal !DeriveT.Val
    deriving (Show)
-- | A variable to be substituted via the "Derive.Call.Macro" mechanism.
newtype Var = Var Text deriving (Show)

instance ShowVal.ShowVal Expr where
    show_val (Expr calls) = Text.intercalate " | " $
        map ShowVal.show_val (NonEmpty.toList calls)

instance ShowVal.ShowVal Call where
    show_val (Call sym args) = Text.unwords $
        ShowVal.show_val sym : map ShowVal.show_val args

instance ShowVal.ShowVal Term where
    show_val (VarTerm var) = ShowVal.show_val var
    show_val (ValCall call) = "(" <> ShowVal.show_val call <> ")"
    show_val (Literal val) = ShowVal.show_val val

instance ShowVal.ShowVal Var where
    show_val (Var name) = "$" <> name

-- * parsers

-- | As 'Expr' parallels 'Expr.Expr', these parsers parallel 'p_expr' and so
-- on.
p_expr_ky :: A.Parser Expr
p_expr_ky = do
    -- It definitely matches at least one, because p_null_call always matches.
    c : cs <- A.sepBy1 p_toplevel_call_ky Parse.p_pipe
    return $ Expr (c :| cs)

p_toplevel_call_ky :: A.Parser Call
p_toplevel_call_ky =
    call_to_ky <$> Parse.p_unparsed_expr
    <|> p_equal_ky
    <|> p_call_ky
    <|> call_to_ky <$> Parse.p_null_call

call_to_ky :: DeriveT.Call -> Call
call_to_ky (Expr.Call sym args) = Call sym (map convert args)
    where
    convert (Expr.Literal val) = Literal val
    convert (Expr.ValCall call) = ValCall (call_to_ky call)

p_equal_ky :: A.Parser Call
p_equal_ky = do
    (lhs, sym, rhs) <- Parse.p_equal_generic p_term_ky
    return $ Call Symbols.equal $
        literal lhs : rhs ++ maybe [] (:[]) (literal <$> sym)
    where literal = Literal . DeriveT.VStr

p_sub_call_ky :: A.Parser Call
p_sub_call_ky = ParseText.between (A.char '(') (A.char ')') p_call_ky

p_call_ky :: A.Parser Call
p_call_ky = Call <$> Parse.lexeme (Parse.p_symbol False) <*> many p_term_ky

p_term_ky :: A.Parser Term
p_term_ky =
    Parse.lexeme $ VarTerm <$> p_var
    <|> Literal <$> Parse.p_val
    <|> ValCall <$> p_sub_call_ky

p_var :: A.Parser Var
p_var = A.char '$' *> (Var <$> A.takeWhile1 is_var_char)

is_var_char :: Char -> Bool
is_var_char c = 'a' <= c || 'z' <= c || c == '-'
