-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Derive.Parse.Ky (
    Code
    , Line
    , Ky(..), Loaded(..)
    , TScore
    , Definitions(..), Definition
    , loadKyCode
    , loadKy
    , parseKy
    , replaceSection
    -- ** types
    , Expr(..), Call(..), Term(..), Var(..)
#ifdef TESTING
    , module Derive.Parse.Ky
#endif
) where
import           Control.Applicative (many)
import qualified Control.Exception as Exception
import qualified Control.Monad.Except as Except

import qualified Data.Attoparsec.Text as A
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import           System.FilePath ((</>))
import qualified System.IO.Error as IO.Error

import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Util.ParseA as ParseA
import qualified Util.ParseM as ParseM

import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse
import qualified Derive.Parse.Instruments as Instruments
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Symbols as Symbols
import qualified Derive.TScore.Parse as TScore.Parse
import qualified Derive.TScore.T as TScore.T

import qualified Ui.Id as Id
import qualified Ui.UiConfig as UiConfig

import           Global


type Error = Text
-- | Ky code.  For the whole file or whole sections, not fragments or lines.
type Code = Text

-- | A parsed .ky file
data Ky a = Ky
    { kyDefinitions :: Definitions
    , kyImports :: [a]
    , kyInstruments :: UiConfig.Allocations
    , kyTscores :: [TScore]
    } deriving (Show)

type TScore = (FilePath, Line, TScore.T.Code, TScore.T.Score)

instance Semigroup (Ky a) where
    Ky a1 b1 c1 d1 <> Ky a2 b2 c2 d2 =
        Ky (a1<>a2) (b1<>b2) (c1<>c2) (d1<>d2)
instance Monoid (Ky a) where
    mempty = Ky mempty mempty mempty mempty

-- | Record a loaded .ky file, with its path and content.
-- A path of "" is used for the code directly in the UiConfig.
data Loaded = Loaded !FilePath !Code
    deriving (Eq, Show)
-- | A requested import.  Importing path, path to import.
data Import = Import !FilePath !FilePath
    deriving (Eq, Show)

-- | This is a mirror of 'Derive.Library', but with expressions instead of
-- calls.  (generators, transformers)
data Definitions = Definitions
    { defNote :: !([Definition], [Definition])
    , defControl :: !([Definition], [Definition])
    , defPitch :: !([Definition], [Definition])
    , defVal :: ![Definition]
    , defAliases :: ![(ScoreT.Instrument, ScoreT.Instrument)]
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

-- | Legacy path to read ky from a Text instead of a file.
loadKyCode :: [FilePath] -> Code -> IO (Either ParseM.Error (Ky Loaded))
loadKyCode paths code = Except.runExceptT $ do
    ky <- tryRight $ parseKy "<internal>" code
    kys <- importKy Set.empty paths (kyImports ky)
    pure $ mconcat $ ky { kyImports = [Loaded "" code] } : kys

-- | Parse ky text and load and parse all the files it imports.  'parseKy'
-- describes the format of the ky file.
loadKy :: [FilePath] -> FilePath -> IO (Either ParseM.Error (Ky Loaded))
loadKy paths fname = Except.runExceptT $
    mconcat <$> importKy Set.empty paths [Import "" fname]

importKy :: Set FilePath -> [FilePath] -> [Import]
    -> Except.ExceptT ParseM.Error IO [Ky Loaded]
importKy _ _ [] = pure []
importKy loaded paths (Import importer fname : imports) = do
    (fname, code) <- if importer == ""
        then do
            code <- tryRight . first (ParseM.setFilename fname) . extract
                =<< liftIO (readKy fname)
            pure (fname, code)
        else tryRight . first (ParseM.setFilename importer . ParseM.message)
            =<< liftIO (findKy paths fname)
    if fname `Set.member` loaded then pure [] else do
        ky <- tryRight $ parseKy fname code
        kys <- importKy (Set.insert fname loaded) paths
            (imports ++ kyImports ky)
        pure $ ky { kyImports = [Loaded fname code] } : kys
    where
    extract Nothing = Left $ ParseM.message "not found"
    extract (Just (Left err)) = Left $ ParseM.message err
    extract (Just (Right a)) = Right a

-- | Find the file in the given paths and return its filename and contents.
findKy :: [FilePath] -> FilePath -> IO (Either Error (FilePath, Code))
findKy paths fname =
    maybe (Left notFound) id <$>
        firstJusts (map (\dir -> get (dir </> fname)) paths)
    where
    notFound = "ky file not found: " <> txt fname
        <> " (searched " <> Text.intercalate ", " (map txt paths) <> ")"
    get fn = fmap (fmap (fn,)) <$> readKy fn

readKy :: FilePath -> IO (Maybe (Either Error Code))
readKy fname = Exception.try (Text.IO.readFile fname) >>= pure . \case
    Left exc
        | IO.Error.isDoesNotExistError exc -> Nothing
        | otherwise -> Just $ Left $ txt (show exc)
    Right t -> Just $ Right t

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
parseKy :: FilePath -> Code -> Either ParseM.Error (Ky Import)
parseKy fname text = first (ParseM.setFilename fname) $ do
    (imports, sections) <- first ParseM.message $ checkedSections text
    (definitionSections, sections) <- pure $
        partitionKeys (`Set.member` definitionSections) sections
    (mbInstruments, sections) <- pure $
        takeKey Instruments.instrumentSection sections
    (mbTscores, sections) <- pure $ takeKey tscoreSection sections
    unless (Map.null sections) $
        Left $ ParseM.message $
            "unknown sections: " <> Text.intercalate ", " (Map.keys sections)
    imports <- ParseA.parse pImports imports
    parsed <- traverse parseSection definitionSections
    let get header = Map.findWithDefault [] header parsed
        get2 kind =
            ( get (kind <> " " <> generator)
            , get (kind <> " " <> transformer)
            )
    defAliases <- first ParseM.message $ mapM parseAlias (get alias)
    kyInstruments <- case mbInstruments of
        Just lines@((lineno, _) : _) ->
            parseInstruments fname lineno $ map snd lines
        _ -> Right mempty
    let addFname = map (fname,)
        addFname2 = bimap addFname addFname
    kyTscores <- case mbTscores of
        Just lines@((lineno, _) : _) -> do
            tscore <- parseTscore fname lineno code
            pure [(fname, lineno, code, tscore)]
            where code = Text.unlines (map snd lines)
        _ -> pure []
    pure $ Ky
        { kyDefinitions = Definitions
            { defNote = addFname2 $ get2 note
            , defControl = addFname2 $ get2 control
            , defPitch = addFname2 $ get2 pitch
            , defVal = addFname $ get val
            , defAliases
            }
        , kyImports = map (Import fname) imports
        , kyInstruments
        , kyTscores
        }
    where
    val = "val"
    note = "note"
    control = "control"
    pitch = "pitch"
    generator = "generator"
    transformer = "transformer"
    alias = "alias"
    definitionSections = Set.fromList $ val : alias :
        [ t1 <> " " <> t2
        | t1 <- [note, control, pitch], t2 <- [generator, transformer]
        ]
    parseSection [] = pure []
    parseSection ((lineno, line0) : lines) =
        first (ParseM.addRow lineno) $ ParseA.parse pSection $
            Text.unlines (line0 : map snd lines)

-- | The alias section allows only @alias = inst@ definitions.
parseAlias :: (Expr.Symbol, Expr)
    -> Either Error (ScoreT.Instrument, ScoreT.Instrument)
parseAlias (lhs, rhs) = first (msg<>) $ case rhs of
    Expr (Call rhs [] :| [])
        | not (Id.validSymbol (Expr.unsym lhs)) -> Left "lhs not a valid id"
        | not (Id.validSymbol (Expr.unsym rhs)) -> Left "rhs not a valid id"
        | otherwise -> Right (convert lhs, convert rhs)
        where convert (Expr.Symbol a) = ScoreT.Instrument a
    _ -> Left "rhs not a symbol"
    where
    msg = "alias " <> ShowVal.showVal lhs <> " = " <> ShowVal.showVal rhs
        <> ": "

-- | The lineno is the 0-based line of the first line of the section, which
-- becomes the 1-based starting line for megaparsec.
parseInstruments :: FilePath -> Line -> [Code]
    -> Either ParseM.Error UiConfig.Allocations
parseInstruments fname lineno lines =
    first (ParseM.addRow lineno . ParseM.fromMegaparsec) $
        ParseM.parseE fname Instruments.pInstruments
            (Text.unlines lines)

-- * tscore

tscoreSection :: Text
tscoreSection = "tscore"

parseTscore :: FilePath -> Line -> Code
    -> Either ParseM.Error TScore.T.Score
parseTscore fname lineno code =
    first (ParseM.addRow lineno . ParseM.fromMegaparsec) $
        ParseM.parseE fname TScore.Parse.score code

-- * parse ky file

-- | Most of the sections are not line-oriented, but the instrument section is.
-- Line numbers are used for to add as offsets to parsing errors.
type Section = (Title, [(Line, Code)])
type Title = Text
type Line = Int

-- | Split sections into a Map with section name keys, and numbered lines.
checkedSections :: Code -> Either Error (Code, Map Title [(Line, Code)])
checkedSections = traverse check . extract . parseSections
    where
    check (sections, []) = Right sections
    check (_, dups) =
        Left $ "duplicate sections: " <> Text.unwords (map fst dups)
    extract ((_, pre) : sections) =
        (Text.unlines (map snd pre), Maps.unique2 sections)
    extract [] = ("", (mempty, []))

-- | Split ky code into Sections.  A Title of "" is used for the implicit
-- section before the first section title, used for imports.
-- A title is a line that's all letters or spaces, ending with ':',
-- enumerated in valid_titles.
parseSections :: Code -> [Section]
parseSections = merge . Lists.splitWith parseHeader . zip [0..] . Text.lines
    where
    merge (pre, sections) = map (fmap strip) $ ("", pre) : sections
    parseHeader (_, line)
        | Just (title, ':') <- Text.unsnoc line
        , Text.all (\c -> Char.isAsciiLower c || c == ' ') title =
            Just title
        | otherwise = Nothing
    strip = dropWhile (Text.null . snd)

unparseSection :: (Title, [Text]) -> Code
unparseSection (section, lines) =
    (if section == "" then "" else section <> ":\n") <> Text.unlines lines

replaceSection :: Title -> (Code -> Code) -> Code -> Code
replaceSection title modify =
    mconcatMap unparseSection . map mod . parseSections
    where
    mod (title2, lines)
        | title == title2 = (modify title, map (modify . snd) lines)
        | otherwise = (title2, map snd lines)

-- * parse inside sections

pImports :: A.Parser [FilePath]
pImports =
    A.skipMany Parse.emptyLine *> many pImport <* A.skipMany Parse.emptyLine
    where
    pImport = A.string "import" *> Parse.spaces
        *> (untxt <$> Parse.pSingleQuoteString)
        <* Parse.spaces <* A.char '\n'

pSection :: A.Parser [(Expr.Symbol, Expr)]
pSection = A.skipMany Parse.emptyLine *> many pDefinition
    <* A.skipMany Parse.emptyLine

pDefinition :: A.Parser (Expr.Symbol, Expr)
pDefinition = do
    assignee <- Parse.pSymbol True
    Parse.spaces
    A.skip (=='=')
    Parse.spaces
    expr <- pExprKy
    A.skipMany Parse.emptyLine
    pure (assignee, expr)

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
    showVal (Expr calls) = Text.intercalate " | " $
        map ShowVal.showVal (NonEmpty.toList calls)

instance ShowVal.ShowVal Call where
    showVal (Call sym args) = Text.unwords $
        ShowVal.showVal sym : map ShowVal.showVal args

instance ShowVal.ShowVal Term where
    showVal (VarTerm var) = ShowVal.showVal var
    showVal (ValCall call) = "(" <> ShowVal.showVal call <> ")"
    showVal (Literal val) = ShowVal.showVal val

instance ShowVal.ShowVal Var where
    showVal (Var name) = "$" <> name

-- * parsers

-- | As 'Expr' parallels 'Expr.Expr', these parsers parallel 'p_expr' and so
-- on.
pExprKy :: A.Parser Expr
pExprKy = do
    -- It definitely matches at least one, because p_null_call always matches.
    c : cs <- A.sepBy1 pToplevelCallKy Parse.pPipe
    pure $ Expr (c :| cs)

pToplevelCallKy :: A.Parser Call
pToplevelCallKy =
    callToKy <$> Parse.pUnparsedExpr
    <|> pEqualKy
    <|> pCallKy
    <|> callToKy <$> Parse.pNullCall

callToKy :: DeriveT.Call -> Call
callToKy (Expr.Call sym args) = Call sym (map convert args)
    where
    convert (Expr.Literal val) = Literal val
    convert (Expr.ValCall call) = ValCall (callToKy call)

pEqualKy :: A.Parser Call
pEqualKy = do
    (lhs, sym, rhs) <- Parse.pEqualGeneric pTermKy
    pure $ Call Symbols.equal $
        literal lhs : rhs ++ maybe [] (:[]) (literal <$> sym)
    where literal = Literal . DeriveT.VStr

pSubCallKy :: A.Parser Call
pSubCallKy = ParseA.between (A.char '(') (A.char ')') pCallKy

pCallKy :: A.Parser Call
pCallKy = Call <$> Parse.lexeme (Parse.pSymbol False) <*> many pTermKy

pTermKy :: A.Parser Term
pTermKy =
    Parse.lexeme $ VarTerm <$> pVar
    <|> Literal <$> Parse.pVal Parse.UnquotedLoose
    <|> ValCall <$> pSubCallKy

pVar :: A.Parser Var
pVar = A.char '$' *> (Var <$> A.takeWhile1 isVarChar)

isVarChar :: Char -> Bool
isVarChar c = 'a' <= c || 'z' <= c || c == '-'

-- * util

partitionKeys :: Eq k => (k -> Bool) -> Map k a -> (Map k a, Map k a)
partitionKeys f m = (Map.fromAscList yes, Map.fromAscList no)
    where (yes, no) = List.partition (f . fst) (Map.toAscList m)

takeKey :: Ord k => k -> Map k a -> (Maybe a, Map k a)
takeKey k m = (Map.lookup k m, Map.delete k m)
