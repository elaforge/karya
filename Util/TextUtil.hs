-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.TextUtil where
import Control.Arrow (first)
import Control.Monad
import qualified Control.Monad.Identity as Identity

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Monoid(..), (<>))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)

import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified Util.Regex as Regex
import qualified Util.Seq as Seq


-- | Replace substrings simultaneously.
replaceMany :: [(Text, Text)] -> Text -> Text
replaceMany replace = mconcat . go
    where
    go text
        | Text.null text = []
        | otherwise = maybe [text] continue $
            Seq.minimum_on (Text.length . fst . fst)
                [(Text.breakOn (fst r) text, r) | r <- replace]
    continue ((pre, post), (from, to))
        | Text.null post = [pre]
        | otherwise = pre : to : go (Text.drop (Text.length from) post)

-- | Join the two pieces with a space, if they are non-empty.
join2 :: Text -> Text -> Text
join2 t1 t2 = Text.unwords $ filter (not . Text.null) [t1, t2]

joinWith :: Text -> Text -> Text -> Text
joinWith sep t1 t2 = Text.intercalate sep $ filter (not . Text.null) [t1, t2]

-- | Format the given rows into columns, aligned vertically.
formatColumns :: Int -> [[Text]] -> [Text]
formatColumns padding rows = map format_row rows
    where
    format_row = Text.concat . zipWith pad widths
    pad w cell = cell
        <> Text.replicate (w - Text.length cell + padding) (Text.pack " ")
    by_col = map (map (Maybe.fromMaybe Text.empty)) (Seq.rotate2 rows)
    widths = replace $ map (List.maximum . (0:) . map Text.length) by_col
    replace = reverse . (0:) . drop 1 . reverse

-- | Apply a function to the contents delimited by the given Char.  You can
-- quote a delimiter with a backslash.
mapDelimited :: Bool -> Char -> (Text -> Text) -> Text -> Text
mapDelimited withSpaces delimiter f =
    Identity.runIdentity . mapDelimitedM withSpaces delimiter (return . f)

mapDelimitedM :: Monad m => Bool -> Char -> (Text -> m Text) -> Text -> m Text
mapDelimitedM withSpaces delimiter f =
    liftM (mconcat . mconcat) . mapM apply
        . extractDelimited withSpaces delimiter
    where
    apply (text, Just word) = do
        replace <- f word
        return [text, replace]
    apply (text, Nothing) = return [text]

-- | This is more awkward than a parser, but... ok, maybe I should have used
-- a parser.
extractDelimited :: Bool -- ^ If false, a delimiter doesn't count if the text
    -- after it has a space.
    -> Char -> Text -> [(Text, Maybe Text)]
extractDelimited withSpaces delimiter = go
    where
    go text
        | Text.null within || Text.null post = [(pre <> within, Nothing)]
        | not withSpaces && Text.any (==' ') word =
            (pre <> delim, Nothing) : go (Text.drop 1 within)
        | otherwise = (pre, Just word) : go (Text.drop 1 post)
        where
        (pre, within) = break text
        (word, post) = break $ Text.drop 1 within
    break text
        | "\\" `Text.isSuffixOf` pre =
            first ((Text.take (Text.length pre - 1) pre <> delim) <>) $
                break (Text.drop 1 post)
        | otherwise = (pre, post)
        where (pre, post) = Text.breakOn delim text
    delim = Text.singleton delimiter

-- * interpolate

-- | Replace @${variable}@.
interpolate :: Text -> Map.Map Text Text -> Either Text Text
interpolate template variables
    | notInTemplate /= mempty = Left $ "variables not in template: "
        <> commas (Set.toList notInTemplate)
    | notInVariables /= mempty = Left $ "template holes not in variables: "
        <> commas (Set.toList notInVariables)
    | otherwise = Right $ replaceMany
        [("${" <> k <> "}", v) | (k, v) <- Map.toList variables] template
    where
    inTemplate = Set.fromList $ concatMap snd $ Regex.groups variable template
    inVariables = Map.keysSet variables
    notInTemplate = inVariables `Set.difference` inTemplate
    notInVariables = inTemplate `Set.difference` inVariables
    commas = Text.intercalate ", "
    variable = Regex.compileUnsafe "interpolate" "\\$\\{([a-z0-9_]+)\\}"

-- * haddockUrl

type Files = Set.Set FilePath
type Url = String

-- | This doesn't really belong here, but it can't go in 'Util.Linkify' since
-- that's a main module.
haddockUrl :: Files -> FilePath -> Text -> Maybe Url
haddockUrl files haddock_dir text
    | moduleExists components = Just $ moduleLink components
    | moduleExists (Seq.rdrop 1 components) = symbolLink
    | otherwise = Nothing
    where
    components = map Text.unpack $ Text.split (=='.') text
    moduleExists path = exists (FilePath.joinPath path ++ ".hs")
        || exists (FilePath.joinPath path ++ ".hsc")
    exists = (`Set.member` files)

    moduleLink path = strip $ haddock_dir </> Seq.join "-" path ++ ".html"
    strip ('.' : '/' : path) = path
    strip path = path
    symbolLink = case Seq.viewr components of
        Just (mod, sym) -> Just $ moduleLink mod ++ symbolAnchor sym
        Nothing -> Nothing

symbolAnchor :: String -> String
symbolAnchor sym = case sym of
    c : _ | Char.isUpper c -> "#t:" ++ sym
    _ -> "#v:" ++ sym
