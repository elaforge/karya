-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Util.Texts where
import           Prelude hiding (lines)
import           Control.Arrow (first)
import           Control.Monad (liftM)
import qualified Control.Monad.Identity as Identity

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Lazy.Encoding

import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

import qualified Util.Regex as Regex
import qualified Util.Seq as Seq


-- * conversion

class Textlike a where
    toText :: a -> Text
    fromText :: Text -> a

    toByteString :: a -> ByteString.ByteString
    toByteString = Encoding.encodeUtf8 . toText

    toLazyByteString :: a -> ByteString.Lazy.ByteString
    toLazyByteString = Lazy.Encoding.encodeUtf8 . Text.Lazy.fromStrict . toText

instance Textlike Text where
    toText = id
    fromText = id

instance Textlike String where
    toText = Text.pack
    fromText = Text.unpack

instance Textlike ByteString.ByteString where
    toText = Encoding.decodeUtf8With Encoding.Error.lenientDecode
    fromText = toByteString
    toByteString = id
    toLazyByteString = ByteString.Lazy.fromStrict

instance Textlike ByteString.Lazy.ByteString where
    toText = toText . ByteString.Lazy.toStrict
    fromText = ByteString.Lazy.fromStrict . fromText
    toByteString = ByteString.Lazy.toStrict
    toLazyByteString = id


-- * operations

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
join2 :: Textlike a => a -> a -> a
join2 = joinWith (fromText " ")

joinWith :: Textlike a => a -> a -> a -> a
joinWith sep a b = join sep $ filter (not . Text.null . toText) [a, b]

join :: Textlike a => a -> [a] -> a
join sep = fromText . Text.intercalate (toText sep) . map toText

unlines :: Textlike a => [a] -> a
unlines = fromText . Text.unlines . map toText

split1 :: Text -> Text -> (Text, Text)
split1 sep text = (pre, Text.drop (Text.length sep) post)
    where (pre, post) = Text.breakOn sep text

ellipsis :: Int -> Text -> Text
ellipsis maxWidth text
    | Text.length text <= maxWidth = text
    | otherwise = Text.take (maxWidth - 3) text <> "..."

ellipsisList :: Int -> [Text] -> [Text]
ellipsisList max xs
    | null post = xs
    | otherwise = pre ++ ["..."]
    where (pre, post) = splitAt max xs

dropPrefix :: Text -> Text -> Text
dropPrefix prefix text
    | prefix `Text.isPrefixOf` text = Text.drop (Text.length prefix) text
    | otherwise = text

enumeration :: (Textlike a, Monoid a, String.IsString a) => [a] -> a
enumeration = join "\n" . map ("- "<>)

-- | Format the given rows into columns, aligned vertically.
columns :: Int -> [[Text]] -> [Text]
columns padding rows = map formatRow rows
    where
    formatRow = Text.stripEnd . mconcat . zipWith pad widths
    pad w = Text.justifyLeft (w + padding) ' '
    byCol = map (map (Maybe.fromMaybe Text.empty)) (Seq.rotate2 rows)
    widths = map (List.maximum . (0:) . map Text.length) byCol

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
    | notInTemplate /= mempty = Left $ "given variables not in template: "
        <> commas (Set.toList notInTemplate)
    | notInVariables /= mempty = Left $ "template variable not given: "
        <> commas (Set.toList notInVariables)
    | otherwise = Right $ replaceMany
        [("${" <> k <> "}", v) | (k, v) <- Map.toList variables] template
    where
    inTemplate = Set.fromList $ concatMap snd $ Regex.groups variable template
    inVariables = Map.keysSet variables
    notInTemplate = inVariables `Set.difference` inTemplate
    notInVariables = inTemplate `Set.difference` inVariables
    commas = Text.intercalate ", "
    variable = Regex.compileUnsafe "\\$\\{([a-z0-9_]+)\\}"

-- * haddockUrl

type Files = Set.Set FilePath
type Url = String

-- | This doesn't really belong here, but it can't go in 'Util.Linkify' since
-- that's a main module.
haddockUrl :: Files -> FilePath -> Text -> Maybe Url
haddockUrl files haddockDir text
    | moduleExists components = Just $ moduleLink components
    | moduleExists (Seq.rdrop 1 components) = symbolLink
    | otherwise = Nothing
    where
    components = map Text.unpack $ Text.split (=='.') text
    moduleExists path = exists (FilePath.joinPath path ++ ".hs")
        || exists (FilePath.joinPath path ++ ".hsc")
    exists = (`Set.member` files)

    moduleLink path = strip $ haddockDir </> Seq.join "-" path ++ ".html"
    strip ('.' : '/' : path) = path
    strip path = path
    symbolLink = case Seq.viewr components of
        Just (mod, sym) -> Just $ moduleLink mod ++ symbolAnchor sym
        Nothing -> Nothing

symbolAnchor :: String -> String
symbolAnchor sym = case sym of
    c : _ | Char.isUpper c -> "#t:" ++ sym
    _ -> "#v:" ++ sym
