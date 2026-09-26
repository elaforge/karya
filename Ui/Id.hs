-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Id (
    Id, Namespace, id, namespace

    -- * access
    , unId, unNamespace, idName, idNamespace, setNamespace
    , setName, modifyName

    -- * read / show
    , readId, showId, readShort, showShort

    -- * validate
    , validSymbol, symbolDescription, isIdChar

    -- * Ident
    , Ident(..)
    , showIdent, readIdent
    , identText, textIdent
    , identName, identNamespace
    , modify

    -- * constants
    , global, globalNamespace

    -- * instances
    , BlockId(..), ViewId(..), TrackId(..), RulerId(..)
) where
import qualified Prelude
import           Prelude hiding (id)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.Read as Read

import qualified Util.Seed as Seed
import qualified Util.Serialize as Serialize

import           Global


-- | IDs come in two parts, a namespace and a name.
--
-- This is so so that you can merge two scores together and not have their IDs
-- clash.  Since block calls within a score will generally leave the namespace
-- implicit, the merged score should still be playable.
data Id = Id !Namespace !Text
    deriving (Eq, Ord, Show)

instance Aeson.ToJSON Id where
    toJSON = Aeson.String . identText
instance Aeson.FromJSON Id where
    parseJSON (Aeson.String a) = pure $ readId a
    parseJSON _ = fail "expecting String"

-- | The Namespace should pass 'valid_symbol', and is guaranteed to not contain
-- \/s.  This is because the git backend uses the namespace for a directory
-- name.
newtype Namespace = Namespace Text
    deriving (Eq, Ord, Show, Read, DeepSeq.NFData, Seed.Seed,
        Serialize.Serialize)

-- | Convert @/@ to @-@.  This is because @/@ is used to separate namespace and
-- ident.
clean :: Text -> Text
clean = Text.map (\c -> if c == '/' then '-' else c)

id :: Namespace -> Text -> Id
id ns = Id ns . clean

namespace :: Text -> Namespace
namespace = Namespace . clean

instance Serialize.Serialize Id where
    get = Id <$> Serialize.get <*> Serialize.get
    put (Id a b) = Serialize.put a >> Serialize.put b

instance Seed.Seed Id where
    toSeed n (Id ns name) = n Seed.& ns Seed.& name

instance Pretty Namespace where pretty = unNamespace
instance Pretty Id where pretty = showId

instance DeepSeq.NFData Id where
    rnf (Id ns name) = ns `seq` name `seq` ()

-- * access

unId :: Id -> (Namespace, Text)
unId (Id ns ident) = (ns, ident)

idName :: Id -> Text
idName (Id _ name) = name

idNamespace :: Id -> Namespace
idNamespace (Id ns _) = ns

setNamespace :: Namespace -> Id -> Id
setNamespace ns (Id _ name) = Id ns name

setName :: Text -> Id -> Id
setName name (Id ns _) = id ns name

modifyName :: (Text -> Text) -> Id -> Id
modifyName modify (Id ns name) = id ns (modify name)

unNamespace :: Namespace -> Text
unNamespace (Namespace s) = s

-- * read / show

readId :: Text -> Id
readId s = id (namespace pre) (Text.drop 1 post)
    where (pre, post) = Text.breakOn "/" s

showId :: Id -> Text
showId (Id (Namespace ns) ident) = ns <> "/" <> ident

-- | A smarter constructor that only applies the namespace if the string
-- doesn't already have one.
readShort :: Namespace -> Text -> Id
readShort defaultNs text = case Text.breakOn "/" text of
    (ident, "") -> id defaultNs ident
    (ns, ident) -> id (namespace ns) (Text.drop 1 ident)

-- | The inverse of 'read_short'.
showShort :: Namespace -> Id -> Text
showShort defaultNs ident@(Id ns name)
    | defaultNs == ns = name
    | otherwise = showId ident

-- * validate

{- | True if this Namespace or Id name follows some strict rules, which are
    a superset of the rules that make it parseable as an unquoted symbol.

    A valid identifier is @[a-z][a-z0-9.-]*@, as in 'symbol_description'.
    Hyphens are intended to separate words, and dots intended to separate
    syntactic elements, whatever those may be.  The rules are intentionally
    restrictive, to force standardization on names, and also to keep some
    syntactic flexibility in case I want to add special syntax.

    I originally used dots for relative calls, but they turn out to be annoying
    because you can't start a tracklang symbol with one, so now they use
    a hyphen.  Dots are still used for divisions in automatically generated
    names, for instance, TrackIds are generated as block.t1.

    Several kinds of tracklang names use this definition of validity, not just
    Ids (e.g. instrument or control names).  It's easier to remember a single
    rule for a valid name rather than each syntactic form have its own rules.
-}
validSymbol :: Text -> Bool
validSymbol s = not (Text.null s) && asciiLowerAlpha (Text.head s)
    && Text.all isIdChar s

validId :: Id -> Maybe Id
validId (Id (Namespace ns) ident)
    | validSymbol ns && validSymbol ident = Just $ Id (Namespace ns) ident
    | otherwise = Nothing

-- | 'BlockId's are more lenient, only spaces and "" are forbidden.
validBlockId :: Id -> Maybe Id
validBlockId (Id (Namespace ns) ident)
    | validSymbol ns && not (Text.null ident)
            && not (" " `Text.isInfixOf` ident) =
        Just $ Id (Namespace ns) ident
    | otherwise = Nothing

-- | Describe a valid identifier for docs and error messages.
symbolDescription :: Text
symbolDescription = "[a-z][a-z0-9.-]*"

-- | This defines the set of valid characters allowed in an ID.
isIdChar :: Char -> Bool
isIdChar c = asciiLowerAlpha c || asciiDigit c || c == '-' || c == '.'

asciiLowerAlpha :: Char -> Bool
asciiLowerAlpha c = 'a' <= c && c <= 'z'

asciiDigit :: Char -> Bool
asciiDigit c = '0' <= c && c <= '9'

-- * Ident

-- | BlockIds, RulerIds, etc. are just wrappers around Ids.  Giving them a
-- consistent display format lets me copy and paste them on the repl socket,
-- which puts the constructors in scope.
class Ident a where
    unpackId :: a -> Id
    constructorName :: Proxy a -> String
    make :: Id -> Maybe a
    makeUnchecked :: Id -> a

instance Ident Id where
    unpackId = Prelude.id
    constructorName _ = "id"
    make = Just
    makeUnchecked a = a

showIdent :: forall a. Ident a => a -> String
showIdent ident = "(" ++ con ++ " " ++ show (showId id) ++ ")"
    where
    id = unpackId ident
    con = constructorName (Proxy :: Proxy a)

readIdent :: forall a. Ident a => ReadPrec.ReadPrec (Maybe a)
readIdent = do
    Read.Punc "(" <- Read.lexP
    Read.Ident sym <- Read.lexP
    guard (sym == constructorName (Proxy :: Proxy a))
    Read.String str <- Read.lexP
    Read.Punc ")" <- Read.lexP
    pure $ make (readId (txt str))

-- | SomethingId -> "ns/name"
identText :: Ident a => a -> Text
identText = showId . unpackId

-- | "ns/name" -> SomethingId
textIdent :: Ident a => Text -> Maybe a
textIdent = make . readId

-- | SomethingId -> "name"
identName :: Ident a => a -> Text
identName = idName . unpackId

identNamespace :: Ident a => a -> Namespace
identNamespace = idNamespace . unpackId

modify :: Ident a => (Id -> Id) -> a -> a
modify f = makeUnchecked . f . unpackId

-- * constants

global :: Text -> Id
global = id globalNamespace

globalNamespace :: Namespace
globalNamespace = namespace ""


-- * instances

-- | Reference to a Block.  Use this to look up Blocks in the State.
--
-- The convention is that BlockId should name a block which is expected to
-- exist, and the only way to create a BlockId is via 'Ui.State.create_block'.
-- The name of a block which is to be created is simply 'Id'.
--
-- However, since the constructor is exported, this isn't rigorously enforced.
--
-- Unlike other Ids, block names have no restrictions, except no spaces.  This
-- is because they become note calls, and it's convenient to have arbitrary
-- names for the same reason it's convenient to allow arbitrary characters in
-- call names.
newtype BlockId = BlockId Id
    deriving (Eq, Ord, DeepSeq.NFData, Serialize.Serialize,
        Seed.Seed)

-- | Reference to a View, as per 'BlockId'.
newtype ViewId = ViewId Id
    deriving (Eq, Ord, DeepSeq.NFData, Serialize.Serialize)

newtype TrackId = TrackId Id
    deriving (Eq, Ord, DeepSeq.NFData, Serialize.Serialize, Seed.Seed)

newtype RulerId = RulerId Id
    deriving (Eq, Ord, DeepSeq.NFData, Serialize.Serialize)

instance Show BlockId where show = showIdent
instance Show ViewId where show = showIdent
instance Show TrackId where show = showIdent
instance Show RulerId where show = showIdent

instance Pretty BlockId where pretty = showt
instance Pretty ViewId where pretty = showt
instance Pretty TrackId where pretty = showt
instance Pretty RulerId where pretty = showt

instance Read BlockId where readPrec = require readIdent
instance Read ViewId where readPrec = require readIdent
instance Read TrackId where readPrec = require readIdent
instance Read RulerId where readPrec = require readIdent

require :: Read.ReadPrec (Maybe a) -> Read.ReadPrec a
require = (maybe Read.pfail pure =<<)

instance Ident BlockId where
    unpackId (BlockId a) = a
    constructorName _ = "bid"
    make = fmap makeUnchecked . validBlockId
    makeUnchecked = BlockId
instance Ident ViewId where
    unpackId (ViewId a) = a
    constructorName _ = "vid"
    make = fmap ViewId . validId
    makeUnchecked = ViewId
instance Ident TrackId where
    unpackId (TrackId a) = a
    constructorName _ = "tid"
    make = fmap TrackId . validId
    makeUnchecked = TrackId
instance Ident RulerId where
    unpackId (RulerId a) = a
    constructorName _ = "rid"
    make = fmap RulerId . validId
    makeUnchecked = RulerId

instance Aeson.ToJSON BlockId where toJSON = Aeson.toJSON . unpackId
instance Aeson.FromJSON BlockId where parseJSON = fmap BlockId . Aeson.parseJSON

instance Aeson.ToJSON ViewId where toJSON = Aeson.toJSON . unpackId
instance Aeson.FromJSON ViewId where parseJSON = fmap ViewId . Aeson.parseJSON

instance Aeson.ToJSON TrackId where toJSON = Aeson.toJSON . unpackId
instance Aeson.FromJSON TrackId where parseJSON = fmap TrackId . Aeson.parseJSON

instance Aeson.ToJSON RulerId where toJSON = Aeson.toJSON . unpackId
instance Aeson.FromJSON RulerId where parseJSON = fmap RulerId . Aeson.parseJSON
