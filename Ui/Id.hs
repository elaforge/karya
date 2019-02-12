-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Id (
    Id, Namespace, id, namespace

    -- * access
    , un_id, un_namespace, id_name, id_namespace, set_namespace
    , set_name, modify_name

    -- * read / show
    , read_id, show_id, read_short, show_short

    -- * validate
    , valid_symbol, symbol_description, is_id_char

    -- * Ident
    , Ident(..)
    , show_ident, read_ident
    , ident_text, ident_name, ident_namespace

    -- * constants
    , global, global_namespace

    -- * instances
    , BlockId(..), ViewId(..), TrackId(..), RulerId(..)
) where
import qualified Prelude
import           Prelude hiding (id)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Text as Text

import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.Read as Read

import           Util.Crc32Instances () -- Text instance
import qualified Util.Serialize as Serialize

import           Global


-- | IDs come in two parts, a namespace and a name.
--
-- This is so so that you can merge two scores together and not have their IDs
-- clash.  Since block calls within a score will generally leave the namespace
-- implicit, the merged score should still be playable.
data Id = Id !Namespace !Text
    deriving (Eq, Ord, Show, Read)

instance Aeson.ToJSON Id where
    toJSON = Aeson.String . ident_text
instance Aeson.FromJSON Id where
    parseJSON (Aeson.String a) = pure $ read_id a
    parseJSON _ = fail "expecting String"

-- | The Namespace should pass 'valid_symbol', and is guaranteed to not contain
-- \/s.  This is because the git backend uses the namespace for a directory
-- name.
newtype Namespace = Namespace Text
    deriving (Eq, Ord, Show, Read, DeepSeq.NFData, CRC32.CRC32,
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

instance CRC32.CRC32 Id where
    crc32Update n (Id ns name) =
        n `CRC32.crc32Update` ns `CRC32.crc32Update` name

instance Pretty Namespace where pretty = un_namespace
instance Pretty Id where pretty = show_id

instance DeepSeq.NFData Id where
    rnf (Id ns name) = ns `seq` name `seq` ()

-- * access

un_id :: Id -> (Namespace, Text)
un_id (Id ns ident) = (ns, ident)

id_name :: Id -> Text
id_name (Id _ name) = name

id_namespace :: Id -> Namespace
id_namespace (Id ns _) = ns

set_namespace :: Namespace -> Id -> Id
set_namespace ns (Id _ name) = Id ns name

set_name :: Text -> Id -> Id
set_name name (Id ns _) = id ns name

modify_name :: (Text -> Text) -> Id -> Id
modify_name modify (Id ns name) = id ns (modify name)

un_namespace :: Namespace -> Text
un_namespace (Namespace s) = s

-- * read / show

read_id :: Text -> Id
read_id s = id (namespace pre) (Text.drop 1 post)
    where (pre, post) = Text.breakOn "/" s

show_id :: Id -> Text
show_id (Id (Namespace ns) ident) = ns <> "/" <> ident

-- | A smarter constructor that only applies the namespace if the string
-- doesn't already have one.
read_short :: Namespace -> Text -> Id
read_short default_ns text = case Text.breakOn "/" text of
    (ident, "") -> id default_ns ident
    (ns, ident) -> id (namespace ns) (Text.drop 1 ident)

-- | The inverse of 'read_short'.
show_short :: Namespace -> Id -> Text
show_short default_ns ident@(Id ns name)
    | default_ns == ns = name
    | otherwise = show_id ident

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
valid_symbol :: Text -> Bool
valid_symbol s = not (Text.null s) && ascii_lower_alpha (Text.head s)
    && Text.all is_id_char s

valid_id :: Id -> Maybe Id
valid_id (Id (Namespace ns) ident)
    | valid_symbol ns && valid_symbol ident = Just $ Id (Namespace ns) ident
    | otherwise = Nothing

-- | 'BlockId's are more lenient, only spaces are forbidden.
valid_block_id :: Id -> Maybe Id
valid_block_id (Id (Namespace ns) ident)
    | valid_symbol ns && not (" " `Text.isInfixOf` ident) =
        Just $ Id (Namespace ns) ident
    | otherwise = Nothing

-- | Describe a valid identifier for docs and error messages.
symbol_description :: Text
symbol_description = "[a-z][a-z0-9.-]*"

-- | This defines the set of valid characters allowed in an ID.
is_id_char :: Char -> Bool
is_id_char c = ascii_lower_alpha c || ascii_digit c || c == '-' || c == '.'

ascii_lower_alpha :: Char -> Bool
ascii_lower_alpha c = 'a' <= c && c <= 'z'

ascii_digit :: Char -> Bool
ascii_digit c = '0' <= c && c <= '9'

-- * Ident

-- | BlockIds, RulerIds, etc. are just wrappers around Ids.  Giving them a
-- consistent display format lets me copy and paste them on the repl socket,
-- which puts the constructors in scope.
class Ident a where
    unpack_id :: a -> Id
    constructor_name :: Proxy a -> String
    make :: Id -> Maybe a

instance Ident Id where
    unpack_id = Prelude.id
    constructor_name _ = "id"
    make = Just

show_ident :: forall a. Ident a => a -> String
show_ident ident = "(" ++ con ++ " " ++ show (show_id id) ++ ")"
    where
    id = unpack_id ident
    con = constructor_name (Proxy :: Proxy a)

read_ident :: forall a. Ident a => ReadPrec.ReadPrec (Maybe a)
read_ident = do
    Read.Punc "(" <- Read.lexP
    Read.Ident sym <- Read.lexP
    guard (sym == constructor_name (Proxy :: Proxy a))
    Read.String str <- Read.lexP
    Read.Punc ")" <- Read.lexP
    return $ make (read_id (txt str))

-- | SomethingId -> "ns/name"
ident_text :: Ident a => a -> Text
ident_text = show_id . unpack_id

-- | SomethingId -> "name"
ident_name :: Ident a => a -> Text
ident_name = id_name . unpack_id

ident_namespace :: Ident a => a -> Namespace
ident_namespace = id_namespace . unpack_id

-- * constants

global :: Text -> Id
global = id global_namespace

global_namespace :: Namespace
global_namespace = namespace ""


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
    deriving (Eq, Ord, DeepSeq.NFData, Serialize.Serialize, CRC32.CRC32)

-- | Reference to a View, as per 'BlockId'.
newtype ViewId = ViewId Id
    deriving (Eq, Ord, DeepSeq.NFData, Serialize.Serialize, CRC32.CRC32)

newtype TrackId = TrackId Id
    deriving (Eq, Ord, DeepSeq.NFData, Serialize.Serialize, CRC32.CRC32)

newtype RulerId = RulerId Id
    deriving (Eq, Ord, DeepSeq.NFData, Serialize.Serialize, CRC32.CRC32)

instance Show BlockId where show = show_ident
instance Show ViewId where show = show_ident
instance Show TrackId where show = show_ident
instance Show RulerId where show = show_ident

instance Pretty BlockId where pretty = showt
instance Pretty ViewId where pretty = showt
instance Pretty TrackId where pretty = showt
instance Pretty RulerId where pretty = showt

instance Read BlockId where readPrec = require read_ident
instance Read ViewId where readPrec = require read_ident
instance Read TrackId where readPrec = require read_ident
instance Read RulerId where readPrec = require read_ident

require :: Read.ReadPrec (Maybe a) -> Read.ReadPrec a
require = (maybe Read.pfail return =<<)

instance Ident BlockId where
    unpack_id (BlockId a) = a
    constructor_name _ = "bid"
    make = fmap BlockId . valid_block_id
instance Ident ViewId where
    unpack_id (ViewId a) = a
    constructor_name _ = "vid"
    make = fmap ViewId . valid_id
instance Ident TrackId where
    unpack_id (TrackId a) = a
    constructor_name _ = "tid"
    make = fmap TrackId . valid_id
instance Ident RulerId where
    unpack_id (RulerId a) = a
    constructor_name _ = "rid"
    make = fmap RulerId . valid_id

instance Aeson.ToJSON BlockId where toJSON = Aeson.toJSON . unpack_id
instance Aeson.FromJSON BlockId where parseJSON = fmap BlockId . Aeson.parseJSON

instance Aeson.ToJSON ViewId where toJSON = Aeson.toJSON . unpack_id
instance Aeson.FromJSON ViewId where parseJSON = fmap ViewId . Aeson.parseJSON

instance Aeson.ToJSON TrackId where toJSON = Aeson.toJSON . unpack_id
instance Aeson.FromJSON TrackId where parseJSON = fmap TrackId . Aeson.parseJSON

instance Aeson.ToJSON RulerId where toJSON = Aeson.toJSON . unpack_id
instance Aeson.FromJSON RulerId where parseJSON = fmap RulerId . Aeson.parseJSON
