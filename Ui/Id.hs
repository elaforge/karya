-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Ui.Id (
    Id, Namespace, id, namespace

    -- * access
    , un_id, un_namespace, id_name, id_namespace, set_namespace
    , set_name, modify_name

    -- * read / show
    , read_id, show_id, read_short, read_short_validate, show_short

    -- * validate
    , valid, is_id_char, is_lower_alpha, is_digit

    -- * Ident
    , Ident(..)
    , show_ident, read_ident
    , ident_string, ident_text, ident_name, ident_namespace

    -- * constants
    , global, global_namespace

    -- * instances
    , BlockId(..), ViewId(..), TrackId(..), RulerId(..)
) where
import qualified Prelude
import Prelude hiding (id)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Text as Text
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.Read as Read

import Util.Crc32Instances () -- Text instance
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize

import Global


-- | IDs come in two parts, a namespace and a name.
--
-- This is so so that you can merge two scores together and not have their IDs
-- clash.  Since block calls within a score will generally leave the namespace
-- implicit, the merged score should still be playable.
data Id = Id !Namespace !Text
    deriving (Eq, Ord, Show, Read)

-- | The Namespace should pass 'valid', but is guaranteed to not contain \/s.
-- This is because the git backend uses the namespace for a directory name.
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

instance Pretty.Pretty Namespace where pretty = untxt . un_namespace
instance Pretty.Pretty Id where
    pretty = untxt . show_id
    prettyt = show_id

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
read_short default_ns = fst . read_short_validate default_ns

-- | 'read_short' but also return if the namespace and ident passed 'valid'.
read_short_validate :: Namespace -> Text -> (Id, Bool)
read_short_validate default_ns text = case Text.breakOn "/" text of
    (ident, "") -> (id default_ns ident, valid ident)
    (ns, ident) -> (id (namespace ns) (Text.drop 1 ident),
        valid ns && valid (Text.drop 1 ident))

-- | The inverse of 'read_short'.
show_short :: Namespace -> Id -> Text
show_short default_ns ident@(Id ns name)
    | default_ns == ns = name
    | otherwise = show_id ident

-- * validate

-- | True if this Namespace or Id name is parseable as a tracklang literal.
-- You probably want to insist on this when creating new Ids to ensure they
-- can be easily called from the track.
valid :: Text -> Bool
valid s =
    not (Text.null s) && is_lower_alpha (Text.head s) && Text.all is_id_char s

is_id_char :: Char -> Bool
is_id_char c = is_lower_alpha c || is_digit c || c == '-' || c == '.'

is_lower_alpha :: Char -> Bool
is_lower_alpha c = 'a' <= c && c <= 'z'

is_digit :: Char -> Bool
is_digit c = '0' <= c && c <= '9'

-- * Ident

-- | BlockIds, RulerIds, etc. are just wrappers around Ids.  Giving them a
-- consistent display format lets me copy and paste them on the repl socket,
-- which puts the constructors in scope.
class Ident a where
    unpack_id :: a -> Id
    constructor_name :: Proxy a -> String
    make :: Id -> a

instance Ident Id where
    unpack_id = Prelude.id
    constructor_name _ = "id"
    make = Prelude.id

show_ident :: forall a. Ident a => a -> String
show_ident ident = "(" ++ con ++ " " ++ show (show_id id) ++ ")"
    where
    id = unpack_id ident
    con = constructor_name (Proxy :: Proxy a)

read_ident :: forall a. Ident a => ReadPrec.ReadPrec a
read_ident = do
    Read.Punc "(" <- Read.lexP
    Read.Ident sym <- Read.lexP
    guard (sym == constructor_name (Proxy :: Proxy a))
    Read.String str <- Read.lexP
    Read.Punc ")" <- Read.lexP
    return (make (read_id (txt str)))

-- | SomethingId -> "ns/name"
ident_string :: Ident a => a -> String
ident_string = untxt . show_id . unpack_id

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

instance Pretty.Pretty BlockId where pretty = show
instance Pretty.Pretty ViewId where pretty = show
instance Pretty.Pretty TrackId where pretty = show
instance Pretty.Pretty RulerId where pretty = show

instance Read BlockId where readPrec = read_ident
instance Read ViewId where readPrec = read_ident
instance Read TrackId where readPrec = read_ident
instance Read RulerId where readPrec = read_ident

instance Ident BlockId where
    unpack_id (BlockId a) = a
    constructor_name _ = "bid"
    make = BlockId
instance Ident ViewId where
    unpack_id (ViewId a) = a
    constructor_name _ = "vid"
    make = ViewId
instance Ident TrackId where
    unpack_id (TrackId a) = a
    constructor_name _ = "tid"
    make = TrackId
instance Ident RulerId where
    unpack_id (RulerId a) = a
    constructor_name _ = "rid"
    make = RulerId
