-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- NFData instance
module Ui.Id (
    Id, Namespace, id, namespace

    -- * access
    , un_id, un_namespace, id_name, id_namespace, set_namespace, set_name

    -- * read / show
    , read_id, show_id, read_short, show_short

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
import qualified Data.ByteString.Char8 as B
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.Read as Read

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize


-- | IDs come in two parts, a namespace and a name.
--
-- This is so so that you can merge two scores together and not have their IDs
-- clash.  Since block calls within a score will generally leave the namespace
-- implicit, the merged score should still be playable.
--
-- TODO the ByteString is historical, it should be Text but it's a pain to
-- change.
data Id = Id !Namespace !B.ByteString
    deriving (Eq, Ord, Show, Read)

-- | The Namespace should pass 'valid', but is guaranteed to not contain \/s.
-- This is because the git backend uses the namespace for a directory name.
newtype Namespace = Namespace B.ByteString
    deriving (Eq, Ord, Show, Read, DeepSeq.NFData, CRC32.CRC32)

to_text :: B.ByteString -> Text
to_text = Encoding.decodeUtf8

-- | Convert from Text and convert @/@.  This is because @/@ is used to
-- separate namespace and ident.
from_text :: Text -> B.ByteString
from_text = Encoding.encodeUtf8 . Text.map (\c -> if c == '/' then '-' else c)

id :: Namespace -> Text -> Id
id ns = Id ns . from_text

namespace :: Text -> Namespace
namespace = Namespace . from_text

instance Serialize.Serialize Id where
    put (Id ns ident) = Serialize.put val
        where
        val :: (Namespace, String)
        val = (ns, B.unpack ident)
    get = do
        (ns, ident) <- Serialize.get
        return $ Id ns (B.pack ident)
    -- TODO These have inefficient put and get due to historical accident.
    -- I should fix it some day, but it breaks all saves.  And as long as I'm
    -- going to do that, I might as well switch from ByteString to Text.
    -- put (Id a b) = Serialize.put a >> Serialize.put b
    -- get = Id <$> Serialize.get <*> Serialize.get

instance Serialize.Serialize Namespace where
    put (Namespace a) = Serialize.put (B.unpack a)
    get = Namespace . B.pack <$> Serialize.get
    -- TODO These have inefficient put and get due to historical accident.
    -- I should fix it some day, but it breaks all saves.  And as long as I'm
    -- going to do that, I might as well switch from ByteString to Text.
    -- put (Namespace a) = Serialize.put a
    -- get = Namespace <$> Serialize.get

instance CRC32.CRC32 Id where
    crc32Update n (Id ns name) =
        n `CRC32.crc32Update` ns `CRC32.crc32Update` name

instance Pretty.Pretty Namespace where pretty = untxt . un_namespace
instance Pretty.Pretty Id where pretty = show_id

instance DeepSeq.NFData Id where
    rnf (Id ns name) = ns `seq` name `seq` ()

-- * access

un_id :: Id -> (Namespace, Text)
un_id (Id ns ident) = (ns, to_text ident)

id_name :: Id -> Text
id_name (Id _ name) = to_text name

id_namespace :: Id -> Namespace
id_namespace (Id ns _) = ns

set_namespace :: Namespace -> Id -> Id
set_namespace ns (Id _ name) = Id ns name

set_name :: Text -> Id -> Id
set_name name (Id ns _) = id ns name

un_namespace :: Namespace -> Text
un_namespace (Namespace s) = to_text s

-- * read / show

read_id :: Text -> Id
read_id s = id (namespace pre) (Text.drop 1 post)
    where (pre, post) = Text.breakOn "/" s

show_id :: Id -> String
show_id (Id ns ident) = pretty ns ++ "/" ++ B.unpack ident

-- | A smarter constructor that only applies the namespace if the string
-- doesn't already have one.
read_short :: Namespace -> Text -> Id
read_short default_ns text = case Text.breakOn "/" text of
    (ident, "") -> id default_ns ident
    (ns, ident) -> id (namespace ns) (Text.drop 1 ident)

-- | The inverse of 'read_short'.
show_short :: Namespace -> Id -> String
show_short default_ns ident@(Id ns name)
    | default_ns == ns = B.unpack name
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
    constructor_name :: a -> String
    make :: Id -> a

instance Ident Id where
    unpack_id = Prelude.id
    constructor_name _ = "id"
    make = Prelude.id

show_ident :: Ident a => a -> String
show_ident ident = "(" ++ con ++ " " ++ show (show_id id) ++ ")"
    where
    id = unpack_id ident
    con = constructor_name ident

read_ident :: Ident a => a -> ReadPrec.ReadPrec a
read_ident witness = do
    Read.Punc "(" <- Read.lexP
    Read.Ident sym <- Read.lexP
    guard (sym == constructor_name witness)
    Read.String str <- Read.lexP
    Read.Punc ")" <- Read.lexP
    return (make (read_id (txt str)))

-- | SomethingId -> "ns/name"
ident_string :: (Ident a) => a -> String
ident_string = show_id . unpack_id

ident_text :: (Ident a) => a -> Text
ident_text = txt . show_id . unpack_id

-- | SomethingId -> "name"
ident_name :: (Ident a) => a -> Text
ident_name = id_name . unpack_id

ident_namespace :: (Ident a) => a -> Namespace
ident_namespace = id_namespace . unpack_id

-- * constants

global :: Text -> Id
global = id global_namespace

global_namespace :: Namespace
global_namespace = namespace ""


-- * instances

-- | Reference to a Block.  Use this to look up Blocks in the State.
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

instance Read BlockId where readPrec = read_ident undefined
instance Read ViewId where readPrec = read_ident undefined
instance Read TrackId where readPrec = read_ident undefined
instance Read RulerId where readPrec = read_ident undefined

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
