{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- NFData instance
module Ui.Id (
    Namespace, namespace, unsafe_namespace, un_namespace, Id

    -- * construction
    , id, unsafe_id, make

    -- * naming enforcement
    , is_id, is_id_char, is_strict_id, is_strict_id_char, ascii_lower
    , clean_id, enforce_id, enforce_strict_id

    -- * access
    , un_id, id_name, set_name, id_namespace, set_namespace

    -- * read / show
    , read_id, show_id

    -- * Ident
    , Ident(..)
    , show_ident, read_ident
    , ident_string, ident_name, ident_namespace

    -- * constants
    , global, global_namespace
) where
import Prelude hiding (id)
import qualified Control.DeepSeq as DeepSeq
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as Char
import qualified Data.Hashable as Hashable

import qualified System.IO.Unsafe as Unsafe
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.Read as Read

import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize


-- | Type of a project ID.
--
-- It doesn't so much belong in this module, but Ui.Block etc. all use it and
-- it's easier to put it here than make a whole new module.
newtype Namespace = Namespace B.ByteString
    deriving (Eq, Ord, Show, Read, DeepSeq.NFData, Hashable.Hashable)
data Id = Id !Namespace !B.ByteString
    deriving (Eq, Ord, Show, Read)

instance Serialize.Serialize Id where
    put = Serialize.put . un_id
    get = Serialize.get >>= \(a, b) -> return (unsafe_id a b)

instance Serialize.Serialize Namespace where
    put = Serialize.put . un_namespace
    get = Serialize.get >>= \a -> return (unsafe_namespace a)

instance Hashable.Hashable Id where
    hash (Id ns name) = Hashable.hash ns `Hashable.hashWithSalt` name

-- | Create a namespace, if the characters are valid.
namespace :: String -> Maybe Namespace
namespace ns
    | null ns || is_strict_id ns = Just (Namespace (B.pack ns))
    | otherwise = Nothing

-- | Like 'namespace', but will strip and log invalid characters.
unsafe_namespace :: String -> Namespace
unsafe_namespace = Namespace . B.pack . enforce_strict_id_null_ok

un_namespace :: Namespace -> String
un_namespace (Namespace s) = B.unpack s

instance DeepSeq.NFData Id where
    rnf (Id ns ident) = ns `seq` ident `seq` ()

instance Pretty.Pretty Namespace where pretty = un_namespace
instance Pretty.Pretty Id where pretty = show_id

-- * construction

-- | Construct an Id, or return Nothing if there were invalid characters in it.
id :: Namespace -> String -> Maybe Id
id ns ident
    | is_id ident = Just $ Id ns (B.pack ident)
    | otherwise = Nothing

-- | Like 'id', but will strip and log invalid characters.
unsafe_id :: Namespace -> String -> Id
unsafe_id ns ident = Id ns (B.pack (enforce_id ident))

-- | A smarter constructor that only applies the namespace if the string
-- doesn't already have one.
make :: Namespace -> String -> Maybe Id
make default_ns text = case break (=='/') text of
    (ident, "") -> id default_ns ident
    (ns, ident) -> do
        ns <- namespace ns
        id ns (drop 1 ident)

-- | To make naming them in events easier, IDs have a restricted character set.
-- @.@ is allowed so there is a "phrase separator" and @`@ is allowed for
-- symbols, of course.
is_id :: String -> Bool
is_id s = not (null s) && all is_id_char s

is_id_char :: Char -> Bool
is_id_char c = is_strict_id_char c || c == '`' || c == '.'

-- | Many other symbols have an even more restrictive character set.  In
-- addition since they show up as tracklang literals, they must start with
-- a letter, to avoid ambiguity with numbers or other literals.
is_strict_id :: String -> Bool
is_strict_id (c:cs) = ascii_lower c && all is_strict_id_char cs
is_strict_id "" = False

is_strict_id_char :: Char -> Bool
is_strict_id_char c = ascii_lower c || ascii_digit c || c == '-'

ascii_lower :: Char -> Bool
ascii_lower c = 'a' <= c && c <= 'z'

ascii_digit :: Char -> Bool
ascii_digit c = '0' <= c && c <= '9'

clean_id :: Bool -> String -> (String, Maybe String)
clean_id null_ok s
    | null cleaned = ("", Just $
        "identifier consisted entirely of illegal characters: " ++ show s)
    | not null_ok && null s = ("", Just "null identifier")
    | s /= cleaned =
        (cleaned, Just $ "stripped illegal characters from " ++ show s)
    | otherwise = (cleaned, Nothing)
    where
    cleaned = filter is_strict_id_char $ dropWhile (not . ascii_lower) $
        map Char.toLower s

-- | Enforce that a String conforms to the rules for a strict ID.
--
-- Illegal characters are stripped, but it's a runtime error if that results
-- in an empty string.  This is a bit dangerous, but strict IDs should be
-- applied to various tracklang constructs, which are either declared
-- statically in the source, or parsed via "Derive.ParseBs".
--
-- TODO not too happy with unsafe logging and runtime errros
enforce_strict_id :: String -> String
enforce_strict_id s
    | Just warn <- maybe_warn = Unsafe.unsafePerformIO $ do
        Log.warn $ "enforce_strict_id: " ++ warn
        return result
    | otherwise = result
    where (result, maybe_warn) = clean_id False s

enforce_strict_id_null_ok :: String -> String
enforce_strict_id_null_ok s = if null s then s else enforce_strict_id s

-- | More relaxed version of 'enforce_strict_id', used by the various instances
-- of the Ident class.
enforce_id :: String -> String
enforce_id s
    -- It turns out it's pretty inconvenient to throw errors like
    -- 'enforce_strict_id' because they tend to be created directly in code.
    | s /= cleaned = Unsafe.unsafePerformIO $ do
        Log.warn $ "enforce_id: stripped illegal characters from "
            ++ show s
        return cleaned
    | otherwise = cleaned
    where cleaned = filter is_id_char s

-- * access

un_id :: Id -> (Namespace, String)
un_id (Id ns ident) = (ns, B.unpack ident)

id_name :: Id -> String
id_name (Id _ name) = B.unpack name

set_name :: String -> Id -> Id
set_name name (Id ns _) = unsafe_id ns name

id_namespace :: Id -> Namespace
id_namespace (Id ns _) = ns

set_namespace :: Namespace -> Id -> Id
set_namespace ns (Id _ name) = unsafe_id ns (B.unpack name)

-- * read / show

read_id :: String -> Id
read_id s = unsafe_id (unsafe_namespace pre) (drop 1 post)
    where (pre, post) = break (=='/') s

show_id :: Id -> String
show_id (Id ns ident) = Pretty.pretty ns ++ "/" ++ B.unpack ident


-- * Ident

-- | BlockIds, RulerIds, etc. are just wrappers around Ids.  Giving them a
-- consistent display format lets me copy and paste them on the lang socket,
-- which puts the constructors in scope.
class Ident a where
    unpack_id :: a -> Id
    cons_name :: a -> String
    cons :: Id -> a

show_ident :: Ident a => a -> String
show_ident ident = "(" ++ con ++ " " ++ show (show_id id) ++ ")"
    where
    id = unpack_id ident
    con = cons_name ident

read_ident :: Ident a => a -> ReadPrec.ReadPrec a
read_ident witness = do
    Read.Punc "(" <- Read.lexP
    Read.Ident sym <- Read.lexP
    guard (sym == cons_name witness)
    Read.String str <- Read.lexP
    Read.Punc ")" <- Read.lexP
    return (cons (read_id str))

-- | SomethingId -> "ns/name"
ident_string :: (Ident a) => a -> String
ident_string = show_id . unpack_id

-- | SomethingId -> "name"
ident_name :: (Ident a) => a -> String
ident_name = id_name . unpack_id

ident_namespace :: (Ident a) => a -> Namespace
ident_namespace = id_namespace . unpack_id

-- * constants

global :: String -> Id
global = unsafe_id global_namespace

global_namespace :: Namespace
global_namespace = unsafe_namespace ""
