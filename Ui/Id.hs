{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- NFData instance
module Ui.Id (
    Namespace, namespace, un_namespace, Id

    -- * construction
    , id, make

    -- * naming enforcement
    , is_id, is_id_char, is_strict_id, is_strict_id_char, ascii_lower
    , enforce_id, enforce_strict_id

    -- * access
    , un_id, id_name, set_name, id_namespace, set_namespace

    -- * read / show
    , read_id, show_id

    -- * Ident
    , Ident(..)
    , show_ident, read_ident
    , ident_string, ident_name

    -- * constants
    , global
) where
import Prelude hiding (id)
import qualified Control.DeepSeq as DeepSeq
import Control.Monad
import qualified System.IO.Unsafe as Unsafe
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.Read as Read

import qualified Util.Log as Log
import qualified Util.Pretty as Pretty


-- | Type of a project ID.
--
-- It doesn't so much belong in this module, but Ui.Block etc. all use it and
-- it's easier to put it here than make a whole new module.
newtype Namespace = Namespace String
    deriving (Eq, Ord, Show, Read, DeepSeq.NFData)
data Id = Id !Namespace !String
    deriving (Eq, Ord, Show, Read)

namespace :: String -> Namespace
namespace = Namespace . enforce_strict_id_null_ok

un_namespace :: Namespace -> String
un_namespace (Namespace s) = s

instance DeepSeq.NFData Id where
    rnf (Id ns ident) = ns `seq` ident `seq` ()

instance Pretty.Pretty Namespace where pretty = un_namespace
instance Pretty.Pretty Id where pretty = show_id

-- * construction

-- | Construct an Id.  Non-identifier characters are stripped out.
id :: Namespace -> String -> Id
id ns ident = Id ns (enforce_id ident)

-- | A smarter constructor that only applies the namespace if the string
-- doesn't already have one.
make :: Namespace -> String -> Id
make default_ns text = id ns ident
    where
    (w0, w1) = break (=='/') text
    (ns, ident) = if null w1 then (default_ns, w0)
        else (namespace w0, drop 1 w1)

-- | To make naming them in events easier, IDs have a restricted character set.
-- @.@ is allowed so there is a "phrase separator" and @`@ is allowed for
-- symbols, of course.
is_id_char :: Char -> Bool
is_id_char c = is_strict_id_char c || c == '`' || c == '.'

is_id :: String -> Bool
is_id (c:cs) = ascii_lower c && all is_id_char cs
is_id "" = False

-- | Many other symbols have an even more restrictive character set.
is_strict_id_char :: Char -> Bool
is_strict_id_char c = ascii_lower c || c >= '0' && c <= '9' || c == '-'

is_strict_id :: String -> Bool
is_strict_id (c:cs) = ascii_lower c && all is_strict_id_char cs
is_strict_id "" = False

ascii_lower :: Char -> Bool
ascii_lower c = c >= 'a' && c <= 'z'

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
    | null s = error "enforce_strict_id: null identifier"
    | null cleaned = error $ "enforce_strict_id: identifier consisted "
        ++ "entirely of illegal characters: " ++ show s
    | s /= cleaned = Unsafe.unsafePerformIO $ do
        Log.warn $ "enforce_strict_id: stripped illegal characters from "
            ++ show s
        return cleaned
    | otherwise = cleaned
    where cleaned = filter is_strict_id_char $ dropWhile (not . ascii_lower) s

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
    where cleaned = filter is_id_char $ dropWhile (not . ascii_lower) s

-- * access

un_id :: Id -> (Namespace, String)
un_id (Id ns ident) = (ns, ident)

id_name :: Id -> String
id_name (Id _ name) = name

set_name :: String -> Id -> Id
set_name name (Id ns _) = id ns name

id_namespace :: Id -> Namespace
id_namespace (Id ns _) = ns

set_namespace :: Namespace -> Id -> Id
set_namespace ns (Id _ name) = id ns name

-- * read / show

read_id :: String -> Id
read_id s = id (namespace pre) (drop 1 post)
    where (pre, post) = break (=='/') s

show_id :: Id -> String
show_id (Id ns ident) = Pretty.pretty ns ++ "/" ++ ident


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

-- * constants

global :: String -> Id
global = id global_namespace

global_namespace :: Namespace
global_namespace = namespace ""
