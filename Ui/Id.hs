{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- NFData instance
module Ui.Id (
    Namespace, Id

    -- * construction
    , id, make, is_identifier

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
import Control.DeepSeq
import Control.Monad
import qualified Data.Char as Char

import qualified Text.Read as Read
import qualified Text.ParserCombinators.ReadPrec as ReadPrec


-- | Type of a project ID.
--
-- It doesn't so much belong in this module, but Ui.Block etc. all use it and
-- it's easier to put it here than make a whole new module.
type Namespace = String
newtype Id = Id (Namespace, String)
    deriving (Eq, Ord, Show, Read, NFData)

-- * construction

-- | Construct an Id.  Non-identifier characters are stripped out.
id :: Namespace -> String -> Id
id ns ident = Id (filter is_identifier ns, filter is_identifier ident)

-- | A smarter constructor that only applies the namespace if the string
-- doesn't already have one.
make :: Namespace -> String -> Id
make default_ns text = id ns ident
    where
    (w0, w1) = break (=='/') text
    (ns, ident) = if null w1 then (default_ns, w0) else (w0, drop 1 w1)

-- | To make naming them in events easier, IDs and namespaces have
-- a restricted character set.
is_identifier :: Char -> Bool
is_identifier c = Char.isAlphaNum c || c `elem` "-_."

-- * access

un_id :: Id -> (Namespace, String)
un_id (Id ident) = ident

id_name :: Id -> String
id_name (Id (_, name)) = name

set_name :: String -> Id -> Id
set_name name (Id (ns, _)) = id ns name

id_namespace :: Id -> Namespace
id_namespace (Id (ns, _)) = ns

set_namespace :: Namespace -> Id -> Id
set_namespace ns (Id (_, name)) = id ns name

-- * read / show

read_id :: String -> Id
read_id s = let (pre, post) = break (=='/') s in (id pre (drop 1 post))

show_id :: Id -> String
show_id (Id (ns, ident)) = ns ++ "/" ++ ident


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
global_namespace = ""
