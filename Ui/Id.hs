{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- NFData instance
module Ui.Id (
    Ident(..)

    -- * construction
    , Id, Namespace, id, is_identifier, read_id, show_id
    , read_ident, show_ident

    -- * deconstruction
    , un_id, id_name, id_namespace

    -- * modification
    , set_namespace, set_name

    -- * constants
    , global
) where
import Prelude hiding (id)
import Control.DeepSeq
import Control.Monad
import qualified Data.Char as Char

import qualified Text.Read as Read
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

-- * project

-- | Type of a project ID.
--
-- It doesn't so much belong in this module, but Ui.Block etc. all use it and
-- it's easier to put it here than make a whole new module.
type Namespace = String
newtype Id = Id (Namespace, String)
    deriving (Eq, Ord, Show, Read, NFData)
un_id (Id ident) = ident

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

-- | Construct an Id.  Non-identifier characters are stripped out.
id :: Namespace -> String -> Id
id ns ident = Id (filter is_identifier ns, filter is_identifier ident)

-- | To make naming them in events easier, IDs and namespaces have a restricted
-- character set.
is_identifier :: Char -> Bool
is_identifier c = Char.isAlphaNum c || c `elem` "-_."

-- For display convenience, IDs have a string display format.

read_id :: String -> Id
read_id s = let (pre, post) = break (=='/') s in (id pre (drop 1 post))

show_id :: Id -> String
show_id (Id (ns, ident)) = ns ++ "/" ++ ident

id_name (Id (_, name)) = name
id_namespace (Id (ns, _)) = ns

set_name name (Id (ns, _)) = id ns name
set_namespace ns (Id (_, name)) = id ns name

global :: String -> Id
global = id global_namespace

global_namespace :: Namespace
global_namespace = ""
