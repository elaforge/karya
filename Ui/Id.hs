{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Ui.Id (
    Ident(..)

    -- * construction
    , Id, Namespace, id, read_id, show_id

    -- * deconstruction
    , un_id, id_name, id_namespace

    -- * modification
    , set_namespace, set_name

    -- * constants
    , global
) where
import Prelude hiding (id)
import qualified Data.Generics as Generics

-- import qualified Text.Read as Read
-- import qualified Text.ParserCombinators.ReadPrec as ReadPrec
-- import qualified Text.ParserCombinators.ReadP as ReadP

-- * project

-- | Type of a project ID.
--
-- It doesn't so much belong in this module, but Ui.Block etc. all use it and
-- it's easier to put it here than make a whole new module.
type Namespace = String
newtype Id = Id (Namespace, String)
    deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)
un_id (Id ident) = ident

-- | BlockIds, RulerIds, etc. are just wrappers around Ids.  Giving them a
-- consistent display format lets me copy and paste them on the lang socket,
-- which puts the constructors in scope.
class Ident a where
    show_ident :: a -> String
    show_ident ident = "(" ++ con ++ " " ++ show (show_id id) ++ ")"
        where
        id = unpack_id ident
        con = id_con ident

    unpack_id :: a -> Id
    id_con :: a -> String

{-
instance Show Id where
    show (Id (ns, name)) = "<" ++ ns ++ "/" ++ name ++ ">"
instance Read.Read Id where
    readPrec = ReadPrec.lift read_id
    readListPrec = Read.readListPrecDefault

read_id = ReadP.between (ReadP.char '<') (ReadP.char '>') $ do
    ns <- ReadP.munch (/= '/')
    ReadP.char '/'
    name <- ReadP.munch (/= '>')
    return (id ns name)
-}

-- | Construct an Id.
--
-- Sine the Namespace is used as filename, I strip /s and use that as the
-- delimiter.
id :: Namespace -> String -> Id
id ns ident = Id (filter (/= '/') ns, ident)

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
