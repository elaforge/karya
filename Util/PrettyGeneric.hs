-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
-- | Functions to convert a 'G.Generic' data type to a 'Value' and pretty
-- print a 'Value'.
--
-- TODO Value could also be used for a generic diff.
module Util.PrettyGeneric (
    Value(..), Constructor(..), Field(..)
    , Extract(..), makePretty
) where
import qualified Data.Text as Text
import Data.Text (Text)
import qualified GHC.Generics as G
import GHC.Generics ((:+:), (:*:)((:*:)), S1, D1, C1, Rec0)

import qualified Util.Pretty as Pretty


data Value = Prim !String | Data !Constructor
    deriving (Show)
-- | Constructor name [field].
data Constructor = Constructor !String ![Field]
    deriving (Show)
-- | Field name value.  The name is \"\" for unnamed fields.
data Field = Field {
    fieldName :: !String
    , fieldValue :: !Value
    } deriving (Show)

makePretty :: (Text -> Text) -- ^ applied to field labels, to strip prefixes
    -> Value -> Pretty.Doc
makePretty _ (Prim t) = Pretty.text (Text.pack t)
makePretty stripField (Data (Constructor name fields))
    | any (not . null . fieldName) fields =
        Pretty.record (Pretty.string name)
            [ (stripField $ Text.pack name, makePretty stripField val)
            | Field name val <- fields
            ]
    | otherwise = Pretty.constructor (Text.pack name)
        (map (makePretty stripField . fieldValue) fields)

-- * extract

class Extract a where
    extract :: a -> Value
    default extract :: (G.Generic a, ExtractData (G.Rep a)) => a -> Value
    extract = extractData . G.from

{-
    In theory the M1 metadata wrappers can come in order, but in practice we
    get a kind of recursive grammar:

    S1           --> D1 (C1 U1)
    S2 42        --> D1 (C1 (S1 NoSelector (Rec0 Int)))
    S3 42 True   --> D1 (C1 (S1 No (Rec0 Int) :*: S1 No (Rec0 Bool)))
    True         --> D1 (C1 U1 :+: C1 U1)
    T1 40 42     --> D1 (C1 (S1 (Rec0 Int) :*: S1 (Rec0 Int)) :+: C1 (...))

    data        = D1 constructor
    constructor = (constructor :+: constructor) | C1 fields
    fields      = (fields :*: fields) | U1 | S1 (Rec0 val)
    val         = Prim | Data data
-}

class ExtractData f where
    extractData :: f x -> Value
class ExtractConstructor f where
    extractConstructor :: f x -> Constructor
class ExtractFields f where
    extractFields :: f x -> [Field]

instance ExtractConstructor f => ExtractData (D1 t f) where
    extractData m = Data $ extractConstructor (G.unM1 m)

instance (ExtractConstructor f, ExtractConstructor g) =>
        ExtractConstructor (f :+: g) where
    extractConstructor (G.L1 x) = extractConstructor x
    extractConstructor (G.R1 x) = extractConstructor x
instance (ExtractFields f, G.Constructor t) => ExtractConstructor (C1 t f) where
    extractConstructor m = Constructor (G.conName m) (extractFields (G.unM1 m))

instance (ExtractFields f, ExtractFields g) => ExtractFields (f :*: g) where
    extractFields (f :*: g) = extractFields f ++ extractFields g
instance ExtractFields G.U1 where
    extractFields G.U1 = []

instance (Extract val, G.Selector t) => ExtractFields (S1 t (Rec0 val)) where
    extractFields m@(G.M1 (G.K1 val)) = [Field (G.selName m) (extract val)]

instance Extract () where extract = Prim . show
instance Extract Bool where extract = Prim . show
instance Extract Int where extract = Prim . show
instance Extract String where extract = Prim . show
instance Extract Char where extract = Prim . show
