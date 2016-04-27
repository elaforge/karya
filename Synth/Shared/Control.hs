-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Synth.Shared.Control where
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.String as String

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import Global


newtype Control = Control Text
    deriving (Eq, Ord, Show, String.IsString, Aeson.ToJSON, Aeson.FromJSON,
        Serialize.Serialize, Pretty.Pretty)

instance Aeson.ToJSON a => Aeson.ToJSON (Map.Map Control a) where
    toJSON = Aeson.toJSON . Map.fromAscList . map (first (\(Control a) -> a))
        . Map.toAscList
instance Aeson.FromJSON a => Aeson.FromJSON (Map.Map Control a) where
    parseJSON = fmap (Map.fromAscList . map (first Control) . Map.toAscList)
        . Aeson.parseJSON

envelope :: Control
envelope = "envelope"

pitch :: Control
pitch = "pitch"
