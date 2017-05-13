-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Synth.Shared.Control where
import qualified Data.Aeson as Aeson
import qualified Data.String as String

import qualified Util.Serialize as Serialize
import Global


newtype Control = Control Text
    deriving (Eq, Ord, Show, String.IsString, Aeson.ToJSON, Aeson.FromJSON,
        Aeson.ToJSONKey, Aeson.FromJSONKey, Serialize.Serialize, Pretty)

envelope :: Control
envelope = "envelope"

pitch :: Control
pitch = "pitch"
