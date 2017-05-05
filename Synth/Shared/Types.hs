{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Synth.Shared.Types where
import qualified Data.Aeson as Aeson
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import Global

-- | Unique identifier for a patch.
type PatchName = Text

newtype Attributes = Attributes (Set Text)
    deriving (Eq, Ord, Show, Pretty.Pretty, Serialize.Serialize,
        Aeson.FromJSON, Aeson.ToJSON, Monoid.Monoid)

attribute :: Text -> Attributes
attribute = Attributes . Set.singleton
