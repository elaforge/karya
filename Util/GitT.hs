-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Util.GitT where
import qualified Data.ByteString as ByteString

import qualified Util.Pretty as Pretty


newtype Commit = Commit ByteString.ByteString
    deriving (Eq, Ord, Show, Pretty.Pretty)
type Repo = FilePath
