-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Compatibility shim.
module Util.Locale where

#if GHC_VERSION < 71000

import qualified System.Locale as Locale

defaultTimeLocale :: Locale.TimeLocale
defaultTimeLocale = Locale.defaultTimeLocale

#else

import qualified Data.Time as Time

defaultTimeLocale :: Time.TimeLocale
defaultTimeLocale = Time.defaultTimeLocale

#endif
