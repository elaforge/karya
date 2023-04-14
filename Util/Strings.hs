-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Strings (lstrip, rstrip, strip) where
import qualified Data.Char as Char
import qualified Data.List as List


lstrip, rstrip, strip :: String -> String
lstrip = dropWhile Char.isSpace
rstrip = List.dropWhileEnd Char.isSpace
strip = rstrip . lstrip
