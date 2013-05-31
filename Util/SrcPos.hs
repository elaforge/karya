-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.SrcPos where

-- | These are created by tools/hspp to give caller file and line number to
-- a function.
-- By convention, functions whose first argument is a SrcPos end with _srcpos.
--
-- It's a tuple so you can construct one without importing anything.
-- @(file, func_name, lineno)@
type SrcPos = Maybe (String, Maybe String, Int)

srcpos_file (file, _, _) = file
srcpos_func (_, func, _) = func
srcpos_lineno (_, _, line) = line

show_srcpos :: SrcPos -> String
show_srcpos Nothing = "<no_srcpos>"
show_srcpos (Just (file, func_name, line)) = file ++ ":" ++ show line
    ++ " " ++ maybe "" (\s -> "[" ++ s ++ "]") func_name
