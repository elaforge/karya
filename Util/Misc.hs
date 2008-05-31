module Util.Misc where

-- These are created by test/hspp to give caller file and line number to
-- a function.
-- By convention, functions whose first argument is a SrcPos end with _srcpos.
type SrcPos = Maybe (String, Maybe String, Int) -- (file, func_name, lineno)
show_srcpos :: SrcPos -> String
show_srcpos Nothing = "<no_srcpos> "
show_srcpos (Just (file, func_name, line)) = file ++ ":" ++ show line
    ++ " " ++ maybe "" (\s -> "[" ++ s ++ "]") func_name ++ " "
