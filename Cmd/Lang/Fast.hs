{-# LANGUAGE PatternGuards #-}
-- | The full haskell interpretation loads a whole bunch of modules and can be
-- slow.  Shortcut a few common commands so they happen quickly.
module Cmd.Lang.Fast where
import qualified Data.Char as Char

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Lang.Global as Global
import qualified Cmd.Lang.LInst as LInst


-- | 'interpret' loads a whole bunch of modules and can be slow.  Shortcut a
-- few common commands so they happen quickly.
fast_interpret :: String -> Maybe (Cmd.CmdT IO String)
fast_interpret text = case lex_all text of
    Nothing -> Nothing
    Just toks -> interpret toks

interpret :: [String] -> Maybe (Cmd.CmdT IO String)
interpret toks = case toks of
        -- Called by logview.
        ["s", str] | Just arg <- val str -> cmd $ Global.s arg
        ["collapse", int] | Just arg <- val int -> cmd $ Global.collapse arg
        ["expand", int] | Just arg <- val int -> cmd $ Global.expand arg

        -- Called by the browser.
        ["load_instrument", str] | Just arg <- val str ->
            cmd $ LInst.load_instrument arg

        -- Misc.
        ["quit"] -> Just quit
        ["save"] -> cmd Global.save
        ["save_as", str] | Just arg <- val str -> cmd $ Global.save_as arg
        ["load", str] | Just arg <- val str -> cmd $ Global.load arg

        ["show_state"] -> Just Global.show_state
        ["show_views", str] | Just arg <- val str ->
            Just $ Global.show_views arg
        ["show_blocks", str] | Just arg <- val str ->
            Just $ Global.show_blocks arg
        _ -> Nothing
    where
    cmd c = Just (c >> return "")
    val :: (Read a) => String -> Maybe a
    val text = case reads text of
        (val, "") : _ -> Just val
        _ -> Nothing

quit :: Cmd.CmdL String
quit = return magic_quit_string

-- | Hack so that language cmds can quit the app, since they return strings.
magic_quit_string :: String
magic_quit_string = "-- * YES, really quit * --"


lex_all :: String -> Maybe [String]
lex_all text
    | null (dropWhile Char.isSpace text) = Just []
    | otherwise = case lex text of
        [] -> Nothing
        (tok, rest) : _ -> do
            toks <- lex_all rest
            return (tok : toks)
