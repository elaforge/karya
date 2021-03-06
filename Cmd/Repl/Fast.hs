-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | The full haskell interpretation loads a whole bunch of modules and can be
-- slow.  Shortcut a few common commands so they happen quickly.
module Cmd.Repl.Fast where
import qualified Data.Char as Char

import qualified Util.Then as Then
import qualified Ui.Ui as Ui
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Repl.Global as Global
import qualified Cmd.Repl.LDebug as LDebug
import qualified Cmd.Repl.LInst as LInst
import qualified Cmd.Repl.LState as LState

import qualified App.ReplProtocol as ReplProtocol


-- | 'interpret' loads a whole bunch of modules and can be slow.  Shortcut a
-- few common commands so they happen quickly.
fast_interpret :: String -> Maybe (Cmd.CmdT IO ReplProtocol.CmdResult)
fast_interpret text = case lex_all text of
    Nothing -> Nothing
    Just tokens -> interpret tokens

interpret :: [String] -> Maybe (Cmd.CmdT IO ReplProtocol.CmdResult)
interpret tokens = case tokens of
        -- Called by logview.
        ["s", str] | Just arg <- val str -> action $ Global.s arg
        ["collapse", int] | Just arg <- val int -> action $ Global.collapse arg
        ["expand", int] | Just arg <- val int -> action $ Global.expand arg

        -- Called by the browser.
        ["load_instrument", str] | Just arg <- val str ->
            action $ LInst.load arg

        -- Called manually via the REPL.

        -- Make blocks and views.
        ["LState.rename", a1]
            | Just v1 <- val a1 -> action $ LState.rename v1
        ["Create.view", str] | Just arg <- val str -> action $ Create.view arg

        -- Misc.
        ["quit"] -> action Global.quit
        ["save"] -> action Global.save
        ["save_state_as", str] | Just arg <- val str ->
            action $ Global.save_state_as arg
        ["write_state", str] | Just arg <- val str ->
            action $ Global.write_state arg
        ["save_git_as", str] | Just arg <- val str ->
            action $ Global.save_git_as arg
        ["load", str] | Just arg <- val str -> action $ Global.load arg

        -- State
        ["Ui.lookup_root_id"] -> action Ui.lookup_root_id
        ["Ui.set_root_id", str] | Just arg <- val str ->
            action $ Ui.set_root_id arg

        -- So I can see memory stats without loading GHC API.  Though it
        -- happens automatically in the background, so I'd have to disable that
        -- manually.
        ["LDebug.rtsAllocated"] -> action $ LDebug.rtsAllocated
        ["LDebug.rssVsize"] -> action $ LDebug.rssVsize
        _ -> Nothing
    where
    action c = Just (fmap (cmd_result . Global._to_result) c)
    cmd_result result = ReplProtocol.CmdResult result []

val :: Read a => String -> Maybe a
val text = case reads text of
    (val, "") : _ -> Just val
    _ -> Nothing

lex_all :: String -> Maybe [String]
lex_all text
    | null (dropWhile Char.isSpace text) = Just []
    | otherwise = case lex_fancy text of
        [] -> Nothing
        (tok, rest) : _ -> do
            toks <- lex_all rest
            return (tok : toks)

-- | A version of 'lex' that understands qualified names.
--
-- It also lexes parenthesized text as a single token, but it doesn't count
-- open parens so it doesn't work for nested ones.
-- TODO count parens, or come up with a better way to parse haskell
lex_fancy :: String -> [(String, String)]
lex_fancy s = case lex s of
    [(tok1, '.':rest1)] ->
        [(tok1 ++ "." ++ tok2, rest2) | (tok2, rest2) <- lex_fancy rest1]
    [("(", rest)] ->
        let (pre, post) = Then.break1 (==')') rest
        in [('(' : pre, post)]
    val -> val
