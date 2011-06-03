{-# LANGUAGE PatternGuards #-}
-- | The full haskell interpretation loads a whole bunch of modules and can be
-- slow.  Shortcut a few common commands so they happen quickly.
module Cmd.Lang.Fast where
import qualified Data.Char as Char

import qualified Util.PPrint as PPrint
import qualified Util.Seq as Seq

import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
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
        ["s", str] | Just arg <- val str -> action $ Global.s arg
        ["unerror"] -> action Global.unerror
        ["collapse", int] | Just arg <- val int -> action $ Global.collapse arg
        ["expand", int] | Just arg <- val int -> action $ Global.expand arg

        -- Called by the browser.
        ["load_instrument", str] | Just arg <- val str ->
            action $ LInst.load arg

        -- Called manually via the REPL.

        -- Make blocks and views.
        ["Create.rename_project", a1]
            | Just v1 <- val a1 -> action $ Create.rename_project v1
        ["Create.view", str] | Just arg <- val str -> action $ Create.view arg

        -- Misc.
        ["quit"] -> Just quit
        ["save"] -> action Global.save
        ["save_as", str] | Just arg <- val str -> action $ Global.save_as arg
        ["load", str] | Just arg <- val str -> action $ Global.load arg

        ["show_state"] -> Just Global.show_state
        ["show_views", str] | Just arg <- val str ->
            Just $ Global.show_views arg
        ["show_blocks", str] | Just arg <- val str ->
            Just $ Global.show_blocks arg

        -- State
        ["State.lookup_root_id"] -> Just $ fmap show State.lookup_root_id
        ["State.set_root_id", str] | Just arg <- val str ->
            action $ State.set_root_id arg
        ["State.get_midi_config"] -> pretty State.get_midi_config
        _ -> Nothing
    where
    -- Command that doesn't return a value.
    action c = Just (c >> return "")
    -- Command returns a value to be pretty-printed.
    pretty c = Just (fmap PPrint.pshow c)

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
        let (pre, post) = Seq.break1 (==')') rest
        in [('(' : pre, post)]
    val -> val
