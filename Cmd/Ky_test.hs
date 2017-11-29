-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Ky_test where
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Test
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Ky as Ky
import qualified Cmd.PlayUtil as PlayUtil

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse

import Global


test_ky_file = do
    let run text tracks = DeriveTest.extract extract $ CmdTest.eval
            (CmdTest.make_tracks tracks) CmdTest.default_cmd_state $
            put_library text >> PlayUtil.uncached_derive UiTest.default_block_id
        extract = DeriveTest.e_attributes
        defs = Text.unlines
            [ "note generator:"
            , "with-a = +a"
            , "like-attr = attr"
            , "note transformer:"
            , "d = +a"
            ]
    equal (run defs [(">", [(0, 1, "with-a")])]) (["+a"], [])
    strings_like (snd $ run defs [(">", [(0, 1, "with-a x")])])
        ["too many arguments"]
    -- Args are ok if the definition doesn't have any.
    equal (run defs [(">", [(0, 1, "like-attr +a")])]) (["+a"], [])
    -- A local note transformer shadows the built-in 'd' transformer.
    equal (run defs [(">", [(0, 1, "d |")])]) (["+a"], [])

    -- Definitions can be full expressions.
    let defs = Text.unlines
            [ "note generator:"
            , "with-a = +a |"
            , "note transformer:"
            , "d = +a | +b"
            ]
    equal (run defs [(">", [(0, 1, "with-a")])]) (["+a"], [])
    equal (run defs [(">", [(0, 1, "d |")])]) (["+a+b"], [])

test_check_cache = do
    let f ky_cache ky = Ky.check_cache
            (Ui.config#Ui.ky #= ky $ Ui.empty)
            (CmdTest.default_cmd_state { Cmd.state_ky_cache = ky_cache })
        extract Nothing = Right Nothing
        extract (Just (Cmd.KyCache lib (Cmd.Fingerprint fnames fprint))) =
            case lib of
                Left err -> Left err
                Right (lib, _) -> Right $ Just (e_library lib, (fnames, fprint))
        extract (Just (Cmd.PermanentKy (lib, _))) =
            Right $ Just (e_library lib, ([], 0))

    io_equal (extract <$> f Nothing "") (Right Nothing)
    let define_a = "note generator:\na = +a\n"
    result <- f Nothing define_a
    equal (extract result) (Right (Just (["a"], ([], 1304129647))))
    -- No change, so don't update.
    io_equal (extract <$> f result define_a) (Right Nothing)
    -- Now it has changed.
    io_equal (extract <$> f result "note generator:\nb = +b\n")
        (Right (Just (["b"], ([], 1120727268))))

    result <- f Nothing "error"
    left_like (extract result) "expected eof"
    -- But don't update after two errors in a row.
    io_equal (extract <$> f result "error") (Right Nothing)

    -- TODO track imported files

e_library :: Derive.Library -> [Expr.Symbol]
e_library lib = [name | Derive.LookupMap m <- gen, name <- Map.keys m]
    where gen = Derive.scope_note $ Derive.scopes_generator lib

put_library :: Cmd.M m => Text -> m ()
put_library text = Cmd.modify $ \st -> st
    { Cmd.state_ky_cache = Just $ case Parse.parse_ky "fname.ky" text of
        Left err -> Cmd.KyCache (Left err) mempty
        Right (_, defs) ->
            Cmd.KyCache (Right (Ky.compile_library defs, mempty)) mempty
    }
