-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Ky_test where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.ParseText as ParseText
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Ky as Ky
import qualified Cmd.PlayUtil as PlayUtil

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Expr as Expr
import qualified Derive.Parse.Ky as Parse.Ky

import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_ky_file :: Test
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

test_check_cache :: Test
test_check_cache = do
    let f ky_cache ky = fmap (fmap (fst . fst)) $
            Ky.check_cache lookup_backend ky_cache mempty [] ky
        extract Nothing = Right Nothing
        extract (Just (Cmd.KyCache builtins (Cmd.Fingerprint fnames _fprint))) =
            case builtins of
                Left err -> Left err
                Right (builtins, _) ->
                    Right $ Just (e_builtins builtins, fnames)
        extract (Just (Cmd.PermanentKy (builtins, _))) =
            Right $ Just (e_builtins builtins, [])
    io_equal (extract <$> f Nothing "") (Right Nothing)
    let define_a = "note generator:\na = +a\n"
    result <- f Nothing define_a
    equal (extract result) (Right (Just (["a"], [])))
    -- No change, so don't update.
    io_equal (extract <$> f result define_a) (Right Nothing)
    -- Now it has changed.
    io_equal (extract <$> f result "note generator:\nb = +b\n")
        (Right (Just (["b"], [])))

    result <- f Nothing "error"
    left_like (extract result) "expected eof"
    -- But don't update after two errors in a row.
    io_equal (extract <$> f result "error") (Right Nothing)

    -- TODO track imported files

lookup_backend :: InstT.Qualified -> Maybe Inst.Backend
lookup_backend _qual = Nothing

e_builtins :: Derive.Builtins -> [Expr.Symbol]
e_builtins = concatMap (Map.keys . Derive.call_map) . Map.elems
    . Derive.scope_note . Derive.scopes_generator

put_library :: Cmd.M m => Text -> m ()
put_library text = do
    cache <- case Parse.Ky.parse_ky "fname.ky" text of
        Left err ->
            return $ Cmd.KyCache (Left (ParseText.show_error err)) mempty
        Right (Parse.Ky.Ky defs imported _allocs) -> do
            -- This is not right for compile_library because it's imports
            -- and not imported paths, but it doesn't care because it's just
            -- for the error msg.
            let imports = [imp | Parse.Ky.Import _ imp <- imported]
            let (builtins, _logs) = Ky.compile_library imports $
                    Ky.compile_definitions defs
            return $ Cmd.KyCache (Right (builtins, mempty)) mempty
    Cmd.modify $ \st -> st { Cmd.state_ky_cache = Just cache }
