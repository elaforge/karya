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
import qualified Perform.Midi.Patch as Patch
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_ky_file :: Test
test_ky_file = do
    let run text tracks = DeriveTest.extract extract $ CmdTest.eval
            (CmdTest.makeTracks tracks) CmdTest.defaultCmdState $
            putLibrary text >> PlayUtil.uncachedDerive UiTest.defaultBlockId
        extract = DeriveTest.eAttributes
        defs = Text.unlines
            [ "note generator:"
            , "with-a = +a"
            , "like-attr = attr"
            , "note transformer:"
            , "d = +a"
            ]
    equal (run defs [(">", [(0, 1, "with-a")])]) (["+a"], [])
    stringsLike (snd $ run defs [(">", [(0, 1, "with-a x")])])
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
    let f kyCache ky = fmap (fmap (fst . fst)) $
            Ky.checkCache lookupBackend kyCache mempty [] ky
        extract Nothing = Right Nothing
        extract (Just (Cmd.KyCache builtins (Cmd.Fingerprint fnames _fprint))) =
            case builtins of
                Left err -> Left err
                Right (builtins, _) ->
                    Right $ Just (eBuiltins builtins, fnames)
        extract (Just (Cmd.PermanentKy (builtins, _))) =
            Right $ Just (eBuiltins builtins, [])
    -- empty ky
    ioEqual (extract <$> f Nothing "") (Right (Just ([], [])))
    let defineA = "note generator:\na = +a\n"
    result <- f Nothing defineA
    equal (extract result) (Right (Just (["a"], [])))
    -- No change, so don't update.
    ioEqual (extract <$> f result defineA) (Right Nothing)
    -- Now it has changed.
    ioEqual (extract <$> f result "note generator:\nb = +b\n")
        (Right (Just (["b"], [])))

    result <- f Nothing "error"
    leftLike (extract result) "expected eof"
    -- But don't update after two errors in a row.
    ioEqual (extract <$> f result "error") (Right Nothing)

    result <- f Nothing "instrument:\n>i1 a/b [ms] im\n"
    leftLike (extract result) "i1: patch not found: a/b"
    result <- f Nothing
        "instrument:\n>i1 midi/ [ms] d 1\n>i2 midi/ [ms] d 1 2\n"
    leftLike (extract result) "i1 and i2 on d overlap chans: 1"
    -- TODO track imported files
    where
    lookupBackend (InstT.Qualified synth _)
        | synth == "midi" = Just $ Inst.Midi $ Patch.patch (0, 0) "name"
        | otherwise = Nothing

eBuiltins :: Derive.Builtins -> [Expr.Symbol]
eBuiltins = concatMap (Map.keys . Derive.callMap) . Map.elems
    . Derive.scopeNote . Derive.scopesGenerator

putLibrary :: Cmd.M m => Text -> m ()
putLibrary text = do
    cache <- case Parse.Ky.parseKy "fname.ky" text of
        Left err ->
            pure $ Cmd.KyCache (Left (ParseText.showError err)) mempty
        Right (Parse.Ky.Ky defs imported _allocs) -> do
            -- This is not right for compile_library because it's imports
            -- and not imported paths, but it doesn't care because it's just
            -- for the error msg.
            let imports = [imp | Parse.Ky.Import _ imp <- imported]
            let (builtins, _logs) = Ky.compileLibrary imports $
                    Ky.compileDefinitions defs
            pure $ Cmd.KyCache (Right (builtins, mempty)) mempty
    Cmd.modify $ \st -> st { Cmd.stateKyCache = Just cache }
