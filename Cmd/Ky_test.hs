-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Ky_test where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.ParseM as ParseM
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Ky as Ky
import qualified Cmd.PlayUtil as PlayUtil

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Expr as Expr
import qualified Derive.Parse.Ky as Parse.Ky

import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_ky_definition :: Test
test_ky_definition = do
    let run text tracks = DeriveTest.extract extract $
            CmdTest.eval (CmdTest.makeTracks tracks) CmdTest.defaultCmdState $
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

-- Take stateFocusedView back out, because I don't have any blocks!
cmdState :: Cmd.State
cmdState = CmdTest.defaultCmdState { Cmd.stateFocusedView = Nothing }

test_reload :: Test
test_reload = do
    let f extract ky = fmap ex $
            CmdTest.runIo UiTest.empty cmdState (Ky.reload True (Just ky))
            where
            ex r = case CmdTest.resultVal r of
                Left err -> Left err
                Right Nothing -> Left "<abort>"
                Right (Just ()) -> Right (extract r)
    let eDefs r = case Cmd.stateKyCache (CmdTest.resultCmdState r) of
            Just (Cmd.KyCache cache) -> eBuiltins (Cmd.kyBuiltins cache)
            _ -> []
    let eInsts = Map.keys . UiConfig.unallocations
            . (Ui.config#UiConfig.allocations #$)
            . CmdTest.resultUiState
    let eFprint r = case Cmd.stateKyCache (CmdTest.resultCmdState r) of
            Just (Cmd.KyCache cache) -> Just $ Cmd.kyTscoreFingerprint cache
            _ -> Nothing
    let eBlocks = Map.keys . Ui.stateBlocks . CmdTest.resultUiState

    io rightEqual (f (const ()) "") ()
    -- parse error, line numbers correct
    io leftLike
        (f (const ()) "note generator:\n\nsyntax error blah\n")
        "<internal>:3:1:*| ^*expected eof"
    -- defs and instruments
    io rightEqual (f eDefs "note generator:\na = +a\n") ["a"]
    io rightEqual (f eInsts "instrument:\n>i1 s/1 [ms] d 1\n") ["i1"]
    io leftLike (f eInsts "instrument:\n>i1 a/b [ms] d 1\n")
        "i1: patch not found: a/b"
    io leftLike (f eInsts "instrument:\n>i1 s/1 [ms] d 1\n>i2 s/2 [ms] d 1 2\n")
        "i1 and i2 on d overlap chans: 1"
    -- tscore
    io leftLike (f (const ()) "tscore:\nxyz = error?\n")
        "2:7:*unexpected 'e'*"
    blocks <- f eBlocks "tscore:\nxyz = [ 4s1 ]\n"
    rightEqual blocks [UiTest.bid "xyz"]
    -- Loading records the fingerprint without integrating, so the same
    -- tscore doesn't integrate later, but a changed one does.
    let reload integrate ky (uiState, cmdState) = do
            r <- CmdTest.runIo uiState cmdState (Ky.reload integrate (Just ky))
            pure (CmdTest.resultUiState r, CmdTest.resultCmdState r)
        blockIds = Map.keys . Ui.stateBlocks . fst
    states <- reload False "tscore:\nxyz = [ 4s1 ]\n" (UiTest.empty, cmdState)
    equal (blockIds states) []
    states <- reload True "tscore:\nxyz = [ 4s1 ]\n" states
    equal (blockIds states) []
    states <- reload True "tscore:\nxyz = [ 4s2 ]\n" states
    equal (blockIds states) [UiTest.bid "xyz"]

    fprint1 <- f eFprint "tscore:\nxyz = [ 4s1 ]\n"
    fprint2 <- f eFprint "tscore:\nxyz = [ 4s2 ]\n"
    notEqual fprint1 fprint2
    -- TODO proper error
    io leftLike (f eFprint "tscore:\nxyz = [ 4s4 | 4s4 ]\n")
        "Error:Pos 12:barline check"

eBuiltins :: Derive.Builtins -> [Expr.Symbol]
eBuiltins = concatMap (Map.keys . Derive.callMap) . Map.elems
    . Derive.scopeNote . Derive.scopesGenerator

putLibrary :: Cmd.M m => Text -> m ()
putLibrary text = do
    Parse.Ky.Ky defs _imported _allocs _tscore _tscoreLines <-
        Cmd.requireRight ParseM.showError $
            Parse.Ky.parseKy "fname.ky" text
    let (builtins, _logs) = Ky.compileLibrary (Ky.compileDefinitions defs)
    Cmd.modify $ \st -> st
        { Cmd.stateKyCache = Just $ Cmd.KyCache $ Cmd.KyCached
            { kyBuiltins = builtins
            , kyAliases = mempty
            -- , kyFingerprint = mempty
            , kyTscoreFingerprint = mempty
            }
        }
