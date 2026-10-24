-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Parse.Ky_test where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Directory as Directory
import           System.FilePath ((</>))

import qualified Util.ParseA as ParseA
import qualified Util.ParseM as ParseM
import qualified Util.Test.Testing as Testing

import qualified Derive.Expr as Expr
import qualified Derive.Parse.Ky as Ky
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import           Derive.TestInstances ()

import           Global
import           Util.Test


test_loadKy :: Test
test_loadKy = do
    let makeKy imports defs = Text.unlines $
            ["import '" <> i <> "'" | i <- imports]
            ++ "note generator:" : [d <> " = z" | d <- defs]
    dir <- Testing.tmpDir "ky"
    let lib = dir </> "lib"
    Directory.createDirectory lib
    Text.IO.writeFile (lib </> "lib1") $ makeKy ["lib2"] ["lib1-call"]

    let write imports =
            Text.IO.writeFile (dir </> "defs") (makeKy imports ["defs-call"])
    let load = bimap ParseM.showError extract
            <$> Ky.loadKy [dir, lib] (dir </> "defs")
        extract (Ky.Ky defs imports allocs _tscore) =
            ( map (fst . snd) . fst . Ky.defNote $ defs
            , imports
            , allocs
            )
    write ["z1"]
    io leftLike load "defs: ky file not found: z1"
    write ["lib1"]
    io leftLike load "lib1: ky file not found: lib2"
    -- cycle: defs imports itself
    write ["defs"]
    io rightEqual load
        ( ["defs-call"]
        , [Ky.Loaded (dir </> "defs") (makeKy ["defs"] ["defs-call"])]
        , mempty
        )

    Text.IO.writeFile (lib </> "lib2") $ makeKy [] ["lib2-call"]
    let bad = dir </> "bad"
    Text.IO.writeFile bad "instrument:\n123\n"
    write ["bad"]
    io leftLike load (txt bad <> ":2:1:unexpected '1'")
    write ["lib1"]
    [lib1, lib2, defs] <- mapM Text.IO.readFile
        [lib </> "lib1", lib </> "lib2", dir </> "defs"]
    ioEqual load $ Right
        ( ["defs-call", "lib1-call", "lib2-call"]
        , [ Ky.Loaded (dir </> "defs") defs
          , Ky.Loaded (lib </> "lib1") lib1
          , Ky.Loaded (lib </> "lib2") lib2
          ]
        , mempty
        )

eExpr :: Ky.Expr -> [(Expr.Symbol, [Text])]
eExpr (Ky.Expr (call :| calls)) = eCall call : map eCall calls
    where eCall (Ky.Call sym terms) = (sym, map ShowVal.showVal terms)

test_parseKy :: Test
test_parseKy = do
    let f extract = bimap ParseM.showError extract
            . Ky.parseKy "fname.ky"
        note = f eNote . ("note generator:\n"<>)
        eNote = map (second (head . eExpr) . snd) . fst . Ky.defNote
            . Ky.kyDefinitions
        eVal = map (second (head . eExpr) . snd) . Ky.defVal . Ky.kyDefinitions
    rightEqual (f eNote "") mempty
    rightEqual (f eNote "-- hi") mempty
    rightEqual (f eNote "instrument:\n-- hi") mempty
    leftLike (f id "x:\na = b\n") "unknown sections: x"
    rightEqual (note " --c\na = b\n\n") [("a", ("b", []))]
    rightEqual (note "a = b\n --c\n\n c\nd = e\n  -- hi\n")
        [("a", ("b", ["c"])), ("d", ("e", []))]
    rightEqual (note "a = b\n c\n --comment\n\nd = e\n")
        [("a", ("b", ["c"])), ("d", ("e", []))]
    leftLike (note "a = b\nc\n") "fname.ky:3:1:expected eof"
    rightEqual (note "a = b $c") [("a", ("b", ["$c"]))]
    rightEqual (f eNote "-- hi") []
    rightEqual (f eNote "-- note_generator:") []
    rightEqual (note "a = c =+ 1\n") [("a", ("=", ["c", "1", "'+'"]))]
    rightEqual (f eVal "val:\nx = thing\n") [("x", ("thing", []))]

    -- imports
    rightEqual (f Ky.kyImports "import 'x' -- blah\nimport 'y'\n")
        [Ky.Import "fname.ky" "x", Ky.Import "fname.ky" "y"]
    rightEqual (f Ky.kyImports "import\n\t'x'\n")
        [Ky.Import "fname.ky" "x"]
    leftLike (f Ky.kyImports "blort x\nimport y\n")
        "fname.ky:1:1:expected eof"

    let aliases = Ky.defAliases . Ky.kyDefinitions
    rightEqual (f aliases "alias:\na = b\n") $
        [(ScoreT.Instrument "a", ScoreT.Instrument "b")]
    leftLike (f aliases "alias:\n>a = >b\n") "lhs not a valid id"

test_instruments :: Test
test_instruments = do
    let f = bimap ParseM.showError extract
            . Ky.parseKy "fname.ky"
        extract = Ky.kyInstruments
    -- line offset to 4
    leftLike (f ky1) "fname.ky:4:1:unexpected '1'"
    where
    ky1 =
        "-- blah\n\
        \-- blah\n\
        \instrument:\n\
        \123\n"

test_tscore :: Test
test_tscore = do
    let f = bimap ParseM.showError extract
            . Ky.parseKy "fname.ky"
        extract ky = Ky.kyTscores ky
    -- Properly on line 3 instead of 2.
    leftLike (f "tscore:\nx = [ \n")
        "fname.ky:3:1:unexpected end of input"

test_p_definition :: Test
test_p_definition = do
    let f = bimap ParseM.showError (second eExpr)
            . ParseA.parse Ky.pDefinition
    equal (f "a =\n b\n c\n") (Right ("a", [("b", ["c"])]))
    leftLike (f "a =\n b\nc = d\n") "3:1:expected eof"
    equal (f "a = n +z\n") (Right ("a", [("n", ["+z"])]))
    equal (f "a = b $c") (Right ("a", [("b", ["$c"])]))
    equal (f "a = b = $c") (Right ("a", [("=", ["b", "$c"])]))

test_checkedSections :: Test
test_checkedSections = do
    let f = Ky.checkedSections . Text.unlines
    rightEqual (f
        [ "import x"
        , "-- hi:"
        , "sec a:"
        , ""
        , "hi"
        , ""
        , "-- xyz:"
        , "sec b:"
        , "there"
        , ""
        ])
        ( "import x\n-- hi:\n"
        , Map.fromList
            [ ("sec a", [(4, "hi"), (5, ""), (6, "-- xyz:")])
            , ("sec b", [(8, "there"), (9, "")])
            ]
        )
    leftLike (f ["sec:", "hi", "sec:", "there"]) "duplicate sections: sec"

test_replaceSection :: Test
test_replaceSection = do
    let f = Ky.replaceSection "sec" ("-- "<>) . Text.unlines
    equal (f []) ""
    equal (f ["x", "sec:", "a"]) "x\n-- sec:\n-- a\n"
    equal (f ["abc:", "a", "sec:", "b", "c", "xyz:", "z"])
        "abc:\n\
        \a\n\
        \-- sec:\n\
        \-- b\n\
        \-- c\n\
        \xyz:\n\
        \z\n"
