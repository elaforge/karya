-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Parse.Ky_test where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Directory as Directory
import           System.FilePath ((</>))

import qualified Util.ParseText as ParseText
import qualified Util.Test.Testing as Testing
import qualified Derive.Expr as Expr
import qualified Derive.Parse.Ky as Ky
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import           Derive.TestInstances ()

import           Global
import           Util.Test


test_load_ky :: Test
test_load_ky = do
    let make_ky imports defs = unlines $
            ["import '" <> i <> "'" | i <- imports]
            ++ "note generator:" : [d <> " = z" | d <- defs]
    dir <- Testing.tmp_dir "ky"
    let lib = dir </> "lib"
    Directory.createDirectory lib
    writeFile (lib </> "lib1") $ make_ky ["lib2"] ["lib1-call"]

    let write imports =
            writeFile (dir </> "defs") (make_ky imports ["defs-call"])
    let load = bimap ParseText.show_error extract
            <$> Ky.load_ky [dir, lib] "import 'defs'"
        extract (Ky.Ky defs imports allocs) =
            ( map (fst . snd) . fst . Ky.def_note $ defs
            , imports
            , allocs
            )
    write ["z1"]
    v <- load
    left_like v "ky file not found: z"

    write ["lib1"]
    v <- load
    left_like v "ky file not found: lib2"

    writeFile (lib </> "lib2") $ make_ky [] ["lib2-call"]
    [lib1, lib2, defs] <- mapM Text.IO.readFile
        [lib </> "lib1", lib </> "lib2", dir </> "defs"]
    io_equal load $ Right
        ( ["defs-call", "lib1-call", "lib2-call"]
        , [ Ky.Loaded "" "import 'defs'"
          , Ky.Loaded (dir </> "defs") defs
          , Ky.Loaded (lib </> "lib1") lib1
          , Ky.Loaded (lib </> "lib2") lib2
          ]
        , Nothing
        )

e_expr :: Ky.Expr -> [(Expr.Symbol, [Text])]
e_expr (Ky.Expr (call :| calls)) = e_call call : map e_call calls
    where e_call (Ky.Call sym terms) = (sym, map ShowVal.show_val terms)

test_parse_ky :: Test
test_parse_ky = do
    let f extract = bimap ParseText.show_error extract
            . Ky.parse_ky "fname.ky"
        note = f e_note . ("note generator:\n"<>)
        e_note = map (second (head . e_expr) . snd) . fst . Ky.def_note
            . Ky.ky_definitions
    left_like (f id "x:\na = b\n") "unknown sections: x"
    equal (note " --c\na = b\n\n") $
        Right [("a", ("b", []))]
    equal (note "a = b\n --c\n\n c\nd = e\n  -- hi\n") $
        Right [("a", ("b", ["c"])), ("d", ("e", []))]
    equal (note "a = b\n c\n --comment\n\nd = e\n") $
        Right [("a", ("b", ["c"])), ("d", ("e", []))]
    left_like (note "a = b\nc\n") "3:1: expected eof"
    equal (note "a = b $c") $ Right [("a", ("b", ["$c"]))]
    equal (f e_note "-- hi") (Right [])
    equal (f e_note "-- note_generator:") (Right [])
    equal (note "a = c =+ 1\n") $ Right [("a", ("=", ["c", "1", "'+'"]))]

    -- imports
    right_equal (f Ky.ky_imports "import 'x' -- blah\nimport 'y'\n")
        [Ky.Import "fname.ky" "x", Ky.Import "fname.ky" "y"]
    right_equal (f Ky.ky_imports "import\n\t'x'\n")
        [Ky.Import "fname.ky" "x"]
    left_like (f Ky.ky_imports "blort x\nimport y\n") "1:1: expected eof"

    let aliases = Ky.def_aliases . Ky.ky_definitions
    right_equal (f aliases "alias:\na = b\n") $
        [(ScoreT.Instrument "a", ScoreT.Instrument "b")]
    left_like (f aliases "alias:\n>a = >b\n") "lhs not a valid id"

test_split_sections :: Test
test_split_sections = do
    let f = Ky.split_sections
    right_equal (f
        [ "import x"
        , "  -- hi"
        , "sec1:"
        , "hi"
        , ""
        , "-- xyz"
        , "sec2:"
        , "there"
        ])
        ( "import x\n"
        , Map.fromList [("sec1", [(3, "hi")]), ("sec2", [(7, "there")])]
        )
    left_like (f ["sec1:", "hi", "sec1:", "there"])
        "duplicate sections: sec1"

test_p_definition :: Test
test_p_definition = do
    let f = bimap ParseText.show_error (second e_expr)
            . ParseText.parse Ky.p_definition
    equal (f "a =\n b\n c\n") (Right ("a", [("b", ["c"])]))
    left_like (f "a =\n b\nc = d\n") "3:1: expected eof"
    equal (f "a = n +z\n") (Right ("a", [("n", ["+z"])]))
    equal (f "a = b $c") (Right ("a", [("b", ["$c"])]))
    equal (f "a = b = $c") (Right ("a", [("=", ["b", "$c"])]))
