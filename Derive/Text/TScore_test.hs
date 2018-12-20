-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE TypeApplications #-}
module Derive.Text.TScore_test where
import qualified GHC.Stack as Stack

import qualified Derive.Text.TScore as TScore
import qualified Ui.Id as Id

import           Global
import           Util.Test


test_score = do
    let score =
            "block1 = %block1=directive \"block1 title\" [\n\
            \    \">inst1\" a\n\
            \    //\n\
            \    \">inst2\" b\n\
            \]\n\
            \block2 = [c]\n"
    let f = second TScore.unparse . parse @TScore.Score TScore.parse
    equal (f score) $ Right
        "block1 = %block1=directive \"block1 title\"\
        \ [ \">inst1\" a // \">inst2\" b ]\n\
        \block2 = [ c ]\n"
    either putStrLn (putStrLn . untxt) (f score)

roundtrip :: forall a. (Stack.HasCallStack, TScore.Element a)
    => Proxy a -> Text -> IO Bool
roundtrip Proxy t =
    right_equal (second TScore.unparse (parse @a TScore.parse t)) t

test_parse = do
    roundtrip (Proxy @Id.BlockId) "block1"
    roundtrip (Proxy @Id.BlockId) "x/a"
    roundtrip (Proxy @TScore.Directive) "%a=b"

test_track = do
    let f = fmap TScore.track_tokens . parse TScore.parse
    let bar = TScore.TBarline . TScore.Barline
        no_dur = TScore.Duration Nothing 0 False
        no_oct = TScore.Relative 0
    right_equal (f "| ||") [bar 1, bar 2]
    right_equal (f "a") [token "" no_oct "a" no_dur]
    right_equal (f "a -- hi") [token "" no_oct "a" no_dur]
    let rest = TScore.TRest . TScore.Rest
    right_equal (f "_4 | _.")
        [ rest (TScore.Duration (Just 4) 0 False)
        , bar 1
        , rest (TScore.Duration Nothing 1 False)
        ]

    right_equal (f "a b/")
        [ token "" no_oct "a" no_dur
        , token "b" no_oct "" no_dur
        ]
    right_equal (f "\"\" \"a b\"/") [token "a b" no_oct "" no_dur]
    right_equal (f "\"\" \"a \"() b\"/") [token "a \"() b" no_oct "" no_dur]

test_token = do
    let f = parse TScore.parse
    let no_dur = TScore.Duration Nothing 0 False
    left_like (f "") "unexpected end of input"
    right_equal (f "a") $ token "" (TScore.Relative 0) "a" no_dur
    right_equal (f "+pizz/") $ token "+pizz" (TScore.Relative 0) "" no_dur
    right_equal (f "a/'b1.~") $
        token "a" (TScore.Relative 1) "b" (TScore.Duration (Just 1) 1 True)

test_token_roundtrip = do
    let p = Proxy @TScore.Token
    roundtrip p "a"
    roundtrip p "+pizz/"
    roundtrip p "\"a b\"/"
    roundtrip p "a/'b1.~"

token :: Text -> TScore.Octave -> Text -> TScore.Duration -> TScore.Token
token call oct pitch dur = TScore.TNote $
    TScore.Note (TScore.Call call) (TScore.Pitch oct pitch) dur

parse :: TScore.Parser a -> Text -> Either String a
parse = TScore.pparse
