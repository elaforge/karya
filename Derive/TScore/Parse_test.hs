-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE TypeApplications #-}
module Derive.TScore.Parse_test where
import qualified GHC.Stack as Stack

import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T
import qualified Ui.Id as Id

import           Global
import           Util.Test


test_score = do
    let f = second Parse.unparse . parse @T.Score Parse.parse
    let score =
            "%meter=adi\n\
            \block1 = %block1=directive \"block1 title\" [\n\
            \    \">inst1\" a\n\
            \    //\n\
            \    \">inst2\" b\n\
            \]\n\
            \block2 = [c]\n"
    right_equal (f score) $
        "%meter=adi\n\
        \block1 = %block1=directive \"block1 title\"\
        \ [ >inst1 a // >inst2 b ]\n\
        \block2 = [ c ]\n"

roundtrip :: forall a. (Stack.HasCallStack, Parse.Element a)
    => Proxy a -> Text -> IO Bool
roundtrip Proxy t =
    right_equal (second Parse.unparse (parse @a Parse.parse t)) t

test_parse = do
    roundtrip (Proxy @Id.BlockId) "block1"
    roundtrip (Proxy @Id.BlockId) "x/a"
    roundtrip (Proxy @T.Directive) "%a=b"

test_track = do
    let f = fmap T.track_tokens . parse Parse.parse
    let bar = T.TBarline . T.Barline
        no_dur = T.Duration Nothing 0 False
        no_oct = T.Relative 0
    right_equal (f "| ||") [bar 1, bar 2]
    right_equal (f "a") [token "" no_oct "a" no_dur]
    right_equal (f "a -- hi") [token "" no_oct "a" no_dur]
    let rest = T.TRest . T.Rest
    right_equal (f "_4 | _.")
        [ rest (T.Duration (Just 4) 0 False)
        , bar 1
        , rest (T.Duration Nothing 1 False)
        ]
    right_equal (f "a b/")
        [ token "" no_oct "a" no_dur
        , token "b" no_oct "" no_dur
        ]
    right_equal (f "> \"a b\"/") [token "a b" no_oct "" no_dur]
    right_equal (f "> \"a \"() b\"/") [token "a \"() b" no_oct "" no_dur]

test_token = do
    let f = parse Parse.parse
    let no_dur = T.Duration Nothing 0 False
    left_like (f "") "unexpected end of input"
    right_equal (f "a") $ token "" (T.Relative 0) "a" no_dur
    right_equal (f "+pizz/") $ token "+pizz" (T.Relative 0) "" no_dur
    right_equal (f "a/'b1.~") $
        token "a" (T.Relative 1) "b" (T.Duration (Just 1) 1 True)
    right_equal (f "a'/a#4") $
        token "a'" (T.Relative 0) "a#" (T.Duration (Just 4) 0 False)

test_token_roundtrip = do
    let p = Proxy @(T.Token T.Pitch T.Duration)
    roundtrip p "a"
    roundtrip p "+pizz/"
    roundtrip p "\"a b\"/"
    roundtrip p "a/'b1.~"

token :: Text -> T.Octave -> Text -> T.Duration -> T.Token T.Pitch T.Duration
token call oct pitch dur = T.TNote $
    T.Note (T.Call call) (T.Pitch oct pitch) dur

parse :: Parse.Parser a -> Text -> Either String a
parse = Parse.parse_text
