-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.TScore.Check_test where
import qualified Data.Either as Either

import qualified Util.Test.Testing as Testing
import qualified Derive.TScore.Check as Check
import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T

import           Global
import           Util.Test


test_process = do
    let f = map extract . Check.process config . parse
        config = Check.default_config
        extract = fmap $ fromMaybe "" . T.note_pitch . snd
    -- TODO resolve_pitch happens before resolve_time
    -- equal (f "4s ~") [Right (4, 0)]

    equal (f "4s r g") [Right "4s", Right "4r", Right "4g"]
    equal (f "4n s") [Right "4n", Right "5s"]
    equal (f "4s n") [Right "4s", Right "3n"]
    -- mid-point goes down.
    equal (f "4p s") [Right "4p", Right "4s"]
    equal (f "4n ,s") [Right "4n", Right "4s"]
    equal (f "4s 'n") [Right "4s", Right "4n"]
    equal (f "4s 's") [Right "4s", Right "5s"]
    equal (f "4s ,s") [Right "4s", Right "3s"]

test_default_call = do
    let f = map (fmap snd) . Check.process config . parse
        config = Check.default_config { Check.config_default_call = True }
        note call pitch = T.Note (T.Call call) pitch 1
    equal (f "a b") [Right (note "a" Nothing), Right (note "b" Nothing)]
    equal (f "a/s c")
        [ Right (note "a" (Just "4s"))
        , Right (note "c" (Just "4s"))
        ]
    equal (f "4a2") [Right (note "4a" Nothing) { T.note_duration = 1/2 }]

test_resolve_time = do
    let f = map extract . Check.resolve_time . map Right
            . Check.multiplicative . parse
        extract = fmap (second T.note_duration)
    equal (f "a b c") [Right (0, 1), Right (1, 1), Right (2, 1)]
    equal (f "a~ a b") [Right (0, 2), Right (2, 1)]
    equal (f "a~ b c")
        [ Left (Check.Error 1 "note tied to different pitch: a ~ b")
        , Right (2, 1)
        ]
    equal (f "a~ a~ _") [Left (Check.Error 2 "note tied to rest")]
    equal (f "_~ a") [Left (Check.Error 1 "rest tied to note")]
    equal (f "a~ | a") [Right (0, 2)]
    equal (f "_~ | _ a") [Right (2, 1)]
    equal (f "a~") [Left (Check.Error 0 "final note has a tie")]

test_barlines = do
    let f = Either.lefts . Check.barlines Check.meter_44 . Check.multiplicative
            . parse
    equal (f "| a4 b c e |") []
    equal (f "| a4 ; b ; c ; e |") []
    equal (f "| a4 b | c e |") [Check.Error (1/2) "token 3: saw |, expected ;"]
    equal (f "a8 | b") [Check.Error (1/8) "token 1: saw |, expected none"]

test_multiplicative = do
    let f = map (fmap fst . e_duration) . Check.multiplicative . parse
    equal (f "a b c") [Just 1, Just 1, Just 1]
    equal (f "a2 b.") [Just (1/2), Just (3/4)]
    equal (f "a1..") [Just (1 + 3/4)]
    pprint (parse "a b 4c ~")

e_duration :: T.Token pitch dur -> Maybe dur
e_duration = \case
    T.TNote note -> Just $ T.note_duration note
    _ -> Nothing

parse :: Text -> [T.Token T.Pitch T.Duration]
parse = Testing.expect_right . Parse.parse_text Parse.p_tokens
