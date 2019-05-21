-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.TScore.Check_test where
import qualified Control.Monad.Combinators as P
import qualified Control.Monad.Identity as Identity
import qualified Data.Either as Either

import qualified Util.EList as EList
import           Util.Test hiding (check)
import qualified Util.Test.Testing as Testing

import qualified Derive.TScore.Check as Check
import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T
import qualified Derive.TScore.TScore as TScore

import qualified Ui.UiTest as UiTest

import           Global


test_check = do
    let f = map extract . check config . parse
        config = Check.default_config
        extract = fmap $ second
            (\n -> (T.note_call n, T.note_pitch n, T.note_duration n))
    equal (f "_ 4s") [Right (1, ("", Just "4s", 1))]
    equal (f "na/ _ din/")
        [ Right (0, ("na", Nothing, 1))
        , Right (2, ("din", Nothing, 1))
        ]

test_resolve_pitch = do
    let f = map extract . check Check.default_config . parse
        extract = fmap $ fromMaybe "" . T.note_pitch . snd
    equal (f "4s r g") [Right "4s", Right "4r", Right "4g"]
    equal (f "4n s") [Right "4n", Right "5s"]
    equal (f "4s n") [Right "4s", Right "3n"]
    -- mid-point goes down.
    equal (f "4m s") [Right "4m", Right "4s"]
    equal (f "4n ,s") [Right "4n", Right "4s"]
    equal (f "4s 'n") [Right "4s", Right "4n"]
    equal (f "4s 's") [Right "4s", Right "5s"]
    equal (f "4s ,s") [Right "4s", Right "3s"]
    -- If the pitch is carried it remains empty, since the tracklang pitch
    -- track will carry it, but is still carried for octave inference.
    equal (f "5s c/ r") [Right "5s", Right "", Right "5r"]
    equal (f "5s 4 r") [Right "5s", Right "", Right "5r"]

-- TODO implement this
_test_carry_sub_block = do
    let f = fmap UiTest.extract_blocks . TScore.ui_state get_ext_dur
    -- duration and pitch get carried into sub-blocks
    right_equal (f "b = [1s2 [r]/]")
        [ ("b", UiTest.note_track [(0, 0.5, "1s"), (0.5, 0.5, "-t1c1 --")])
        , ("b-t1c1", UiTest.note_track [(0, 0.5, "1r")])
        ]
    right_equal (f "b = [1s2 alt[r][g]/]")
        [ ("b", UiTest.note_track
            [(0, 0.5, "1s"), (0.5, 0.5, "alt -t1c1a -t1c1b")])
        , ("b-t1c1a", UiTest.note_track [(0, 0.5, "1r")])
        , ("b-t1c1b", UiTest.note_track [(0, 0.5, "1g")])
        ]

test_resolve_repeats = do
    let f = map (bimap pretty extract) . check Check.default_config . parse
        extract = strip_note . snd
    let sa = Right . mk_pnote "4s"
    equal (f "4r4 . .") $ replicate 3 (Right $ mk_pnote "4r" (1/4))
    equal (f "3s1 | .") $ replicate 2 (Right $ mk_pnote "3s" 1)
    equal (f "4s4 ~") [sa (2/4)]
    equal (f "4s4 ~ .") [sa (2/4), sa (1/4)]
    equal (f "4s4 . ~") [sa (1/4), sa (2/4)]
    equal (f "4s4 ~ ~") [sa (3/4)]
    equal (f "4s4 . .") [sa (1/4), sa (1/4), sa (1/4)]
    equal (f "4s4~ . .") [sa (2/4), sa (1/4)]
    equal (f ".") [Left "0: repeat with no previous note"]

test_resolve_pitch_twelve = do
    let f = map extract . check config . parse
        config = Check.default_config
            { Check.config_scale = Check.scale_twelve }
        extract = fmap $ fromMaybe "" . T.note_pitch . snd
    equal (f "4c e") [Right "4c", Right "4e"]
    equal (f "4g c") [Right "4g", Right "5c"]
    equal (f "4f c") [Right "4f", Right "4c"]

mk_pnote :: pitch -> dur -> T.Note Text (Maybe pitch) dur
mk_pnote pitch = mk_note "" (Just pitch)

mk_note :: call -> pitch -> dur -> T.Note call pitch dur
mk_note call pitch dur = T.Note
    { note_call = call
    , note_pitch = pitch
    , note_zero_duration = False
    , note_duration = dur
    , note_pos = T.Pos 0
    }

test_resolve_time = do
    let f = second extract . Check.resolve_time . Check.multiplicative
            . parse_cdur
        extract = map (bimap error_msg (second T.note_duration))
            . just_errors . map EList.toEither
    equal (f "a b c") (3, [Right (0, 1), Right (1, 1), Right (2, 1)])
    equal (f "a b _") (3, [Right (0, 1), Right (1, 1)])
    equal (f "a~ a b") (3, [Right (0, 2), Right (2, 1)])
    equal (f "a~ b c") $ (3,)
        [ Left "note tied to different pitch: a ~ b"
        , Right (2, 1)
        ]
    equal (f "a~ a~ _") (3, [Left "note tied to rest"])
    equal (f "_~ a") (2, [Left "rest tied to note"])
    equal (f "a~ | a") (2, [Right (0, 2)])
    equal (f "_~ | _ a") (3, [Right (2, 1)])
    equal (f "a~") (0, [Left "final note has a tie"])

test_check_barlines = do
    let f = map error_msg . Either.lefts . EList.metas
            . Check.check_barlines Check.meter_44
            . Check.multiplicative . parse_cdur
    equal (f "| a4 b c e |") []
    equal (f "| a4 ; b ; c ; e |") []
    equal (f "| a4 b | c e |") ["barline check: token 3: saw |, expected none"]
    equal (f "a8 | b") ["barline check: token 1: saw |, expected none"]

test_multiplicative = do
    let f = map (fmap (fmap fst . e_ndur)) . Check.multiplicative . parse_cdur
        rjs = map (EList.Elt . Just)
    equal (f "a b c") (rjs [1, 1, 1])
    equal (f "a2 b.") (rjs [1/2, 3/4])
    equal (f "a1..") (rjs [1 + 3/4])
    equal (f "a2:1 b") (rjs [2, 2])
    -- Even though I can differentiate :2 from just 2, it becomes 1:2, just
    -- like plain 2.  I could make it like 2:1, but that seems confusing.
    equal (f "a:2 b") (rjs [1/2, 1/2])
    -- The numerator defaults back to 1 if you don't carry both.
    equal (f "a2:1 b2") (rjs [2, 1/2])

test_additive = do
    let f = map (fmap (fmap fst . e_ndur)) . Check.additive . parse_cdur
        rjs = map (EList.Elt . Just)
    equal (f "a1 b") (rjs [1/4, 1/4])
    equal (f "a1:8 b") (rjs [1/8, 1/8])
    equal (f "a2:3 b") (rjs [2/3, 2/3])
    equal (f "a:6 b") (rjs [1/6, 1/6])
    equal (f "a:6 b2") (rjs [1/6, 2/6])


-- * implementation

error_msg :: T.Error -> Text
error_msg (T.Error _ msg) = msg

just_errors :: [Either (Either a b) c] -> [Either a c]
just_errors = mapMaybe $ \case
    Left (Left a) -> Just $ Left a
    Left (Right _) -> Nothing
    Right c -> Just $ Right c

strip_note :: T.Note call pitch dur -> T.Note call pitch dur
strip_note note = note { T.note_pos = T.Pos 0 }

e_ndur :: T.Token call pitch ndur rdur -> Maybe ndur
e_ndur = \case
    T.TNote _ note -> Just $ T.note_duration note
    _ -> Nothing

parse_cdur :: Text
    -> Check.Stream (T.Token T.CallText T.Pitch (Either T.Time T.Duration)
        T.Duration)
parse_cdur = resolve_call_duration . parse

resolve_call_duration :: [T.Token T.CallText T.Pitch T.NDuration rdur]
    -> Check.Stream (T.Token T.CallText T.Pitch (Either T.Time T.Duration) rdur)
resolve_call_duration =
    map (EList.Elt . Identity.runIdentity . T.map_note_duration resolve)
    where
    resolve (T.NDuration dur) = pure $ Right dur
    resolve T.CallDuration = pure $ Left 0

-- | Rather than actually doing a TScore.resolve_sub_block, I'll just fake it.
convert_call :: T.Token T.Call pitch ndur rdur
    -> T.Token T.CallText pitch ndur rdur
convert_call = T.map_call $ \case
    T.Call call -> call
    sub@(T.SubBlock {}) -> "((" <> pretty sub <> "))"

parse :: Text -> [T.Token T.CallText T.Pitch T.NDuration T.Duration]
parse = map convert_call . Testing.expect_right . Parse.parse_text p_tokens
    where
    p_tokens = P.some (Parse.lexeme (Parse.parse Parse.default_config))

check :: Check.Config -> [T.Token T.CallText T.Pitch T.NDuration T.Duration]
    -> [Either T.Error (T.Time, T.Note T.CallText (Maybe Text) T.Time)]
check config = just_errors . fst
    . Check.check (const (Left "get_dur not supported", [])) config

get_ext_dur :: TScore.GetExternalCallDuration
get_ext_dur = \_ _ -> (Left "external call dur not supported", [])
