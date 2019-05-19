-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE TypeApplications #-}
module Derive.TScore.Parse_test where
import qualified Data.Text as Text
import qualified GHC.Stack as Stack

import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T
import qualified Derive.TScore.TScore as TScore

import qualified Ui.Id as Id

import           Global
import           Util.Test


everything_score :: Text
everything_score =
    "-- Line comments use double-dash.\n\
    \-- Toplevel directives apply to everything below them.\n\
    \-- The supported directives are in 'Check.parse_directive'.\n\
    \%meter=adi\n\
    \-- Optional per-block directives, and quoted block title.\n\
    \block1 = %scale=sargam \"block1 title\" [\n\
    \    -- Tracks start with an optional track title, which should start\n\
    \    -- with >. It doesn't need quotes if there are no spaces.\n\
    \    -- Ties (~) and dots (.) go at the end of the note.\n\
    \    >inst1 4s4 r g m~ | m d n. s8 |\n\
    \    // -- // to separate tracks\n\
    \    -- Bare tie ~ to sustain previous note, bare dot . to repeat,\n\
    \    -- so this is like \"2s~ 2s | 2s 2s |\"\n\
    \    \">inst2 | +pizz\" 2s ~ | . . |\n\
    \]\n\
    \\n\
    \-- Simple block with only one track, and one note.  kebab-case is ok.\n\
    \simple-block = [c]\n\
    \\n\
    \-- Wrapped tracks.\n\
    \wrapped = [\n\
    \    >i1 s r // >i2 g m\n\
    \    ///\n\
    \    >i1 p d // >i2 n s\n\
    \]\n\
    \\n\
    \calls = [\n\
    \    -- Call is separated from <pitch><dur> with a /.  Pitch and dur are\n\
    \    -- optional:\n\
    \    simple-block/r2 simple-block/\n\
    \]\n\
    \\n\
    \sub-blocks = [\n\
    \    -- Any place a call can appear can also have a sub-block:\n\
    \    [ 4s1 r // 4g2 ]/1 |\n\
    \    -- The sub-block can get a transformer.  Quotes needed for spaces:\n\
    \    +pizz[s]/1 | \"+pizz | harm\"[g]/\n\
    \]\n"

test_score = do
    let f = second unparse . parse @T.Score
    let score =
            "%meter=adi\n\
            \block1 = %block1=directive \"block1 title\" [\n\
            \    \">inst1\" a -- comment\n\
            \    // -- comment\n\
            \    >inst2 b\n\
            \]\n\
            \block2 = [c]\n"
    right_equal (f score) $
        "%meter=adi\n\
        \block1 = %block1=directive \"block1 title\"\
        \ [>inst1 a // >inst2 b]\n\
        \block2 = [c]\n"

    -- Parse -> unparse -> parse -> unparse reaches a fixpoint.
    let normalized = f everything_score
    equal normalized (f =<< normalized)
    putStrLn $ either id untxt normalized

test_default_call = do
    let f = fmap (\(T.Score defs) -> defs) . parse

    -- let score = "%default-call\nb = [b1/0 b2 b3]\n"
    -- right_equal (unparse . T.Score <$> f score) score
    -- right_equal (e_score_tokens <$> f score)
    --     [ tnote "b1" no_oct "" T.CallDuration
    --     , tnote "b2" no_oct "" no_dur
    --     , tnote "b3" no_oct "" no_dur
    --     ]
    --
    -- let score = "b = %default-call [a // b]\n"
    -- right_equal (unparse . T.Score <$> f score) score
    -- right_equal (e_score_tokens <$> f score)
    --     [ tnote "a" no_oct "" no_dur
    --     , tnote "b" no_oct "" no_dur
    --     ]

    let score = "b = [b1/0 b2 b3]\n"
    right_equal (unparse . T.Score <$> f score) score
    right_equal (e_score_tokens <$> f score)
        [ tnote "b1" no_oct "" T.CallDuration
        , tnote "" no_oct "b" (idur 2)
        , tnote "" no_oct "b" (idur 3)
        ]

e_score_tokens :: [(pos, T.Toplevel)]
    -> [T.Token T.Call T.Pitch T.NDuration T.Duration]
e_score_tokens defs = concat
    [ concatMap (map strip_pos . T.track_tokens) (e_tracks block)
    | T.BlockDefinition block <- map snd defs
    ]

test_p_whitespace = do
    let f = Parse.parse_text Parse.p_whitespace
    left_like (f "   a") "unexpected"
    right_equal (f "") ()
    right_equal (f "   ") ()
    right_equal (f " \n  \n") ()
    right_equal (f "-- hi\n") ()
    right_equal (f " -- hi\n") ()
    right_equal (f " -- hi\n   -- there") ()

test_pos = do
    let f = fmap (\(T.Score defs) -> defs) . parse
    let score =
            "%meter=adi\n\
            \block1 = %block1=directive \"block1 title\" [\n\
            \    \">inst1\" a b\n\
            \]\n"
    let show_pos pos = putStr $ untxt $
            T.show_error score (T.Error (T.Pos pos) "some error")
    let Right defs = f score

    show_pos 0
    show_pos 11
    equal (map fst defs) $ map T.Pos [0, 11]
    let tokens = concatMap T.track_tokens $ concat
            [ e_tracks block
            | (_, T.BlockDefinition block) <- defs
            ]
    show_pos 68
    show_pos 70
    equal (map (\t -> (T.token_pos t, unparse t)) tokens)
        [(T.Pos 68, "a"), (T.Pos 70, "b")]

test_roundtrip = do
    roundtrip (Proxy @Id.BlockId) "block1"
    roundtrip (Proxy @Id.BlockId) "x/a"
    roundtrip (Proxy @T.Directive) "%a=b"
    let p = Proxy @T.Score
    roundtrip p "b = [a]"
    roundtrip p "b = [[x y]/]"
    roundtrip p "b = [>hi a[x y]/4]"
    roundtrip p "b = [>hi \"a b\"[x y]/]"

test_tracks_wrapped = do
    let f = fmap extract . parse
        extract (T.WrappedTracks _ wrapped) =
            map (map strip_track . T.untracks) wrapped
    right_equal (f "[ a // b /// c // d ]")
        [ [T.Track "" [pnote0 "a"], T.Track "" [pnote0 "b"]]
        , [T.Track "" [pnote0 "c"], T.Track "" [pnote0 "d"]]
        ]

test_track = do
    let parse_track = fmap extract . parse
        extract (T.Track title tokens) = (title, map strip_pos tokens)
    let bar = T.TBarline no_pos . T.Barline
    let rest = T.TRest no_pos . T.Rest
    let title = fmap fst . parse_track
        tokens = fmap snd . parse_track
    right_equal (title "> a") ">"
    right_equal (title ">inst a") ">inst"
    right_equal (title "\">inst | trans\" a") ">inst | trans"
    right_equal (title "\"> | trans\" a") "> | trans"

    right_equal (tokens "| ||") [bar 1, bar 2]
    right_equal (tokens "a") [tnote "" no_oct "a" no_dur]
    right_equal (tokens "a -- hi") [tnote "" no_oct "a" no_dur]
    right_equal (tokens "_4 | _.")
        [ rest (T.Duration (Just 4) Nothing 0 False)
        , bar 1
        , rest (T.Duration Nothing Nothing 1 False)
        ]
    right_equal (tokens "a b/")
        [ tnote "" no_oct "a" no_dur
        , tnote "b" no_oct "" no_dur
        ]
    right_equal (tokens "> \"a b\"/") [tnote "a b" no_oct "" no_dur]
    right_equal (tokens "> \"a \"() b\"/") [tnote "a \"() b" no_oct "" no_dur]

    right_equal (tokens "a [b]/")
        [ tnote "" no_oct "a" no_dur
        , tnote (sub "" [[[tnote "" no_oct "b" no_dur]]]) no_oct "" no_dur
        ]
    right_equal (tokens "a\\ [b]/")
        [ tnote (sub "a" [[[tnote "" no_oct "b" no_dur]]]) no_oct "" no_dur
        ]

test_token = do
    let f = fmap strip_pos . parse
    left_like (f "") "unexpected end of input"
    right_equal (f "a") $ pnote "a" no_dur
    right_equal (f "a/") $ tnote "a" no_oct "" no_dur
    right_equal (f "a.") $ pnote "a" (dur Nothing Nothing 1 False)
    right_equal (f "+pizz/") $ tnote "+pizz" no_oct "" no_dur
    right_equal (f "a/'b1.~") $
        tnote "a" (T.Relative 1) "b" (dur (Just 1) Nothing 1 True)
    right_equal (f "a'/a#4") $
        tnote "a'" no_oct "a#" (dur (Just 4) Nothing 0 False)
    right_equal (f "a0") $ pnote "a" T.CallDuration
    right_equal (f "a/a0") $ tnote "a" no_oct "a" T.CallDuration
    right_equal (f "a/1") $ tnote "a" no_oct "" (idur 1)
    right_equal (f "a1:2") $ pnote "a" (dur (Just 1) (Just 2) 0 False)
    right_equal (f "a:2") $ pnote "a" (dur Nothing (Just 2) 0 False)
    right_equal (f "a*") $
        set_zero_dur $ pnote "a" (dur Nothing Nothing 0 False)

    -- These are treated specially by Check, but are normal notes according to
    -- the  parser.
    right_equal (f ".") $ tnote "" no_oct "" (dur Nothing Nothing 1 False)
    right_equal (f "~") $ tnote "" no_oct "" (dur Nothing Nothing 0 True)

test_token_sub_block = do
    let f = fmap strip_pos . parse
        pitch p dur = tnote "" no_oct p dur

    right_equal (f "[a]/2") $ tnote
        (sub "" [[[pitch "a" no_dur]]])
        no_oct "" (idur 2)
    right_equal (f "[a // b2]/") $ tnote
        (sub "" [[[pitch "a" no_dur], [pitch "b" (idur 2)]]])
        no_oct "" no_dur
    right_equal (f "[[x]/]/") $ tnote
        (sub "" [[[tnote (sub "" [[[pitch "x" no_dur]]]) no_oct "" no_dur]]])
        no_oct "" no_dur
    right_equal (f "a[b]/") $
        tnote (sub "a" [[[pitch "b" no_dur]]]) no_oct "" no_dur
    right_equal (f "\"x y\"[b]/") $
        tnote (sub "x y" [[[pitch "b" no_dur]]]) no_oct "" no_dur
    right_equal (f "a[b][c]/") $ tnote
        (sub "a" [[[pitch "b" no_dur]], [[pitch "c" no_dur]]])
        no_oct "" no_dur

    right_equal (f "a[b]\\ -- hi\n  [c]/") $ tnote
        (sub "a" [[[pitch "b" no_dur]], [[pitch "c" no_dur]]])
        no_oct "" no_dur
    right_equal (f "a\\ [b]\\ [c]/") $ tnote
        (sub "a" [[[pitch "b" no_dur]], [[pitch "c" no_dur]]])
        no_oct "" no_dur

test_token_roundtrip = do
    -- Lots of things can roundtrip but still not parse correctly, so this is
    -- not as good as 'test_token'.
    let p = Proxy @(T.Token T.Call T.Pitch T.NDuration T.Duration)
    roundtrip p "4a"
    roundtrip p "a."
    roundtrip p "a~"
    roundtrip p ",a"
    roundtrip p "+pizz/"
    roundtrip p "\"a b\"/"
    roundtrip p "a/'b1.~"
    roundtrip p "a*4:2"
    roundtrip p "a[b]/"
    roundtrip p "a[b][c]/"

-- * implementation

roundtrip :: forall a. (Stack.HasCallStack, Parse.Element a)
    => Proxy a -> Text -> IO Bool
roundtrip Proxy t =
    right_equal (Text.strip <$> second unparse (parse @a t)) t

strip_pos :: T.Token T.Call pitch ndur rdur -> T.Token T.Call pitch ndur rdur
strip_pos = \case
    T.TBarline _ a -> T.TBarline no_pos a
    T.TNote _ a -> T.TNote no_pos (strip_note a)
    T.TRest _ a -> T.TRest no_pos a
    where
    strip_note note = note
        { T.note_call = case T.note_call note of
            T.SubBlock prefix subs -> T.SubBlock prefix (map strip_tracks subs)
            call -> call
        , T.note_pos = no_pos
        }
    strip_tracks (T.Tracks tracks) = T.Tracks $ map strip_track tracks

e_tracks :: T.Block T.WrappedTracks -> [T.Track T.Call]
e_tracks = T.untracks . expect_right . TScore.unwrap_tracks . T.block_tracks

strip_track :: T.Track T.Call -> T.Track T.Call
strip_track track =
    track { T.track_tokens = map strip_pos (T.track_tokens track) }

no_oct :: T.Octave
no_oct = T.Relative 0

no_dur :: T.NDuration
no_dur = Parse.empty_nduration

dur :: Maybe Int -> Maybe Int -> Int -> Bool -> T.NDuration
dur int1 int2 dots tie = T.NDuration (T.Duration int1 int2 dots tie)

idur :: Int -> T.NDuration
idur int1 = dur (Just int1) Nothing 0 False

no_pos :: T.Pos
no_pos = T.Pos 0

tracks :: [(Text, [T.Token call T.Pitch T.NDuration T.Duration])]
    -> T.Tracks call
tracks = T.Tracks . map (uncurry T.Track)

tnote :: call -> T.Octave -> Text -> T.NDuration
    -> T.Token call T.Pitch T.NDuration T.Duration
tnote call oct pitch dur = T.TNote no_pos $ T.Note
    { note_call = call
    , note_pitch = T.Pitch oct pitch
    , note_zero_duration = False
    , note_duration = dur
    , note_pos = no_pos
    }

pnote :: Text -> T.NDuration -> T.Token T.Call T.Pitch T.NDuration T.Duration
pnote p dur = tnote "" no_oct p dur

pnote0 :: Text -> T.Token T.Call T.Pitch T.NDuration T.Duration
pnote0 p = tnote "" no_oct p no_dur

sub :: T.CallText -> [[[T.Token T.Call T.Pitch T.NDuration T.Duration]]]
    -> T.Call
sub prefix ts = T.SubBlock prefix (map (tracks . zip (repeat "")) ts)

set_zero_dur :: T.Token call pitch ndur rdur -> T.Token call pitch ndur rdur
set_zero_dur = \case
    T.TNote pos note -> T.TNote pos $ note { T.note_zero_duration = True }
    tnote -> tnote

parse :: Parse.Element a => Text -> Either String a
parse = Parse.parse_text (Parse.parse Parse.default_config)

unparse :: Parse.Element a => a -> Text
unparse = Parse.unparse Parse.default_config
