-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE StrictData #-}
{- | Similar to "Derive.TScore.Parse", but specialized for javanese notation.

    - Use numbers for pitches instead of letters.  Infer octave based on
    instrument, and ' ,
    - Parse notes without spaces.  Possible because pitch is a single digit.
    - Trailing / for damped notes.
    - Use . for continued pitch, _ for rest.  Usually duration is implicit.
    But inferring gender style from pipilan seems complicated.
    - Can't use . for low if I use it for rest!  Or , and ' like lilypond?
    - Infer left and right hand for instruments.
    . Associate tags (irama, name, balungan, prev seleh, seleh)
    . Switch laras and pathet, e.g. slendro manyura to pelog barang.
    . Possibly allow short and long names?  E.g. kutuk-kuning = kk
    - "|" is the same.  I could use || for gatra if I have multiple gatra things
        (puthut gelut?  or just have two separate calls?)
    . Bar | is optional in TS, but I could make it required to infer rhythm.
    . Biggest difference is to infer durations.  This limits | ability to catch
    errors, but since I only allow 4 and 8, not really.  With explicit dots
    it's easy, parse all notes in the measure, check for 4 or 8.  With implicit
    trailing dots I have to remember spaces and require space separation.

    How to do tempo?  Hard to draw overbars.
    In tanggung, one bar is 2 notes.
    In dadi, one bar is 1 note.
    In wiled, one bar is 1 note.

    Since I know expected bar duration, I can infer 4 or 8 per.
    Then could use explicit dots.  Could then infer trailing dots if
    I separate notes with spaces.  But since I only go up to 2x, if there
    are >4 notes and <=8, I can infer groups?  Not sure, maybe explicit dots is
    better.

    TODO it would be nicer if this were simply a subset of normal tscore.
    Not sure if possible.  At the least it should reuse as much as possible
    of Check, and all of TScore.
-}
module Derive.TScore.Java where
import qualified Data.Text as Text

import qualified Util.P as P
import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T
import qualified Ui.Id as Id

import           Global


data Pitch = Pitch Octave PitchClass
    deriving (Eq, Show)
type Octave = Int

data PitchClass = P1 | P2 | P3 | P4 | P5 | P6 | P7
    deriving (Eq, Ord, Show)

-- | Valid directives at the score level.
score_tags :: [Text]
score_tags = ["source", "piece", "pathet", "inst", "section", "irama"]

-- | Valid directives at the block level.
block_tags :: [Text]
block_tags = ["gatra"]

-- | Valid values for %irama tag.  Along with %inst, affects expected
-- number of notes per barline.
irama :: [Text]
irama = ["lancar", "tanggung", "dadi", "wiled", "rangkep"]

{-
    Then I can write down by names and balungan, and it matches in the same
    piece.  Later if not found, I could try to snag from other pieces, adapt
    from other pathet, adjust seleh 2 to seleh 1, etc.

    Alternately, I could go the solkattu route and make this a haskell DSL.
    Benefits are haskell tools and ghci, downsides are no dedicated syntax.
    Notes would have to be in a FromString, at which point I'm back to having
    to parse again.  But the declarations would be in a big list or Map,
    given the All.hs generation thing like solkattu.

    If I go line-based then I have to use indentation, but less clutter.
    Otherwise, I need []s for tracks.

    %irama = tanggung
    %inst = gb -- gender-barung
    dualolo b=3231 = [
        > 5653 | .6 .5 6. 1.
        > .12. | 6. 2. 32 1.
    ]

    tumurun b=3216 =
        > 5 6 .1 .6 | 1 2 1. 6
        > . 5 3. 5. | 6 1 21 6

    -- Could either have qualifiers like kempyung and gembyang separate, or
    -- just parse them out of the name.
    kutuk-kuning-kempyung %b=1632 =
        > 1 2 .3 .2 | 3 2 1. 6
        > . 1 6  1  | 2 3 53 2
-}

parse_score :: Text -> Either String Score
parse_score = Parse.parse_text (Parse.parse Parse.default_config)

newtype Score = Score [(T.Pos, Toplevel)]
    deriving (Eq, Show)

instance Parse.Element Score where
    parse config = Parse.p_whitespace
        *> (Score <$> P.many (Parse.parse config))
    unparse config (Score toplevels) =
        Text.unlines $ map (Parse.unparse config) toplevels

data Toplevel = ToplevelDirective T.Directive | BlockDefinition Block
    deriving (Eq, Show)

instance Parse.Element Toplevel where
    parse config = Parse.lexeme $
        ToplevelDirective . un <$> Parse.parse config
        <|> BlockDefinition <$> Parse.parse config
        where un (Parse.ToplevelDirective d) = d
    unparse config = \case
        ToplevelDirective a -> Parse.unparse config (Parse.ToplevelDirective a)
        BlockDefinition a -> Parse.unparse config a

data Block = Block {
    block_name :: Text
    , block_directives :: [T.Directive]
    , block_tracks :: Tracks
    } deriving (Eq, Show)

-- | > name = directive [ tracks ]
instance Parse.Element Block where
    parse config = do
        block_name <- Parse.lexeme $ P.takeWhile1 Id.is_id_char
        -- block_name <- Parse.lexeme $ P.takeWhile1 $ \c ->
        --     'a' <= c && c <= 'z' || c == '-'
        Parse.keyword "="
        block_directives <- P.many (Parse.lexeme (Parse.parse config))
        block_tracks <- Parse.parse config
        return $ Block { block_name, block_directives, block_tracks }
    unparse config (Block { block_name, block_directives, block_tracks }) =
        Text.unwords $ filter (not . Text.null) $ concat
            [ [block_name, "="]
            , map (Parse.unparse config) block_directives
            , [Parse.unparse config block_tracks]
            ]

newtype Tracks = Tracks [Track]
    deriving (Eq, Show)

instance Parse.Element Tracks where
    parse config = fmap Tracks $ Parse.lexeme "[" *> tracks <* Parse.lexeme "]"
        where tracks = P.some (Parse.parse config)
    unparse config (Tracks tracks) =
        "[ " <> Text.unwords (map (Parse.unparse config) tracks) <> " ]"

data Track = Track {
    track_tokens :: [Token]
    , track_pos :: T.Pos
    } deriving (Eq, Show)

instance Parse.Element Track where
    parse config = do
        track_pos <- Parse.get_pos
        Parse.keyword ">"
        track_tokens <- P.many $ Parse.lexeme $ Parse.parse config
        return $ Track { track_tokens, track_pos }
    unparse config (Track { track_tokens }) =
        Text.unwords $ ">" : map (Parse.unparse config) track_tokens
        -- If I do this, barlines also get no spaces.
        -- mconcat $ "> " : map (Parse.unparse config) track_tokens

type Token = T.Token () Pitch () Rest

instance Parse.Element Token where
    parse config =
        T.TBarline <$> Parse.get_pos <*> Parse.parse config
        <|> T.TRest <$> Parse.get_pos <*> Parse.parse config
        <|> T.TNote <$> Parse.get_pos <*> Parse.parse config
    unparse config (T.TBarline _ bar) = Parse.unparse config bar
    unparse config (T.TNote _ note) = Parse.unparse config note
    unparse config (T.TRest _ rest) = Parse.unparse config rest

data Rest = RestSustain | RestStop
    deriving (Eq, Show)

instance Parse.Element (T.Rest Rest) where
    parse _ = fmap T.Rest $
        P.char '.' *> pure RestSustain <|> P.char '_' *> pure RestStop
    unparse _ (T.Rest r) = case r of
        RestSustain -> "."
        RestStop -> "_"

instance Parse.Element (T.Note () Pitch ()) where
    parse config = do
        pos <- Parse.get_pos
        pitch <- Parse.parse config
        note_zero_duration <- P.option False (P.char '/' *> pure True)
        return $ T.Note
            { note_call = ()
            , note_pitch = pitch
            , note_zero_duration
            , note_duration = ()
            , note_pos = pos
            }
    unparse config (T.Note { note_pitch, note_zero_duration }) =
        Parse.unparse config note_pitch
        <> if note_zero_duration then "/" else ""

instance Parse.Element PitchClass where
    parse _ = P.satisfy (\c -> '1' <= c && c <= '9') >>= \case
        '1' -> pure P1
        '2' -> pure P2
        '3' -> pure P3
        '4' -> pure P4
        '5' -> pure P5
        '6' -> pure P6
        '7' -> pure P7
        _ -> mzero
    unparse _ p = txt $ drop 1 (show p)

instance Parse.Element Pitch where
    parse config = do
        oct <- Text.foldl' (\n c -> n + if c == ',' then -1 else 1) 0 <$>
            P.takeWhile (`elem` (",'" :: String))
        p <- Parse.parse config
        pure $ Pitch oct p
    unparse config (Pitch oct pc) = octs <> Parse.unparse config pc
        where
        octs = case compare oct 0 of
            EQ -> ""
            LT -> Text.replicate (- oct) ","
            GT -> Text.replicate oct "'"

-- variations: append 123567 for e.g. gantung 2, cilik, kecil, gede, besar,
-- kempyung / gembyang
standardNames :: [(Text, Text)]
standardNames =
    [ ("ayu kuning", "ak")
    , ("debyang debyung", "dby")
    , ("dualolo", "dll")
    , ("duduk", "ddk")
    , ("gantung", "gant")
    , ("gelut", "g")
    , ("jarik kawung", "jk")
    , ("kacaryan", "kcy")
    , ("kutuk kuning", "kk")
    , ("puthut semedi", "ps")
    , ("puthut", "p") -- 2 part pattern puthut gelut
    , ("tumurun", "tm")
    ]
