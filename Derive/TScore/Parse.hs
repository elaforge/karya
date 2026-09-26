-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Parse the tscore language.
module Derive.TScore.Parse (
    -- * Parse
    parseScore
    -- , parse_allocation, unparse_allocations
    , defaultCall, defaultNamespace
    , showBlock, showBlockTrack
    -- * Note
    , dotNote, tieNote
    -- * util
    , stripComment
    , pWhitespace_
    -- * parser implementation
    , Element(..)
    , Parser
    , parseText
    , defaultConfig
    , getPos
    , keyword
    , lexeme
    , pDirective
    , pWhitespace

#ifdef TESTING
    , module Derive.TScore.Parse
#endif
) where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Void as Void

import qualified Util.Control as Control
import qualified Util.P as P
import           Util.P ((<?>))
import qualified Util.Parse as Parse

import qualified Derive.TScore.T as T
import qualified Ui.Id as Id

import           Global
import           Types


-- | Parsing config.  Parsed 'T.Directive's can affect further parsing, which
-- is not very nice, but convenient for concise notation.
newtype Config = Config
    -- | If true, notes with no call get the pitch as their call.
    { _defaultCall :: Bool
    } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config False

-- * parse

parseScore :: Text -> Either String T.Score
parseScore = parseText (parse defaultConfig)

parseText :: Parser a -> Text -> Either String a
parseText p = first P.errorBundlePretty . P.parse (p <* P.eof) ""

type Parser a = P.Parsec Void.Void Text a

getPos :: Parser T.Pos
getPos = T.Pos . P.stateOffset <$> P.getParserState

class Element a where
    parse :: Config -> Parser a
    unparse :: Config -> a -> Text

instance Element a => Element (T.Pos, a) where
    parse config = (,) <$> getPos <*> parse config
    unparse config = unparse config . snd

instance Element T.Score where
    parse config = fmap T.Score $ do
        pWhitespace
        Control.loop1 config $ \loop config ->
            P.optional (lexeme (parse config)) >>= \case
                Nothing -> pure []
                Just toplevel -> (toplevel:) <$>
                    loop (updateConfig (directivesOf (snd toplevel)) config)
    unparse config (T.Score toplevels) =
        Text.unlines $ snd $ List.mapAccumL un config toplevels
        where
        un config toplevel =
            ( updateConfig (directivesOf (snd toplevel)) config
            , unparse config toplevel
            )

updateConfig :: [T.Directive] -> Config -> Config
updateConfig directives config
    | defaultCallSet directives = config { _defaultCall = True }
    | otherwise = config

defaultCallSet :: [T.Directive] -> Bool
defaultCallSet directives = not $ null
    [() | T.Directive _ name _ <- directives, name == defaultCall]

defaultCall :: Text
defaultCall = "default-call"

directivesOf :: T.Toplevel -> [T.Directive]
directivesOf (T.ToplevelDirective d) = [d]
directivesOf _ = []

instance Element T.Toplevel where
    parse config =
        T.ToplevelDirective . un <$> parse config
        <|> T.BlockDefinition <$> parse config
        where un (ToplevelDirective d) = d
    unparse config = \case
        T.ToplevelDirective a -> unparse config (ToplevelDirective a)
        T.BlockDefinition a -> unparse config a

-- > ns/block = %directive "block title" [ ... ]
instance Element (T.Block T.WrappedTracks) where
    parse config = do
        blockId <- lexeme (parse config)
        keyword "="
        blockDirectives <- P.many (lexeme (parse config))
        blockTitle <- P.option "" (lexeme pString)
        blockTracks <- parse (updateConfig blockDirectives config)
        pure $ T.Block
            { blockId, blockDirectives, blockTitle, blockTracks }
    unparse config (T.Block
            { blockId, blockDirectives, blockTitle, blockTracks }) =
        Text.unwords $ filter (not . Text.null) $ concat
            [ [unparse config blockId, "="]
            , map (unparse config) blockDirectives
            , [if Text.null blockTitle then "" else unString blockTitle]
            , [unparse (updateConfig blockDirectives config) blockTracks]
            ]

instance Element Id.BlockId where
    parse _ = do
        a <- P.takeWhile1 Id.isIdChar
        mb <- P.optional $ P.try $ P.char '/' *> (P.takeWhile1 Id.isIdChar)
        let bid = maybe (Id.id defaultNamespace a)
                (\b -> Id.id (Id.namespace a) b) mb
        maybe (fail $ "invalid BlockId: " <> prettys bid) pure (Id.make bid)
        <?> "BlockId"
    unparse _ = showBlock

showBlock :: Id.BlockId -> Text
showBlock = Id.showShort defaultNamespace . Id.unpackId

showBlockTrack :: Id.BlockId -> TrackNum -> Text
showBlockTrack blockId tracknum =
    showBlock blockId <> ":" <> showt tracknum

defaultNamespace :: Id.Namespace
defaultNamespace = Id.namespace "tscore"

-- | Tracks are bounded with [ ], and multiple Tracks can be simply written
-- in sequence, like [ a b ] [ c d ], which then get unwrapped as [ a+c c+d ].
instance Element T.WrappedTracks where
    parse config = T.WrappedTracks <$> getPos
        <*> P.some (lexeme (parse config))
    unparse config (T.WrappedTracks _ wrapped) =
        Text.unwords $ map (unparse config) wrapped

-- | Tracks are surrounded by [ ].  Each track is delimited by a word starting
-- with >.
instance Element (T.Tracks T.Call) where
    parse config = fmap T.Tracks $ lexeme "[" *> tracks <* lexeme "]"
        where
        tracks = P.sepBy1 (lexeme (parse config)) $
            P.lookAhead $ lexeme (P.char '>' *> pure () <|> "\">" *> pure ())
    unparse config (T.Tracks tracks) =
        "[" <> Text.unwords (map (unparse config) tracks) <> "]"

instance Element (T.Track T.Call) where
    parse config = do
        trackPos <- getPos
        (trackKey, trackTitle) <- P.option ("", "") $ lexeme $ (,)
            <$> (P.char '>' *> P.takeWhile isKeyChar)
            <*> fmap (">"<>) (pString <|> P.takeWhile Id.isIdChar)
        trackDirectives <- P.many $ lexeme $ parse config
        trackTokens <- P.many $ lexeme $ parse config
        pure $ T.Track
            { trackKey, trackTitle, trackDirectives, trackTokens
            , trackPos
            }
    unparse config (T.Track
            { trackKey, trackTitle, trackDirectives, trackTokens }) =
        Text.unwords $ filter (not . Text.null) $ concat
            -- See 'T.track_title' for why this is so complicated.
            [ (:[]) $ if trackKey == "" && trackTitle == "" then ""
                else ">" <> trackKey <> if " " `Text.isInfixOf` trackTitle
                    then unString (Text.drop 1 trackTitle)
                    else Text.drop 1 trackTitle
            , map (unparse config) trackDirectives
            , map (unparse config) trackTokens
            ]

isKeyChar :: Char -> Bool
isKeyChar c = c `elem` ("!@#$%^&*" :: [Char])
    -- Technically this could be `c /='"' && not (Id.is_id_char c)`, but I'll be
    -- more restrictive for now.

-- | Apply different parsing rules to a 'T.ToplevelDirective'.
newtype ToplevelDirective = ToplevelDirective T.Directive
    deriving (Eq, Show)

-- | At the toplevel, I need a % to disambiguate from 'T.BlockDefinition'.
-- But, I can have spaces around =.
instance Element ToplevelDirective where
    parse _ = ToplevelDirective <$> pDirective True
    unparse _ (ToplevelDirective d) = unparseDirective True d

-- | Block level directives don't allow spaces around = even though I don't
-- think they would cause a problem.  Previously I thought I could omit %s,
-- but it turns out to make it ambiguous with notes, especially for
-- track_directives.
instance Element T.Directive where
    parse _ = pDirective False
    unparse _ = unparseDirective False

pDirective :: Bool -> Parser T.Directive
pDirective spaces = do
    pos <- getPos
    P.char '%'
    key <- P.takeWhile1 Id.isIdChar
    when spaces pWhitespace
    val <- P.optional $ do
        P.char '='
        when spaces pWhitespace
        pMultiString <|> pWord
    pure $ T.Directive pos key val

unparseDirective :: Bool -> T.Directive -> Text
unparseDirective spaces (T.Directive _ lhs rhs) =
    mconcat ["%", lhs, maybe "" ((equal<>) . unparseDirectiveValue) rhs]
    where equal = if spaces then " = " else "="

unparseDirectiveValue :: Text -> Text
unparseDirectiveValue val
    | Text.any Char.isSpace val = "''\n" <> val <> "\n''"
    | otherwise = val

pMultiString :: Parser Text
pMultiString = "''" *> contents <* "''"
    where
    contents = dedent . Text.intercalate "'" <$>
        ((:) <$> content <*> P.many chunk)
    content = P.takeWhile1 (/='\'')
    -- TODO this is inefficient, surely there's a way to takeWhile but look at
    -- the next char?
    chunk = P.try $ "'" *> P.takeWhile1 (/='\'')

dedent :: Text -> Text
dedent t = Text.strip $ case Text.lines (Text.dropWhile (=='\n') t) of
    [] -> ""
    x : xs -> Text.unlines (map (strip indent) (x:xs))
        where indent = Text.takeWhile Char.isSpace x
    where strip pref s = fromMaybe s $ Text.stripPrefix pref s

type Token = T.Token T.Call (T.NPitch T.Pitch) T.NDuration T.Duration

instance Element Token where
    parse config =
        T.TBarline <$> getPos <*> parse config
        <|> T.TRest <$> getPos <*> parse config
        <|> T.TNote <$> getPos <*> parse config
    unparse config (T.TBarline _ bar) = unparse config bar
    unparse config (T.TNote _ note) = unparse config note
    unparse config (T.TRest _ rest) = unparse config rest

instance Pretty Token where
    pretty = unparse defaultConfig

instance Pretty (T.Token T.CallText T.Pitch T.NDuration T.Duration) where
    pretty = \case
        T.TNote _ note -> pretty note
        T.TBarline _ bar -> pretty bar
        T.TRest _ rest -> pretty rest

-- ** barline

instance Element T.Barline where
    parse _ = T.Barline <$> (Text.length <$> P.takeWhile1 (=='|'))
        <|> (P.char ';' *> pure T.AssertCoincident)
    unparse _ (T.Barline n)
        | n >= 1 = Text.replicate n "|"
        | otherwise = error $ "Barline <= 0: " <> show n
    unparse _ T.AssertCoincident = ";"

instance Pretty T.Barline where pretty = unparse defaultConfig

-- * Note

-- | Parse a note with a letter pitch.
--
-- > <call><oct><pitch><zero-dur><dur><dots><tie>
-- > call/  4    s      *         4    .     ~
-- >        ,    s                :1
--
-- oct is optional, but oct without pitch is not so useful, so pitch >=1 char.
--
-- > a a2 call/a2
-- > a2.
-- > a~ a2~
-- > "call with spaces"/
instance Element (T.Note T.Call (T.NPitch T.Pitch) T.NDuration) where
    parse config
        -- I need this P.try because / is an empty note and I need to backtrack
        -- when it fails, to parse //.
        | _defaultCall config = P.try $ do
            pos <- getPos
            call <- P.optional $ parse config
            (pitch, zeroDur, dur) <-
                P.option (emptyNpitch, False, emptyNduration) $ do
                    P.char '/'
                    (,,) <$> P.option emptyNpitch (P.try (parse config))
                         <*> pZeroDur
                         <*> parse config
            makeNote pos call pitch zeroDur dur
        | otherwise = do
            pos <- getPos
            call <- P.optional $ P.try $ parse config <* P.char '/'
            -- I need a try, because if it starts with a number it could be
            -- an octave, or a duration.
            pitch <- P.option emptyNpitch $ P.try (parse config)
            zeroDur <- pZeroDur
            dur <- parse config
            makeNote pos call pitch zeroDur dur
        where
        pZeroDur = P.option False (P.char '*' *> pure True)
        makeNote notePos call pitch noteZeroDuration noteDuration = do
            -- If I allow "" as a note, I can't get P.many of them.
            guard (note { T.notePos = T.Pos 0 } /= emptyNote)
            pure note
            where
            note = T.Note
                { noteCall = fromMaybe (T.Call "") call
                , notePitch = pitch
                , noteZeroDuration
                , noteDuration
                , notePos
                }
    unparse config (T.Note
            { noteCall, notePitch, noteZeroDuration, noteDuration })
        | _defaultCall config =
            unparse config noteCall
            <> if (notePitch, noteZeroDuration, noteDuration) == empty
                then "" else mconcat
                [ "/"
                , unparse config notePitch
                , if noteZeroDuration then "*" else ""
                , unparse config noteDuration
                ]
        | otherwise = mconcat
            [ if noteCall == T.Call "" then ""
                else unparse config noteCall <> "/"
            , unparse config notePitch
            , if noteZeroDuration then "*" else ""
            , unparse config noteDuration
            ]
        where
        empty = (emptyNpitch, False, emptyNduration)

-- | This is the output from Check.check.
instance Pretty (T.Note T.CallText (Maybe Text) T.Time) where
    pretty (T.Note call pitch _zeroDur dur _pos) = mconcat
        [ if call == "" then "" else call <> "/"
        , fromMaybe "" pitch, pretty dur
        ]

instance Pretty (T.Note T.CallText T.Pitch T.NDuration) where
    pretty (T.Note call pitch _zeroDur dur _pos) = mconcat
        [ if call == "" then "" else call <> "/"
        , unparse defaultConfig pitch, unparse defaultConfig dur
        ]

instance Element (T.NPitch T.Pitch) where
    parse config =
        P.char '^' *> pure T.CopyFrom <|> T.NPitch <$> parse config
    unparse config = \case
        T.CopyFrom -> "^"
        T.NPitch pitch -> unparse config pitch

emptyNote :: T.Note T.Call (T.NPitch T.Pitch) T.NDuration
emptyNote = T.Note
    { noteCall = T.Call ""
    , notePitch = emptyNpitch
    , noteZeroDuration = False
    , noteDuration = emptyNduration
    , notePos = T.Pos 0
    }

emptyNduration :: T.NDuration
emptyNduration = T.NDuration emptyDuration

emptyDuration :: T.Duration
emptyDuration = T.Duration
    { durInt1 = Nothing
    , durInt2 = Nothing
    , durDots = 0
    , durTie = False
    }

emptyNpitch :: T.NPitch T.Pitch
emptyNpitch = T.NPitch emptyPitch

emptyPitch :: T.Pitch
emptyPitch = T.Pitch (T.Relative 0) ""

-- | This note is treated specially by the Check layer, to repeat of the
-- previous note.
dotNote :: T.Note T.CallText (T.NPitch T.Pitch) T.NDuration
dotNote = emptyNote
    { T.noteCall = ""
    , T.noteDuration = T.NDuration $ emptyDuration { T.durDots = 1 }
    }

-- | This note is treated specially by the Check layer, to repeat of the
-- previous note, plus put a tie on the previous note.
tieNote :: T.Note T.CallText (T.NPitch T.Pitch) T.NDuration
tieNote = emptyNote
    { T.noteCall = ""
    , T.noteDuration = T.NDuration $ emptyDuration { T.durTie = True }
    }

-- |
-- > plain-word
-- > "ns/block"
-- > "word with spaces"
-- > "with embedded "() quote"
-- > [sub // block]
-- > plain-word[sub // block]
-- > "spaces word"[sub]
instance Element T.Call where
    parse config =
        uncurry T.SubBlock <$> P.try (pSubblock config)
        <|> T.Call <$> pString
        <|> T.Call <$> P.takeWhile1 callChar
        <?> "call"
    unparse _ (T.Call call)
        | Text.any (`elem` [' ', '/']) call = "\"" <> call <> "\""
        | otherwise = call
    unparse config (T.SubBlock prefix tracks) = mconcat $
        (if Text.null prefix then "" else unparse config (T.Call prefix))
        : map (unparse config) tracks

instance Pretty T.Call where pretty = unparse defaultConfig

pSubblock :: Config -> Parser (Text, [T.Tracks T.Call])
pSubblock config = (,)
    <$> lexemeB (pString <|> P.takeWhile1 callChar <|> pure "")
    <*> P.some (lexemeB (parse config))

pString :: Parser Text
pString = fmap mconcat $ P.between (P.char '"') (P.char '"') $
    P.many $ P.try (P.string "\"(") <|> (Text.singleton <$> P.satisfy (/='"'))

unString :: Text -> Text
unString str = "\"" <> str <> "\""

instance Element (T.Rest T.Duration) where
    -- I could possibly forbid ~ tie for rests, but I don't see why
    parse config = T.Rest <$> (P.char '_' *> parse config)
    unparse config (T.Rest dur) = "_" <> unparse config dur

instance Pretty (T.Rest T.Duration) where
    pretty = unparse defaultConfig

callChar :: Char -> Bool
callChar = notIn
    [ '/'
    , '\\'
    , '['
    , ']' -- so a Note inside a SubBlock doesn't eat the ]
    , '>' -- so I can use > to separate tracks when default-call is on
    ]

pitchChar :: Char -> Bool
pitchChar c = notIn exclude c && not (Char.isDigit c)
    where
    -- This breaks modularity because I have to just know all the syntax that
    -- could come after, which is Duration, end of Tracks, Rest.  But the more
    -- I can get into Pitch, the more I can get into Call without needing ""s.
    exclude =
        [ '~', '.' -- pitch is followed by T.NDuration
        , ':' -- T.Duration
        , ']' -- end of T.Tracks
        , '*' -- zero duration marker
        , '\\' -- skip whitespace
        , '>' -- this separates tracks
        ]

-- ** Pitch

instance Element T.Pitch where
    parse config = T.Pitch <$>
        parse config <*> P.takeWhile1 pitchChar <?> "pitch"
    unparse config (T.Pitch octave call) = unparse config octave <> call

instance Pretty T.Pitch where pretty = unparse defaultConfig

instance Element T.Octave where
    parse _ =
        T.Absolute <$> Parse.pInt <|> T.Relative <$> pRelative <?> "octave"
        where
        pRelative = Text.foldl' (\n c -> n + if c == ',' then -1 else 1) 0 <$>
            P.takeWhile (`elem` (",'" :: String))
    unparse _ (T.Absolute oct) = showt oct
    unparse _ (T.Relative n)
        | n >= 0 = Text.replicate n "'"
        | otherwise = Text.replicate (-n) ","

instance Pretty T.Octave where pretty = unparse defaultConfig

-- ** Duration

instance Element T.NDuration where
    parse config =
        P.char '0' *> pure T.CallDuration <|> T.NDuration <$> parse config
    unparse config = \case
        T.CallDuration -> "0"
        T.NDuration a -> unparse config a

instance Element T.Duration where
    parse _ = T.Duration
        <$> P.optional Parse.pNat
        <*> P.optional (P.char ':' *> Parse.pNat)
        <*> (Text.length <$> P.takeWhile (=='.'))
        <*> P.option False (P.char '~' *> pure True)
        <?> "duration"
    unparse _ (T.Duration int1 int2 dots tie) = mconcat
        [ maybe "" showt int1
        , maybe "" ((":"<>) . showt) int2
        , Text.replicate dots "."
        , if tie then "~" else ""
        ]

-- * util

pWord :: Parser Text
pWord = P.takeWhile1 (notIn "")

pWhitespace :: Parser ()
pWhitespace = P.skipMany pSpace

-- | 'p_whitespace', but True if there was any space to skip.
pWhitespace_ :: Parser Bool
pWhitespace_ =
    (maybe False (\() -> True) <$> P.optional pSpace) <* (P.skipMany pSpace)

pSpace :: Parser ()
pSpace = P.space1 <|> pComment
    where
    pComment = do
        P.string "--"
        P.skipWhile (/='\n')
        P.option () (void $ P.char '\n')

stripComment :: Text -> Text
stripComment = head . Text.splitOn "--"

lexeme :: Parser a -> Parser a
lexeme = (<* pWhitespace)

-- | Use a backslash to allow whitespace.  If I had a separate tokenizer, maybe
-- I could implement this as a general-purpose glue together tokens thing, but
-- at least for the moment I only want to do this in a specific spot.
lexemeB :: Parser a -> Parser a
lexemeB = (<* P.option () (P.char '\\' *> pWhitespace))

keyword :: Text -> Parser ()
keyword str = void $ lexeme (P.string str)

notIn :: [Char] -> Char -> Bool
notIn cs = \c -> not (Char.isSpace c) && c `notElem` cs
