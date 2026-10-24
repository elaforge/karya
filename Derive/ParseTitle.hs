-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ViewPatterns #-}
-- | Utilities to deal with block and track titles.
--
-- This module is used by both Cmd and Derive since Cmd also wants to know
-- track types for track specific cmds.
--
-- Note track titles are just tracklang expressions, so no extra code is
-- needed.  Control tracks titles are just a hardcoded list of special cases,
-- though they are parsed as tracklang Vals.
module Derive.ParseTitle where
import qualified Data.Attoparsec.Text as A
import           Data.Attoparsec.Text ((<?>))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

import qualified Util.ParseA as ParseA
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse
import           Derive.Parse (lexeme)
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Symbols as Symbols

import qualified Perform.Pitch as Pitch

import           Global


-- * blocks

-- | A block title is a normal expression, applied as a transform.
parseBlock :: Text -> Either Text DeriveT.Expr
parseBlock = Parse.parseExpr

-- * tracks

data Type = TempoTrack | ControlTrack | PitchTrack | NoteTrack
    deriving (Eq, Show)

instance Pretty Type where pretty = showt

trackType :: Text -> Type
trackType title
    | isNoteTrack title = NoteTrack
    | isPitchTrack title = PitchTrack
    | isTempoTrack title = TempoTrack
    | otherwise = ControlTrack

-- ** note track

-- TODO this is actually unused, and I think track-call is unimplemented?
{-# SCC parseNoteTrack #-}
parseNoteTrack :: Text -> Either Text (ScoreT.Instrument, Maybe TrackCall)
parseNoteTrack = ParseA.parse1 pNoteTrack

-- > >inst !track-call
pNoteTrack :: A.Parser (ScoreT.Instrument, Maybe TrackCall)
pNoteTrack = (,)
    <$> lexeme (A.char '>'
        *> (ScoreT.Instrument <$> Parse.pIdentifier True ""))
    <*> ParseA.optional pTrackCall

-- ** control track

-- | Parse the first part of the control track title.  This is special syntax,
-- and is not the usual call plus list of argument values.
{-# SCC parseControlType #-}
parseControlType :: Text -> Either Text ControlType
parseControlType = fmap fst . parseControlTitle

parseControlTitle :: Text -> Either Text (ControlType, [DeriveT.Call])
parseControlTitle = ParseA.parse1 pControlTitle

data ControlType
    -- | Tempo track with an optional modifying symbol.
    = Tempo (Maybe Expr.Symbol)
    -- | Pitch track that sets a ScaleId (unless it's 'Pitch.empty_scale'),
    -- and sets the given pitch signal.
    | Pitch Pitch.ScaleId (Either TrackCall ScoreT.PControl)
    -- | Control track with an optional combining operator.
    | Control (Either TrackCall (ScoreT.Typed ScoreT.Control)) (Maybe Merge)
    deriving (Eq, Show)

type Merge = Expr.Symbol
type TrackCall = Expr.Symbol

instance ShowVal.ShowVal ControlType where
    showVal = controlTypeToTitle

pControlTitle :: A.Parser (ControlType, [DeriveT.Call])
pControlTitle = do
    ctype <- pControlType
    expr <- A.option [] (Parse.pPipe >> NonEmpty.toList <$> Parse.pExpr True)
    pure (ctype, expr)

pControlType :: A.Parser ControlType
pControlType = pTempo <|> pPitch <|> pControl

pTempo :: A.Parser ControlType
pTempo = Tempo <$>
    (lexeme (A.string "tempo") *> ParseA.optional (lexeme pMerge))

-- | *twelve (#name | !track-call) merge
pPitch :: A.Parser ControlType
pPitch = Pitch
    <$> lexeme pScaleId
    <*> (lexeme $ (Left <$> pTrackCall)
        <|> (Right <$> A.option ScoreT.defaultPitch (lexeme pcontrol)))
    where
    pcontrol = (\(DeriveT.Ref c _) -> c) <$> Parse.pPcontrolRef

-- | (!track-call | % | control:typ) merge
pControl :: A.Parser ControlType
pControl = Control
    <$> (lexeme $ A.choice
        [ Left <$> pTrackCall
        , Right <$> (A.char '%' *> pure empty <|> pTypedControl)
        ])
    <*> ParseA.optional (lexeme pMerge)
    where
    empty = ScoreT.untyped $ ScoreT.Control ""

pTypedControl :: A.Parser (ScoreT.Typed ScoreT.Control)
pTypedControl = flip ScoreT.Typed
    <$> (ScoreT.Control <$> Parse.pIdentifier False ":")
    <*> A.option ScoreT.Untyped pTypeAnnotation

pTypeAnnotation :: A.Parser ScoreT.Type
pTypeAnnotation = do
    A.char ':'
    typ <- Parse.pIdentifier False ""
    case ScoreT.codeToType typ of
        Nothing -> fail $ "unknown type: " <> show typ
        Just typ -> pure typ

-- ** unparse

-- | Invert 'parse_control_title'.
controlTypeToTitle :: ControlType -> Text
controlTypeToTitle ctype = Text.unwords $ case ctype of
    Tempo sym -> "tempo" : maybeSym sym
    Pitch (Pitch.ScaleId scaleId) pcontrol ->
        "*" <> scaleId
        : either ((:[]) . showTcall) showPcontrol pcontrol
    Control c merge -> either showTcall controlToTitle c : showMerge merge
    where
    maybeSym = maybe [] ((:[]) . Expr.unsym)
    showMerge = maybe [] ((:[]) . ShowVal.showVal)
    showTcall = ("!"<>) . ShowVal.showVal
    showPcontrol pcontrol = [showRef pcontrol | pcontrol /= ""]
    -- I use the # ref prefix in the pitch track title as special syntax.
    -- I guess it's a mnemonic for the ref that will resolve this signal.
    showRef =
        ShowVal.showVal . DeriveT.VPControlRef . flip DeriveT.Ref Nothing

-- | This is different from ShowVal (Typed Control) because the control doesn't
-- need a % in the control track title.
controlToTitle :: ScoreT.Typed ScoreT.Control -> Text
controlToTitle (ScoreT.Typed typ c)
    | c == "" = "%"
    | otherwise = ScoreT.controlName c
        <> if typ == ScoreT.Untyped then ""
            else ":" <> ScoreT.typeToCode typ

-- ** parse util

pMerge :: A.Parser Merge
pMerge = Expr.Symbol <$> Parse.pIdentifier False ""

pTrackCall :: A.Parser Expr.Symbol
pTrackCall = A.char '!' *> Parse.pSymbol True

-- | This is special syntax that's only allowed in control track titles.
pScaleId :: A.Parser Pitch.ScaleId
pScaleId = do
    A.char '*'
    Pitch.ScaleId <$> A.option "" (Parse.pIdentifier True "")
    <?> "scale id"

-- * util

-- | Convert a track title to its control.
titleToControl :: Text -> Maybe ScoreT.Control
titleToControl title = case parseControlType title of
    Right (Control (Right control) _) -> Just (ScoreT.valOf control)
    _ -> Nothing

-- | A pitch track is also considered a control track.
isControlTrack :: Text -> Bool
isControlTrack = not . isNoteTrack

-- | This is like 'is_control_track' but doesn't include pitch tracks.
isSignalTrack :: Text -> Bool
isSignalTrack title =
    isControlTrack title && case parseControlType title of
        Right (Control {}) -> True
        _ -> False

isTempoTrack :: Text -> Bool
isTempoTrack title = case parseControlType title of
    Right (Tempo {}) -> True
    _ -> False

-- ** note

-- | Parse a note track like @>inst@ as @note-track inst@.  Other than
-- this, note track titles are normal expressions.
{-# SCC parseNote #-}
parseNote :: Text -> Either Text DeriveT.Expr
parseNote title = case Text.uncons title of
    Just ('>', rest) -> Parse.parseExpr (prefix <> rest)
        where prefix = Expr.unsym Symbols.noteTrack <> " "
    _ -> Left $ "note track title should start with >: " <> showt title

unparseNote :: DeriveT.Expr -> Text
unparseNote = strip . ShowVal.showVal
    where
    strip t = maybe t ((">"<>) . Text.stripStart) $
        Text.stripPrefix (Expr.unsym Symbols.noteTrack) t

-- | Convert a track title into its instrument.
{-# SCC titleToInstrument #-}
titleToInstrument :: Text -> Maybe ScoreT.Instrument
titleToInstrument title = case parseNoteTrack title of
    Right (inst, _) -> Just inst
    _ -> Nothing

-- | Convert from an instrument to the title of its instrument track.
instrumentToTitle :: ScoreT.Instrument -> Text
instrumentToTitle (ScoreT.Instrument a) = ">" <> a

isNoteTrack :: Text -> Bool
isNoteTrack = (">" `Text.isPrefixOf`)

stripExpr :: Text -> Text
stripExpr = Text.stripEnd . Text.takeWhile (/='|')

noteTrack :: Text
noteTrack = ">"

-- ** pitch

titleToScale :: Text -> Maybe Pitch.ScaleId
titleToScale title = case parseControlType title of
    Right (Pitch scaleId _) -> Just scaleId
    _ -> Nothing

scaleToTitle :: Pitch.ScaleId -> Text
scaleToTitle scaleId =
    ShowVal.showVal (Pitch scaleId (Right ScoreT.defaultPitch))

isPitchTrack :: Text -> Bool
isPitchTrack = ("*" `Text.isPrefixOf`)
    -- Previously it was 'Maybe.isJust . title_to_scale', but this is called
    -- a lot during slicing so efficiency matters.

pitchTrack :: Text
pitchTrack = "*"
