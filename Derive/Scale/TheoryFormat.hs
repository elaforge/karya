-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | This is the part of "Derive.Scale.Theory" that's concerned with converting
    'Pitch.Pitch'es to and from 'Pitch.Note's.

    It's split off to avoid cluttering Theory, but also because the
    "Derive.Scale" import would make it a circular dependency.

    This is basically just a bunch of functions that take a million arguments
    to configure octave format, accidental format, key parsing, etc.  To
    avoid annoyingly long argument lists, they are mostly packaged up into
    records, such as 'Config', 'KeyConfig', and 'RelativeFormat'.

    It's basically all just an attempt to parameterize scale creation, so I
    can reuse the shared parts but still be able to configure them.  It winds
    up being complicated, probably due to how it's evolved in reaction to
    increasingly varied scales, rather than according to an overarching design.
-}
module Derive.Scale.TheoryFormat where
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import           Data.Vector ((!))

import qualified Util.Num as Num
import qualified Util.ParseA as ParseA
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Scale.Theory as Theory
import qualified Perform.Pitch as Pitch

import           Global


-- * types

-- | General purpose config.
data Config = Config
    { configShowOctave :: ShowOctave
    , configParseOctave :: ParseOctave
    , configAccidental :: AccidentalFormat
    }

defaultConfig :: Config
defaultConfig = Config
    { configShowOctave = showOctave
    , configParseOctave = parseOctave
    , configAccidental = asciiAccidentals
    }

setOctave :: ShowOctave -> ParseOctave -> Config -> Config
setOctave showOctave parseOctave config = config
    { configShowOctave = showOctave
    , configParseOctave = parseOctave
    }

type ShowOctave = Pitch.Octave -> Text -> Text
-- | This can't just be A.Parser Pitch.Octave because I don't know where the
-- octave is in the pitch text.
type ParseOctave = A.Parser (Pitch.PitchClass, Maybe Pitch.Accidentals)
    -> A.Parser RelativePitch

-- | Key config is only necessary for formatting that depends on the key, e.g.
-- 'RelativeFormat'.
data KeyConfig key = KeyConfig
    { keyParse :: Maybe Pitch.Key -> Either DeriveT.PitchError key
    -- | Default key if there is none, or it's not parseable.  Otherwise, a bad
    -- or missing key would mean you couldn't even display notes.
    , keyDefault :: key
    }

-- | This is a just-parsed pitch.  It hasn't yet been adjusted according to the
-- key, so it's not yet an absolute 'Pitch.Pitch'.  It also represents
-- a natural explicitly, via Just 0.
--
-- 'fmt_to_absolute' is responsible for converting this to a 'Pitch.Pitch',
-- likely via 'rel_to_absolute'.
data RelativePitch =
    RelativePitch !Pitch.Octave !Pitch.PitchClass !(Maybe Pitch.Accidentals)
    deriving (Show)

instance Pretty RelativePitch where
    pretty (RelativePitch oct pc accs) = "relative:"
        <> showt oct <> "-" <> showt pc <> "-" <> pretty accs

relativeToAbsolute :: RelativePitch -> Pitch.Pitch
relativeToAbsolute (RelativePitch oct pc acc) =
    Pitch.Pitch oct (Pitch.Degree pc (fromMaybe 0 acc))

-- * absolute

-- | Make an absolute scale starting at @a@.
letters :: Pitch.PitchClass -> Format
letters pcPerOctave =
    makeAbsoluteFormat (makePattern degrees) degrees
    where degrees = makeDegrees $ take pcPerOctave letterDegrees

letterDegrees :: [Text]
letterDegrees = map Text.singleton ['a'..'z']

-- | The usual 7 note scale, which wraps around at @c@ instead of @a@.
absoluteC :: Format
absoluteC = makeAbsoluteFormat (makePattern degrees) degrees
    where degrees = makeDegrees absoluteCDegrees

absoluteCDegrees :: [Text]
absoluteCDegrees = ["c", "d", "e", "f", "g", "a", "b"]

-- * relative

-- | Args for a relative scale format.
--
-- The 'Pitch.Pitch'es handled by a relative scale are still absolute, the same
-- as the Pitches of an absolute scale.  The difference is that the
-- 'read_pitch' and 'show_pitch' functions adjust based on the key to display
-- the absolute Pitch relative to the tonic of the key.
data RelativeFormat key = RelativeFormat
    { relConfig :: Config
    , relKeyConfig :: KeyConfig key
    , relShowDegree :: ShowDegree key
    , relToAbsolute :: ToAbsolute key
    }

-- | Given a RelativePitch relative to the default key, adjust it to
-- be absolute.  This is so I can figure out if a relative pitch is valid
-- without knowing the key, as described in 'fmt_to_absolute'.
type ToAbsolute key = key -> Degrees -> RelativePitch -> Pitch.Pitch

-- | This is a specialization of 'ShowPitch' for show functions that need
-- a key.
type ShowDegree key = key -> ShowOctave -> Degrees -> AccidentalFormat
    -> Either Pitch.Degree Pitch.Pitch -> Pitch.Note

sargam :: RelativeFormat key -> Format
sargam = makeRelativeFormat (makePattern degrees) degrees
    where degrees = makeDegrees ["s", "r", "g", "m", "p", "d", "n"]

cipher :: Pitch.PitchClass -> RelativeFormat key -> Format
cipher pcPerOctave = makeRelativeFormat pattern degrees
    where
    degrees = makeDegrees ["-" <> showt pc | pc <- [1..pcPerOctave]]
    pattern = "-[1-" <> showt pcPerOctave <> "]"

zhCipher :: Pitch.PitchClass -> RelativeFormat key -> Format
zhCipher pcPerOctave =
    makeRelativeFormat (makePattern degrees) degrees
    where
    degrees = makeDegrees $ take pcPerOctave ds
    ds = ["一", "二", "三", "四", "五", "六", "七", "八", "九", "十"]

gongche :: RelativeFormat key -> Format
gongche = makeRelativeFormat (makePattern degrees) degrees
    where degrees = makeDegrees ["士", "下", "ㄨ", "工", "六"]

makePattern :: Degrees -> Text
makePattern degrees = "[" <> mconcat (Vector.toList degrees) <> "]"

-- * Format

-- | This is the central data structure for this module.  It has the set of
-- functions needed to parse and generate symbolic pitches.  Making one of
-- these gives you access to the set of functions in here, such as 'show_pitch'
-- and 'read_pitch', which in turn can be used to implement
-- a 'Derive.Scale.Scale'.
data Format = Format
    { fmtShow :: ShowPitch
    -- | This doesn't need the key because that work is split off to
    -- 'fmt_to_absolute'.
    , fmtRead :: A.Parser RelativePitch
    -- | I need the key to parse the pitch, but that can't happen unless I want
    -- to give all pattern_lookup calls access to the env in scope.  But
    -- I don't need the env to recognize if it's a valid call or not.
    , fmtToAbsolute :: Maybe Pitch.Key -> RelativePitch
        -> Either DeriveT.PitchError Pitch.Pitch
    -- | Goes in 'Scale.scale_pattern'.
    , fmtPattern :: !Text
    -- TODO why do I need this?  can I have keys with different octaves?
    -- It's used only by JustScales
    , fmtPcPerOctave :: Pitch.PitchClass
    -- | True if this scale is relative to the key.  Only used by
    -- ChromaticScales.  TODO split it out?
    , fmtRelative :: !Bool
    }

-- | This is used to show both a Pitch with an octave, and a Degree without
-- one (used for key names).  This is because the code is presumably mostly the
-- same.
type ShowPitch = Maybe Pitch.Key -> Either Pitch.Degree Pitch.Pitch
    -> Pitch.Note
type Degrees = Vector.Vector Text

makeDegrees :: [Text] -> Degrees
makeDegrees = Vector.fromList

-- ** show keys

showKey :: Format -> Theory.Key -> Pitch.Key
showKey fmt key =
    Pitch.Key $ tonic <> (if Text.null name then "" else "-" <> name)
    where
    tonic = showDegree fmt Nothing (Theory.keyTonic key)
    name = Theory.keyName key

-- | Show a key along with its key signature.
showKeySignature :: Format -> Theory.Key -> Text
showKeySignature fmt key =
    Pitch.keyText (showKey fmt key) <> " -- " <> intervals
        <> "\n    " <> commas (showSignature fmt key)
    where
    intervals = commas $ map showt $ Vector.toList $ Theory.keyIntervals key
    commas = Text.intercalate ", "

-- | Show the signature of the given key by showing each scale degree with the
-- the accidentals implied by the key signature.
showSignature :: Format -> Theory.Key -> [Text]
showSignature fmt key =
    map (showDegree fmt Nothing . Pitch.pitchDegree) pitches
    where
    pcPerOctave = Vector.length (Theory.keyIntervals key)
    pitches = map transpose [0 .. pcPerOctave - 1]
    transpose pc = Theory.transposeDiatonic key pc
        (Pitch.Pitch 0 (Theory.keyTonic key))

-- | Get the degree names of a chromatic scale in this key.
keyDegrees :: Theory.Key -> Format -> [Text]
keyDegrees key fmt = map (showDegree fmt Nothing . Pitch.pitchDegree) pitches
    where
    perOctave = Theory.layoutSemisPerOctave (Theory.keyLayout key)
    pitches = map transpose [0 .. perOctave - 1]
    transpose steps = Theory.transposeChromatic key steps
        (Pitch.Pitch 0 (Pitch.Degree 0 0))

-- ** show pitches

showPitch :: Format -> Maybe Pitch.Key -> Pitch.Pitch -> Pitch.Note
showPitch fmt key = fmtShow fmt key . Right

-- | 'show_pitch' adapted to 'Scale.scale_show'.
scaleShowPitch :: Format -> Maybe Pitch.Key -> Pitch.Pitch
    -> Either DeriveT.PitchError Pitch.Note
scaleShowPitch fmt key = Right . showPitch fmt key

showDegree :: Format -> Maybe Pitch.Key -> Pitch.Degree -> Text
showDegree fmt key = Pitch.noteText . fmtShow fmt key . Left

readPitch :: Format -> Maybe Pitch.Key -> Pitch.Note
    -> Either DeriveT.PitchError Pitch.Pitch
readPitch fmt key = fmtToAbsolute fmt key <=< readRelativePitch fmt

-- | Parse a Note, but don't adjust it for the key.  This means that relative
-- pitches will likely be incorrect.  'ToAbsolute' documents why this needs
-- to be separate.
readRelativePitch :: Format -> Pitch.Note
    -> Either DeriveT.PitchError RelativePitch
readRelativePitch fmt note = justErr (DeriveT.UnparseableNote note) $
    -- Debug.traceRet "read_rel" note $
    ParseA.maybeParse (fmtRead fmt) $
    Pitch.noteText note

-- ** make

makeAbsoluteFormat :: Text -> Degrees -> Format
makeAbsoluteFormat = makeAbsoluteFormatConfig defaultConfig

makeAbsoluteFormatKeyed :: Config -> KeyConfig Theory.Key
    -> Text -> Degrees -> Format
makeAbsoluteFormatKeyed config keyConfig pattern degrees =
    (makeAbsoluteFormatConfig config pattern degrees)
    { fmtShow = showPitchKeyedAbsolute config keyConfig degrees
    , fmtRead = pPitch config degrees
    , fmtToAbsolute = \maybeKey pitch -> do
        key <- keyParse keyConfig maybeKey
        pure $ toAbs key pitch
    }
    where
    -- A note with no explicit accidentals gets the accidentals of the key
    -- signature.
    toAbs key (RelativePitch octave pc maybeAcc) = case maybeAcc of
        Nothing -> Pitch.Pitch octave
            (Pitch.Degree pc (Theory.accidentalsAtPc key pc))
        Just acc -> Pitch.Pitch octave (Pitch.Degree pc acc)

-- | A configurable version of 'make_absolute_format'.
makeAbsoluteFormatConfig :: Config -> Text -> Degrees -> Format
makeAbsoluteFormatConfig config pattern degrees = Format
    { fmtShow = showPitchAbsolute config degrees
    , fmtRead = pPitch config degrees
    , fmtToAbsolute = \_ -> Right . relativeToAbsolute
    , fmtPattern = octavePattern <> pattern <> accPattern
    , fmtPcPerOctave = Vector.length degrees
    , fmtRelative = False
    }

-- | Make a Format from a 'RelativeFormat'.
makeRelativeFormat :: Text -> Degrees -> RelativeFormat key -> Format
makeRelativeFormat pattern degrees relFmt = Format
    { fmtShow
    , fmtRead = pPitch relConfig degrees
    , fmtToAbsolute
    , fmtPattern = octavePattern <> pattern <> accPattern
    , fmtPcPerOctave = Vector.length degrees
    , fmtRelative = True
    }
    where
    RelativeFormat
        { relConfig, relKeyConfig, relShowDegree, relToAbsolute
        } = relFmt
    fmtShow key = relShowDegree
        (either (const (keyDefault relKeyConfig)) id
            (keyParse relKeyConfig key))
        (configShowOctave relConfig) degrees (configAccidental relConfig)
    fmtToAbsolute maybeKey pitch = do
        key <- keyParse relKeyConfig maybeKey
        pure $ relToAbsolute key degrees pitch

accPattern :: Text
accPattern = "(bb|b|n|#|x)?"

octavePattern :: Text
octavePattern = "[-1-9]"

-- *** absolute

showPitchAbsolute :: Config -> Degrees -> ShowPitch
showPitchAbsolute config degrees _key pitch =
    Pitch.Note $ case pitch of
        Left (Pitch.Degree pc acc) ->
            degree <> showAccidentals (configAccidental config) acc
            where (_, degree) = degreeAt degrees pc
        Right (Pitch.Pitch oct (Pitch.Degree pc acc)) ->
            configShowOctave config (oct + pcOct) $
                degree <> showAccidentals (configAccidental config) acc
            where (pcOct, degree) = degreeAt degrees pc

-- *** relative

showDegreeChromatic :: ShowDegree Theory.Key
showDegreeChromatic key showOctave degrees accFmt degreePitch =
    Pitch.Note $ case degreePitch of
        Left _ -> pcText <> accText
        Right (Pitch.Pitch oct _) -> -- Debug.trace "show_degree_chromatic" $
            showOctave (oct + pcOct) (pcText <> accText)
    where
    Pitch.Degree pc acc = either id Pitch.pitchDegree degreePitch
    accText = showAccidentals accFmt $ acc - Theory.accidentalsAtPc key pc
    (pcOct, pcText) =
        showPc degrees (Pitch.degreePc (Theory.keyTonic key)) pc

chromaticToAbsolute :: ToAbsolute Theory.Key
chromaticToAbsolute key degrees (RelativePitch octave pc maybeAcc) =
    -- Debug.traceRet "ch to abs" (tonic, (pc, maybe_acc)) $
        Pitch.Pitch (octave + oct) (Pitch.Degree pc2 acc2)
    where
    -- sa in D is D, so add 1 PC for the tonic.
    -- If it's ri in c#-maj then it's considered d# or Relative 1 0 -> Pitch 1 1
    -- So, add the key signature.
    --
    -- But for pelog, Relative 3 -> Pitch 2 1.  So I take Relative as
    -- white notes and subtract layout to make Pitch into something relative to
    -- pathet (2+1) but which winds up being absolute "4" if acc + pc.
    -- Isn't it overcomplicated?  Why not Relative 3 -> Pitch 3, then show
    -- Pitch 3 in pelog is "4".  The place to care about pathet is diatonic
    -- transposition and kbd to call.
    (oct, pc2) = (pc + Pitch.degreePc tonic) `divMod` Vector.length degrees
    acc2 = fromMaybe 0 maybeAcc + case Theory.keySignature key of
        -- If it's chromatic then I can't adjust for the mode, but I still
        -- want to map degree 1 to C# if I'm in C#.
        Nothing -> Pitch.degreeAccidentals tonic
        Just sig -> fromMaybe 0 (sig Vector.!? pc) -- Should never get Nothing.
    tonic = Theory.keyTonic key

type Tonic = Pitch.PitchClass

showDegreeDiatonic :: ShowDegree Tonic
showDegreeDiatonic tonic showOctave degrees accFmt degreePitch =
    Pitch.Note $ case degreePitch of
        Left _ -> pcText <> accText
        Right (Pitch.Pitch oct _) ->
            showOctave (oct + pcOct) (pcText <> accText)
    where
    Pitch.Degree pc acc = either id Pitch.pitchDegree degreePitch
    accText = showAccidentals accFmt acc
    (pcOct, pcText) = showPc degrees tonic pc

showPc :: Degrees -> Tonic -> Pitch.PitchClass -> (Pitch.Octave, Text)
showPc degrees tonic pc = (oct, degrees ! i)
    where (oct, i) = (pc - tonic) `divMod` Vector.length degrees

degreeAt :: Degrees -> Pitch.PitchClass -> (Pitch.Octave, Text)
degreeAt degrees pc = showPc degrees 0 pc

-- | Convert a relative pitch using a simple diatonic key system, where the
-- key is just a note in the scale.
diatonicToAbsolute :: ToAbsolute Tonic
diatonicToAbsolute tonic degrees (RelativePitch octave pc maybeAcc) =
    Pitch.Pitch (octave + oct) (Pitch.Degree pc2 (fromMaybe 0 maybeAcc))
    where (oct, pc2) = (pc + tonic) `divMod` Vector.length degrees

-- *** keyed relative

-- | Like 'show_degree_chromatic', but format the accidentals as staff notation
-- does, in that accidentals implicit in the key signature are omitted, and
-- a natural that differs from the key signature is emitted.
-- TODO tons of args, can I package some up?
showPitchKeyedAbsolute :: Config -> KeyConfig Theory.Key -> Degrees
    -> ShowPitch
showPitchKeyedAbsolute config keyConfig degrees maybeKey degreePitch =
    Pitch.Note $ case degreePitch of
        Left _ -> pitch
        Right (Pitch.Pitch oct _) -> showOctave (oct + pcOct) pitch
    where
    Pitch.Degree pc_ acc = either id Pitch.pitchDegree degreePitch
    (pcOct, pc) = pc_ `divMod` Vector.length degrees
    key = either (const (keyDefault keyConfig)) id
        (keyParse keyConfig maybeKey)
    pitch = degrees ! pc <> accText
    accText = showAccidentalsKeyed (configAccidental config)
        (Theory.accidentalsAtPc key pc) acc

-- * parse

pPitch :: Config -> Degrees -> A.Parser RelativePitch
pPitch config degrees =
    configParseOctave config
        ((,) <$> pDegree <*> pAccidentals (configAccidental config))
    where
    pDegree = A.choice
        [ A.string text >> pure i
        | (i, text) <- zip [0..] (Vector.toList degrees)
        ]

-- ** octave

-- | Most scales display the octave as a leading number.
showOctave :: ShowOctave
showOctave oct = (showt oct <>)

parseOctave :: ParseOctave
parseOctave parse = do
    oct <- ParseA.pInt
    (pc, acc) <- parse
    pure $ RelativePitch oct pc acc

-- | Require a single digit octave.
parseOctave1 :: ParseOctave
parseOctave1 parse = do
    oct <- maybe mzero pure . Num.readDigit =<< A.satisfy ParseA.isDigit
    (pc, acc) <- parse
    pure $ RelativePitch oct pc acc

-- ** accidentals

-- | natural, sharp1, sharp2, flat1, flat2
data AccidentalFormat = AccidentalFormat !Text !Text !Text !Text !Text
    deriving (Show)

asciiAccidentals :: AccidentalFormat
asciiAccidentals = AccidentalFormat "n" "#" "x" "b" "bb"

symbolAccidentals :: AccidentalFormat
symbolAccidentals = AccidentalFormat "`n`" "`#`" "`##`" "`b`" "`bb`"

pAccidentals :: AccidentalFormat -> A.Parser (Maybe Pitch.Accidentals)
pAccidentals (AccidentalFormat natural sharp1 sharp2 flat1 flat2) =
    pNatural <|> Just . Num.sum <$> A.many1 pAcc <|> pure Nothing
    where
    pNatural = A.string natural *> pure (Just 0)
    pAcc = A.choice
        [ A.string sharp1 *> pure 1
        , A.string sharp2 *> pure 2
        , A.string flat1 *> pure (-1)
        , A.string flat2 *> pure (-2)
        ]

-- | Show accidentals relative to the key signature.
showAccidentalsKeyed :: AccidentalFormat -> Pitch.Accidentals
    -> Pitch.Accidentals -> Text
showAccidentalsKeyed fmt@(AccidentalFormat natural _ _ _ _) keyAccs accs
    | accs == keyAccs = ""
    | accs == 0 && keyAccs /= 0 = natural
    | otherwise = showAccidentals fmt accs

showAccidentals :: AccidentalFormat -> Pitch.Accidentals -> Text
showAccidentals (AccidentalFormat _ sharp1 sharp2 flat1 flat2) acc
    | acc == 0 = ""
    | acc < 0 = Text.replicate x flat2 <> Text.replicate s flat1
    | otherwise = Text.replicate x sharp2 <> Text.replicate s sharp1
    where (x, s) = abs acc `divMod` 2
