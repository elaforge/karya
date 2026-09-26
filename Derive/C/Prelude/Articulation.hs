-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Calls that are similar to staff-notation articulations, or could be.
    This means calls that modify notes in relatively straightforward ways,
    either by adding an attribute or modifying their environment.

    There is already general purpose syntax to add attributes to notes, e.g.
    @attr = +x@ or @n +x@ or just @+x@, and instruments may supply special
    calls for their attributes, but there are several attributes which look
    nice with their own calls and are used by many instruments.

    TODO There are too many ways to apply attributes to notes, and they work in
    inconsistent ways.
-}
module Derive.C.Prelude.Articulation where
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.StringUtil as StringUtil
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.SubT as SubT
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Symbols as Symbols
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.both
        [ ("o", cHarmonic)
        , (Symbols.mute, Make.attributedNote Module.prelude Attrs.mute)
        , (".", Make.attributedNote Module.prelude Attrs.staccato)
        , ("{", Make.attributedNote Module.prelude Attrs.porta)
        -- I'd use '>', but then it overrides the empty instrument call in note
        -- tracks.  Besides, this way it has a nice symmetry with '^'.
        , (Symbols.accent, cAccent)
        , (Symbols.weak, cWeak)

        , ("-", cShortenLengthen True)
        , ("+", cShortenLengthen False)
        ]
    , Library.generators
        [ ("(", cSlur Nothing)
        -- These do different things in lilypond mode, but in normal
        -- performance they are just the same as a slur.
        , ("^(", cSlur (Just Call.Up))
        , ("_(", cSlur (Just Call.Down))
        ]
    , Library.transformers
        [ ("sus-a", cSustainAbs)
        , ("sus", cSustain)
        ]
    , Library.pattern patternAttrGenerator
    , Library.pattern patternAttrTransformer
    ]

-- * attr pattern

patternAttrGenerator :: Derive.PatternCall (Derive.Generator Derive.Note)
patternAttrGenerator = makePatternAttr $ \attrs ->
    Library.generator $ Make.attributedNote Module.prelude attrs

patternAttrTransformer :: Derive.PatternCall (Derive.Transformer Derive.Note)
patternAttrTransformer = makePatternAttr $ \attrs ->
    Library.transformer $ Make.attributedNote Module.prelude attrs

makePatternAttr :: (Attrs.Attributes -> call) -> Derive.PatternCall call
makePatternAttr call = Derive.PatternCall
    { patDescription = "attribute starting with `+` or `=`"
    , patDoc = Derive.extractDoc $ Library.generator $
        Make.attributedNote Module.prelude (Attrs.attr "example-attr")
    , patFunction = \(Expr.Symbol sym) -> parseSymbol sym
    }
    where
    parseSymbol sym = case Text.uncons sym of
        Just (c, _) | c == '+' || c == '=' -> case Parse.parseVal sym of
            Right (DeriveT.VAttributes attrs) -> pure $ Just (call attrs)
            _ -> pure Nothing
        _ -> pure Nothing

-- * harmonic

cHarmonic :: Library.Calls Derive.Note
cHarmonic = Make.transformNotes Module.prelude "harmonic"
    (Tags.attr <> Tags.ly)
    "Harmonic, with lilypond for natural and artificial harmonic notation."
    ((,)
    <$> Sig.defaulted "type" (Nothing :: Maybe Sig.Dummy) "Type of harmonic."
    <*> lilyHarmonicSig
    ) $ \(htype, lilyArgs) deriver -> Ly.whenLilypond
            (lilyHarmonic lilyArgs (fromMaybe Natural htype) deriver)
            (Call.addAttributes (Attrs.harm <> harmAttrs htype) deriver)
    where
    harmAttrs htype = case htype of
        Nothing -> mempty
        Just Natural -> Attrs.natural
        Just Artificial -> Attrs.artificial

data HarmonicType = Natural | Artificial
    deriving (Eq, Show, Bounded, Enum)

instance Typecheck.Typecheck HarmonicType
instance ShowVal.ShowVal HarmonicType where
    showVal Natural = "nat"
    showVal Artificial = "art"

-- | Args for 'lily_harmonic'.
lilyHarmonicSig :: Sig.Parser ([PSignal.Pitch], Maybe PSignal.Pitch, Bool)
lilyHarmonicSig = (,,)
    <$> Sig.environKey EnvKey.openStrings ([] :: [Sig.Dummy])
        "Pitches of open strings."
    <*> Sig.environKey EnvKey.string (Nothing :: Maybe Sig.Dummy)
        "Play on this string."
    <*> Sig.environKey "harmonic-force-diamond" False
        "If true, use string+diamond notation even for the 2nd natural\
        \ harmonic."

lilyHarmonic :: ([PSignal.Pitch], Maybe PSignal.Pitch, Bool)
    -> HarmonicType -> Derive.NoteDeriver -> Derive.NoteDeriver
lilyHarmonic (openStrings, string, forceDiamond) htype deriver = do
    openStrings <- mapM StringUtil.string openStrings
    string <- traverse StringUtil.string string
    Post.emapM_ id
        (lilyHarmonicEvent forceDiamond htype openStrings string)
        =<< deriver
    -- Ly should have one that skips code events

lilyHarmonicEvent :: Bool -> HarmonicType -> [StringUtil.String]
    -> Maybe StringUtil.String -> Score.Event -> Derive.Deriver [Score.Event]
lilyHarmonicEvent forceDiamond htype openStrings string event = do
    nn <- Derive.require "no pitch" $ Score.initialNn event
    (string, harmonic) <- Derive.requireRight id $ case htype of
        Natural -> naturalHarmonic openStrings string nn
        Artificial -> artificialHarmonic lowest nn
            where
            lowest = maybe 0 StringUtil.strNn $
                string <|> Lists.head openStrings
    -- When the lilypond backend sees Attrs.harm it knows it's inherently nv.
    let addHarm = Score.addAttributes Attrs.harm
    map addHarm <$> if harmonic <= 2 && not forceDiamond
        then pure
            [Ly.addNoteCode (Ly.append Constants.All, "-\\flageolet") event]
        else do
            interval <- Derive.require
                ("harmonic not supported: " <> showt harmonic)
                (touchInterval harmonic)
            pure $ harmonicCode string (string + interval) event

harmonicCode :: Pitch.NoteNumber -> Pitch.NoteNumber -> Score.Event
    -> [Score.Event]
harmonicCode stopped touched event =
    [ withPitch stopped
    , Ly.addNoteCode (Ly.noteAppend Constants.All, "\\harmonic") $
        withPitch touched
    ]
    where
    withPitch nn =
        Score.setPitch (PSignal.constant (Twelve.nnPitch nn)) event

-- | Where should I touch the string to play the nth harmonic of a base
-- frequency?
touchInterval :: StringUtil.Harmonic -> Maybe Pitch.NoteNumber
touchInterval harmonic = case harmonic of
    2 -> Just 12
    3 -> Just 7
    4 -> Just 5
    5 -> Just 4
    6 -> Just 3
    _ -> Nothing
    -- In principle I want the interval that corresponds to 1/harmonic of
    -- the string.  In practice, I need to show an integral pitch number, and
    -- high harmonics get too close and should be notated via some other means.
    -- So I'll just hard code some low harmonics and deal with high ones if
    -- I need them some day.

highestHarmonic :: StringUtil.Harmonic
highestHarmonic = 6

-- | If string is given, try to find this pitch in the harmonics of that
-- string.  Otherwise, find the string from open_strings which has this as
-- its lowest harmonic.
naturalHarmonic :: [StringUtil.String] -> Maybe StringUtil.String
    -> Pitch.NoteNumber -> Either Text (Pitch.NoteNumber, StringUtil.Harmonic)
    -- ^ (selected string, harmonic)
naturalHarmonic [] Nothing nn = Right (Pitch.modifyHz (/2) nn, 2)
naturalHarmonic openStrings maybeString nn =
    first StringUtil.strNn <$>
        StringUtil.findHarmonic False highestHarmonic openStrings
            maybeString nn

-- | Pick the lowest harmonic which is above the given lowest string.
artificialHarmonic :: Pitch.NoteNumber -> Pitch.NoteNumber
    -> Either Text (Pitch.NoteNumber, StringUtil.Harmonic)
    -- ^ (stopped pitch, harmonic)
artificialHarmonic lowestString nn =
    tryJust ("artificial harmonic for " <> pretty nn
        <> " must be above lowest string " <> pretty lowestString) $
    fmap (first (Pitch.nn . round)) $
        Lists.head $ filter ((>lowestString) . fst) $
        -- I assume the octave is not convenient for an artificial harmonic,
        -- but that's not true in higher pitches.  Maybe I could allow it, but
        -- make it least preferred?
        Lists.keyOn baseOf [3..highestHarmonic]
    where baseOf h = Pitch.modifyHz (/ fromIntegral h) nn

-- * slur

-- | I'm not really sure how fancy calls should be.  On one hand, high level
-- calls should get a nice result automatically.  On the other hand, they're
-- not very composable if they override things like %sus-abs.
cSlur :: Maybe Call.UpDown -> Derive.Generator Derive.Note
cSlur direction = Derive.generator Module.prelude "legato"
    (Tags.attr <> Tags.subs <> Tags.ly)
    "Play the transformed notes legato. This just makes all but the last\
    \ overlap slightly.\
    \\nYou can combine this with other controls to get fancier phrasing.\
    \ For example, you can be detached by default but have legato connect\
    \ notes, by setting `%legato-overlap = .05 | %sus-abs = -.05`.\
    \\nOtherwise, you can use the `detach` and `dyn` args.\
    \\nThe `^` and `_` variants are the same in normal performance, but force\
    \ lilypond slurs to go above or below, respectively."
    $ Sig.call ((,,)
    <$> Sig.defaulted "overlap" (0.1 :: RealTime)
        "All notes but the last have their durations extended by this amount."
    <*> Sig.defaulted "detach" (Nothing :: Maybe Sig.Dummy)
        "Shorten the final note by this amount, by setting `%sus-abs`.\
        \ The distinction between not given and 0 is important, because 0\
        \ will still override `%sus-abs`, which you may not want."
    <*> Sig.defaulted "dyn" (1 :: Double)
        "Scale dyn for notes after the first one by this amount."
    ) $ \(overlap, maybeDetach, dyn) args ->
    Ly.whenLilypond (lilySlur direction args) $
        noteSlur overlap maybeDetach dyn =<< Sub.subEvents args

noteSlur :: RealTime -> Maybe RealTime -> Signal.Y -> [[SubT.Event]]
    -> Derive.NoteDeriver
noteSlur overlap maybeDetach dyn = Sub.derive . concatMap apply
    where
    apply = Lists.mapInit (fmap (setSustain overlap))
        . applyDyn dyn . maybe id applyDetach maybeDetach

lilySlur :: Maybe Call.UpDown -> Derive.PassedArgs d -> Derive.NoteDeriver
lilySlur direction =
    Ly.notesAroundLy (Ly.append Constants.First, prefix <> "(")
        (Ly.append Constants.Last, ")")
    where
    prefix = case direction of
        Nothing -> ""
        Just Call.Up -> "^"
        Just Call.Down -> "_"

{- NOTE [legato]
    Previously, it would set @+legato@, and the default note deriver would
    then respond by overlapping with the next note.  The theory was that it
    would allow more flexibility since I could then swap out the default
    note deriver.  However, in practice, the note deriver doesn't know about
    the extent of the legato phrase, so it would need @+legato@ on all but
    the last note.  And I wound up swapping out the legato call itself since
    samplers with legato samples need the legato keyswitch on all notes, not
    just all-but-the-last, so I'd have to swap out both the legato call and
    the note call.  In addition, I added features like @detach@ and @dyn@
    and delegating note overlap to the note didn't make so much sense.
-}

-- | This is not in 'note_calls', instruments that support this are expected to
-- override @(@ with it.
cAttrSlur :: Attrs.Attributes -> Attrs.Attributes
    -> Derive.Generator Derive.Note
cAttrSlur firstAttr restAttr = Derive.generator Module.instrument "legato"
    (Tags.attr <> Tags.subs <> Tags.ly)
    "Make a phrase legato by applying the `+legato` attribute. This is for\
    \ instruments that understand it, for instance with a keyswitch for\
    \ transition samples."
    $ Sig.call ((,)
    <$> Sig.defaulted "detach" (Nothing :: Maybe Sig.Dummy)
        "If set, shorten the final note by this\
        \ amount. This is to avoid triggering legato from the previous note."
    <*> Sig.defaulted "dyn" (1 :: Double)
        "Scale dyn for notes after the first one by\
        \ this amount. Otherwise, transition samples can be too loud."
    ) $ \(detach, dyn) args -> Ly.whenLilypond (lilySlur Nothing args) $
        noteSlur 0.02 detach dyn
            . map (Lists.mapHead (fmap (Call.addAttributes firstAttr)))
            . map (Lists.mapTail (fmap (Call.addAttributes restAttr)))
                =<< Sub.subEvents args

applyDetach :: RealTime -> [SubT.Event] -> [SubT.Event]
applyDetach detach = Lists.mapLast (fmap (setSustain (-detach)))

applyDyn :: Signal.Y -> [SubT.Event] -> [SubT.Event]
applyDyn dyn = Lists.mapTail (fmap (Call.multiplyDynamic dyn))

setSustain :: RealTime -> Derive.Deriver a -> Derive.Deriver a
setSustain = Call.withConstant Controls.sustainAbs . RealTime.toSeconds

-- * misc

-- | This is the same as 'c_lengthen', but it's here for symmetry with
-- 'c_sustain'.  Also, conceptually this is lower level, while c_lengthen
-- is meant to be modified to whatever is locally appropriate.
cSustainAbs :: Derive.Transformer Derive.Note
cSustainAbs = Derive.transformer Module.prelude "sus-a" mempty
    ("Simple legato, extend the duration of the transformed notes by the given\
    \ amount. This works by setting " <> ShowVal.doc Controls.sustainAbs
    <> "."
    ) $ Sig.callt (Sig.defaulted "time" (Typecheck.real 0.25)
        "Add this duration to the note.")
    $ \(Typecheck.DefaultReal time) args deriver -> do
        time <- Call.realDuration (Args.end args) time
        setSustain time deriver

cSustain :: Derive.Transformer Derive.Note
cSustain = Derive.transformer Module.prelude "sus" mempty
    ("Simple legato, extend the duration of the transformed notes by the given\
    \ amount. This works by setting " <> ShowVal.doc Controls.sustain
    <> "."
    ) $ Sig.callt (Sig.defaulted "amount" (1.5 :: Double)
        "Multiply the note's duration by this.")
    $ \amount _args -> Call.withConstant Controls.sustain amount

cShortenLengthen :: Bool -> Library.Calls Derive.Note
cShortenLengthen shorten = Make.transformNotes Module.prelude
    (if shorten then "shorten" else "lengthen") mempty
    ("Lengthen or Shorten a note duration, by adding to or subtracting from "
        <> ShowVal.doc Controls.sustainAbs <> ".")
    (Sig.defaulted "time" (0.15 :: Double) "Subtract this duration.") $ \time ->
        Call.withConstant Controls.sustainAbs
            (if shorten then -time else time)

cAccent :: Library.Calls Derive.Note
cAccent = Make.transformNotes Module.prelude "accent" Tags.ly
    "Accent the note by multiplying its dynamic."
    (Sig.defaulted "dyn" (1.5 :: Double) "Multiply dynamic.") $ \dyn ->
        -- Adding Attrs.accent makes lilypond attach a '>'.
        Call.addAttributes Attrs.accent . Call.multiplyDynamic dyn

cWeak :: Library.Calls Derive.Note
cWeak = Make.transformNotes Module.prelude "weak" mempty
    "Weaken the note by multiplying its dynamic."
    (Sig.defaulted "dyn" (0.35 :: Double) "Multiply dynamic.") $ \dyn ->
        Call.multiplyDynamic dyn
