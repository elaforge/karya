-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.Constants where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Derive.Env as Env
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Typecheck as Typecheck

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT

import           Global
import           Types


-- * ly-global instrument

-- | This is a pseudo-instrument used to mark notes which are actually global
-- lilypond directives.  E.g., meter changes, page breaks, movement titles.
lyGlobal :: ScoreT.Instrument
lyGlobal = ScoreT.Instrument "ly-global"

lyQualified :: InstT.Qualified
lyQualified = InstT.Qualified "ly" "global"

lySynth :: code -> Inst.SynthDecl code
lySynth code = Inst.SynthDecl
    { synthdName = "ly"
    , synthdDoc = "Fake synth for fake lilypond instrument."
    , synthdPatches =
        [ ("global"
          , Inst.Inst (Inst.Dummy dummyDoc)
            (Common.doc #= doc $ Common.common code)
          )
        ]
    , synthdWarns = []
    }
    where
    dummyDoc = "fake instrument for lilypond directives"
    doc = "The lilypond deriver will automatically allocate `>ly-global`, and\
        \ instruments with global lilypond directives will get this instrument."

-- * code fragments

-- | A free-standing code fragment is merged in with its nearest
data FreeCodePosition = FreePrepend | FreeAppend
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Typecheck.Typecheck FreeCodePosition
instance Typecheck.ToVal FreeCodePosition

instance ShowVal.ShowVal FreeCodePosition where
    showVal FreePrepend = "prepend"
    showVal FreeAppend = "append"

-- | A code fragment that has to be attached to notes.
data CodePosition = CodePosition Attach Position Distribution
    deriving (Eq, Ord, Show)

-- | Chord goes before or after the whole chord, Note goes before or after the
-- individual pitch within the chord.
data Attach = Chord | Note
    deriving (Eq, Ord, Show)

data Position = Prepend | Append
    deriving (Eq, Ord, Show)

allPositions :: [CodePosition]
allPositions =
    [ CodePosition a p d
    | a <- [Chord, Note], p <- [Prepend, Append], d <- [First, Last, All]
    ]

-- | If the note is split into multiple tied notes, which ones should get the
-- code?
data Distribution = First | Last | All
    deriving (Eq, Ord, Show)

instance Pretty FreeCodePosition where pretty = showt
instance Pretty CodePosition where pretty = showt

positionKey :: CodePosition -> Env.Key
positionKey (CodePosition attach pos distribution) =
    Text.intercalate "-" $ "ly" :
        [ case attach of
            Chord -> "chord"
            Note -> "note"
        , case pos of
            Prepend -> "prepend"
            Append -> "append"
        , case distribution of
            First -> "first"
            Last -> "last"
            All -> "all"
        ]

keyPosition :: Env.Key -> Maybe CodePosition
keyPosition k = Map.lookup k m
    where m = Map.fromList $ Lists.keyOn positionKey allPositions

environCode :: Env.Environ -> [(CodePosition, Text)]
environCode env =
    [ (code, val)
    | (Just code, Just val)
        <- map (bimap keyPosition Typecheck.fromValSimple) (Env.toList env)
    ]

withCode :: CodePosition -> Text -> Env.Environ -> Env.Environ
withCode pos code env = Env.insertVal key (old <> code) env
    where
    old = fromMaybe "" $ Env.maybeVal key env
    key = positionKey pos

freeCodeKey :: FreeCodePosition -> Env.Key
freeCodeKey FreePrepend = "ly-prepend"
freeCodeKey FreeAppend = "ly-append"

keyFreeCode :: Env.Key -> Maybe FreeCodePosition
keyFreeCode "ly-prepend" = Just FreePrepend
keyFreeCode "ly-append" = Just FreeAppend
keyFreeCode _ = Nothing

environFreeCode :: Env.Environ -> [(FreeCodePosition, Text)]
environFreeCode env =
    [ (code, val)
    | (Just code, Just val)
        <- map (bimap keyFreeCode Typecheck.fromValSimple) (Env.toList env)
    ]

withFreeCode :: FreeCodePosition -> Text -> Env.Environ -> Env.Environ
withFreeCode pos code = Env.insertVal (freeCodeKey pos) code

-- ** other env keys

-- | String: append after the pitch, and before the duration.  This is for
-- pitch modifiers like reminder accidentals (!) and cautionary accidentals
-- (?).  TODO this isn't integrated with 'CodePosition', but maybe could be.
-- Would Prepend make any sense?
vAppendPitch :: Env.Key
vAppendPitch = "ly-append-pitch"

-- | String: \"^\" or \"_\", manually sets tie direction, if this note is
-- tied.
vTieDirection :: Env.Key
vTieDirection = "ly-tie-direction"

-- * tuplet

-- | Set the env vars that signals that the lilypond converter should make
-- the following notes into a tuplet.
setTuplet :: RealTime -- ^ score_dur is the visible duration in the score
    -> RealTime -- ^ real_dur is the duration it actually consumes, so
    -- 3 quarters into 1 whole will be 3/4.
    -> Env.Environ
setTuplet scoreDur realDur = Env.fromList
    [ ("ly-tuplet-score-dur", Typecheck.toVal scoreDur)
    , ("ly-tuplet-real-dur", Typecheck.toVal realDur)
    ]

getTuplet :: Env.Environ -> Maybe (RealTime, RealTime)
getTuplet env = (,) <$> get "ly-tuplet-score-dur" <*> get "ly-tuplet-real-dur"
    where get k = Env.maybeVal k env

-- * tremolo

-- | This marks a tremolo event, which triggers special treatment for
-- coincident notes.
vTremolo :: Env.Key
vTremolo = "ly-tremolo"

-- * ly-global

-- | String: should be parseable by 'Meter.parse_meter',
-- e.g. @\'3/4\'@.  Used only on @>ly-global@ events.
vMeter :: Env.Key
vMeter = "ly-meter"

-- | String: this has the same format as 'v_meter', but it affects the rhythmic
-- spelling for the instrument.
vSubdivision :: Env.Key
vSubdivision = "ly-subdivision"

-- | String: Gives the title of a new movement.  An event with 'ly_global'
-- instrument and this env val will cause a movement break.
vMovement :: Env.Key
vMovement = "ly-movement"

-- * common code

-- | Emit Ped___^___/ style pedal markings.
mixedPedalStyle :: Text
mixedPedalStyle = "\\set Staff.pedalSustainStyle = #'mixed"
