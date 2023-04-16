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
ly_global :: ScoreT.Instrument
ly_global = ScoreT.Instrument "ly-global"

ly_qualified :: InstT.Qualified
ly_qualified = InstT.Qualified "ly" "global"

ly_synth :: code -> Inst.SynthDecl code
ly_synth code = Inst.SynthDecl "ly" "Fake synth for fake lilypond instrument."
    [ ("global"
      , Inst.Inst (Inst.Dummy dummy_doc)
        (Common.doc #= doc $ Common.common code)
      )
    ]
    where
    dummy_doc = "fake instrument for lilypond directives"
    doc = "The lilypond deriver will automatically allocate `>ly-global`, and\
        \ instruments with global lilypond directives will get this instrument."

-- * code fragments

-- | A free-standing code fragment is merged in with its nearest
data FreeCodePosition = FreePrepend | FreeAppend
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Typecheck.Typecheck FreeCodePosition
instance Typecheck.ToVal FreeCodePosition

instance ShowVal.ShowVal FreeCodePosition where
    show_val FreePrepend = "prepend"
    show_val FreeAppend = "append"

-- | A code fragment that has to be attached to notes.
data CodePosition = CodePosition Attach Position Distribution
    deriving (Eq, Ord, Show)

-- | Chord goes before or after the whole chord, Note goes before or after the
-- individual pitch within the chord.
data Attach = Chord | Note
    deriving (Eq, Ord, Show)

data Position = Prepend | Append
    deriving (Eq, Ord, Show)

all_positions :: [CodePosition]
all_positions =
    [ CodePosition a p d
    | a <- [Chord, Note], p <- [Prepend, Append], d <- [First, Last, All]
    ]

-- | If the note is split into multiple tied notes, which ones should get the
-- code?
data Distribution = First | Last | All
    deriving (Eq, Ord, Show)

instance Pretty FreeCodePosition where pretty = showt
instance Pretty CodePosition where pretty = showt

position_key :: CodePosition -> Env.Key
position_key (CodePosition attach pos distribution) =
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

key_position :: Env.Key -> Maybe CodePosition
key_position k = Map.lookup k m
    where m = Map.fromList $ Lists.keyOn position_key all_positions

environ_code :: Env.Environ -> [(CodePosition, Text)]
environ_code env =
    [ (code, val)
    | (Just code, Just val)
        <- map (bimap key_position Typecheck.from_val_simple) (Env.to_list env)
    ]

with_code :: CodePosition -> Text -> Env.Environ -> Env.Environ
with_code pos code env = Env.insert_val key (old <> code) env
    where
    old = fromMaybe "" $ Env.maybe_val key env
    key = position_key pos

free_code_key :: FreeCodePosition -> Env.Key
free_code_key FreePrepend = "ly-prepend"
free_code_key FreeAppend = "ly-append"

key_free_code :: Env.Key -> Maybe FreeCodePosition
key_free_code "ly-prepend" = Just FreePrepend
key_free_code "ly-append" = Just FreeAppend
key_free_code _ = Nothing

environ_free_code :: Env.Environ -> [(FreeCodePosition, Text)]
environ_free_code env =
    [ (code, val)
    | (Just code, Just val)
        <- map (bimap key_free_code Typecheck.from_val_simple) (Env.to_list env)
    ]

with_free_code :: FreeCodePosition -> Text -> Env.Environ -> Env.Environ
with_free_code pos code = Env.insert_val (free_code_key pos) code

-- ** other env keys

-- | String: append after the pitch, and before the duration.  This is for
-- pitch modifiers like reminder accidentals (!) and cautionary accidentals
-- (?).  TODO this isn't integrated with 'CodePosition', but maybe could be.
-- Would Prepend make any sense?
v_append_pitch :: Env.Key
v_append_pitch = "ly-append-pitch"

-- | String: \"^\" or \"_\", manually sets tie direction, if this note is
-- tied.
v_tie_direction :: Env.Key
v_tie_direction = "ly-tie-direction"

-- * tuplet

-- | Set the env vars that signals that the lilypond converter should make
-- the following notes into a tuplet.
set_tuplet :: RealTime -- ^ score_dur is the visible duration in the score
    -> RealTime -- ^ real_dur is the duration it actually consumes, so
    -- 3 quarters into 1 whole will be 3/4.
    -> Env.Environ
set_tuplet score_dur real_dur = Env.from_list
    [ ("ly-tuplet-score-dur", Typecheck.to_val score_dur)
    , ("ly-tuplet-real-dur", Typecheck.to_val real_dur)
    ]

get_tuplet :: Env.Environ -> Maybe (RealTime, RealTime)
get_tuplet env = (,) <$> get "ly-tuplet-score-dur" <*> get "ly-tuplet-real-dur"
    where get k = Env.maybe_val k env

-- * tremolo

-- | This marks a tremolo event, which triggers special treatment for
-- coincident notes.
v_tremolo :: Env.Key
v_tremolo = "ly-tremolo"

-- * ly-global

-- | String: should be parseable by 'Meter.parse_meter',
-- e.g. @\'3/4\'@.  Used only on @>ly-global@ events.
v_meter :: Env.Key
v_meter = "ly-meter"

-- | String: this has the same format as 'v_meter', but it affects the rhythmic
-- spelling for the instrument.
v_subdivision :: Env.Key
v_subdivision = "ly-subdivision"

-- | String: Gives the title of a new movement.  An event with 'ly_global'
-- instrument and this env val will cause a movement break.
v_movement :: Env.Key
v_movement = "ly-movement"

-- * common code

-- | Emit Ped___^___/ style pedal markings.
mixed_pedal_style :: Text
mixed_pedal_style = "\\set Staff.pedalSustainStyle = #'mixed"
