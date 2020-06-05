-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Handle 'physical_key' layout.
module Cmd.PhysicalKey where
import qualified Data.Map as Map

import qualified Util.CallStack as CallStack
import qualified Cmd.KeyLayouts as KeyLayouts
import qualified Local.KeyLayout
import qualified Perform.Pitch as Pitch

import           Global


-- * key layout

-- | Map a physical key, written relative to USA qwerty layout, to whatever
-- character that key actually emits (if the layout is already USA qwerty then
-- it's id of course).  This is for layouts which should be done based on
-- physical key position, like piano-style keyboards.  It makes the
-- overlapping-ness of non-mapped keys hard to predict though.
--
-- Since it's intended to map literal key characters, there is no accomodation
-- for failure.  Really this should be done at compile time, so it's
-- conceptually a compile time error.
--
-- TODO isn't there some way I can get this at compile time?  template haskell?
physical_key :: CallStack.Stack => Char -> Char
physical_key c =
    fromMaybe (errorStack $ showt c <> " not found") $
    KeyLayouts.from_qwerty Local.KeyLayout.layout c

-- | Map logical keys to the pitches they emit when kbd entry is on.
-- It's in this module so it can be shared with instruments that also want
-- to take over those keys.  I intentionally omit some keys so their editing
-- functions still work in kbd entry.
pitch_map :: Map Char Pitch.Pitch
pitch_map = Map.fromList $ concat
    -- I leave '-' free since it's mapped to change octave.
    [ [('1', Pitch.Pitch 1 (Pitch.Degree 0 (-1)))]
    , keys 1 "234567890" 1
    , keys 1 "qwertyuiop" 0
    -- 'a' is also the append cmd.
    -- This omits symbol characters so they can retain their edit bindings.
    , keys 0 "sdfghjkl;" 1
    , keys 0 "zxcvbnm,." 0
    ]
    where
    keys oct letters accs =
        [ (c, Pitch.Pitch oct $ Pitch.Degree pc accs)
        -- The mapping should happen at compile time, even though it doesn't.
        | (pc, c) <- zip [0..] (map physical_key letters)
        ]
