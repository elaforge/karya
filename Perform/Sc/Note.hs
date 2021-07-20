-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Sc.Note (
    Note(..), end, ControlId(..), PatchName
    , Notes, PlayNotes(..)
) where
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified GHC.Generics as Generics

import qualified Util.Pretty as Pretty
import qualified Derive.LEvent as LEvent
import qualified Perform.Midi.MSignal as MSignal

import           Global
import           Types


data Note = Note {
    patch :: !PatchName
    , start :: !RealTime
    , duration :: !RealTime
    , duration_control :: !ControlId
    , controls :: !(Map ControlId MSignal.Signal)
    } deriving (Eq, Show, Generics.Generic)

end :: Note -> RealTime
end n = start n + duration n

instance Pretty.Pretty Note where format = Pretty.formatG

newtype ControlId = ControlId Int.Int32
    deriving (Eq, Ord, Show, Pretty)

type PatchName = ByteString.ByteString


type Notes = [LEvent.LEvent Note]

-- | Store notes with shift (backwards in time) and stretch.
--
-- For MIDI, I apply the transformation directly to the msgs because they are
-- already in their low level form so the transform will stream.  But SC Notes
-- are at a higher level and converted to low level OSC only in Sc.Play... not
-- for any really good reason, but I guess it seemed simpler to not expose the
-- "perform" step, and keep it inside Sc.Play.
data PlayNotes = PlayNotes {
    shift :: RealTime
    , stretch :: RealTime
    , notes :: Notes
    } deriving (Show)
