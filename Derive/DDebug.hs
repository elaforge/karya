-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to debug derivers.  These are intended for temporary use via
-- "Util.Debug".  They're not in a test because I wind up putting them in
-- call definitions, and non-test code can't link test modules.
module Derive.DDebug where
import qualified Util.Seq as Seq
import qualified Derive.Call.SubT as SubT
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.Stream as Stream

import qualified Perform.Pitch as Pitch

import           Global


apply_sub :: Functor f => (a -> f b) -> SubT.EventT a -> f (SubT.EventT b)
apply_sub f (SubT.EventT s d n) = SubT.EventT s d <$> f n

showr :: Maybe Derive.NoteDeriver -> Derive.Deriver Text
showr = maybe (return "-") showd

showd :: Derive.NoteDeriver -> Derive.Deriver Text
showd d = do
    es <- Stream.events_of <$> d
    return $ maybe "?" Pitch.note_text $ Score.initial_note =<< Seq.head es
