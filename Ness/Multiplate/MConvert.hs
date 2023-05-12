-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ness.Multiplate.MConvert where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Perform.RealTime as RealTime
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global
import Ness.Global
import qualified Ness.Multiplate as Multiplate
import qualified Ness.Multiplate.Patch as Patch


-- | Scale dynamic=1 to this.
maxAmp :: Newtons
maxAmp = 1500

type Error = Text

-- * implementation

convert :: Multiplate.Instrument -> [Note.Note] -> Either Error Multiplate.Score
convert inst notes = do
    strikes <- mapM (convertNote inst) notes
    let score = makeScore strikes
    let errors = Multiplate.verify inst score
    unless (null errors) $
        Left $ Multiplate.iName inst <> ": " <> Text.intercalate "; " errors
    return score

makeScore :: [Multiplate.Strike] -> Multiplate.Score
makeScore strikes = Multiplate.Score
    { sDecay = 6
    , sStrikes = strikes
    }

convertNote :: Multiplate.Instrument -> Note.Note
    -> Either Error Multiplate.Strike
convertNote inst note = first ((pretty note <> ": ")<>) $ do
    let object = Note.element note
    unless (object `elem` map Multiplate.pName (Multiplate.iPlates inst)) $
        Left $ "no object: " <> object
    let get c = tryJust ("no " <> pretty c) $ Map.lookup c (Note.controls note)
    let getStart c = tryJust ("no value: " <> pretty c)
            . flip Signal.at_maybe (Note.start note) =<< get c
    dyn <- getStart Control.dynamic
    (x, y) <- (,) <$> getStart Patch.c_x <*> getStart Patch.c_y
    dur <- getStart Patch.c_duration
    return $ Multiplate.Strike
        { sObject = object
        , sStart = RealTime.to_seconds $ Note.start note
        , sDuration = dur
        , sPosition = (x, y)
        , sForce = dyn * maxAmp
        }
