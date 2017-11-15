-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ness.Convert where
import qualified Data.Map as Map
import qualified Data.Text.IO as Text.IO

import qualified Util.PPrint as PPrint
import qualified Util.Seq as Seq
import qualified Ui.Id as Id
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Note as Note
import Global
import Ness.Global
import qualified Ness.Guitar as Guitar
import qualified Ness.Guitar.GConvert as GConvert
import qualified Ness.Multiplate as Multiplate
import qualified Ness.Multiplate.MConvert as MConvert
import qualified Ness.Patches as Patches
import Ness.Patches (Patch(..), Performance(..))
import qualified Ness.Util as Util

import Types


srate :: SamplingRate
srate = 11000

-- TODO don't hard code this
namespace :: Id.Namespace
namespace = Id.namespace "ness2"

mkBlockId :: Text -> BlockId
mkBlockId block = Id.BlockId $ Id.id namespace block

printPerformance :: Text -> IO ()
printPerformance block =
    mapM_ ppr =<< either errorIO return =<< loadConvert block
    where
    ppr (inst, perf) = Text.IO.putStrLn inst >> case perf of
        Guitar _ s -> PPrint.pprint s
        Multiplate _ s -> PPrint.pprint s

run :: Text -> IO ()
run block = do
    let blockId = mkBlockId block
    let notesFilename = Config.notesFilename Config.ness blockId
    instPerformances <- either errorIO return =<< loadConvert block
    Util.submitInstruments "convert"
        (map (nameScore notesFilename) instPerformances)
    where
    nameScore notesFilename (inst, p) =
        ( Config.outputFilename notesFilename (Just inst)
        , inst
        , renderPerformance srate p
        )


-- * implementation

type Error = Text

loadConvert :: Text -> IO (Either Error [(Note.InstrumentName, Performance)])
loadConvert b =
    convert <$> load (Config.notesFilename Config.ness (mkBlockId b))

renderPerformance :: SamplingRate -> Performance -> (Text, Text)
renderPerformance sr (Guitar i s) = Guitar.renderAll sr (i, s)
renderPerformance sr (Multiplate i s) = Multiplate.renderAll sr (i, s)

convert :: [Note.Note] -> Either Error [(Note.InstrumentName, Performance)]
convert notes = do
    -- Group by patches, and then instruments within the patches.
    patches <- forM notes $ \n -> tryJust ("no patch: " <> pretty n) $
        Map.lookup (Note.patch n) Patches.patches
    concatMapM (uncurry convertPatch) $ Seq.group_fst (zip patches notes)

convertPatch :: Patch -> [Note.Note]
    -> Either Error [(Note.InstrumentName, Performance)]
convertPatch patch = mapM convert1 . Seq.keyed_group_sort Note.instrument
    where
    convert1 (inst, notes) = (inst,) <$> case patch of
        PGuitar i -> Guitar i <$> GConvert.convert i notes
        PMultiplate i -> Multiplate i <$> MConvert.convert i notes

load :: FilePath -> IO [Note.Note]
load fname = either (errorIO . pretty) return =<< Note.unserialize fname
