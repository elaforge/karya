module Ness.Instruments where
import qualified Data.Map as Map
import qualified Util.Seq as Seq
import Global
import qualified Ness.Guitar as Guitar
import qualified Ness.Guitar.Patch as Guitar.Patch
import qualified Ness.Multiplate as Multiplate
import qualified Ness.Multiplate.Patch as Multiplate.Patch


instruments :: Map Text Instrument
instruments = Map.fromList $ Seq.key_on instrumentName $
    map IGuitar Guitar.Patch.instruments
    ++ map IMultiplate Multiplate.Patch.instruments

data Performance =
    Guitar Guitar.Instrument Guitar.Score
    | Multiplate Multiplate.Instrument Multiplate.Score
    deriving (Show)

data Instrument = IGuitar Guitar.Instrument | IMultiplate Multiplate.Instrument
    deriving (Eq, Ord, Show)

performanceName :: Performance -> Text
performanceName p = case p of
    Guitar i _ -> "guitar-" <> Guitar.iName i
    Multiplate i _ -> "multiplate-" <> Multiplate.iName i

instrumentName :: Instrument -> Text
instrumentName i = case i of
    IGuitar i -> "guitar-" <> Guitar.iName i
    IMultiplate i -> "multiplate-" <> Multiplate.iName i
