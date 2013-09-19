module Cmd.Tala_test where
import qualified Data.Map as Map

import Util.Control
import Util.Test
import qualified Ui.Ruler as Ruler
import qualified Cmd.Tala as Tala


test_make_ruler = do
    let adi = Tala.make_ruler 1 1 4 1 Tala.adi_tala
    equal (extract 20 adi)
        ["1.0", "1.1", "1.2", "1.3", "1.X", "1.O", "1.X", "1.O", "2.0"]

extract :: Double -> Ruler.Ruler -> [Text]
extract zoom =
    map Ruler.mark_name . filter ((<=zoom) . Ruler.mark_name_zoom_level)
    . map snd . Ruler.ascending 0 . snd . head
    . Map.toList . Ruler.ruler_marklists
