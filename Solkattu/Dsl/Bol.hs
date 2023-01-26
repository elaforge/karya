-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Details in "Solkattu.Bol".
module Solkattu.Dsl.Bol (
    module Solkattu.Dsl.Bol
    , module Solkattu.Talas
    , module Solkattu.Dsl.Generic
    , module Solkattu.Dsl.Interactive
) where
import           Prelude hiding ((.), (^))
import qualified Data.String as String

import qualified Util.CallStack as CallStack
import qualified Solkattu.Bol as Bol
import           Solkattu.Talas (tintal, kehrwa, jhaptal, rupak)
import           Solkattu.Dsl.Interactive (diff, diffw)
import qualified Solkattu.Format.Terminal as Terminal
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Talas as Talas

import           Global
import           Solkattu.Dsl.Generic


type Sequence = SequenceT (Realize.Stroke Bol.Bol)

type Section = Korvai.Section Sequence

instance String.IsString Sequence where
    -- Even with InstanceSigs, this doesn't actually work to add a call stack.
    -- fromString :: CallStack.Stack => String -> Sequence
    fromString s = strS (txt s)

-- | Parse a string to bols.  Look for syllables inside words.
strS :: CallStack.Stack => Text -> Sequence
strS str = mconcatMap (maybe __ make) $ check $ Bol.parseBols str
    where
    make = \case
        Bol.S1 b1 -> _bol b1
        Bol.S2 b1 b2 -> su (_bol b1 . _bol b2)

-- * sollus

_bol :: Bol.Bol -> Sequence
_bol s = S.singleton $ S.Note (Solkattu.Note (Solkattu.note (Realize.stroke s)))

dha = _bol Bol.Dha
ti  = _bol Bol.Ti
ra  = _bol Bol.Ra
ki  = _bol Bol.Ki
ta  = _bol Bol.Ta
ka  = _bol Bol.Ka
taa = _bol Bol.Taa
tu  = _bol Bol.Tu
na  = _bol Bol.Na
tet = _bol Bol.Tet
te  = _bol Bol.Te
ga  = _bol Bol.Ga
di  = _bol Bol.Di
ge  = _bol Bol.Ge
ne  = _bol Bol.Ne

-- * fragments

tr, trkt, kt, kttk :: Sequence
tr = "tr"
trkt = "trkt"
kt = "tk"
kttk = "kttk"

tetekata :: Sequence
tetekata = namedT Solkattu.GPattern "8n" $
    Solkattu.Standard ^ "tette kata gadi gene"

-- * realize

realize :: Korvai -> IO ()
realize = Terminal.printBol (concrete $ wide Terminal.defaultConfig)

realize_ :: Int -> Korvai -> IO ()
realize_ width = Terminal.printBol
    (concrete $ Terminal.defaultConfig { Terminal._terminalWidth = width })

{-
realizet :: Korvai.Korvai -> IO ()
realizet = _printInstrument Just Korvai.ITabla concrete

_printInstrument
    :: (Solkattu.Notation stroke1, Solkattu.Notation stroke2, Ord stroke1)
    => (Realize.Stroke stroke1 -> Maybe (Realize.Stroke stroke2))
    -> Korvai.Instrument stroke1
    -> (Terminal.Config -> Terminal.Config)
    -> Korvai -> IO ()
_printInstrument postproc inst setConfig =
    Interactive.printInstrument True True inst (_defaultStrokes inst)
        (setConfig Terminal.defaultConfig) postproc
-}

-- * korvai

korvai :: Talas.Tal -> [Section] -> Korvai
korvai = Korvai.tablaKorvai

korvai1 :: Talas.Tal -> Section -> Korvai
korvai1 tala section = korvai tala [section]

korvaiS :: Talas.Tal -> [Sequence] -> Korvai
korvaiS tala = korvai tala • Korvai.inferSections

korvaiS1 :: Talas.Tal -> Sequence -> Korvai
korvaiS1 tala seq = korvaiS tala [seq]

-- * metadata

akash :: Korvai -> Korvai
akash = source "akash"

qaida :: Korvai -> Korvai
qaida = withType "qaida"

tukra :: Korvai -> Korvai
tukra = withType "tukra"
