-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Instrument definitions for mridangam.  These are shared between multiple
-- mridangam definitions.
module Cmd.Instrument.Mridangam (
    code
    , stops
    , allStrokes
    , ki, ta, nam, din, dim, chapu, muru, arai
    , kin, tan
    , tha, thom
    , palm, fingers, fingertips
    -- * used by pakhawaj
    , makeBoth, makeCode
) where
import           Prelude hiding (min, tan)
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.ImInst as ImInst

import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.PSignal as PSignal

import qualified Perform.Pitch as Pitch
import qualified Solkattu.Instrument.Mridangam as Mridangam
import           Solkattu.Instrument.Mridangam
    (Tha(..), Thom(..), Thoppi(..), Valantalai(..))

import           Global


code :: CUtil.Thru -> Pitch.NoteNumber
    -> Maybe (Derive.TransformerF Derive.Note) -> ImInst.Code
code thru naturalNn transform =
    makeCode thru pitchedStrokes naturalNn transform allStrokes bothCalls

-- | (symbol, dispatchTo, bindToKey)
type BothStroke = (Expr.Symbol, [Expr.Symbol], Maybe Char)

toSym :: Expr.ToExpr a => a -> Expr.Symbol
toSym = Expr.Symbol . Expr.showValExpr . Expr.toExpr

-- | Strokes which have a pitch, which should change with the sruti.
pitchedStrokes :: [Attrs.Attributes]
pitchedStrokes =
    [ nam, din
    , kin, tan, min
    , chapu
    , dim
    ]

{-
    dyn=1 is too much for tha.  But maybe the variation should be built in to
    samples.  So if you say dyn=.75, then do accent for loud ones at 1.
    I guess the dyn here would have to be relative to global dyn.
    Since I use valantalai_attrs, I don't have one for light or strong accent.
    But, I only use Symbols.accent and Symbols.weak now.  So I can have a Light
    Normal Heavy annotation instead of Dyn.  Is it better?
    Getting away from the ad-hoc symbols is better, maybe I could even render
    `^ x` as a lighter `x` eventually.  Or put a numebr in the dyn column,
    which is more orthogonal, but less readable.

    Having an arbitrary symbol for (stroke + dyn) is not orthogonal, there's
    nothing saying they have to be consistent.  Dyn is another dimension not
    really in solkattu.
-}

allStrokes :: [Drums.Stroke]
bothCalls :: [BothStroke]
stops :: Drums.Stops
(allStrokes, bothCalls, stops) =
    ( map make (map Left lhs ++ map Right rhs)
    , concatMap (uncurry makeBoth) [(lh, rh) | lh <- lhs, rh <- rhs]
    , stops
    )
    where
    makeBoth lh rh =
        ( toSym (Mridangam.Both lh rh)
        , [toSym lh, toSym rh]
        , Map.lookup (Mridangam.Both lh rh) keys
        ) : case Mridangam.extraCalls lh rh of
            Nothing -> []
            Just sym -> [(Expr.Symbol sym, [toSym lh, toSym rh], Nothing)]
    keys = Map.fromList $ map (\(c, s) -> (s, c)) $ concat
        [ map (second Mridangam.Thoppi)
            -- This reflects my bias to palm tha, but maybe it should write
            -- just tha attr, and leave palm or fingers separately configured?
            [ ('a', Tha Palm) -- TODO dyn 0.5
            , ('z', Tha Palm)
            , ('Z', Tha Fingers)
            , ('X', Tha Fingertips) -- duplicate with 'C'
            , ('s', Thom Open) -- TODO dyn 0.5
            , ('x', Thom Open)
            , ('c', Thom Low)
            , ('C', Tha Fingertips) -- by analogy with Thom Low
            , ('v', Thom Up)
            ]
            -- TODO when I have samples, have 'o 0' to 'o 1' for arbitrary
            -- pitches.
        , map (second Mridangam.Valantalai)
            [ ('1', Mi)
            , ('q', Ki)
            , ('w', Ta)
            , ('3', Tan)
            , ('e', Nam)
            , ('4', Kin)
            , ('r', Din)
            -- This reflects my bias towards MuruChapu, but like palm vs
            -- fingers tha, maybe it should be left ambiguous?
            , ('5', MuruChapu)
            , ('t', AraiChapu)
            , ('y', Dim)
            , (',', Tra)
            ]
        -- TODO need flam call
        , [('.', Mridangam.Flam (Tha Palm) Ki)]
        , map (second (uncurry Mridangam.Both))
          [ ('g', (Thom Open, Nam))
          , ('b', (Thom Open, Din))
          , ('h', (Thom Open, Ki))
          , ('n', (Thom Open, Ta))
          , ('j', (Tha Palm, Ki))
          , ('m', (Tha Palm, Ta))
          ]
        ]
    make stroke = Drums.Stroke
        { _name = either toSym toSym stroke
        , _attributes = either thoppiAttrs valantalaiAttrs stroke
        , _char = Map.lookup
            (either Mridangam.Thoppi Mridangam.Valantalai stroke) keys
        , _dynamic = 1
        , _group = either tGroup vGroup stroke
        }
    rhs = [minBound ..] :: [Mridangam.Valantalai]
    lhs =
        [ Tha Palm, Tha Fingertips
        , Thom Open, Thom Up
        , Gum
        ]
    tGroup = \case
        Tha {} -> tClosed
        Thom {} -> tOpen
        Gum -> tOpen
    vGroup = \case
        Ki -> vClosed
        Ta -> vClosed
        Tra -> vClosed
        Mi -> vClosed
        Min -> vMeetu
        Nam -> vMeetu
        Din -> vMeetu
        AraiChapu -> vOpen
        MuruChapu -> vOpen
        Dim -> vOpen
        Kin -> vMeetu
        Tan -> vMeetu
        Dhe -> vClosed
        Re -> vClosed
    tClosed = "t-closed"
    tOpen = "t-open"
    vClosed = "v-closed"
    vMeetu = "v-meetu"
    vOpen = "v-open"
    stops =
        [ (tClosed, [tOpen])
        , (vClosed, [vMeetu, vOpen])
        , (vMeetu, [vOpen])
        ]

valantalaiAttrs :: Mridangam.Valantalai-> Attrs.Attributes
valantalaiAttrs = \case
    Ki -> ki
    Ta -> ta
    Tra -> tra
    Mi -> mi
    Min -> min
    Nam -> nam
    Din -> din
    AraiChapu -> arai <> chapu
    MuruChapu -> muru <> chapu
    Dim -> dim
    Kin -> kin
    Tan -> tan
    Dhe -> dhe
    Re -> re

thoppiAttrs :: Mridangam.Thoppi -> Attrs.Attributes
thoppiAttrs = \case
    Tha t -> tha <> case t of
        Palm -> mempty -- could be palm, but let's consider it the default
        Fingers -> fingers
        Fingertips -> fingertips
    Thom t -> case t of
        Open-> thom
        Low -> thom <> Attrs.low
        Up -> thom <> Attrs.up
    Gum -> gum

-- valantalai
ki = Attrs.attr "ki"
ta = Attrs.attr "ta"
tra = Attrs.attr "tra"
mi = Attrs.attr "mi"
min = Attrs.attr "min"
nam = Attrs.attr "nam"
din = Attrs.attr "din"
dim = Attrs.attr "dim"
chapu = Attrs.attr "chapu"
muru = Attrs.attr "muru"
arai = Attrs.attr "arai"
kin = Attrs.attr "kin"
tan = Attrs.attr "tan"
dhe = Attrs.attr "dhe"
re = Attrs.attr "re"

-- thoppi
tha = Attrs.attr "tha"
thom = Attrs.attr "thom"
gum = Attrs.attr "gum"

-- tha variations
palm = Attrs.attr "palm"
fingers = Attrs.attr "fingers" -- played with flat fingers, not palm
fingertips = Attrs.attr "fingertips"


-- * two-handed pitched drums

-- | Make code for a pitched two-handed drum.  This isn't mridangam-specific.
makeCode :: CUtil.Thru -> [Attrs.Attributes] -> Pitch.NoteNumber
    -> Maybe (Derive.TransformerF Derive.Note) -> [Drums.Stroke]
    -> [BothStroke] -> ImInst.Code
makeCode thru pitchedStrokes naturalNn transform strokes both = mconcat
    [ ImInst.noteGenerators generators
    , ImInst.valCalls vals
    , ImInst.cmd (CUtil.insertCall thru charToCall)
    ]
    where
    add t = map (second (Make.modifyGenerator_ "" t))
    generators = maybe id add transform $ concat
        [ CUtil.drumCalls (zip strokes (map config strokes))
        , DUtil.multipleCalls [(call, subcalls) | (call, subcalls, _) <- both]
        ]
    config = CUtil.pitchedStrokes pitchedStrokes naturalNn
        . Drums._attributes
    vals =
        [ ("natural", Make.constantVal Module.instrument "natural"
            doc (PSignal.nnPitch naturalNn))
        ]
        where doc = "Emit the drum's recorded pitch. Use like `#=(natural)`."
    charToCall = concat
        [ [(c, _name) | Drums.Stroke { _char = Just c, _name } <- strokes]
        , [(char, call) | (call, _, Just char) <- both]
        ]

-- | Create calls for all simultaneous left and right hand combinations, and
-- key bindings for a few common ones.
makeBoth :: [Drums.Stroke] -> [Drums.Stroke]
    -> [(Expr.Symbol, [Expr.Symbol])] -- ^ special names for pairs
    -> [(Expr.Symbol, Char)] -> [BothStroke]
makeBoth left right specialNames keys =
    [ (call, subcalls, lookup call keys)
    | (call, subcalls) <- specialNames ++ pairs
    ]
    where
    pairs =
        [ (Expr.Symbol $ u lcall <> u rcall, [lcall, rcall])
        | lcall <- map Drums._name left
        , rcall <- map Drums._name right
        , Text.length (u lcall) == 1 && Text.length (u rcall) == 1
        ]
    u = Expr.unsym
