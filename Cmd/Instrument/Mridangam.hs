-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Instrument definitions for mridangam.  These are shared between multiple
-- mridangam definitions.
module Cmd.Instrument.Mridangam (
    code
    , stops
    , all_strokes
    , ki, ta, nam, din, dim, chapu, muru, arai
    , kin, tan
    , tha, thom
    , palm, fingers, fingertips
    -- * used by pakhawaj
    , make_both, make_code
) where
import           Prelude hiding (tan)
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
code thru natural_nn transform =
    make_code thru pitched_strokes natural_nn transform all_strokes both_calls

-- | (symbol, dispatchTo, bindToKey)
type BothStroke = (Expr.Symbol, [Expr.Symbol], Maybe Char)

to_sym :: Expr.ToExpr a => a -> Expr.Symbol
to_sym = Expr.Symbol . Expr.show_val_expr . Expr.to_expr

-- | Strokes which have a pitch, which should change with the sruti.
pitched_strokes :: [Attrs.Attributes]
pitched_strokes =
    [ nam, din
    , kin, tan
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

all_strokes :: [Drums.Stroke]
both_calls :: [BothStroke]
stops :: Drums.Stops
(all_strokes, both_calls, stops) =
    ( map make (map Left lhs ++ map Right rhs)
    , concatMap (uncurry make_both) [(lh, rh) | lh <- lhs, rh <- rhs]
    , stops
    )
    where
    make_both lh rh =
        ( to_sym (Mridangam.Both lh rh)
        , [to_sym lh, to_sym rh]
        , Map.lookup (Mridangam.Both lh rh) keys
        ) : case Mridangam.extraCalls lh rh of
            Nothing -> []
            Just sym -> [(Expr.Symbol sym, [to_sym lh, to_sym rh], Nothing)]
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
        { _name = either to_sym to_sym stroke
        , _attributes = either thoppi_attrs valantalai_attrs stroke
        , _char = Map.lookup
            (either Mridangam.Thoppi Mridangam.Valantalai stroke) keys
        , _dynamic = 1
        , _group = either t_group v_group stroke
        }
    rhs = [minBound ..] :: [Mridangam.Valantalai]
    lhs =
        [ Tha Palm, Tha Fingertips
        , Thom Open, Thom Up
        , Gum
        ]
    t_group = \case
        Tha {} -> t_closed
        Thom {} -> t_open
        Gum -> t_open
    v_group = \case
        Ki -> v_closed
        Ta -> v_closed
        Tra -> v_closed
        Mi -> v_closed
        Nam -> v_meetu
        Din -> v_meetu
        AraiChapu -> v_open
        MuruChapu -> v_open
        Dim -> v_open
        Kin -> v_meetu
        Tan -> v_meetu
        Dhe -> v_closed
        Re -> v_closed
    t_closed = "t-closed"
    t_open = "t-open"
    v_closed = "v-closed"
    v_meetu = "v-meetu"
    v_open = "v-open"
    stops =
        [ (t_closed, [t_open])
        , (v_closed, [v_meetu, v_open])
        , (v_meetu, [v_open])
        ]

valantalai_attrs :: Mridangam.Valantalai-> Attrs.Attributes
valantalai_attrs = \case
    Ki -> ki
    Ta -> ta
    Tra -> tra
    Mi -> mi
    Nam -> nam
    Din -> din
    AraiChapu -> arai <> chapu
    MuruChapu -> muru <> chapu
    Dim -> dim
    Kin -> kin
    Tan -> tan
    Dhe -> dhe
    Re -> re

thoppi_attrs :: Mridangam.Thoppi -> Attrs.Attributes
thoppi_attrs = \case
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
make_code :: CUtil.Thru -> [Attrs.Attributes] -> Pitch.NoteNumber
    -> Maybe (Derive.TransformerF Derive.Note) -> [Drums.Stroke]
    -> [BothStroke] -> ImInst.Code
make_code thru pitched_strokes natural_nn transform strokes both = mconcat
    [ ImInst.note_generators generators
    , ImInst.val_calls vals
    , ImInst.cmd (CUtil.insert_call thru char_to_call)
    ]
    where
    add t = map (second (Make.modify_generator_ "" t))
    generators = maybe id add transform $ concat
        [ CUtil.drum_calls (zip strokes (map config strokes))
        , DUtil.multiple_calls [(call, subcalls) | (call, subcalls, _) <- both]
        ]
    config = CUtil.pitched_strokes pitched_strokes natural_nn
        . Drums._attributes
    vals =
        [ ("natural", Make.constant_val Module.instrument "natural"
            doc (PSignal.nn_pitch natural_nn))
        ]
        where doc = "Emit the drum's recorded pitch. Use like `#=(natural)`."
    char_to_call = concat
        [ [(c, _name) | Drums.Stroke { _char = Just c, _name } <- strokes]
        , [(char, call) | (call, _, Just char) <- both]
        ]

-- | Create calls for all simultaneous left and right hand combinations, and
-- key bindings for a few common ones.
make_both :: [Drums.Stroke] -> [Drums.Stroke]
    -> [(Expr.Symbol, [Expr.Symbol])] -- ^ special names for pairs
    -> [(Expr.Symbol, Char)] -> [BothStroke]
make_both left right special_names keys =
    [ (call, subcalls, lookup call keys)
    | (call, subcalls) <- special_names ++ pairs
    ]
    where
    pairs =
        [ (Expr.Symbol $ u lcall <> u rcall, [lcall, rcall])
        | lcall <- map Drums._name left
        , rcall <- map Drums._name right
        , Text.length (u lcall) == 1 && Text.length (u rcall) == 1
        ]
    u = Expr.unsym
