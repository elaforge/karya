-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This module has Val utilities.
module Derive.TrackLang (
    module Derive.TrackLang, module Derive.BaseTypes
) where
import qualified Util.Seq as Seq
import qualified Ui.ScoreTime as ScoreTime
import Derive.BaseTypes
       (Val(..), vals_equal, Quoted(..),
        ControlFunction(..), Dynamic(..), empty_dynamic, Symbol(..), unsym,
        Ref(..), PControlRef, ControlRef, show_call_val, CallId, Expr, Call(..),
        PitchCall, Term(..))
import qualified Derive.ScoreTypes as Score

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Types


-- * make Val literals

-- | Make an untyped VNum.
num :: Double -> Val
num = VNum . Score.untyped

str :: Text -> Val
str = VSymbol . Symbol

score_time :: ScoreTime -> Val
score_time = VNum . Score.Typed Score.Score . ScoreTime.to_double

real_time :: RealTime -> Val
real_time = VNum . Score.Typed Score.Real . RealTime.to_seconds

transposition :: Pitch.Transpose -> Val
transposition t = VNum $ case t of
    Pitch.Diatonic d -> Score.Typed Score.Diatonic d
    Pitch.Chromatic d -> Score.Typed Score.Chromatic d
    Pitch.Nn d -> Score.Typed Score.Nn d

type_to_transpose :: Score.TypedVal -> Maybe Pitch.Transpose
type_to_transpose (Score.Typed typ val) = case typ of
    Score.Diatonic -> Just $ Pitch.Diatonic val
    Score.Chromatic -> Just $ Pitch.Chromatic val
    Score.Nn -> Just $ Pitch.Nn val
    _ -> Nothing

to_scale_id :: Val -> Maybe Pitch.ScaleId
to_scale_id (VSymbol (Symbol a)) = Just (Pitch.ScaleId a)
to_scale_id _ = Nothing

sym_to_scale_id :: Symbol -> Pitch.ScaleId
sym_to_scale_id (Symbol s) = Pitch.ScaleId s

scale_id_to_sym :: Pitch.ScaleId -> Symbol
scale_id_to_sym (Pitch.ScaleId s) = Symbol s

-- | Defaulted control from a RealTime.
real_control :: Score.Control -> RealTime -> ControlRef
real_control c deflt = DefaultedControl c $
    Score.untyped $ Signal.constant (RealTime.to_seconds deflt)

constant_control :: Signal.Y -> ControlRef
constant_control = ControlSignal . Score.untyped . Signal.constant

quoted :: Symbol -> [Val] -> Quoted
quoted name args = Quoted $ literal_call name args :| []


-- * expressions

-- | Transform the Symbols in an expression.  This affects both symbols in call
-- position, and argument symbols.
map_symbol :: (Symbol -> Symbol) -> Expr -> Expr
map_symbol f = fmap call
    where
    call (Call sym terms) = Call (f sym) (map term terms)
    term (ValCall c) = ValCall (call c)
    term (Literal (VSymbol sym)) = Literal (VSymbol (f sym))
    term (Literal lit) = Literal lit

-- | Transform the arguments in an expression.  This affects only vals in
-- argument position.
map_args :: (Val -> Val) -> Expr -> Expr
map_args f = fmap call
    where
    call (Call sym terms) = Call sym (map term terms)
    term (ValCall c) = ValCall (call c)
    term (Literal lit) = Literal (f lit)

map_call_id :: (CallId -> CallId) -> Call -> Call
map_call_id f (Call call args) = Call (f call) args

-- | Transform only the CallId in the generator position.
map_generator :: (CallId -> CallId) -> Expr -> Expr
map_generator f (call1 :| calls) = case calls of
    [] -> map_call_id f call1 :| []
    _ : _ -> call1 :| Seq.map_last (map_call_id f) calls

-- | Convenient constructor for Call.
call :: Symbol -> [Term] -> Call
call sym = Call sym

call0 :: Symbol -> Call
call0 sym = Call sym []

literal_call :: Symbol -> [Val] -> Call
literal_call sym args = call sym (map Literal args)

inst :: Text -> Term
inst = Literal . VInstrument . Score.Instrument

val_call :: Symbol -> [Val] -> Term
val_call sym args = ValCall (literal_call sym args)
