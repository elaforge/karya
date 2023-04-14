-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls that configure other calls.
module Derive.C.Prelude.Config (library) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Derive.Args as Args
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Flags as Flags
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Synth.Shared.Note as Note

import           Global


library :: Library.Library
library = mconcat
    [ Library.poly_transformers [("h", c_hold)]
    , Library.transformers
        [ ("add-flag", c_add_flag)
        , ("infer-dur", c_infer_dur)
        , ("initialize", c_initialize)
        ]
    ]

c_add_flag :: Derive.Transformer Derive.Note
c_add_flag = Derive.transformer Module.prelude "add-flag" Tags.postproc
    "Add the given flags to transformed events.\
    \ Mostly for debugging and testing."
    $ Sig.callt (Sig.many1 "flag" "Add these flags.") $ \flags _args ->
        fmap $ Post.emap1_ $ Score.add_flags $ mconcatMap Flags.flag $
            NonEmpty.toList flags

c_hold :: Derive.Taggable d => Derive.Transformer d
c_hold = Make.environ Module.prelude EnvKey.hold "'Derive.EnvKey.hold'"
    (Sig.defaulted "time" (Typecheck.real 0.25) "Hold this long.")
    Typecheck._real

c_infer_dur :: Derive.Transformer Derive.Note
c_infer_dur = Derive.transformer Module.prelude "infer-dur" Tags.postproc
    "Add 'Derive.Flags.infer_duration' to the events."
    $ Sig.call0t $ \_args ->
        fmap $ Post.emap1_ $ Score.add_flags Flags.infer_duration

-- * initialize

c_initialize :: Derive.Transformer Derive.Note
c_initialize = Derive.transformer Module.prelude "initialize" mempty
    "Emit initialization controls for im instruments." $
    Sig.callt ((,)
        <$> Sig.required "inst" "Instrument to initialize."
        <*> Sig.many_pairs "control,val" "(control, val) pairs.\
            \ If the val is a list, expect the instrument to have that many\
            \ elements, and set the control for each element."
    ) $ \(inst, pairs) args deriver -> do
        elements <- Derive.inst_elements . snd <$> Derive.get_instrument inst
        start <- Args.real_start args
        -- [(_, [Either _ Pitch])] -> [(_, [Either _ Transposed])]
        pairs <- (traverse . traverse . traverse . traverse)
            (Derive.resolve_pitch start) pairs
        element_controls <- Derive.require_right id $
            parse_pairs (Set.toAscList elements) pairs
        let notes = map (uncurry (dummy_note inst))
                (Map.toList element_controls)
        (Stream.from_events notes <>) <$> deriver

dummy_note :: ScoreT.Instrument -> Note.Element -> Map ScoreT.Control Double
    -> Score.Event
dummy_note inst element controls = Score.empty_event
    { Score.event_pitch = case Map.lookup pitch controls of
        Nothing -> mempty
        Just nn -> PSignal.constant $ PSignal.nn_pitch (Pitch.nn nn)
    , Score.event_text = "faust initialize"
    , Score.event_instrument = inst
    , Score.event_environ =
        (Env.from_list [(EnvKey.element, Typecheck.to_val element)] <>) $
        Env.from_controls $ make_val <$>
            Map.insert Controls.dynamic 0 (Map.delete pitch controls)
    }
    where
    pitch = "pitch"
    make_val = ScoreT.untyped . Signal.constant

parse_pairs :: [Note.Element]
    -> [(ScoreT.Control, [Either Double PSignal.Transposed])]
    -> Either Text (Map Note.Element (Map ScoreT.Control Double))
parse_pairs elements =
    fmap (fmap Map.fromList . Maps.multimap) . concatMapM parse
    where
    parse (_, []) = return []
    parse (control, [val]) = do
        val <- resolve val
        return [(fromMaybe "" (Lists.head elements), (control, val))]
    parse (control, vals)
        | length vals /= length elements = Left $ showt (length vals)
            <> " values for " <> showt (length elements) <> " elements: "
            <> pretty vals <> " vs. " <> pretty elements
        | otherwise = do
            vals <- mapM resolve vals
            return $ zip elements (map (control,) vals)
    resolve (Left nn) = Right nn
    resolve (Right p) = bimap pretty Pitch.nn_to_double $ DeriveT.pitch_nn p
