-- | Calls for randomized scores.
module Derive.Call.Random where
import Util.Control
import qualified Derive.Call as Call
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import Derive.CallSig (optional)
import qualified Derive.Derive as Derive
import qualified Derive.ParseBs as ParseBs
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang


-- * note calls

note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("omit", c_omit)
    , ("alt", c_alternate)
    ]

control_calls :: Derive.ControlCallMap
control_calls = Derive.make_calls
    [ ("omit", c_omit)
    , ("alt", c_alternate)
    ]

pitch_calls :: Derive.PitchCallMap
pitch_calls = Derive.make_calls
    [ ("omit", c_omit)
    , ("alt", c_alternate)
    ]

c_omit :: (Derive.Derived d) => Derive.Call d
c_omit = Derive.transformer "omit"
    "Omit the derived call a certain percentage of the time."
    $ CallSig.call1t
    (optional "omit" 0.5
        "Chance, from 0 to 1, that the transformed note will be omitted."
    ) $ \omit _args deriver -> ifM (Util.chance omit) (return mempty) deriver

c_alternate :: (Derive.Derived d) => Derive.Call d
c_alternate = Derive.stream_generator "alternate"
    ("Pick one of several expressions and evaluate it."
    <> " They have to be strings since calls themselves are not first class."
    ) $ CallSig.parsed_manually "Variable number of string arguments." $
    \args -> do
        exprs <- mapM string_of (zip [1..] (Derive.passed_vals args))
        expr <- Util.pick exprs
        case ParseBs.parse_expr (ParseBs.from_string expr) of
            Left err -> Derive.throw $ "alternate: " ++ err
            Right expr -> Call.reapply args expr
    where
    string_of (_, TrackLang.VString s) = return s
    string_of (i, val) = Derive.throw_arg_error $ "arg " ++ show i
        ++ ": expected a string, but got " ++ ShowVal.show_val val


-- * val calls

val_calls :: Derive.ValCallMap
val_calls = Derive.make_calls
    [ ("pick", c_pick) -- or ?
    , ("range", c_range) -- or -?
    ]

c_pick :: Derive.ValCall
c_pick = Derive.val_call "pick" "Pick one of the arguments randomly." $
    CallSig.parsed_manually "Variable number of arguments." $ \args ->
        Util.pick $ Derive.passed_vals args

c_range :: Derive.ValCall
c_range = Derive.val_call "range" "Pick a random number within a range." $
    CallSig.call2g
    ( optional "low" 0 "Bottom of range, inclusive."
    , optional "high" 1 "Top of range, inclusive."
    ) $ \low high _args -> TrackLang.num <$> Util.random_in low high
