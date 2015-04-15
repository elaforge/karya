module Derive.Call.Post.Map where
import qualified Util.Log as Log
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import Global


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("mapc", c_mapc)
    ]


c_mapc :: Derive.Transformer Derive.Note
c_mapc = Derive.transformer Module.prelude "mapc" Tags.postproc
    "Apply a signal transformer to the pitch or control signals of each event."
    $ Sig.callt ((,)
    <$> Sig.required "control" "Apply to this control. Pitch signals start\
        \ with `#`."
    <*> Sig.required "transformer" "Transformer to apply."
    ) $ \(control, transformer) args deriver -> do
        control <- Derive.require_right ("parsing control: "<>) $
            Score.parse_generic_control control
        let mapper = case control of
                Left control -> map_control
                    (Derive.coerce_call_info (Args.info args)) control
                    transformer
                Right pcontrol -> map_pcontrol
                    (Derive.coerce_call_info (Args.info args)) pcontrol
                    transformer
        Post.emap_m_ id mapper =<< deriver

map_control :: Derive.CallInfo Derive.Control -> Score.Control
    -> TrackLang.Quoted -> Score.Event -> Derive.Deriver [Score.Event]
map_control cinfo control transformer event = do
    let Score.Typed typ sig = fromMaybe mempty $
            Score.event_control control event
    (sig, logs) <- Post.derive_signal $
        Eval.eval_quoted_transformers cinfo transformer $
            return [LEvent.Event sig]
    mapM_ Log.write logs
    return [Score.set_control control (Score.Typed typ sig) event]

map_pcontrol :: Derive.CallInfo Derive.Pitch -> Score.PControl
    -> TrackLang.Quoted -> Score.Event -> Derive.Deriver [Score.Event]
map_pcontrol cinfo control transformer event = do
    let sig = fromMaybe mempty $ Score.event_pitch control event
    (sig, logs) <- Post.derive_signal $
        Eval.eval_quoted_transformers cinfo transformer $
            return [LEvent.Event sig]
    mapM_ Log.write logs
    return [Score.set_named_pitch control sig event]
