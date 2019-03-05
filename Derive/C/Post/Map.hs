-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Post.Map (library) where
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Eval as Eval
import qualified Derive.LEvent as LEvent
import qualified Derive.Library as Library
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream

import           Global


library :: Library.Library
library = Library.transformers
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
        let mapper = case control of
                Left control -> map_control
                    (Derive.coerce_context (Args.context args)) control
                    transformer
                Right pcontrol -> map_pcontrol
                    (Derive.coerce_context (Args.context args)) pcontrol
                    transformer
        Post.emap_m_ id mapper =<< deriver

map_control :: Derive.Context Derive.Control -> ScoreT.Control
    -> DeriveT.Quoted -> Score.Event -> Derive.Deriver [Score.Event]
map_control ctx control transformer event = do
    let ScoreT.Typed typ sig = fromMaybe mempty $
            Score.event_control control event
    sig <- (LEvent.write_snd =<<) $ Post.derive_signal $
        Eval.eval_quoted_transformers ctx transformer $
            return $ Stream.from_event sig
    return [Score.set_control control (ScoreT.Typed typ sig) event]

map_pcontrol :: Derive.Context Derive.Pitch -> ScoreT.PControl
    -> DeriveT.Quoted -> Score.Event -> Derive.Deriver [Score.Event]
map_pcontrol ctx control transformer event = do
    let sig = fromMaybe mempty $ Score.event_named_pitch control event
    sig <- (LEvent.write_snd =<<) $ Post.derive_signal $
        Eval.eval_quoted_transformers ctx transformer $
            return $ Stream.from_event sig
    return [Score.set_named_pitch control sig event]
