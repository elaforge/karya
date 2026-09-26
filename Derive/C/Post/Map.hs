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
    [ ("mapc", cMapc)
    ]

cMapc :: Derive.Transformer Derive.Note
cMapc = Derive.transformer Module.prelude "mapc" Tags.postproc
    "Apply a signal transformer to the pitch or control signals of each event."
    $ Sig.callt ((,)
    <$> Sig.required "control" "Apply to this control. Pitch signals start\
        \ with `#`."
    <*> Sig.required "transformer" "Transformer to apply."
    ) $ \(control, transformer) args deriver -> do
        let mapper = case control of
                Left control -> mapControl
                    (Derive.coerceContext (Args.context args)) control
                    transformer
                Right pcontrol -> mapPcontrol
                    (Derive.coerceContext (Args.context args)) pcontrol
                    transformer
        Post.emapM_ id mapper =<< deriver

mapControl :: Derive.Context Derive.Control -> ScoreT.Control
    -> DeriveT.Quoted -> Score.Event -> Derive.Deriver [Score.Event]
mapControl ctx control transformer event = do
    let ScoreT.Typed typ sig = fromMaybe mempty $
            Score.eventControl control event
    sig <- (LEvent.writeSnd =<<) $ Post.deriveSignal $
        Eval.evalQuotedTransformers ctx transformer $
            pure $ Stream.fromEvent sig
    pure [Score.setControl control (ScoreT.Typed typ sig) event]

mapPcontrol :: Derive.Context Derive.Pitch -> ScoreT.PControl
    -> DeriveT.Quoted -> Score.Event -> Derive.Deriver [Score.Event]
mapPcontrol ctx control transformer event = do
    let sig = fromMaybe mempty $ Score.eventNamedPitch control event
    sig <- (LEvent.writeSnd =<<) $ Post.deriveSignal $
        Eval.evalQuotedTransformers ctx transformer $
            pure $ Stream.fromEvent sig
    pure [Score.setNamedPitch control sig event]
