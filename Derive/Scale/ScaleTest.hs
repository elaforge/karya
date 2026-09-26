-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for scale tests.
module Derive.Scale.ScaleTest where
import           Prelude hiding (lookup)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           GHC.Stack (HasCallStack)

import qualified Util.Texts as Texts
import qualified Cmd.CmdTest as CmdTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global


keyEnviron :: Text -> Env.Environ
keyEnviron key = Env.insertVal EnvKey.key key mempty

getScale :: HasCallStack => [Scale.Definition] -> Text -> Scale.Scale
getScale scales scaleId =
    fromMaybe (error $ "no scale: " <> show scaleId) $
        List.find ((== Pitch.ScaleId scaleId) . Scale.scaleId)
            [scale | Scale.Simple scale <- scales]

get :: HasCallStack => Text -> Scale.Scale
get = either (error . untxt) id . lookup

lookup :: Text -> Either Text Scale.Scale
lookup expr = do
    Expr.Call (Expr.Symbol sym) terms <- NonEmpty.head <$>
        Parse.parseExprRaw expr
    let vals = [val | Expr.Literal val <- terms]
    DeriveTest.eval Ui.empty $ Scale.get (Derive.CallName sym) vals

inputToNote :: Scale.Scale -> Env.Environ
    -> (Pitch.Octave, Pitch.PitchClass, Pitch.Accidentals) -> Text
inputToNote scale env =
    either pretty Pitch.noteText
    . Scale.scaleInputToNote scale env
    . asciiKbd

asciiKbd :: (Pitch.Octave, Pitch.PitchClass, Pitch.Accidentals)
    -> Pitch.Input
asciiKbd (oct, pc, accs) = CmdTest.asciiKbd $ CmdTest.pitch oct pc accs

noteToCall :: Text -> Text -> [Text] -> ([Maybe Pitch.NoteNumber], [Text])
noteToCall scale title =
    DeriveTest.extract Score.initialNn
    . DeriveTest.deriveTracks (Texts.join2 " | " ("scale=" <> scale) title)
    . UiTest.noteTrack1
