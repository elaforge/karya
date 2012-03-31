-- | Tools for interactively tweaking Symbol parameters.
module Cmd.Lang.LSymbol where
import qualified Control.Monad.Trans as Trans

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Symbol as Symbol
import qualified Ui.SymbolC as SymbolC
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Selection as Selection


-- | Make a test block with a dummy track and event that will display the
-- symbol under test.
make :: Cmd.CmdL ()
make = do
    bid <- Create.named_block "symbol-test" State.no_ruler
    tid <- Create.track_ruler bid State.no_ruler 1 100
    State.set_track_width bid 0 0
    State.insert_event tid 0 $ Event.event "symbol" 5
    vid <- Create.fitted_view bid
    Selection.set vid (Just (Types.point_selection 1 0))
    -- ViewConfig.bring_to_front vid

-- | Put the given Symbol into the test block.
set :: Symbol.Symbol -> Cmd.CmdL ()
set sym = do
    fonts <- Trans.liftIO $ SymbolC.insert_symbol sym
    unless (null fonts) $ do
        Cmd.throw $ "Missing fonts: " ++ Pretty.pretty fonts
    (_, _, tid, _) <- Selection.get_insert
    State.insert_event tid 0 $
        Event.event ("`" ++ Symbol.sym_name sym ++ "`") 5

glyph_at :: Int -> (Double, Double) -> Symbol.Glyph -> Symbol.Glyph
glyph_at size align glyph =
    glyph { Symbol.glyph_size = size, Symbol.glyph_align = align }

font :: String -> Symbol.Glyph -> Symbol.Glyph
font name glyph = glyph { Symbol.glyph_font = Just name }

rotate :: Int -> Symbol.Glyph -> Symbol.Glyph
rotate degrees glyph = glyph { Symbol.glyph_rotate = degrees }
