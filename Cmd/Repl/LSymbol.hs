-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Tools for interactively tweaking Symbol parameters.
--
-- TODO using this module on linux will break the REPL.  Apparently
-- ghci on linux has a problem when it has to link in a FFI-using module.
module Cmd.Repl.LSymbol where
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Sel as Sel
import qualified Ui.Ui as Ui
import qualified Ui.Symbol as Symbol
import qualified Ui.SymbolC as SymbolC
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Selection as Selection

import Global


-- | Make a test block with a dummy track and event that will display the
-- symbol under test.
make :: Cmd.CmdL ()
make = do
    ident <- Ui.readId "symbol-test"
    bid <- Create.namedBlock ident Ui.noRuler
    Create.trackEvents bid Ui.noRuler 1 100 $
        Track.track "" (Events.singleton (Event.event 0 5 "symbol"))
    Ui.setTrackWidth bid 0 0
    vid <- Create.view bid
    Selection.set vid (Just (Sel.point 1 0 Sel.Positive))

-- | Put the given Symbol into the test block.
set :: Symbol.Symbol -> Cmd.CmdL ()
set sym = do
    fonts <- liftIO $ SymbolC.insert sym
    unless (null fonts) $
        Cmd.throw $ "Missing fonts: " <> pretty fonts
    (_, _, tid, _) <- Selection.getInsert
    Ui.insertEvent tid $ Event.event 0 5 ("`" <> Symbol.name sym <> "`")

getFonts :: Cmd.CmdL [Symbol.Font]
getFonts = liftIO SymbolC.getFonts

glyphAt :: Int -> (Double, Double) -> Symbol.Glyph -> Symbol.Glyph
glyphAt size align glyph =
    glyph { Symbol.glyphSize = size, Symbol.glyphAlign = align }

font :: String -> Symbol.Glyph -> Symbol.Glyph
font name glyph = glyph { Symbol.glyphFont = Just name }

rotate :: Int -> Symbol.Glyph -> Symbol.Glyph
rotate degrees glyph = glyph { Symbol.glyphRotate = degrees }

arpUp = Symbol.symbol "arp-up" [arp, glyphAt 8 (-0.14, -0.62) arpArrowUp]
arpArrowUp = Symbol.glyph "\xe18a"
arp = (Symbol.glyph "\xe18e") { Symbol.glyphRotate = 90 }
