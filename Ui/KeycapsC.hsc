-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Low level fltk binding for drawing the keycaps.
module Ui.KeycapsC (create, destroy, update) where
##ifdef STUB_OUT_FLTK
import           Ui.KeycapsCStub
##else
import qualified Data.Map as Map
import qualified Util.FFI as FFI
import qualified Util.Rect as Rect

import qualified Ui.Color as Color
import qualified Ui.Fltk as Fltk
import qualified Ui.PtrMap as PtrMap

import           Global
import           ForeignC
import           Ui.KeycapsT

#include "Ui/c_interface.h"


-- | The PtrMap only has room for a single keycaps window, so will destroy
-- any existing window.
create :: (Int, Int) -> Layout -> Fltk.Fltk ()
create (x, y) layout = Fltk.fltk $ do
    destroy_
    layoutp <- FFI.new layout -- The widget will take ownership.
    win <- cCreate (i x) (i y) (i w) (i h) layoutp
    PtrMap.setKeycaps $ Just win
    where
    (w, h) = ltSize layout
    i = FFI.cInt

-- KeycapsWindow *keycaps_create(
--     int x, int y, int w, int h, const Keycaps::Layout *layout);
foreign import ccall "keycaps_create"
    cCreate :: CInt -> CInt -> CInt -> CInt -> Ptr Layout -> IO (Ptr CWindow)

destroy :: Fltk.Fltk ()
destroy = Fltk.fltk destroy_

destroy_ :: IO ()
destroy_ = whenJustM PtrMap.lookupKeycaps $ \win -> do
    cDestroy win
    PtrMap.setKeycaps Nothing

foreign import ccall "keycaps_destroy" cDestroy :: Ptr CWindow -> IO ()

update :: RawBindings -> Fltk.Fltk ()
update (RawBindings bindings) = Fltk.fltk $
    whenJustM PtrMap.lookupKeycaps $ \win -> do
        withArrayLen bindings $ \bindingsLen bindingsp ->
            cUpdate win bindingsp (FFI.cInt bindingsLen)

-- void keycaps_update(
--     KeycapsWindow *window, const Keycaps::Binding *bindings,
--     int bindings_len);
foreign import ccall "keycaps_update"
    cUpdate :: Ptr CWindow -> Ptr RawBinding -> CInt -> IO ()


-- * instances

{-
    struct Layout {
        Color bg_color;
        Color keycap_color; // Base keycap color.
        Color highlight_color; // Change keycap color on mouse over.
        Color label_color; // Color of labels_texts.
        Color binding_color; // Color of Binding::text.

        IRect *rects;
        int rects_len;

        IPoint *labels_points;
        const char **labels_texts;
        int labels_len;
    }
-}
instance CStorable Layout where
    sizeOf _ = #size Keycaps::Layout
    alignment _ = alignment nullPtr
    poke p (Layout
            { ltBgColor, ltKeycapColor, ltHighlightColor
            , ltLabelColor, ltBindingColor
            , ltLabels
            }) = do
        let rects = Map.elems ltLabels
            labels = Map.keys ltLabels
            points =
                [ (x + fst labelOffset, y + snd labelOffset)
                | (x, y) <- map Rect.upperLeft rects
                ]
        (#poke Keycaps::Layout, bg_color) p ltBgColor
        (#poke Keycaps::Layout, keycap_color) p ltKeycapColor
        (#poke Keycaps::Layout, highlight_color) p ltHighlightColor
        (#poke Keycaps::Layout, label_color) p ltLabelColor
        (#poke Keycaps::Layout, binding_color) p ltBindingColor
        (#poke Keycaps::Layout, rects) p =<< newArray rects
        (#poke Keycaps::Layout, rects_len) p $ FFI.cInt (Map.size ltLabels)
        labelps <- mapM FFI.newCStringNull0 labels
        (#poke Keycaps::Layout, labels_points) p =<< newArray points
        (#poke Keycaps::Layout, labels_texts) p =<< newArray labelps
        (#poke Keycaps::Layout, labels_len) p $ FFI.cInt (length ltLabels)

labelOffset :: (Int, Int)
labelOffset = (1, 8)

{-
    struct Binding {
        IPoint point;
        // Text to appear on the keycap, utf8 encoded.
        const char *text;
        // A longer description for the binding, utf8 encoded.
        const char *doc;
        // Replace Layout::keycap_color if != Color::black.
        Color color;
    }
-}
instance CStorable RawBinding where
    sizeOf _ = #size Keycaps::Binding
    alignment _ = alignment nullPtr
    poke p (RawBinding point (Binding { bText, bDoc, bColor })) = do
        (#poke Keycaps::Binding, point) p point
        (#poke Keycaps::Binding, text) p =<< FFI.newCStringNull0 bText
        (#poke Keycaps::Binding, doc) p =<< FFI.newCStringNull0 bDoc
        (#poke Keycaps::Binding, color) p $ fromMaybe Color.black bColor

##endif
