-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NamedFieldPuns #-}
-- | Low level fltk binding for drawing the keycaps.
module Ui.KeycapsC (create, destroy, update) where
import qualified Data.Map as Map
import qualified Util.CUtil as CUtil
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
    layoutp <- CUtil.new layout -- The widget will take ownership.
    win <- c_create (i x) (i y) (i w) (i h) layoutp
    PtrMap.set_keycaps $ Just win
    where
    (w, h) = lt_size layout
    i = CUtil.c_int

-- KeycapsWindow *keycaps_create(
--     int x, int y, int w, int h, const Keycaps::Layout *layout);
foreign import ccall "keycaps_create"
    c_create :: CInt -> CInt -> CInt -> CInt -> Ptr Layout -> IO (Ptr CWindow)

destroy :: Fltk.Fltk ()
destroy = Fltk.fltk destroy_

destroy_ :: IO ()
destroy_ = whenJustM PtrMap.lookup_keycaps $ \win -> do
    c_destroy win
    PtrMap.set_keycaps Nothing

foreign import ccall "keycaps_destroy" c_destroy :: Ptr CWindow -> IO ()

update :: Bindings -> Fltk.Fltk ()
update (Bindings bindings) = Fltk.fltk $
    whenJustM PtrMap.lookup_keycaps $ \win -> do
        withArrayLen bindings $ \bindings_len bindingsp ->
            c_update win bindingsp (CUtil.c_int bindings_len)

-- void keycaps_update(
--     KeycapsWindow *window, const Keycaps::Binding *bindings,
--     int bindings_len);
foreign import ccall "keycaps_update"
    c_update :: Ptr CWindow -> Ptr Binding -> CInt -> IO ()


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
            { lt_bg_color, lt_keycap_color, lt_highlight_color
            , lt_label_color, lt_binding_color
            , lt_labels
            }) = do
        let rects = Map.elems lt_labels
            labels = Map.keys lt_labels
            points =
                [ (x + fst label_offset, y + snd label_offset)
                | (x, y) <- map Rect.upper_left rects
                ]
        (#poke Keycaps::Layout, bg_color) p lt_bg_color
        (#poke Keycaps::Layout, keycap_color) p lt_keycap_color
        (#poke Keycaps::Layout, highlight_color) p lt_highlight_color
        (#poke Keycaps::Layout, label_color) p lt_label_color
        (#poke Keycaps::Layout, binding_color) p lt_binding_color
        (#poke Keycaps::Layout, rects) p =<< newArray rects
        (#poke Keycaps::Layout, rects_len) p $ CUtil.c_int (Map.size lt_labels)
        labelps <- mapM CUtil.newCStringNull0 labels
        (#poke Keycaps::Layout, labels_points) p =<< newArray points
        (#poke Keycaps::Layout, labels_texts) p =<< newArray labelps
        (#poke Keycaps::Layout, labels_len) p $ CUtil.c_int (length lt_labels)

label_offset :: (Int, Int)
label_offset = (1, 8)

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
instance CStorable Binding where
    sizeOf _ = #size Keycaps::Binding
    alignment _ = alignment nullPtr
    poke p (Binding { b_point, b_text, b_doc, b_color }) = do
        (#poke Keycaps::Binding, point) p b_point
        (#poke Keycaps::Binding, text) p =<< CUtil.newCStringNull0 b_text
        (#poke Keycaps::Binding, doc) p =<< CUtil.newCStringNull0 b_doc
        (#poke Keycaps::Binding, color) p $ fromMaybe Color.black b_color
