-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NamedFieldPuns #-}
-- | Low level fltk binding for drawing the keymap.
module Ui.KeymapC (create, destroy, update) where
import qualified Util.CUtil as CUtil
import qualified Ui.Fltk as Fltk

import           ForeignC
import           Ui.KeymapT

#include "Ui/c_interface.h"


create :: (Int, Int) -> Layout -> Fltk.Fltk (Ptr CWindow)
create (x, y) layout = Fltk.fltk $ do
    layoutp <- CUtil.new layout -- The widget will take ownership.
    c_create (i x) (i y) (i w) (i h) layoutp
    where
    (w, h) = lt_size layout
    i = CUtil.c_int

-- KeymapWindow *keymap_create(
--     int x, int y, int w, int h, const Keymap::Layout *layout);
foreign import ccall "keymap_create"
    c_create :: CInt -> CInt -> CInt -> CInt -> Ptr Layout -> IO (Ptr CWindow)

destroy :: Ptr CWindow -> Fltk.Fltk ()
destroy = Fltk.fltk . c_destroy

foreign import ccall "keymap_destroy" c_destroy :: Ptr CWindow -> IO ()

update :: Ptr CWindow -> Bindings -> Fltk.Fltk ()
update winp (Bindings bindings) = Fltk.fltk $
    withArrayLen bindings $ \bindings_len bindingsp ->
        c_update winp bindingsp (CUtil.c_int bindings_len)

-- void keymap_update(
--     KeymapWindow *window, const Keymap::Binding *bindings, int bindings_len);
foreign import ccall "keymap_update"
    c_update :: Ptr CWindow -> Ptr Binding -> CInt -> IO ()


-- * instances

{-
    struct Layout {
        Color bg_color;
        Color keycap_color; // Base keycap color.
        Color highlight_color; // Change keycap color on mouse over.
        Color label_color; // Color of labels_chars.
        Color binding_color; // Color of Binding::text.

        IRect *rects;
        int rects_len;

        IPoint *labels_points;
        char *labels_chars;
        int labels_len;
    }
-}
instance CStorable Layout where
    sizeOf _ = #size Keymap::Layout
    alignment _ = alignment nullPtr
    poke p (Layout
            { lt_bg_color, lt_keycap_color, lt_highlight_color
            , lt_label_color, lt_binding_color
            , lt_rects, lt_labels
            }) = do
        (#poke Keymap::Layout, bg_color) p lt_bg_color
        (#poke Keymap::Layout, keycap_color) p lt_keycap_color
        (#poke Keymap::Layout, highlight_color) p lt_highlight_color
        (#poke Keymap::Layout, label_color) p lt_label_color
        (#poke Keymap::Layout, binding_color) p lt_binding_color
        (#poke Keymap::Layout, rects) p =<< newArray lt_rects
        (#poke Keymap::Layout, rects_len) p (CUtil.c_int (length lt_rects))
        let (points, chars) = unzip lt_labels
        (#poke Keymap::Layout, labels_points) p =<< newArray points
        (#poke Keymap::Layout, labels_chars) p
            =<< newArray (map CUtil.c_char chars)
        (#poke Keymap::Layout, labels_len) p (CUtil.c_int (length lt_labels))

{-
    struct Binding {
        IPoint point;
        // Text to appear on the keycap, utf8 encoded.
        const char *text;
        // A longer description for the binding, utf8 encoded.
        const char *doc;
    }
-}
instance CStorable Binding where
    sizeOf _ = #size Keymap::Binding
    alignment _ = alignment nullPtr
    poke p (Binding { b_point, b_text, b_doc }) = do
        (#poke Keymap::Binding, point) p b_point
        (#poke Keymap::Binding, text) p =<< CUtil.textToCString0 b_text
        (#poke Keymap::Binding, doc) p =<< CUtil.textToCString0 b_doc
