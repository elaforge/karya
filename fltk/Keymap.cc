// Copyright 2020 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <FL/fl_draw.H>

#include "Keymap.h"
#include "config.h"
#include "f_util.h"


Keymap::Keymap(int x, int y, int w, int h, const Layout *layout) :
    Fl_Widget(x, y, w, h),
    layout(layout),
    highlight_index(-1)
{
}

Keymap::~Keymap()
{
    delete layout;
    for (Binding *b : bindings)
        delete b;
}


void
Keymap::set_bindings(const std::vector<Binding *> &bindings)
{
    for (Binding *b : this->bindings)
        delete b;
    this->bindings = bindings;
    this->redraw();
}


const char *
Keymap::highlighted() const
{
    if (0 <= highlight_index && highlight_index < bindings.size()) {
        return bindings[highlight_index]->doc;
    }
    return nullptr;
}


int
Keymap::handle(int evt)
{
    switch (evt) {
    case FL_ENTER:
        return true; // I want FL_MOVE.
    case FL_MOVE: {
        IPoint p(Fl::event_x(), Fl::event_y());
        for (int i = 0; i < layout->rects_len; i++) {
            const IRect &rect = layout->rects[i];
            if (rect.contains(p)) {
                if (highlight_index != i) {
                    highlight_index = i;
                    redraw();
                    do_callback();
                }
                break;
            }
        }
        return true;
    }
    default:
        return false;
    }
}


void
Keymap::draw()
{
    fl_color(layout->bg_color.fl());
    fl_rectf(x(), y(), w(), h());

    // draw layout
    for (int i = 0; i < layout->rects_len; i++) {
        fl_color(i == highlight_index
            ? layout->highlight_color.fl() : layout->keycap_color.fl());
        const IRect &rect = layout->rects[i];
        fl_rectf(rect.x, rect.y, rect.w, rect.h);
    }
    fl_font(Config::font, Config::font_size::keymap_label);
    for (int i = 0; i < layout->labels_len; i++) {
        const char t[] = {layout->labels_chars[i], '\0'};
        const IPoint &p = layout->labels_points[i];
        fl_color(layout->label_color.fl());
        fl_draw(t, p.x, p.y);
    }
    // draw bindings
    fl_color(layout->binding_color.fl());
    fl_font(Config::font, Config::font_size::keymap_binding);
    for (const Binding *binding : bindings) {
        fl_draw(binding->text, binding->point.x, binding->point.y);
    }
}

enum {
    doc_h = Config::Block::track_title_height
};

KeymapWindow::KeymapWindow(int x, int y, int w, int h, const char *title,
        const Keymap::Layout *layout) :
    Fl_Double_Window(x, y, w, h + doc_h, title),
    keymap(0, 0, w, h, layout),
    doc(0, h, w, doc_h)
{
    // border(false);
    resizable(nullptr); // window cannot be resized
    keymap.callback(KeymapWindow::keymap_cb, static_cast<void *>(this));
    doc.textsize(Config::font_size::input);
    doc.box(FL_FLAT_BOX);
    doc.color(layout->bg_color.brightness(0.85).fl());
    // Suppress the under carat thing for selection position.
    doc.visible_focus(false);
}


void
KeymapWindow::keymap_cb(Fl_Widget *w, void *vp)
{
    KeymapWindow *self = static_cast<KeymapWindow *>(vp);
    self->doc.value(self->keymap.highlighted());
}


int
KeymapWindow::handle(int evt)
{
    switch (evt) {
    case FL_ENTER:
        // This should opt out of focus, but doesn't work on OS X, or maybe not
        // for windows.
        // return false;
        return true; // to receive FL_MOVE
    case FL_MOVE:
        return Fl_Double_Window::handle(evt);
    case FL_PUSH:
    case FL_FOCUS:
        return false;
    case FL_DRAG:
        // https://fltk.gitlab.io/fltk/events.html says I won't get this
        // unless I return true for FL_PUSH, but that's not true on OS X.
        // Return true to eat it so Fl::add_handler doesn't get it.
        return true;
    default:
        return false;
    }
}
