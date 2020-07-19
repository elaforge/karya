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
    if (evt == FL_ENTER) {
        return true;
    } else if (evt == FL_MOVE) {
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
    } else if (evt == FL_KEYDOWN && Fl::event_key() == FL_Escape) {
        window()->hide();
    }
    return false;
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
    if (0 <= highlight_index && highlight_index < bindings.size()) {
        // TODO display bindings[highlight_index].doc in the gutter
    }
}

enum {
    doc_h = Config::Block::track_title_height
};

KeymapWindow::KeymapWindow(int x, int y, int w, int h, const char *title,
        const Keymap::Layout *layout) :
    Fl_Double_Window(x, y, w, h, title),
    keymap(0, 0, w, h - doc_h, layout),
    doc(0, h - doc_h, w, doc_h)
{
    resizable(nullptr); // window cannot be resized
    keymap.callback(KeymapWindow::keymap_cb, static_cast<void *>(this));
    // TODO can I get rid of the border, or have the minimal title bar?
    // TODO alternately, make it resizable and scrollable
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
    bool testing = true;
    // DEBUG("evt " << f_util::show_event(evt));
    if (testing && evt == FL_KEYDOWN && Fl::event_key() == FL_Escape) {
        // This is kind of dumb, but I'm used to using this to quit test_block.
        this->hide();
        return true;
    } else {
        Fl_Double_Window::handle(evt);
        return true; // TODO tmp for testing
    }
}
