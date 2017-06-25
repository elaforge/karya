// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <FL/Fl.H>

#include "util.h"

#include "browser_ui.h"


const static int browser_width = 140;
const static int sb_width = 12;

enum { default_font_size = 12 };

int
BrowserInput::handle(int evt)
{
    if (evt == FL_KEYDOWN) {
        switch (Fl::event_key()) {
        case FL_Enter: case FL_KP_Enter:
            if (matches->value() != 0) {
                const char *text = matches->text(matches->value());
                this->msg_callback(msg_choose, text);
            }
            return 1;
        case FL_Down:
            this->matches->value(
                util::clamp(0, matches->size(), matches->value() + 1));
            matches->do_callback();
            return 1;
        case FL_Up:
            this->matches->value(
                util::clamp(0, matches->size(), matches->value() - 1));
            matches->do_callback();
            return 1;
        }
    }
    return Fl_Input::handle(evt);
}

Browser::Browser(int x, int y, int w, int h, MsgCallback cb) :
    Fl_Tile::Fl_Tile(x, y, w, h),
    info_pane(x+browser_width, y, w-browser_width, h),
    select_pane(x, y, browser_width, h),
        query(x, y, browser_width, 20, &matches, cb),
        matches(x, y+20, browser_width, h-20),

    msg_callback(cb)
{
    info_pane.box(FL_THIN_DOWN_BOX);
    info_pane.color(fl_rgb_color(0xff, 0xfd, 0xf0));
    info_pane.textsize(default_font_size);
    info_pane.scrollbar_width(sb_width);
    info_pane.buffer(this->info_buffer);
    info_pane.wrap_mode(true, 0); // Wrap at the edges.
    matches.color(fl_rgb_color(0xff, 0xfd, 0xf0));
    matches.box(FL_FLAT_BOX);
    matches.textsize(default_font_size);
    matches.scrollbar_width(sb_width);
    matches.callback(Browser::matches_cb, static_cast<void *>(this));

    query.color(fl_rgb_color(0xf0, 0xf0, 0xff));
    query.textsize(default_font_size);
    query.when(FL_WHEN_CHANGED);
    query.callback(Browser::query_cb, static_cast<void *>(this));

    select_pane.resizable(matches);

    Fl::focus(&query);
}

void
Browser::set_info(const char *info)
{
    this->info_buffer.text(info);
    // Scroll to the top when new info is displayed.
    this->info_pane.scroll(0, 0);
}


void
Browser::query_cb(Fl_Widget *_w, void *vp)
{
    Browser *self = static_cast<Browser *>(vp);
    self->msg_callback(msg_query, self->query.value());
}

void
Browser::matches_cb(Fl_Widget *w, void *vp)
{
    Browser *self = static_cast<Browser *>(vp);
    int n = self->matches.value();
    if (n != 0) {
        const char *text = self->matches.text(n);
        MsgType type;
        if (Fl::event() == FL_RELEASE && Fl::event_clicks() > 0)
            type = msg_choose;
        else
            type = msg_select;
        self->msg_callback(type, text);
    }
}


BrowserWindow::BrowserWindow(int x, int y, int w, int h, const char *title,
        MsgCallback cb) :
    Fl_Double_Window(0, 0, w, h, title), browser(0, 0, w, h, cb)
{
    Fl::dnd_text_ops(false); // turn off annoying text drag and drop
    Fl::visible_focus(false);
    this->resizable(this);
    this->position(x, y);
}
