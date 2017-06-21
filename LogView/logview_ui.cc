// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <string>
#include <algorithm>
#include <FL/Fl.H>
#include "logview_ui.h"
#include "util.h"

using std::string;

enum { default_font_size = 12 };

enum {
    status_height = 16,
    command_height = 20
};

static Fl_Text_Display::Style_Table_Entry style_table[] =
    { { FL_BLACK, FL_HELVETICA, default_font_size } // A - plain
    , { FL_RED, FL_HELVETICA, default_font_size } // B - warn msgs
    , { FL_BLUE, FL_HELVETICA, default_font_size } // C - clickable text
    , { fl_rgb_color(0, 76, 0), FL_HELVETICA_BOLD, default_font_size } //D-emph
    , { FL_RED, FL_HELVETICA_BOLD, default_font_size } // E - divider
    , { fl_rgb_color(0, 76, 0), FL_HELVETICA, default_font_size } // F-func name
    , { FL_BLACK, FL_HELVETICA_ITALIC, default_font_size } // G - filename
    , { FL_BLACK, FL_COURIER, default_font_size } // H - fixed width
    };


static void
style_update(int pos, int inserted, int deleted, int restyled,
        const char *deleted_text, void *arg)
{
    // DEBUG("style update");
}


static void
style_unfinished_cb(int n, void *p)
{
    // who knows what this does?
}

// LogView /////////////////////////////

LogView::LogView(int x, int y, int w, int h, MsgCallback cb, int max_bytes) :
    Fl_Group(x, y, w, h),
    msg_callback(cb),
    status(x, y, w, status_height),
    command(x, y + status_height, w, command_height),
    display(x, y + status_height + command_height,
            w, h - status_height - command_height),
    max_bytes(max_bytes)
{
    this->resizable(display);
    status.box(FL_FLAT_BOX);
    display.box(FL_THIN_DOWN_BOX);

    status.textsize(default_font_size);
    command.textsize(default_font_size);
    command.callback(LogView::command_cb, static_cast<void *>(this));
    // Wrap at the edge of the widget.
    // TODO this makes it really slow on OS X
    display.wrap_mode(Fl_Text_Display::WRAP_AT_BOUNDS, 0);
    display.scrollbar_width(12);
    display.scrollbar_align(FL_ALIGN_RIGHT);
    display.textfont(FL_HELVETICA);
    display.textsize(8);
    display.buffer(this->buffer);
    // I think style_unfinished_cb is a hook to highlight programmatically.
    // Since I don't use it, I set its code to 'Z', which I'm not using, so
    // it never gets called.
    display.highlight_data(&this->style_buffer, style_table,
            sizeof(style_table) / sizeof(style_table[0]),
            'Z', style_unfinished_cb, 0);
    buffer.add_modify_callback(style_update, &display);

    // Wrapping is done manually in 'wrap_text'.
    status.wrap_mode(false, 0);
    // This should turn scrollbars off.
    status.scrollbar_align(FL_ALIGN_CENTER);
    status.buffer(this->status_buffer);
    status.highlight_data(&this->status_style_buffer, style_table,
            sizeof(style_table) / sizeof(style_table[0]),
            'Z', style_unfinished_cb, 0);
    status_buffer.add_modify_callback(style_update, &status);
}


void
LogView::set_status_height(int height)
{
    int sy = status.y();
    status.resize(status.x(), sy, status.w(), height);
    command.resize(command.x(), sy + height, command.w(), command_height);
    display.resize(
        display.x(), sy + height + command_height,
        display.w(), h() - height - command_height);
    this->redraw();
}


void
LogView::resize(int x, int y, int w, int h)
{
    Fl_Group::resize(x, y, w, h);
    // Re-wrap the status line.
    set_status(unwrapped.c_str(), unwrapped_style.c_str());
}

void
LogView::append_log(const char *msg, const char *style)
{
    ASSERT(strlen(msg) == strlen(style));
    style_buffer.insert(0, style);
    buffer.insert(0, msg);
    // This won't work well with UTF8... on the other hand, chopping a UTF8
    // char in half will just make it not display, which is no big deal.
    if (buffer.length() > this->max_bytes) {
        buffer.remove(max_bytes, buffer.length());
        style_buffer.remove(max_bytes, style_buffer.length());
    }
}


void
LogView::clear_logs()
{
    buffer.remove(0, buffer.length());
    style_buffer.remove(0, style_buffer.length());
}


// YES, it's yet another text wrapping function in C++!
static void
wrap_text(const char *ctext, const char *cstyle, double wrap_width,
    string &wrapped, string &wrapped_style)
{
    static const char *split = " || ";
    string text(ctext), style(cstyle);
    size_t pos = 0;
    size_t line_start = 0;

    // TODO For some reason when using the real logview (not test_logview), it
    // wraps a little late.  I'm too lazy to figure out what the problem is at
    // the moment, so a hack will do.
    wrap_width -= 15;

    // DEBUG("WRAP: 0123456789012345678901234567890123456789");
    // DEBUG("WRAP: " << text << " to " << wrap_width);
    // Measure each substr until it crosses the width, then use the previous
    // break point.
    while (pos < text.length()) {
        size_t i = text.find(split, pos);
        if (i == string::npos)
            i = text.length();
        double width = fl_width(text.c_str() + line_start, i - line_start);
        // DEBUG("width: " << width << " index " << line_start << ", "
        //     << pos << ", " << i);
        if (line_start == pos || width < wrap_width) {
            pos = i + strlen(split);
        } else {
            // DEBUG("line: " << text.substr(line_start, pos));
            size_t len = pos - strlen(split) - line_start;
            wrapped += text.substr(line_start, len) + '\n';
            wrapped_style += style.substr(line_start, len) + 'A';
            line_start = pos;
        }
    }
    if (line_start < pos) {
        wrapped += text.substr(line_start, pos);
        wrapped_style += style.substr(line_start, pos);
    }
}


void
LogView::set_status(const char *text, const char *style)
{
    ASSERT(strlen(text) == strlen(style));
    this->unwrapped = text;
    this->unwrapped_style = style;

    string wrapped, wrapped_style;
    wrap_text(text, style, this->w(), wrapped, wrapped_style);
    int lines = std::count(wrapped.begin(), wrapped.end(), '\n');
    set_status_height(status_height * (lines + 1));
    int end = status_buffer.length();
    status_buffer.replace(0, end, wrapped.c_str());
    status_style_buffer.replace(0, end, wrapped_style.c_str());
}


static void
select_word(Fl_Text_Buffer &buf, int pos, int *set_begin, int *set_end)
{
    // look for { and }, otherwise select to nearest whitespace
    // TODO doesn't match nested braces

    int open_brace = -1;
    int begin = pos;
    while (begin >= 0) {
        if (buf.byte_at(begin) == '{') {
            open_brace = begin;
            break;
        } else if (buf.byte_at(begin) == '}') {
            break;
        } else {
            begin--;
        }
    }

    int close_brace = -1;
    int end = pos;
    while (end < buf.length()) {
        if (buf.byte_at(end) == '}') {
            close_brace = end;
            break;
        } else if (buf.byte_at(end) == '{') {
            break;
        } else {
            end++;
        }
    }

    if (close_brace != -1 && open_brace != -1) {
        *set_begin = open_brace;
        *set_end = close_brace + 1;
        return;
    }
    begin = end = pos;

    while (begin && !isspace(buf.byte_at(begin)))
        begin--;
    if (isspace(buf.byte_at(begin)))
        begin++;

    while (end < buf.length() && !isspace(buf.byte_at(end)))
        end++;
    if (isspace(buf.byte_at(end)))
        end--;

    *set_begin = begin;
    *set_end = end + 1;
}


int
LogView::handle(int evt)
{
    if (evt == FL_PUSH && Fl::event_clicks() >= 1) {
        Fl_Text_Buffer *buf;
        TextDisplay *display = NULL;
        if (Fl::event_inside(&this->display)) {
            display = &this->display;
            buf = &this->buffer;
        } else if (Fl::event_inside(&this->status)) {
            display = &this->status;
            buf = &this->status_buffer;
        } else {
            display = NULL;
            buf = NULL;
        }
        if (display) {
            int pos = display->xy_to_position(Fl::event_x(), Fl::event_y());
            int begin, end;
            select_word(*buf, pos, &begin, &end);
            buf->select(begin, end);
            char *word = buf->selection_text();
            this->msg_callback(cb_click, word);
            free(word);
        }
        return 0;
    }
    return Fl_Group::handle(evt);
}


void
LogView::command_cb(Fl_Widget *_w, void *vp)
{
    LogView *self = static_cast<LogView *>(vp);
    self->msg_callback(cb_command, self->command.value());
}


LogViewWindow::LogViewWindow(int x, int y, int w, int h, const char *label,
        MsgCallback cb, int max_bytes) :
    // TODO for some reason fltk on linux subtracts 100 from width and height
    Fl_Double_Window(0, 0, w + 100, h + 100, label),
    view(0, 0, w + 100, h + 100, cb, max_bytes)
{
    this->resizable(this);
    this->position(x, y);
    Fl::dnd_text_ops(false); // turn off annoying text drag and drop
    Fl::visible_focus(false);
}
