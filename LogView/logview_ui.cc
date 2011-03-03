#include <FL/Fl.H>
#include "logview_ui.h"
#include "util.h"


enum { default_font_size = 12 };

static Fl_Text_Display::Style_Table_Entry style_table[] = {
    { FL_BLACK, FL_HELVETICA, default_font_size }, // A - plain
    { FL_RED, FL_HELVETICA, default_font_size }, // B - warn msgs
    { FL_BLUE, FL_HELVETICA, default_font_size }, // C - clickable text
    { fl_rgb_color(0, 76, 0), FL_HELVETICA_BOLD,
            default_font_size }, // D - emphasis
    { FL_RED, FL_HELVETICA_BOLD, default_font_size }, // E - divider
    { fl_rgb_color(0, 76, 0), FL_HELVETICA, default_font_size }, //F - func name
    { FL_BLACK, FL_HELVETICA_ITALIC, default_font_size } // G - filename
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


enum {
    status_height = 16,
    command_height = 20
};

LogView::LogView(int X, int Y, int W, int H, MsgCallback cb, int max_bytes) :
    Fl_Group(X, Y, W, H),
    msg_callback(cb),
    status(X, Y, W, status_height),
    command(X, Y + status_height, W, command_height),
    display(X, Y + status_height + command_height,
            W, H - status_height - command_height),
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


void
LogView::set_status(const char *text, const char *style)
{
    ASSERT(strlen(text) == strlen(style));

    int end = status_buffer.length();
    status_style_buffer.replace(0, end, style);
    status_buffer.replace(0, end, text);
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


LogViewWindow::LogViewWindow(int X, int Y, int W, int H, MsgCallback cb,
        int max_bytes) :
    Fl_Double_Window(0, 0, W, H), view(0, 0, W, H, cb, max_bytes)
{
    this->resizable(this);
}
