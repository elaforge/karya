#include <FL/Fl.H>
#include "logview_ui.h"
#include "util.h"


enum { default_font_size = 12 };

static Fl_Text_Display::Style_Table_Entry style_table[] = {
    { FL_BLACK, FL_HELVETICA, default_font_size }, // A - plain
    { FL_RED, FL_HELVETICA, default_font_size }, // B - warn msgs
    { FL_BLUE, FL_HELVETICA, default_font_size }, // C - clickable text
    { FL_DARK_GREEN, FL_HELVETICA, default_font_size }, // D - emphasis
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
    status_height = 20,
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
    display.wrap_mode(true, 0);
    display.scrollbar_width(0);
    display.textfont(FL_HELVETICA);
    display.textsize(8);
    display.buffer(this->buffer);
    display.highlight_data(&style_buffer, style_table,
            sizeof(style_table) / sizeof(style_table[0]),
            'A', style_unfinished_cb, 0);
    buffer.add_modify_callback(style_update, &display);
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


static void
select_word(Fl_Text_Buffer &buf, int pos, int *begin, int *end)
{
    int b = pos;
    while (b && !isspace(buf.character(b)))
        b--;
    if (isspace(buf.character(b)))
        b++;

    int e = pos;
    while (e < buf.length() && !isspace(buf.character(e)))
        e++;
    if (isspace(buf.character(b)))
        e--;

    *begin = b;
    *end = e;
}


int
LogView::handle(int evt)
{
    if (evt == FL_PUSH && Fl::event_inside(&display) &&
            Fl::event_clicks() >= 1)
    {
        int pos = this->display.xy_to_position(Fl::event_x(), Fl::event_y());
        int begin, end;
        select_word(this->buffer, pos, &begin, &end);
        this->buffer.select(begin, end);
        char *word = this->buffer.selection_text();
        this->msg_callback(cb_click, word);
        free(word);
        return 0;
    } else {
        return Fl_Group::handle(evt);
    }
    // return Fl_Group::handle(evt);
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
