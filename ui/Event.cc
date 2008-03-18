#include <FL/Fl_draw.H>

#include "f_util.h"

#include "Event.h"


// EventView ///////

void
EventView::draw()
{
    Fl_Box::draw();
}


// These elements get drawn in a separate pass so they are on top of the ruler.
void
EventView::draw_upper_layer()
{
    if (this->model->align_to_top) {
        fl_color(FL_RED);
        fl_line_style(FL_SOLID, 1);
        fl_line(x(), y(), x()+w(), y());

        // TODO
        // if the text is too long it gets blue-blocked off
        fl_font(fl_font(), 12);
        int text_h = fl_height() - fl_descent();
        int textpos = y() + text_h;
        // TODO set according to style
        fl_color(FL_BLACK);
        fl_draw(this->model->text.c_str(), x(), textpos);
    } else {
        // TODO draw line at bottom, align text on top of it
    }
}
