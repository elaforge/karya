#include <FL/Fl_draw.H>

#include "f_util.h"

#include "Event.h"


// EventModel //////

void
EventModel::insert_sub(TrackPos pos, const std::string &text)
{
    for (EventModel::SubEvents::iterator sub = this->subs.begin(); ; ++sub) {
        if (sub == subs.end() || sub->pos > pos) {
            this->subs.insert(sub, SubEvent(pos, text));
            break;
        } else if (sub->pos == pos) {
            sub->text = text;
            break;
        }
    }
    this->update();
}


void
EventModel::remove_sub(TrackPos pos)
{
    for (EventModel::SubEvents::iterator sub = this->subs.begin();
            sub != this->subs.end(); ++sub)
    {
        if (sub->pos == pos) {
            this->subs.erase(sub);
            break;
        }
    }
    this->update();
}


void
EventModel::update()
{
    if (this->view)
        this->view->update();
}

// EventView ///////

void
EventView::draw()
{
    Fl_Box::draw();
}


void
EventView::draw_upper_layer()
{
    fl_color(FL_BLACK);
    fl_font(fl_font(), 12);
    for (EventModel::SubEvents::const_iterator sub = this->model->subs.begin();
            sub != this->model->subs.end(); ++sub)
    {
        Rect r(x(), y() + zoom.to_pixels(sub->pos), w(), 1);
        fl_color(FL_RED);
        fl_rectf(r.x, r.y, r.w, r.h);

        // TODO
        // if the text is too long it gets blue-blocked off
        // draw text last in a separate pass so it's on top of the ruler

        int texth = fl_height() - fl_descent();
        // Try to not go below the event's bottom, but never protrude above the
        // event's top.
        int textpos = std::max(y() + texth - 1,
                std::min(y() + h(), r.y + texth));
        fl_color(FL_BLACK);
        fl_draw(sub->text.c_str(), r.x, textpos);
    }
}
