#include <set>

#include "config.h"
#include "util.h"
#include "alpha_draw.h"
#include "SeqInput.h"
#include "EventTrack.h"

// #define DEBUG(X) ;

// EventTrackModel ///////

EventTrackModel::~EventTrackModel()
{
    // Any remaining views should have kept this model alive.
    ASSERT(this->views.size() == 0);
}


bool
EventTrackModel::insert_event(TrackPos pos, const EventModel &event)
{
    EventTrackModel::Events::const_iterator evt
        = this->events.lower_bound(pos);
    // DEBUG("insert event at " << pos << " dur " << event.duration);
    if (evt != events.begin()) {
        --evt;
        if (evt->first + evt->second.duration > pos)
            return false; // prev event overlaps
        ++evt;
    }
    if (evt != events.end()) {
        if (evt->first == pos)
            ++evt;
        if (pos + event.duration > evt->first)
            return false; // I overlap with next event
    }

    this->events[pos] = event;
    for (int i = 0; i < this->views.size(); i++)
        views[i]->insert_event(pos, event);
    return true;
}


bool
EventTrackModel::remove_event(TrackPos pos)
{
    EventTrackModel::Events::const_iterator evt = this->events.find(pos);
    if (evt == events.end())
        return false;
    this->events.erase(pos);
    for (int i = 0; i < this->views.size(); i++)
        views[i]->remove_event(pos);
}


// EventTrackView ///////

EventTrackView::EventTrackView(boost::shared_ptr<EventTrackModel> model,
        boost::shared_ptr<RulerTrackModel> ruler_model) :
    TrackView("events"),
    model(model),
    title_input(0),
    bg_box(0, 0, 1, 1),
    overlay_ruler(ruler_model)
{
    // this->resizable(0); // don't resize children
    end(); // make sure no one else falls in
    this->add(bg_box);
    this->add(this->overlay_ruler);
    // create event widgets
    bg_box.box(FL_THIN_DOWN_BOX);
    bg_box.color(color_to_fl(model->bg_color));

    this->title_input = new SeqInput(0, 0, 1, 1);
    model->add_view(this);
}


EventTrackView::~EventTrackView()
{
    model->remove_view(this);
}


void
EventTrackView::resize(int x, int y, int w, int h)
{
    // DEBUG("resize " << rect(this) << " -> " << Rect(x, y, w, h));
    // Don't call Fl_Group::resize because I just did the sizes myself.
    Fl_Widget::resize(x, y, w, h);
    this->overlay_ruler.resize(x, y, w, h);
    this->bg_box.resize(x, y, w, h);
}


void
EventTrackView::set_zoom(const ZoomInfo &zoom)
{
    // TODO: if just the offset changed and the move is < h(), I can use the
    // Fl_Scroll blit to do it quickly.
    // DEBUG("zoom " << this->zoom << " to " << zoom);
    this->zoom = zoom;
    this->overlay_ruler.set_zoom(zoom);
}


TrackPos
EventTrackView::time_end() const
{
    int i = 0;
    TrackPos end(0);
    for (EventTrackModel::Events::const_iterator event = model->events.begin();
        event != model->events.end();
        ++event)
    {
        // DEBUG("e"<< i++ << ": " << event->first << " + "
        //     << event->second.duration);
        end = std::max(end, event->first + event->second.duration);
    }
    return std::max(end, this->overlay_ruler.time_end());
}


void
EventTrackView::insert_event(TrackPos pos, const EventModel &event)
{
    this->redraw();
    this->do_callback();
}


void
EventTrackView::remove_event(TrackPos pos)
{
    this->redraw();
    this->do_callback();
}


void
EventTrackView::draw()
{
    /*
    uchar d = this->damage();

    if (d & FL_DAMAGE_ALL) {
        // draw_area(rect(this));
    } else {
        if (d & FL_DAMAGE_SCROLL) {
            // fl_scroll(...)
            // draw_area(...) // revealed areas
        }
    }
    */

    Rect area = rect(this);
    area.h--; // tiles make a 1 pixel lower border
    ClipArea clip_area(area);
    this->draw_area(area);
}


// Draw within 'area', which should be clipped.
// TODO don't pass area and just call clip_rect(this)?
void
EventTrackView::draw_area(Rect area)
{
    this->draw_child(this->bg_box);

    // later do a binary search or something?
    // = std::lower_bound(model->events.begin(), model->events.end(),
    //      this->zoom.to_pixels(event->first)
    //          + this->zoom.to_pixels(event->second.duration) <= 0

    // TODO fix repetition here

    for (EventTrackModel::Events::iterator event = model->events.begin();
        event != model->events.end();
        ++event)
    {
        int offset = y() + this->zoom.to_pixels(event->first);
        int height = this->zoom.to_pixels(zoom.offset + event->second.duration);
        // It's < not <= so that 0 height events on area.y still get drawn.
        if (offset + height < area.y) {
            // DEBUG("skip " << offset << " + " << height << " > " << area.y);
            continue;
        } else if (offset >= area.b())
            break;
        fl_color(color_to_fl(event->second.color));
        fl_rectf(this->x() + 1, offset, this->w() - 2, height);
    }

    this->draw_child(this->overlay_ruler);

    for (EventTrackModel::Events::iterator event = model->events.begin();
        event != model->events.end();
        ++event)
    {
        int offset = y() + this->zoom.to_pixels(event->first);
        int height = this->zoom.to_pixels(zoom.offset + event->second.duration);
        if (offset + height < area.y)
            continue;
        else if (offset >= area.b())
            break;
        this->draw_upper_layer(offset, event->second);
    }
}


void
EventTrackView::draw_upper_layer(int offset, const EventModel &event)
{
    if (event.align_to_bottom) {
        // TODO draw line at bottom, align text on top of it
    } else {
        fl_color(FL_RED);
        fl_line_style(FL_SOLID, 1);
        fl_line(x() + 1, offset, x()+w() - 2, offset);

        // TODO
        // if the text is too long it gets blue-blocked off
        fl_font(fl_font(), 12);
        int text_h = fl_height() - fl_descent();
        int textpos = offset + text_h;
        // TODO set according to style
        fl_color(FL_BLACK);
        fl_draw(event.text.c_str(), x() + 2, textpos);
    }
}
