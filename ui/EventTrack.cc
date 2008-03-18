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


void
EventTrackModel::insert_event(TrackPos pos, const EventModel &event)
{
        this->events[pos] = event;
        for (int i = 0; i < this->views.size(); i++)
            views[i]->insert_event(pos, event);
}


void
EventTrackModel::remove_event(TrackPos pos)
{
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

    // TODO: for just a size change, only pass the added or removed bit
    // TODO: also for a move, a can technically just move the events without
    // checking for new or old ones, not sure if that optimization is worth it.
    this->create_widgets();
}


void
EventTrackView::set_zoom(const ZoomInfo &zoom)
{
    // TODO: if just the offset changed and the move is < h(), I can use the
    // Fl_Scroll blit to do it quickly.
    // DEBUG("zoom " << this->zoom << " to " << zoom);
    this->zoom = zoom;
    this->create_widgets();
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
    this->create_widgets(pos, event.duration);
    this->do_callback();
}


void
EventTrackView::remove_event(TrackPos pos)
{
    this->create_widgets(); // TODO find event duration
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

    Rect draw_area = rect(this);
    draw_area.h--; // tiles make a 1 pixel lower border
    ClipArea clip_area(draw_area);
    TrackView::draw();
    for (int i = 0; i < this->events(); i++) {
        this->event_at(i)->draw_upper_layer();
    }
}


// Create and position EventView widgets depending on the current zoom.
// This does a complete refresh of the events from the model and a complete
// redraw.
// TODO use start and duration
// TODO instead of doing this with widgets can I do it all in draw?  then I
// can't put event drawing code in its own widget and events don't get
// delivired.  But it seems the neither is a big deal.
void
EventTrackView::create_widgets(TrackPos start, TrackPos duration)
{
    // Record the widgets currently onscreen, so I can delete the unused ones.
    std::set<EventView *> cur_displayed;
    // later do a binary search or something?
    // = std::lower_bound(model->events.begin(), model->events.end(),
    //      this->zoom.to_pixels(event->first)
    //          + this->zoom.to_pixels(event->second.duration) <= 0
    EventView *last_view = 0;
    // DEBUG("create_widgets at " << zoom);
    int i=0;
    for (EventTrackModel::Events::iterator event = model->events.begin();
        event != model->events.end();
        ++event, ++i)
    {
        int offset = y() + this->zoom.to_pixels(event->first);
        int height = this->zoom.to_pixels(zoom.offset + event->second.duration);
        // DEBUG(i << ": (" << event->first << ", " << event->second.duration
        //         << "), (" << (offset-y()) << ", " << height << ")");
        if (offset + height <= y())
            continue;
        else if (offset >= y() + h())
            break;
        EventView *v = this->displayed_events[&event->second];
        if (!v) {
            v = new EventView(&event->second);
            // DEBUG("new view: " << event->first);
            // Keep the children in their trackpos order.  The main thing is
            // to keep bg_box at 0 and overlay_ruler at the end, but it's
            // easier to read debugging dumps if they're all in order.
            // This might get messed up if I do a bsearch above.
            if (last_view)
                this->insert(*v, this->find(last_view)+1);
            else
                this->insert(*v, 1);
        } else {
            // DEBUG("using existing view " << v << ": " << event->first);
        }
        cur_displayed.insert(v);
        // DEBUG(i << ": resize " << rect(v) << " -> "
        //         << Rect(x()+1, offset, w()-2, height));
        // Give 2 pixels for the border.
        v->resize(x()+1, offset, w()-2, height);
        last_view = v;
    }
    this->displayed_events.clear();
    for (int i = 0; i < this->events(); ) {
        EventView *v = this->event_at(i);
        if (cur_displayed.find(v) != cur_displayed.end()) {
            this->displayed_events[v->model] = v;
            i++;
        } else {
            // DEBUG("didn't find " << v);
            this->remove(v);
            delete v; // Fl_Group::remove doesn't delete
        }
    }
    this->redraw();
}
