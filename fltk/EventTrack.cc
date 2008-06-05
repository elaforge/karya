#include "config.h"
#include "util.h"
#include "alpha_draw.h"
#include "SeqInput.h"
#include "EventTrack.h"

// #define DEBUG(X) ;

// EventTrackView ///////

EventTrackView::EventTrackView(const EventTrackConfig &config,
            const RulerConfig &ruler_config) :
    TrackView("events"),
    config(config),
    title_input(0),
    bg_box(0, 0, 1, 1),
    overlay_ruler(ruler_config)
{
    // this->resizable(0); // don't resize children
    end(); // make sure no one else falls in
    this->add(bg_box);
    this->add(this->overlay_ruler);
    // create event widgets
    bg_box.box(FL_THIN_DOWN_BOX);
    bg_box.color(color_to_fl(config.bg_color));

    this->title_input = new SeqInput(0, 0, 1, 1, true);
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
    return std::max(this->config.time_end, this->overlay_ruler.time_end());
}


void
EventTrackView::update(const Tracklike &track, FinalizeCallback finalizer,
        TrackPos start, TrackPos end)
{
    ASSERT(track.track && track.ruler);
    finalizer((void *) this->config.find_events);
    this->overlay_ruler.set_config(*track.ruler, finalizer, start, end);
    if (this->config.bg_color != track.track->bg_color) {
        this->bg_box.color(color_to_fl(track.track->bg_color));
        this->redraw();
    }
    this->config = *track.track;
    // TODO should have a damage scheme like with ruler
    // if (start == -1 && end == -1) then update everything
    this->redraw();
}


void
EventTrackView::finalize_callbacks(FinalizeCallback finalizer)
{
    finalizer((void *) this->config.find_events);
    this->overlay_ruler.finalize_callbacks(finalizer);
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

    TrackPos start = this->zoom.offset;
    TrackPos end = this->zoom.to_trackpos(area.h) + this->zoom.offset;
    Event *events;
    TrackPos *event_pos;
    int count = this->config.find_events(&start, &end, &event_pos, &events);

    for (int i = 0; i < count; i++) {
        const Event &event = events[i];
        const TrackPos &pos = event_pos[i];
        int offset = y() + this->zoom.to_pixels(pos - this->zoom.offset);
        int height = this->zoom.to_pixels(event.duration);
        fl_color(color_to_fl(event.color));
        fl_rectf(this->x() + 1, offset, this->w() - 2, height);
    }

    this->draw_child(this->overlay_ruler);

    int previous_bottom = -999;
    for (int i = 0; i < count; i++) {
        const Event &event = events[i];
        const TrackPos &pos = event_pos[i];
        int offset = y() + this->zoom.to_pixels(pos - this->zoom.offset);
        int height = this->zoom.to_pixels(event.duration);
        previous_bottom = this->draw_upper_layer(
            offset, event, previous_bottom);
    }
    if (count) {
        for (int i = 0; i < count; i++)
            free(events[i].text);
        free(events);
        free(event_pos);
    }
}


int
EventTrackView::draw_upper_layer(int offset, const Event &event,
        int previous_bottom)
{
    fl_font(fl_font(), Config::font_size::event);
    bool overlapped = offset < previous_bottom;
    int textpos = offset + (fl_height() - fl_descent());
    int bottom;
    if (event.align_to_bottom) {
        // TODO draw line at bottom, align text on top of it
        bottom = offset + fl_height(); // or something
    } else {
        // If it overlaps with text above it, don't display the text, and draw
        // the mark line in abbreviation_color.
        if (!overlapped) {
            // TODO set according to style
            fl_color(FL_BLACK);
            fl_draw(event.text, x() + 2, textpos);

            // If the text is too long it gets blue-blocked off.
            Point text_size(0, 0);
            fl_measure(event.text, text_size.x, text_size.y, false);
            if (text_size.x > w() - 1) {
                fl_color(color_to_fl(Config::abbreviation_color));
                fl_rectf(x()+w() - 2, offset, 2, fl_height());
            }

            fl_color(FL_RED);
            bottom = offset + fl_height();
        } else {
            // TODO draw the line under the overlapping text, or just a tick
            // on the sides, so it doesn't make the text hard to read
            fl_color(color_to_fl(Config::abbreviation_color));
            bottom = previous_bottom;
        }
        fl_line_style(FL_SOLID, 1);
        fl_line(x() + 1, offset, x()+w() - 2, offset);
    }

    return bottom;
}
