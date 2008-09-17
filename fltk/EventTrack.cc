#include <math.h>
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
    if (this->zoom.factor == zoom.factor) {
        // TODO this would be for scrolling, but it's not implemented yet
        // this->shift = this->zoom.to_pixels(zoom.offset - this->zoom.offset);
        this->damage(FL_DAMAGE_SCROLL);
    } else {
        this->damage(FL_DAMAGE_ALL);
    }
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
    // Doesn't use finalize_callbacks because that finalizes the ruler,
    // which set_config is going to do.
    finalizer((void *) this->config.find_events);
    finalizer((void *) this->config.render.find_samples);
    this->overlay_ruler.set_config(*track.ruler, finalizer, start, end);
    if (this->config.bg_color != track.track->bg_color) {
        this->bg_box.color(color_to_fl(track.track->bg_color));
        this->redraw();
    }
    this->config = *track.track;
    // Use ruler's damage range since both have to be updated at the same time.
    this->damage(OverlayRuler::DAMAGE_RANGE);
}


void
EventTrackView::finalize_callbacks(FinalizeCallback finalizer)
{
    finalizer((void *) this->config.find_events);
    finalizer((void *) this->config.render.find_samples);
    this->overlay_ruler.finalize_callbacks(finalizer);
}


void
EventTrackView::draw()
{
    Rect draw_area = rect(this);
    draw_area.h--; // tiles make a 1 pixel lower border
    // DEBUG("track damage " << show_damage(damage()));
    // The damage is a little hacky, because the overlay_ruler overlaps this
    // widget.  So only having child damage means the overlay will have
    // DAMAGE_RANGE and this widget also needs to redraw in that range.
    if (this->damage() & ~FL_DAMAGE_CHILD) {
        // Leave draw_area with the full size.
        // TODO handle scroll specially
    } else {
        // DEBUG("track intersect: " << draw_area.y << "--"
        //     << overlay_ruler.draw_area.b() << " with "
        //     << damaged_area.y << "--" << overlay_ruler.damaged_area.b());
        draw_area = draw_area.intersect(this->overlay_ruler.damaged_area);
    }
    // The remaining drawing routines will optimize based on the clip rect.
    // When overlay_ruler.draw() is called it will redundantly clip again on
    // damage_range, but that's ok because it needs the clip when called from
    // RulerTrackView::draw().
    ClipArea clip_area(draw_area);
    this->draw_area();
}


/*
static void
show_found_events(TrackPos start, TrackPos end,
        TrackPos *event_pos, Event *events, int count)
{
    printf("%.2f-%.2f: %d events:", start.scale(1), end.scale(1), count);
    for (int i = 0; i < count; i++) {
        printf(" (%.2f %s)", event_pos[i].scale(1), events[i].text);
    }
    printf("\n");
}
*/


void
EventTrackView::draw_area()
{
    Rect clip = clip_rect(rect(this));
    if (clip.w == 0 || clip.h == 0)
        return;

    // TODO It might be cleaner to eliminate bg_box and just call fl_rectf
    // and fl_draw_box myself.
    // fl_color(bg_box.color());
    // fl_rectf(clip.x, clip.y, clip.w, clip.h);
    // DEBUG("DRAW BOX " << clip.y << "--" << clip.b());
    this->draw_child(this->bg_box);

    // Code copy and pasted from OverlayRuler::draw_marklists.
    TrackPos start = this->zoom.to_trackpos(clip.y - this->y());
    TrackPos end = start + this->zoom.to_trackpos(clip.h);
    start = start + this->zoom.offset;
    // Go back far enough to get an event whose text would overlap the damaged
    // area.  This won't work so well if I have different font sizes...
    // but I think I don't have to do that.
    fl_font(fl_font(), Config::font_size::event);
    start = std::max(TrackPos(0), start - this->zoom.to_trackpos(fl_height()));
    end = end + this->zoom.offset;
    // DEBUG("TRACK CLIP: " << start << "--" << end << ", "
    //         << clip.y << "--" << clip.b());

    // Draw event boxes.
    Event *events;
    TrackPos *event_pos;
    int count = this->config.find_events(&start, &end, &event_pos, &events);
    // show_found_events(start, end, event_pos, events, count);
    for (int i = 0; i < count; i++) {
        const Event &event = events[i];
        const TrackPos &pos = event_pos[i];
        int offset = y() + this->zoom.to_pixels(pos - this->zoom.offset);
        int height = this->zoom.to_pixels(event.duration);
        fl_color(color_to_fl(event.color));
        fl_rectf(this->x() + 1, offset, this->w() - 2, height);
    }

    if (config.render.style != RenderConfig::render_none)
        this->draw_samples(start, end);

    if (damage() & ~FL_DAMAGE_CHILD) {
        this->draw_child(this->overlay_ruler);
    } else {
        this->update_child(this->overlay_ruler);
    }

    int previous_bottom = -999;
    for (int i = 0; i < count; i++) {
        const Event &event = events[i];
        const TrackPos &pos = event_pos[i];
        int offset = y() + this->zoom.to_pixels(pos - this->zoom.offset);
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

void
EventTrackView::draw_samples(TrackPos start, TrackPos end)
{
    TrackPos *sample_pos;
    double *samples;
    // DEBUG("CALL find samples " << &config.render.find_samples);
    int sample_count = this->config.render.find_samples(&start, &end,
        &sample_pos, &samples);
    // TODO alpha not supported, I'd need a non-portable drawing routine for it.
    fl_color(color_to_fl(this->config.render.color));
    if (config.render.style == RenderConfig::render_line)
        fl_line_style(FL_SOLID | FL_CAP_ROUND, 2);
    else
        fl_line_style(FL_SOLID | FL_CAP_ROUND, 0);
    // Account for both the 1 pixel track border and the width of the line.
    int min_x = x() + 2;
    int max_x = x() + w() - 2;
    for (int i = 0; i < sample_count; i++) {
        int offset = y() + this->zoom.to_pixels(sample_pos[i] - zoom.offset);
        int next_offset;
        double next_sample;
        if (i+1 < sample_count) {
            next_offset = y() + zoom.to_pixels(sample_pos[i+1] - zoom.offset);
            next_sample = samples[i+1];
        } else {
            next_offset = y() + h();
            next_sample = samples[i];
        }
        int xpos = floor(::scale(double(min_x), double(max_x),
            ::clamp(0.0, 1.0, samples[i])));
        int next_xpos = floor(::scale(double(min_x), double(max_x),
            ::clamp(0.0, 1.0, next_sample)));

        // DEBUG("p0 (" << xpos << ", " << offset << "), p1 ("
        //         << next_xpos << ", " << next_offset << ")");

        switch (config.render.style) {
        case RenderConfig::render_line:
            fl_line(xpos, offset, next_xpos, next_offset);
            break;
        case RenderConfig::render_filled:
            fl_polygon(xpos, offset, next_xpos, next_offset,
                    min_x, next_offset, min_x, offset);
            break;
        case RenderConfig::render_none:
            break;
        }
    }
}


int
EventTrackView::draw_upper_layer(int offset, const Event &event,
        int previous_bottom)
{
    // So the overlap stuff is actually pretty tricky.  I want to not display
    // text when it would overlap with the previous text, so it doesn't get
    // into an unreadable jumble.  So I hide the text if this event overlaps
    // the previous one.  This is simple, but will hide all text when it could
    // actually display every other one.  It also means that whenever I am
    // redrawing, the callback needs to give me the previous event, so I know
    // if the current one overlaps.
    // I can display as much text as possible by only hiding text if it
    // overlaps the previously displayed text.  However, to make drawing
    // consistent, I need to be consistent about where the previously displayed
    // text is, and in common situations this can require going all the way
    // back to the first event since each event can depend on its predecessor.
    // There's probably some way to get around this by caching whether an event
    // has displayed text.  I think I might want to do this, but should
    // wait until I am caching events from the callback in general.
    fl_font(fl_font(), Config::font_size::event);
    bool overlapped = offset < previous_bottom - 6; // a little overlap is ok
    int textpos = offset + (fl_height() - fl_descent());
    if (event.align_to_bottom) {
        // TODO draw line at bottom, align text on top of it
        // bottom = offset + fl_height(); // or something
    } else {
        // If it overlaps with text above it, don't display the text, and draw
        // the mark line in abbreviation_color.
        if (overlapped) {
            // TODO draw the line under the overlapping text, or just a tick
            // on the sides, so it doesn't make the text hard to read
            fl_color(color_to_fl(Config::abbreviation_color));
            // bottom = previous_bottom;
        } else {
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
        }
        fl_line_style(FL_SOLID, 1);
        fl_line(x() + 1, offset, x()+w() - 2, offset);
    }

    return offset + fl_height();
}
