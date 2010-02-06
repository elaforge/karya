#include <math.h>
#include "config.h"
#include "util.h"
#include "alpha_draw.h"

#include "SeqInput.h"
#include "EventTrack.h"


// #define DEBUG(X) ;

// Hack for debugging.
#define SHOW_RANGE(r) (r).y << "--" << (r).b()

// The color of events at a non-zero rank is scaled by this.
static const double rank_brightness = 1.5;
// The color of events with a negative duration is scaled by this.
static const double negative_duration_brightness = .85;

// EventTrackView ///////

EventTrackView::EventTrackView(const EventTrackConfig &config,
            const RulerConfig &ruler_config) :
    TrackView("events"),
    config(config), last_offset(0), brightness(1), bg_color(config.bg_color),
    title_input(NULL),
    bg_box(0, 0, 1, 1),
    overlay_ruler(ruler_config)
{
    // this->resizable(0); // don't resize children
    end(); // make sure no one else falls in
    this->add(bg_box);
    this->add(this->overlay_ruler);
    // create event widgets
    bg_box.box(FL_THIN_DOWN_BOX);
    bg_box.color(color_to_fl(config.bg_color.brightness(this->brightness)));

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
EventTrackView::set_zoom(const ZoomInfo &new_zoom)
{
    // DEBUG("zoom " << this->zoom << " to " << new_zoom);
    if (new_zoom == this->zoom)
        return;
    if (this->zoom.factor == new_zoom.factor)
        this->damage(FL_DAMAGE_SCROLL);
    else
        this->damage(FL_DAMAGE_ALL);
    this->zoom = new_zoom;
    this->overlay_ruler.set_zoom(new_zoom);
}


void
EventTrackView::set_event_brightness(double d)
{
    this->brightness = d;
    this->bg_box.color(
            color_to_fl(this->bg_color.brightness(this->brightness)));
    this->redraw();
}


ScoreTime
EventTrackView::time_end() const
{
    return std::max(this->config.time_end, this->overlay_ruler.time_end());
}


void
EventTrackView::update(const Tracklike &track, FinalizeCallback finalizer,
        ScoreTime start, ScoreTime end)
{
    ASSERT(track.track && track.ruler);
    // Doesn't use finalize_callbacks because that finalizes the ruler,
    // which set_config is going to do.
    finalizer((void *) this->config.find_events);
    finalizer((void *) this->config.render.find_samples);
    this->overlay_ruler.set_config(*track.ruler, finalizer, start, end);
    if (this->config.bg_color != track.track->bg_color) {
        this->bg_color = track.track->bg_color;
        this->set_event_brightness(this->brightness);
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


// I redraw the scroll revealed area separately, so pass a dummy to fl_scroll.
static void dummy_scroll_draw(void *, int, int, int, int) {}

void
EventTrackView::draw()
{
    Rect draw_area = rect(this);

    // DEBUG("event track damage " << show_damage(damage()));
    if (this->damage() == FL_DAMAGE_SCROLL) {
        // Avoid the one pixel upper and lower bezels;
        draw_area.x++; draw_area.w -= 2;
        draw_area.y++; draw_area.h -= 2;
        draw_area = clip_rect(draw_area);

        int scroll = zoom.to_pixels(zoom.offset) - zoom.to_pixels(last_offset);
        // DEBUG("scroll " << SHOW_RANGE(draw_area) << " " << -scroll);
        fl_scroll(draw_area.x, draw_area.y, draw_area.w, draw_area.h,
                0, -scroll, dummy_scroll_draw, NULL);
        ScoreTime shift_pos = std::max(
                zoom.offset - last_offset, last_offset - zoom.offset);
        if (scroll > 0) { // Contents moved up, bottom is damaged.
            ScoreTime bottom = zoom.offset + zoom.to_trackpos(draw_area.h);
            this->overlay_ruler.damage_range(bottom - shift_pos, bottom);
            draw_area.y = draw_area.b() - scroll;
            draw_area.h = scroll;
        } else if (scroll < 0) { // Contents moved down, top is damaged.
            this->overlay_ruler.damage_range(
                    zoom.offset, zoom.offset + shift_pos);
            draw_area.h = -scroll;
        } else {
            draw_area.h = 0;
        }
    } else if (this->damage() == FL_DAMAGE_CHILD) {
        // Only CHILD damage means a selection was set.  But since I overlap
        // with the child, I have to draw too.
        // DEBUG("pre intersect " << SHOW_RANGE(draw_area));
        draw_area = draw_area.intersect(this->overlay_ruler.damaged_area);
        // DEBUG("post intersect " << SHOW_RANGE(draw_area));
    } else {
        // I could technically handle SCROLL | CHILD, but I'd have to tweak
        // the ruler's damaged_area to account for the scroll and that's too
        // much bother right now.
        this->damage(FL_DAMAGE_ALL);
    }
    if (draw_area.w == 0 || draw_area.h == 0)
        return;

    // DEBUG("draw area " << draw_area << " " << SHOW_RANGE(draw_area));
    // When overlay_ruler.draw() is called it will redundantly clip again on
    // damage_range, but that's ok because it needs the clip when called from
    // RulerTrackView::draw().
    ClipArea clip_area(draw_area);

    // TODO It might be cleaner to eliminate bg_box and just call fl_rectf
    // and fl_draw_box myself.  But this draws the all-mighty bevel too.
    this->draw_child(this->bg_box);
    Rect inside_bevel = rect(this);
    inside_bevel.x++; inside_bevel.w -= 2;
    inside_bevel.y++; inside_bevel.h -= 2;
    ClipArea clip_area2(inside_bevel);
    this->draw_area();
    overlay_ruler.damaged_area.w = overlay_ruler.damaged_area.h = 0;
    this->last_offset = this->zoom.offset;
}


/*
static void
show_found_events(ScoreTime start, ScoreTime end,
        ScoreTime *event_pos, Event *events, int count)
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
    int y = this->y() + 1; // top pixel is a bevel

    // Code copy and pasted from OverlayRuler::draw_marklists.
    ScoreTime start = this->zoom.to_trackpos(clip.y - y);
    ScoreTime end = start + this->zoom.to_trackpos(clip.h);
    start = start + this->zoom.offset;
    // Go back far enough to get an event whose text would overlap the damaged
    // area.  This won't work so well if I have different font sizes...
    // but I think I don't have to do that.
    start = std::max(ScoreTime(0), start - this->zoom.to_trackpos(fl_height()));
    end = end + this->zoom.offset;
    // DEBUG("TRACK CLIP: " << start << "--" << end << ", "
    //         << clip.y << "--" << clip.b());

    Event *events;
    ScoreTime *event_pos;
    int *ranks;
    int count = this->config.find_events(
            &start, &end, &event_pos, &events, &ranks);
    // show_found_events(start, end, event_pos, events, count);

    // Draw event boxes.  Rank >0 boxes are not drawn since I'd have to figure
    // out overlaps and they're meant to be used with control tracks anyway.
    for (int i = 0; i < count; i++) {
        if (ranks[i])
            continue;
        const Event &event = events[i];
        const ScoreTime &pos = event_pos[i];
        int offset = y + this->zoom.to_pixels(pos - this->zoom.offset);
        int height = this->zoom.to_pixels(event.duration);
        // Make sure events don't quite extend as far as they should, so it's
        // clearer which direction they're facing.
        if (height > 0)
            height -= 1;
        else if (height < 0)
            height += 1;
        int y0 = std::min(offset, offset + height);
        int y1 = std::max(offset, offset + height);

        Color c = event.color.brightness(this->brightness);
        if (event.duration < ScoreTime(0))
            c = c.brightness(negative_duration_brightness);
        fl_color(color_to_fl(c));
        fl_rectf(this->x() + 1, y0, this->w() - 2, y1-y0);
    }

    if (config.render.style != RenderConfig::render_none)
        this->draw_samples(start, end);

    // Draw the upper layer (event start line, text).
    // Don't use INT_MIN because it overflows too easily.
    Rect previous(x(), -9999, 0, 0);
    int ranked_bottom = -9999;
    int prev_offset = -9999;
    for (int i = 0; i < count; i++) {
        const Event &event = events[i];
        const ScoreTime &pos = event_pos[i];
        int rank = ranks[i];
        int offset = y + this->zoom.to_pixels(pos - this->zoom.offset);
        this->draw_upper_layer(offset, event, rank, &previous,
            &ranked_bottom, prev_offset);
        prev_offset = offset;
    }
    if (count) {
        for (int i = 0; i < count; i++) {
            if (events[i].text)
                free(events[i].text);
        }
        free(events);
        free(event_pos);
        free(ranks);
    }

    // The overlay ruler overlaps me entirely, so I'm sure it's damaged.
    if (damage() & FL_DAMAGE_ALL)
        this->draw_child(this->overlay_ruler);
    else
        this->update_child(this->overlay_ruler);
}

void
EventTrackView::draw_samples(ScoreTime start, ScoreTime end)
{
    ScoreTime *sample_pos;
    double *sample_vals;
    int sample_count = this->config.render.find_samples(&start, &end,
        &sample_pos, &sample_vals);
    int y = this->y() + 1; // avoid bevel
    // TODO alpha not supported, I'd need a non-portable drawing routine for it.
    fl_color(color_to_fl(this->config.render.color.brightness(
        this->brightness)));
    if (config.render.style == RenderConfig::render_line)
        fl_line_style(FL_SOLID | FL_CAP_ROUND, 2);
    else
        fl_line_style(FL_SOLID | FL_CAP_ROUND, 0);

    // Account for both the 1 pixel track border and the width of the line.
    int min_x = x() + 2;
    int max_x = x() + w() - 2;
    int prev_xpos = min_x;
    int prev_offset = 0;
    for (int i = 0; i < sample_count; i++) {
        int offset = y + this->zoom.to_pixels(sample_pos[i] - zoom.offset);
        if (offset <= prev_offset && i > 0)
            continue;
        // TODO later the max should come from the callback
        int xpos = floor(::scale(double(min_x), double(max_x),
            ::clamp(0.0, 1.0, sample_vals[i])));

        int next_offset;
        if (i+1 < sample_count)
            next_offset = y + zoom.to_pixels(sample_pos[i+1] - zoom.offset);
        else
            next_offset = y + h();
        switch (config.render.style) {
        case RenderConfig::render_line:
            fl_line(prev_xpos, offset, xpos, offset, xpos, next_offset);
            break;
        case RenderConfig::render_filled:
            // For some reason, on OS X at least, height 1 rects don't get
            // drawn.
            fl_rectf(min_x, offset, xpos - min_x, (next_offset - offset) + 1);
            break;
        case RenderConfig::render_none:
            break;
        default:
            DEBUG("unknown render style: " << config.render.style);
        }

        prev_xpos = xpos;
        prev_offset = offset;
    }
    fl_line_style(0);
}


void
EventTrackView::draw_upper_layer(int offset, const Event &event, int rank,
        Rect *previous, int *ranked_bottom, int prev_offset)
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

    // A little overlap is ok.
    const static int ok_overlap = 4;
    fl_font(fl_font(), Config::font_size::event);
    Rect text_rect(x() + 2, 0, 0, 0);
    if (event.is_negative()) {
        // Negative duration means text goes above the trigger line.
        text_rect.y = offset - (fl_height() - fl_descent()) - 2;
    } else {
        text_rect.y = offset;
    }
    if (event.text) {
        // params modified through ref args
        fl_measure(event.text, text_rect.w, text_rect.h, false);
    }
    if (rank && text_rect.y >= previous->b() - ok_overlap)
        previous->w = 0;

    // The various pixel tweaks in here were determined by zooming in and
    // squinting.
    bool draw_text = false;
    if (event.text) {
        if (rank) {
            text_rect.x = (x() + w()) - text_rect.w - 2;
            // Only display if I won't overlap text at the left or above.
            if (text_rect.x > previous->r() - 2
                    && text_rect.y >= *ranked_bottom - ok_overlap)
            {
                draw_text = true;
            }
        } else {
            if (text_rect.y >= previous->b() - ok_overlap)
                draw_text = true;
        }
    }

    // Draw trigger line.  Try not to draw two in the same place.
    if (offset != prev_offset) {
        Color trigger_c;
        if (draw_text || !event.text)
            trigger_c = Config::event_trigger_color;
        else
            trigger_c = Config::abbreviation_color;
        if (rank)
            trigger_c = trigger_c.brightness(rank_brightness);
        fl_color(color_to_fl(trigger_c));
        fl_line(x() + 1, offset, x()+w() - 2, offset);
    }

    if (draw_text) {
        if (rank)
            fl_color(color_to_fl(
                        Color(0, 0, 0).brightness(rank_brightness)));
        else
            fl_color(FL_BLACK);
        fl_font(Config::font, Config::font_size::event);
        fl_draw(event.text, text_rect.x, text_rect.b() - fl_descent());
        if (!rank) {
            if (text_rect.w > w() - 4) {
                // If the text is too long it gets truncated with a blue
                // block.
                fl_color(color_to_fl(Config::abbreviation_color));
                fl_rectf(x()+w() - 3, text_rect.y, 2, text_rect.h);
            } else if (event.text && *event.text
                    && isspace(event.text[strlen(event.text)-1]))
            {
                // Hightlight a trailing space.
                fl_color(FL_RED);
                fl_rectf(text_rect.r(), text_rect.y, 2, fl_height());
            }
        }
    }
    if (rank) {
        *ranked_bottom = text_rect.b();
    } else {
        *previous = text_rect;
    }
}
