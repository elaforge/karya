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


// TrackSignal //////////

static bool
compare_control_sample(const TrackSignal::ControlSample &s1,
        const TrackSignal::ControlSample &s2)
{
    return s1.time < s2.time;
}

static bool
compare_pitch_sample(const TrackSignal::PitchSample &s1,
        const TrackSignal::PitchSample &s2)
{
    return s1.time < s2.time;
}

// A pitch signal will find the first sample of a given (from, to) series
// since only the first one gets the label.
int
TrackSignal::find_sample(ScoreTime start) const
{
    if (signal) {
        ControlSample sample(start, 0);
        ControlSample *found =
            std::lower_bound(signal, signal + length, sample,
                compare_control_sample);
        // Back up one to make sure I have the sample before start.
        if (found > signal)
            found--;
        return found - signal;
    } else if (pitch_signal) {
        PitchSample sample(start, 0, 0, 0);
        PitchSample *found = std::lower_bound(pitch_signal,
            pitch_signal + length, sample, compare_pitch_sample);
        // Back up one to make sure I have the sample before start.
        if (found > pitch_signal)
            found--;
        while (found > pitch_signal && found[-1].from == found[0].from
                && found[-1].to == found[0].to)
        {
            found--;
        }
        return found - pitch_signal;
    } else {
        // Render was set but there is no signal... so just say nothing was
        // found.
        return length;
    }
}


int
TrackSignal::time_at(const ZoomInfo &zoom, int i) const {
    ScoreTime at;
    if (signal)
        at = signal[i].time;
    else if (pitch_signal)
        at = pitch_signal[i].time;
    else
        ASSERT(0);
    return zoom.to_pixels((at - shift).divide(stretch) - zoom.offset);
}


// Get the val at the given index, normalized between 0--1.  If appropriate,
// return the val names below and above the val.  Otherwise, the pointers will
// be set to NULL.
//
// TODO normalize to a max val
double
TrackSignal::val_at(int i, const char **lower, const char **upper) const {
    *lower = *upper = NULL;
    if (signal)
        return signal[i].val;
    else if (!pitch_signal)
        ASSERT(0);

    const PitchSample &sample = pitch_signal[i];
    // If there's no range then no need to look up two, and .5 makes for a
    // better looking signal than 1.
    if (sample.from == sample.to) {
        const ValName *val = name_of(sample.from, true);
        if (val)
            *lower = *upper = val->name;
        return 0.5;
    }
    const ValName *low = name_of(std::min(sample.from, sample.to), true);
    const ValName *high = name_of(std::max(sample.from, sample.to), false);
    double low_val, high_val;
    if (low) {
        *lower = low->name;
        low_val = low->val;
    } else {
        low_val = std::min(sample.from, sample.to);
    }
    if (high) {
        *upper = high->name;
        high_val = high->val;
    } else {
        high_val = std::max(sample.from, sample.to);
    }

    double mid = double(::scale(sample.from, sample.to, sample.at));
    double result = ::normalize(low_val, high_val, mid);
    // DEBUG("(" << sample.from << ", " << sample.to << ", " << mid << "): ("
    //         << low_val << ", " << (*lower ? *lower : "null") << ") -- ("
    //         << high_val << ", " << (*upper ? *upper : "null") << ") at "
    //         << result);
    return result;
}


static bool
compare_val_name(const ValName &s1, const ValName &s2)
{
    return s1.val < s2.val;
}

// Find the closest ValName below or above val, depending on 'lower'.
const ValName *
TrackSignal::name_of(double val, bool lower) const {
    if (!val_names)
        return NULL;
    // I don't expect duplicate vals in the map, so upper_bound is not needed.
    const ValName *found = std::lower_bound(val_names,
        val_names + val_names_length, ValName(val, ""), compare_val_name);
    if (lower && found > val_names && found[0].val != val)
        found--;

    if (found == val_names + val_names_length)
        return NULL;
    else
        return found;
}


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
    this->overlay_ruler.set_config(*track.ruler, finalizer, start, end);
    if (this->config.bg_color != track.track->bg_color) {
        this->bg_color = track.track->bg_color;
        this->set_event_brightness(this->brightness);
    }

    TrackSignal tsig = this->config.track_signal;
    this->config = *track.track;
    // Copy the previous track signal over even though it might be out of date
    // now.  At the least I can't forget the pointers or there's a leak.
    this->config.track_signal = tsig;

    // Use ruler's damage range since both have to be updated at the same time.
    this->damage(OverlayRuler::DAMAGE_RANGE);
}

void
EventTrackView::set_track_signal(const TrackSignal &tsig)
{
    this->config.track_signal.free_signals();
    // Copy over the pointers, I'm taking ownership now.
    this->config.track_signal = tsig;
    this->redraw();
}


void
EventTrackView::finalize_callbacks(FinalizeCallback finalizer)
{
    finalizer((void *) this->config.find_events);
    this->config.track_signal.free_signals();
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

    this->draw_signal(clip.y, clip.b(), start);

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
EventTrackView::draw_signal(int min_y, int max_y, ScoreTime start)
{
    if (config.render.style == RenderConfig::render_none)
        return;

    const TrackSignal &tsig = config.track_signal;
    const int found = tsig.find_sample(start);
    if (found == tsig.length)
        return;

    const int y = this->y() + 1; // avoid bevel

    // TODO alpha not supported, I'd need a non-portable drawing routine for
    // it.
    Fl_Color signal_color =
        color_to_fl(config.render.color.brightness(brightness));
    Fl_Color text_color = color_to_fl(config.render.color.brightness(.5));

    // Account for both the 1 pixel track border and the width of the line.
    const int min_x = x() + 2;
    const int max_x = x() + w() - 2;
    int prev_xpos = min_x;
    int prev_offset = 0;
    const char *prev_lower = NULL;
    const char *prev_upper = NULL;

    // Set the font early to make sure subsequent fl_height() is correct.
    fl_font(Config::font, Config::font_size::pitch_signal);

    // Keep drawing fl_height() past max_y to make sure I get any text that
    // might stick up.
    for (int i = found, offset = 0;
            i < tsig.length && (offset = y + tsig.time_at(zoom, i))
                < max_y + fl_height();
            i++, prev_offset = offset)
    {
        // if (i == found)
        //     DEBUG("started at " << found << " offset " << (offset - min_y));
        // Skip coincident samples, or at least ones that are too close.
        if (offset <= prev_offset && i > found)
            continue;
        const char *lower, *upper;
        double val = tsig.val_at(i, &lower, &upper);
        int xpos = floor(::scale(double(min_x), double(max_x),
            ::clamp(0.0, 1.0, val)));

        int next_offset;
        if (i+1 < tsig.length)
            next_offset = y + tsig.time_at(zoom, i + 1);
        else
            next_offset = y + h();

        // Skip drawing things out of the clip area.
        // TODO avoid overlap with event text
        // TODO skip drawing text if they would overlap each other
        bool scale_changed = false;
        if (lower && upper && offset + fl_height() >= min_y) {
            // DEBUG((offset-min_y) << " text in range "
            //         << (void *) prev_lower << " = " << (void *) lower);
            if (lower != prev_lower || upper != prev_upper) {
                // DEBUG("drawing text");
                scale_changed = true;
                fl_line_style(FL_SOLID | FL_CAP_ROUND, 0);
                fl_color(text_color);

                Point text;
                if (lower != upper) {
                    fl_measure(lower, text.x, text.y);
                    fl_draw(lower, min_x, offset - 1);
                    fl_line(min_x, offset, min_x + text.x, offset);
                }
                fl_measure(upper, text.x, text.y);
                fl_draw(upper, max_x - text.x, offset - 1);
                fl_line(max_x - text.x, offset, max_x, offset);
            }
        }

        // TODO draw as one big line, I think this means text has to go in
        // a separate pass
        // TODO omit the jump from previous xpos if it's too small
        if (next_offset > min_y) {
            fl_color(signal_color);
            switch (config.render.style) {
            case RenderConfig::render_line:
                fl_line_style(FL_SOLID | FL_CAP_ROUND, 2);
                // If the xpos scale has changed, it doesn't make much sense to
                // connect with the previous sample, which implies the signal
                // actually made a jump.
                // And don't draw a jump from prev_xpos if it didn't exist.
                if (found == i || scale_changed)
                    fl_line(xpos, offset, xpos, next_offset);
                else
                    fl_line(prev_xpos, offset, xpos, offset, xpos, next_offset);
                break;
            case RenderConfig::render_filled:
                fl_line_style(FL_SOLID | FL_CAP_ROUND, 0);
                // For some reason, on OS X at least, height 1 rects don't get
                // drawn.
                fl_rectf(min_x, offset, xpos - min_x,
                    (next_offset - offset) + 1);
                break;
            case RenderConfig::render_none:
                break;
            default:
                DEBUG("unknown render style: " << config.render.style);
            }
        }
        // DEBUG("draw " << i << " @ " << offset << "--" << next_offset);

        prev_xpos = xpos;
        prev_lower = lower;
        prev_upper = upper;
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
