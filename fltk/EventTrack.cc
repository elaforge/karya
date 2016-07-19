// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <sstream>
#include <math.h>
#include "config.h"
#include "util.h"
#include "f_util.h"
#include "alpha_draw.h"

#include "WrappedInput.h"
#include "EventTrack.h"
#include "SymbolTable.h"
#include "MsgCollector.h"


// #define DEBUG(X) ;

// Hack for debugging.
#define SHOW_RANGE(r) (r).y << "--" << (r).b()

// Turn this off to see the samples more clearly.
static const bool smooth_linear = true;

// The color of events at a non-zero rank is scaled by this.  This should be
// high enough to make them easily distinguishable, but not so high they are
// too low-contrast to read.
static const double rank_brightness = 1.35;
// The color of events with a negative duration is scaled by this.
static const double negative_duration_brightness = .85;

// Don't use INT_MIN because it overflows too easily.
enum { MIN_PIXEL = -10000, MAX_PIXEL = 10000 };


// TrackSignal //////////

void
TrackSignal::free_signals()
{
    if (signal)
        free(signal);
}

static bool
compare_control_sample(const ControlSample &s1, const ControlSample &s2)
{
    return s1.time < s2.time;
}

int
TrackSignal::find_sample(ScoreTime start) const
{
    if (!signal) {
        // Render was set but there is no signal... so just say nothing was
        // found.
        return 0;
    }
    ControlSample sample(to_real(start), 0);
    ControlSample *found =
        std::lower_bound(signal, signal + length, sample,
            compare_control_sample);
    // Back up one to make sure I have the sample before start.
    if (found > signal)
        found--;
    return found - signal;
}


// Get the val at the given index, normalized between 0--1.
double
TrackSignal::val_at(int i) const
{
    ASSERT_MSG(signal, "val_at on empty track signal");
    return util::normalize(this->val_min, this->val_max, signal[i].val);
}


RealTime
TrackSignal::time_at(int i) const
{
    ASSERT_MSG(signal, "time_at on empty track signal");
    return signal[i].time;
}


int
TrackSignal::pixel_time_at(const ZoomInfo &zoom, int i) const
{
    ASSERT_MSG(signal, "pixel_time_at on empty track signal");
    return zoom.to_pixels(from_real(signal[i].time) - zoom.offset);
}


void
TrackSignal::calculate_val_bounds()
{
    RealTime last_time = -9999;
    val_min = 9999;
    val_max = 1;
    for (ControlSample *s = signal; s < signal + length; s++) {
        val_max = std::max(val_max, s->val);
        val_min = std::min(val_min, s->val);
        // Since I'm iterating over the signal I might as well check this.
        // Unsorted samples will cause drawing glitches.
        if (s->time <= last_time) {
            DEBUG("sample time didn't increase: " << s->time << " <= "
                << last_time);
        }
        last_time = s->time;
    }
    // If it looks like a normalized control signal, then it's more convenient
    // to see it on an absolute scale.
    if (val_min >= 0 && val_max <= 1) {
        val_min = 0;
        val_max = 1;
    } else if (val_min >= -1 && val_max <= 1) {
        val_min = -1;
        val_max = 1;
    }
}


std::ostream &
operator<<(std::ostream &os, const TrackSignal &sig)
{
    if (sig.signal) {
        for (int i = 0; i < sig.length; i++) {
            os << "sig[" << i << "] = " << sig.signal[i].time << " -> "
                << sig.signal[i].val << '\n' ;
        }
    } else {
        os << "EMPTY TRACK SIGNAL";
    }
    return os;
}


// EventTrackView ///////

EventTrackView::EventTrackView(const EventTrackConfig &config,
        const RulerConfig &ruler_config) :
    TrackView("events"),
    config(config), last_offset(0), brightness(1), bg_color(config.bg_color),
    title_input(new WrappedInput(0, 0, 1, 1, true)),
    bg_box(0, 0, 1, 1),
    overlay_ruler(ruler_config, false)
{
    // this->resizable(0); // don't resize children
    end(); // make sure no one else falls in
    this->add(bg_box);
    this->add(this->overlay_ruler);
    // create event widgets
    bg_box.box(FL_THIN_DOWN_BOX);
    bg_box.color(config.bg_color.brightness(this->brightness).fl());
}


void
EventTrackView::resize(int x, int y, int w, int h)
{
    // DEBUG("resize " << f_util::rect(this) << " -> " << IRect(x, y, w, h));
    // Don't call Fl_Group::resize because I just did the sizes myself.
    Fl_Widget::resize(x, y, w, h);
    this->overlay_ruler.resize(x, y, w, h);
    this->bg_box.resize(x, y, w, h);
}


void
EventTrackView::set_title(const char *title)
{
    this->title_input->set_text(title);
    // If it has multiple lines, make sure it always displays the first one.
    this->title_input->position(0);
}


void
EventTrackView::set_title_focus()
{
    this->title_input->take_focus();
    // Since focus goes to the text input, the msg collector won't receive
    // any subsequent key ups.
    MsgCollector::get()->all_keys_up();
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
    this->bg_box.color(this->bg_color.brightness(this->brightness).fl());
    this->redraw();
}


ScoreTime
EventTrackView::time_end() const
{
    return std::max(this->config.time_end, this->overlay_ruler.time_end());
}


void
EventTrackView::update(const Tracklike &track, ScoreTime start, ScoreTime end)
{
    ASSERT_MSG(track.track, "updated an event track with a non-event config");
    if (track.ruler)
        this->overlay_ruler.set_config(false, *track.ruler, start, end);
    if (this->config.bg_color != track.track->bg_color) {
        this->bg_color = track.track->bg_color;
        this->set_event_brightness(this->brightness);
    }

    TrackSignal tsig = this->config.track_signal;
    Config::free_haskell_fun_ptr(
        reinterpret_cast<void *>(this->config.find_events));
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
    if (!config.track_signal.empty()
            && this->config.render.style == RenderConfig::render_none) {
        DEBUG("WARNING: got a track signal even though RenderStyle is none");
    }
}


void
EventTrackView::finalize_callbacks()
{
    Config::free_haskell_fun_ptr(
        reinterpret_cast<void *>(this->config.find_events));
    this->config.track_signal.free_signals();
    this->overlay_ruler.delete_config();
}


void
EventTrackView::draw()
{
    IRect draw_area = f_util::rect(this);

    // I used to look for FL_DAMAGE_SCROLL and use fl_scroll() for a fast
    // blit, but it was too hard to get right.  The biggest problem is that
    // events are at floats which are then rounded to ints for pixel positions.
    if (this->damage() == FL_DAMAGE_CHILD) {
        // Only CHILD damage means a selection was set.  But since I overlap
        // with the child, I have to draw too.
        // DEBUG("intersection with child: "
        //     << SHOW_RANGE(draw_area) << " + "
        //     << SHOW_RANGE(overlay_ruler.damaged_area) << " = "
        //     << SHOW_RANGE(draw_area.intersect(overlay_ruler.damaged_area)));
        draw_area = draw_area.intersect(this->overlay_ruler.damaged_area);
    } else {
        // I could technically handle SCROLL | CHILD, but I'd have to tweak
        // the ruler's damaged_area to account for the scroll and that's too
        // much bother right now.
        this->damage(FL_DAMAGE_ALL);
    }

    if (draw_area.w > 0  && draw_area.h > 0) {
        // DEBUG("draw area " << draw_area << " " << SHOW_RANGE(draw_area));
        // When overlay_ruler.draw() is called it will redundantly clip again
        // on damage_range, but that's ok because it needs the clip when called
        // from RulerTrackView::draw().
        f_util::ClipArea clip_area(draw_area);

        // TODO It might be cleaner to eliminate bg_box and just call fl_rectf
        // and fl_draw_box myself.  But this draws the all-mighty bevel too.
        this->draw_child(this->bg_box);

        // This is more than one pixel, but otherwise I draw on top of the
        // bevel on retina displays.
        IRect inside_bevel = f_util::rect(this);
        inside_bevel.x += 2; inside_bevel.w -= 3;
        inside_bevel.y += 2; inside_bevel.h -= 3;
        f_util::ClipArea clip_area2(inside_bevel);

        this->draw_area();
        overlay_ruler.damaged_area.w = overlay_ruler.damaged_area.h = 0;
        this->last_offset = this->zoom.offset;
    }
}


static void
show_found_events(ScoreTime start, ScoreTime end, Event *events, int count)
{
    printf("%.2f-%.2f: %d events:", start.scale(1), end.scale(1), count);
    for (int i = 0; i < count; i++) {
        printf(" (%.2f %s)", events[i].start.scale(1), events[i].text);
    }
    printf("\n");
}

void
EventTrackView::draw_area()
{
    IRect clip = f_util::clip_rect(f_util::rect(this));
    // Expand by a pixel, otherwise I miss little slivers on retina displays.
    clip.y--;
    clip.h++;
    int y = this->track_start();

    ScoreTime start = this->zoom.to_time(clip.y - y);
    ScoreTime end = start + this->zoom.to_time(clip.h);
    start = start + this->zoom.offset;
    end = end + this->zoom.offset;
    // DEBUG("TRACK CLIP: " << start << "--" << end
    //     << ", " << SHOW_RANGE(clip));

    // The results are sorted by (event_start, rank), so lower ranks always
    // come first.
    Event *events;
    int *ranks;
    int count = this->config.find_events(&start, &end, &events, &ranks);
    // If I comment it, I get an unused function warning.
    if (false)
        show_found_events(start, end, events, count);

    std::vector<int> offsets(count);
    for (int i = 0; i < count; i++) {
        offsets[i] =
            y + this->zoom.to_pixels(events[i].start - this->zoom.offset);
    }
    draw_event_boxes(events, ranks, count, offsets);
    this->draw_signal(clip.y, clip.b(), start);

    // The overlay ruler overlaps me entirely, so I'm sure it's damaged.
    if (damage() & FL_DAMAGE_ALL)
        this->draw_child(this->overlay_ruler);
    else
        this->update_child(this->overlay_ruler);

    std::pair<IRect, IRect> prev_rects(IRect(0, 0, 0, 0), IRect(0, 0, 0, 0));
    // Draw the upper layer (event start line, text).
    for (int i = 0; i < count; i++) {
        int rank = ranks[i];
        int next_offset = MAX_PIXEL;
        int prev_offset = i == 0 ? MIN_PIXEL : offsets[i-1];
        // Find the next event offset on the same side.
        // TODO negative events should do this for the prev_offset
        for (int j = i+1; j < count; j++) {
            if ((rank && ranks[j]) || (!rank && !ranks[j])) {
                next_offset = offsets[j];
                break;
            }
        }
        prev_rects = this->draw_upper_layer(
            offsets[i], events[i], rank, prev_offset, next_offset,
            prev_rects.first, prev_rects.second);
    }
    if (count) {
        for (int i = 0; i < count; i++) {
            if (events[i].text)
                free(const_cast<char *>(events[i].text));
        }
        free(events);
        free(ranks);
    }
}


// Draw event boxes.  Rank >0 boxes are not drawn since I'd have to figure out
// overlaps and they're meant to be used with control tracks anyway.
void
EventTrackView::draw_event_boxes(
    const Event *events, const int *ranks, int count,
    const std::vector<int> &offsets)
{
    for (int i = 0; i < count; i++) {
        if (ranks[i])
            continue;
        const Event &event = events[i];
        int height = this->zoom.to_pixels(event.duration);
        // If this event touches the next one, make its box one pixel short,
        // so it's clearer which direction it's facing.
        if (height > 0) {
            int next_offset = MAX_PIXEL;
            for (int j = i+1; j < count; j++) {
                if (!ranks[j]) {
                    next_offset = offsets[j];
                    break;
                }
            }
            if (offsets[i] + height == next_offset) {
                height--;
            }
        } else if (height < 0) {
            int prev_offset = MIN_PIXEL;
            for (int j = i-1; j >= 0; j--) {
                if (!ranks[j]) {
                    prev_offset = offsets[j];
                    break;
                }
            }
            if (offsets[i] + height == prev_offset) {
                height += 2;
            }
        }
        if (height != 0) {
            Color c = StyleTable::get()->get(event.style_id)
                ->event_color.brightness(this->brightness);
            if (event.duration != ScoreTime(0)) {
                int y0 = std::min(offsets[i], offsets[i] + height);
                int y1 = std::max(offsets[i], offsets[i] + height);

                if (event.duration < ScoreTime(0))
                    c = c.brightness(negative_duration_brightness);
                fl_color(c.fl());
                fl_rectf(this->x() + 1, y0, this->w() - 2, y1-y0);
            }
        }
    }
}


// Try to find a linear slope of close-placed samples, so they can be drawn
// as a single line instead of a bunch of separate samples.  This looks a lot
// nicer, especially on retina displays, where fltk doesn't support subpixel
// positioning.
//
// Return the index of the end of a line starting at the given index.  If
// there's no line then it just returns the start index.
// Run along the samples as long as the slope is constant.  When it changes
// too much, return the last index.  Also stop if the slope is too high, which
// implies the signal is itended to be discontinuous, or the samples are far
// apart, which implies the signal is intended to be jagged.
static int
find_linear(const TrackSignal &sig, int i)
{
    // Samples at this distance are likely intended to sound smooth, so they
    // should look smooth too.
    static const double time_threshold = 0.05;
    static const double eta = 0.0001;

    if (i + 1 >= sig.length)
        return i;
    double prev_t = sig.time_at(i);
    double prev_val = sig.val_at(i);
    double t = sig.time_at(i+1);
    double val = sig.val_at(i+1);

    // Avoid divide by zero when two samples occur together.
    if (t == prev_t) {
        // DEBUG("coincident samples: " << i << " " << t);
        return i;
    }
    double expected_slope = (val - prev_val) / (t - prev_t);
    // DEBUG(i << ": slope " << val << "-" << prev_val << " = "
    //     << expected_slope << " time " << fabs(t-prev_t));
    if (fabs(t - prev_t) >= time_threshold)
        return i;

    // DEBUG("expected: " << expected_slope << ": " << val << "-" << prev_val
    //         << " / " << t << "-" << prev_t);
    i += 2;
    for (; i < sig.length; i++) {
        prev_t = t;
        t = sig.time_at(i);
        prev_val = val;
        val = sig.val_at(i);
        double slope = (val - prev_val) / (t - prev_t);
        if (t == prev_t) {
            if (val == prev_val)
                slope = 0;
            else
                break;
        }
        // DEBUG(i << ": slope " << slope);
        if (fabs(t - prev_t) >= time_threshold
            || fabs(slope - expected_slope) > eta)
        {
            break;
        }
    }
    return i - 1;
}

static void
draw_segment(RenderConfig::RenderStyle style, int min_x,
    int prev_xpos, int xpos, int next_xpos,
    int offset, int next_offset)
{
    // Draw horizontal jump from prev_xpos if it exists and is far enough from
    // the xpos.  render_filled doesn't need this.
    switch (style) {
    case RenderConfig::render_line:
        fl_line_style(FL_SOLID | FL_CAP_ROUND, 2);
        // DEBUG("xpos: " << prev_xpos << " " << xpos << " " << next_xpos);
        if (prev_xpos != -1 && abs(prev_xpos - xpos) > 2) {
            fl_line(prev_xpos, offset, xpos, offset);
        }
        fl_line(xpos, offset, next_xpos, next_offset);
        break;
    case RenderConfig::render_filled:
        fl_line_style(FL_SOLID | FL_CAP_ROUND, 0);
        fl_polygon(
            min_x, offset,
            xpos, offset,
            next_xpos, next_offset,
            min_x, next_offset);
        break;
    case RenderConfig::render_none:
        // Shouldn't get here since the caller aborts when it sees this.
        ASSERT_MSG(0, "tried to draw a signal with render_none");
        break;
    default:
        DEBUG("unknown render style: " << style);
    }
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

    const int y = this->track_start();
    // TODO alpha not supported, I'd need a non-portable drawing routine for
    // it.
    const Fl_Color signal_color =
        config.render.color.brightness(brightness).fl();

    // Account for both the 1 pixel track border and the width of the line.
    const int min_x = x() + 2;
    const int max_x = x() + w() - 2;
    int next_i;
    int offset = 0;
    int prev_xpos = -1;

    for (int i = found; i < tsig.length; i = next_i) {
        // I draw from offset to next_offset.
        // For the first sample, 'found' should be at or before start.
        if (smooth_linear)
            next_i = find_linear(tsig, i);
        else
            next_i = i + 1;

        int xpos = floor(util::scale(double(min_x), double(max_x),
            util::clamp(0.0, 1.0, tsig.val_at(i))));
        int next_xpos, next_offset;
        if (i + 1 >= tsig.length) {
            // Out of signal, last sample goes to the bottom.
            next_i = tsig.length;
            next_xpos = xpos;
            next_offset = max_y;
        } else if (next_i > i + 1) {
            next_xpos = floor(util::scale(double(min_x), double(max_x),
                util::clamp(0.0, 1.0, tsig.val_at(next_i))));
            next_offset = y + tsig.pixel_time_at(zoom, next_i);
        } else {
            next_i = i + 1;
            next_xpos = xpos;
            next_offset = y + tsig.pixel_time_at(zoom, next_i);
        }

        offset = y + tsig.pixel_time_at(zoom, i);
        // If the next sample is too close then don't draw this one.
        if (next_offset <= offset)
            continue;

        // DEBUG("sample " << i << "--" << next_i
        //     << " val " << xpos << "--" << next_xpos
        //     << ", " << offset << "--" << next_offset);
        fl_color(signal_color);
        draw_segment(config.render.style, min_x,
            prev_xpos, xpos, next_xpos,
            offset, next_offset);
        prev_xpos = next_xpos;
    }
    // Reset line style to not mess up other draw routines.
    fl_line_style(0);
}


// Draw trigger line.  This will draw ranked trigger lines on top of unranked
// ones, but it's ok because they coincide.
static void
draw_trigger(bool draw_text, int x, int y, int w, const Event &event, int rank)
{
    Color color = draw_text || !event.text
        ? Config::event_trigger_color
        : Config::abbreviation_color;

    fl_color(color.fl());
    fl_line_style(FL_SOLID, 0);
    double h1 = 6;
    double cx = 2;
    double cy = 2;
    double h2 = 0.5;
    if (event.is_negative()) {
        h1 *= -1;
        cy *= -1;
        h2 *= -1;
    }

    fl_begin_polygon();
    if (rank == 0) {
        // left side
        fl_vertex(x, y);
        fl_vertex(x, y+h1);
        fl_vertex(x+cx, y+cy);
    }
    fl_vertex(x + (w/2), y+h2); // center
    // right side
    fl_vertex(x+w-cx, y+cy);
    fl_vertex(x+w, y+h1);
    fl_vertex(x+w, y);
    if (rank > 0) {
        fl_vertex(x + (w/2), y);
    }
    fl_end_polygon();
}


// Draw the stuff that goes on top of the event boxes: trigger line and text.
std::pair<IRect, IRect>
EventTrackView::draw_upper_layer(
    int offset, const Event &event, int rank,
    int prev_offset, int next_offset,
    const IRect &prev_unranked_rect, const IRect &prev_ranked_rect)
{
    /* The overlap stuff is actually pretty tricky.  I want to hide
        overlapping text so it doesn't get into an unreadable jumble.  It's
        also important that the algorithm be consistent and not require
        context, because this will be called to redraw various small fragments.
        This is further complicated by the fact that text can be all different
        sizes and that there is text aligned to the left (unranked) and lower
        priority text aligned to the right (ranked).  Also, text of negative
        events goes above the trigger line, while for positive events it goes
        below.

        So the current plan is to for positive events to hide text if it will
        overlap with the offset of the next ranked or unranked event, as
        appropriate.  Negative events are the same but for the previous offset.
        Ranked events have the additional restriction that they can't overlap
        with the previous text rect, so if they bump into it horizontally they
        won't be drawn.

        This draws negative ranked text incorrectly since I should be checking
        the next_unranked_rect instead of the prev one, but I can fix that if
        it ever becomes a problem.

        This scheme hides all text that might overlap with other text, so it
        hides a lot of text that could be displayed.  I experimented with a
        scheme that recorded where all text was drawn to try to display more,
        but in addition to being complicated it wound up looking cluttered and
        ugly, and I don't think knowing some random note in the middle of a run
        is actually that useful.  If I want to give hints about the contents of
        small notes I'll have to come up with some other mechanism, like color
        coding.
    */

    const EventStyle *event_style = StyleTable::get()->get(event.style_id);
    const SymbolTable::Style style(event_style->font, event_style->size,
        (rank ? event_style->text_color.brightness(rank_brightness)
            : event_style->text_color).fl());

    // DEBUG("---------event " << event.text << " " << prev_offset << ", "
    //     << offset << ", " << next_offset
    //     << " prev " << prev_unranked_rect << " / " << prev_ranked_rect);

    IRect text_rect(0, 0, 0, 0);
    bool draw_text = false;
    if (event.text) {
        // The first measure tells me if one line of text will fit.  If not
        // even one line fits, then don't bother to draw any text.
        IPoint box = SymbolTable::get()->measure(event.text, style);
        text_rect.w = box.x;
        text_rect.h = box.y;
        // Text goes above the trigger line for negative events, plus spacing.
        if (event.is_negative())
            text_rect.y = offset - box.y - 1;
        else
            text_rect.y = offset + 1;
        if (rank)
            text_rect.x = (x() + w()) - text_rect.w - 2;
        else
            text_rect.x = x() + 2;

        if (event.is_negative()) {
            // Negative event text draws above, so check for overlap with the
            // previous text box.  This is regardless of whether or not the
            // previous event text was actually drawn!
            if (rank)
                draw_text = text_rect.y >= prev_ranked_rect.b();
            else
                draw_text = text_rect.y >= prev_unranked_rect.b();
            if (rank && text_rect.intersects(prev_unranked_rect))
                draw_text = false;
        } else {
            draw_text = text_rect.b() <= next_offset;
            if (rank && text_rect.intersects(prev_unranked_rect))
                draw_text = false;
        }
    }

    // Negative events at the top of the track wind up with text above the top.
    text_rect.y = std::max(
        text_rect.y, this->track_start() - zoom.to_pixels(zoom.offset));
    // Similarly, avoid going past the end of the ruler, unless there's space
    // to draw.
    int bottom = std::max(
        y() + zoom.to_pixels(overlay_ruler.time_end() - zoom.offset),
        y() + h() - 4);
        // Just like track_start(), subtract a few pixels from the bottom to
        // avoid the bevel.
    text_rect.b(std::min(text_rect.b(), bottom));

    // The various pixel tweaks in here were determined by zooming in and
    // squinting.

    // DEBUG("offset " << offset << ", text_rect " << text_rect);
    // fl_color(FL_BLUE);
    // fl_rect(text_rect.x, text_rect.y, text_rect.w, text_rect.h);

    draw_trigger(draw_text, x()+1, offset, w()-2, event, rank);
    if (draw_text) {
        // Word wrapping only applies to positive events.  I would need
        // additional code to get them working for negative events, since the
        // text goes up rather than down.
        int track_width = w() - 3;
        if (config.text_wrap == EventTrackConfig::wrap && event.is_positive()) {
            // Ranked events align to the right, non-ranked ones align to the
            // left.
            bool right_justify = rank > 0;
            // The *_wrapped functions start at upper left, not lower left.
            IPoint draw_pos(
                right_justify ? text_rect.r() : text_rect.x, text_rect.y - 2);
            SymbolTable::get()->draw_wrapped(
                event.text, draw_pos, track_width,
                next_offset - draw_pos.y, style, right_justify);
        } else {
            // Due to boundary issues, drawing text that touches the bottom of
            // a box means drawing one above the bottom.  I don't totally
            // understand this.
            IPoint draw_pos = IPoint(text_rect.x, text_rect.b() - 1);
            SymbolTable::get()->draw(event.text, draw_pos, style, false);
        }
    }
    if (rank) {
        return std::make_pair(prev_unranked_rect, text_rect);
    } else {
        return std::make_pair(text_rect, prev_ranked_rect);
    }
}


std::string
EventTrackView::dump() const
{
    std::ostringstream out;
    out << "type event title " << f_util::show_string(this->get_title());
    return out.str();
}
