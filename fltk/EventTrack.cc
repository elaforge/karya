// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <string>
#include <utility>
#include <vector>
#include <sstream>
#include <math.h>

#include "FloatingInput.h"
#include "SymbolTable.h"
#include "config.h"
#include "f_util.h"
#include "util.h"

#include "EventTrack.h"


using std::string;
using std::pair;
using std::vector;

// #define DEBUG(X) ;

// DEBUG text drawing.
// #define TEXT(X) DEBUG(X)
#define TEXT(X) ;

// Hack for debugging.
#define SHOW_RANGE(r) (r).y << "--" << (r).b()

// The color of events at a non-zero rank is scaled by this.  This should be
// high enough to make them easily distinguishable, but not so high they are
// too low-contrast to read.
static const double rank_brightness = 1.35;
// The color of events with a negative duration is scaled by this.
static const double negative_duration_brightness = .85;

// Height of the ears on either side of the trigger line.
static const double trigger_height = 7;

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
TrackSignal::pixel_time_at(const Zoom &zoom, int i) const
{
    ASSERT_MSG(signal, "pixel_time_at on empty track signal");
    return zoom.to_pixels(from_real(signal[i].time) - zoom.offset);
}


void
TrackSignal::calculate_val_bounds(const char *track_name)
{
    RealTime last_time = -9999;
    val_min = 9999;
    val_max = 1;
    for (ControlSample *s = signal; s < signal + length; s++) {
        val_max = std::max(val_max, s->val);
        val_min = std::min(val_min, s->val);
        // Since I'm iterating over the signal I might as well check this.
        // Unsorted samples will cause drawing glitches.  Coincident samples
        // are explicit discontinuities, so they're ok.
        if (s->time < last_time) {
            DEBUG("track " << track_name << ": sample time decreased: "
                << s->time << " < " << last_time);
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


// EventTrack ///////

EventTrack::EventTrack(const EventTrackConfig &config,
        const RulerConfig &ruler_config) :
    Track("events"),
    config(config), brightness(1), bg_color(config.bg_color),
    title_input(0, 0, 1, 1),
    bg_box(0, 0, 1, 1),
    ruler_overlay(ruler_config, false),
    floating_input(nullptr)
{
    end(); // make sure no one else falls in
    this->add(bg_box);
    // create event widgets
    bg_box.box(FL_THIN_DOWN_BOX);
    bg_box.color(config.bg_color.brightness(this->brightness).fl());

    title_input.callback(title_input_focus_cb, static_cast<void *>(this));
}


void
EventTrack::resize(int x, int y, int w, int h)
{
    // Don't call Fl_Group::resize because I just did the sizes myself.
    // TODO wait, what?
    Fl_Widget::resize(x, y, w, h);
    this->bg_box.resize(x, y, w, h);
}

// title ////////////////

void
EventTrack::set_title(const char *title)
{
    title_input.value(title);
    // If it has multiple lines, make sure it always displays the first one.
    title_input.position(0);
}


void
EventTrack::set_title_focus()
{
    focus_title();
}


// TODO: duplicated with RulerTrack, move this into Track?
void
EventTrack::set_selection(int selnum, const std::vector<Selection> &news)
{
    for (auto &sel : selection_overlay.get(selnum))
        damage_range(sel.low(), sel.high(), true);
    selection_overlay.set(selnum, news);
    for (auto &sel : news)
        damage_range(sel.low(), sel.high(), true);
}


void
EventTrack::title_input_focus_cb(Fl_Widget *_w, void *arg)
{
    int evt = Fl::event();
    EventTrack *self = static_cast<EventTrack *>(arg);
    self->focus_title();

    // Set the insertion point based on the click position.
    if (evt == FL_PUSH) {
        int p = self->title_input.mouse_position();
        // TODO Because I create a new window, it doesn't notice that the mouse
        // button is down, so a drag will be lost.
        // Unfortunately it seems giving it a push doesn't work.
        // self->floating_input->handle(FL_PUSH);
        self->floating_input->cursor_position(p, p);
    }
}


void
EventTrack::floating_input_done_cb(Fl_Widget *_w, void *arg)
{
    EventTrack *self = static_cast<EventTrack *>(arg);
    self->unfocus_title();
}


void
EventTrack::focus_title()
{
    if (floating_input)
        return;
    Fl_Window *w = window();
    this->floating_input = new FloatingInput(
        w->x() + title_input.x(), w->y() + title_input.y(),
        std::max(int(Config::Block::floating_input_min_width), title_input.w()),
        Config::Block::track_title_height,
        window(), title_input.value(), true);
    floating_input->callback(floating_input_done_cb, static_cast<void *>(this));
    int len = strlen(title_input.value());
    floating_input->cursor_position(len, len);
}


void
EventTrack::unfocus_title()
{
    if (!floating_input)
        return;
    set_title(floating_input->get_text());
    floating_input->hide();
    Fl::delete_widget(floating_input);
    this->floating_input = nullptr;
    do_callback();
}

/////////////////////////


void
EventTrack::set_event_brightness(double d)
{
    this->brightness = d;
    this->bg_box.color(this->bg_color.brightness(this->brightness).fl());
    this->redraw();
}


ScoreTime
EventTrack::time_end() const
{
    return std::max(this->config.time_end, this->ruler_overlay.time_end());
}


void
EventTrack::update(const Tracklike &track, ScoreTime start, ScoreTime end)
{
    ASSERT_MSG(track.track, "updated an event track with a non-event config");
    this->damage_range(start, end, false);

    if (track.ruler)
        this->ruler_overlay.set_config(false, *track.ruler);
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
}

void
EventTrack::set_track_signal(const TrackSignal &tsig)
{
    if (this->config.track_signal.empty() && tsig.empty())
        return;
    this->config.track_signal.free_signals();
    // Copy over the pointers, I'm taking ownership now.
    this->config.track_signal = tsig;
    this->redraw();
    if (!config.track_signal.empty()
            && this->config.render.style == RenderConfig::render_none) {
        DEBUG("WARNING: got a track signal even though RenderStyle is none");
    }
}


static float
get_max_peak(const std::vector<std::shared_ptr<PeakCache::Entry>> &peaks)
{
    float max = 0;
    for (const auto &entry : peaks) {
        max = std::max(max, entry->max_peak);
    }
    return max;
}


void
EventTrack::set_waveform(int chunknum, const PeakCache::Params &params)
{
    // Clear any chunks above.
    this->peaks.resize(size_t(chunknum+1));
    // chunknum=-1 means clear all.
    if (chunknum >= 0) {
        peaks[chunknum] = PeakCache::get()->load(params);
        float new_peak = get_max_peak(peaks);
        if (new_peak != this->max_peak) {
            this->max_peak = new_peak;
            this->redraw();
        }
    }
}


void
EventTrack::finalize_callbacks()
{
    Config::free_haskell_fun_ptr(
        reinterpret_cast<void *>(this->config.find_events));
    this->config.track_signal.free_signals();
    this->ruler_overlay.delete_config();
}


// TODO: parts of this are the same as RulerTrack::draw
// This just figures out the area to draw, the actual work is done in
// draw_area().
void
EventTrack::draw()
{
    IRect draw_area = f_util::rect(this);
    // DEBUG("damage: " << f_util::show_damage(damage()));

    // I used to look for FL_DAMAGE_SCROLL and use fl_scroll() for a fast
    // blit, but it was too hard to get right.  The biggest problem is that
    // events are at floats which are then rounded to ints for pixel positions.
    if (damage() == Track::DAMAGE_RANGE) {
        // DEBUG("intersection draw_area with damaged_area: "
        //     << SHOW_RANGE(draw_area) << " \\/ "
        //     << SHOW_RANGE(damaged_area) << " = "
        //     << SHOW_RANGE(draw_area.intersect(damaged_area)));
        draw_area = draw_area.intersect(this->damaged_area);
    } else {
        // I could technically handle SCROLL, but I'd have to tweak the ruler's
        // damaged_area to account for the scroll and that's too much bother
        // right now.
        this->damage(FL_DAMAGE_ALL);
    }
    if (draw_area.w <= 0 || draw_area.h <= 0)
        return;

    // DEBUG("draw " << get_title() << ": " << f_util::show_damage(damage())
    //     << ": " << draw_area << " " << SHOW_RANGE(draw_area));
    // When ruler_overlay.draw() is called it will redundantly clip again
    // on damage_range, but that's ok because it needs the clip when called
    // from RulerTrack::draw().
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
    damaged_area.w = damaged_area.h = 0;
}


static void
show_found_events(ScoreTime start, ScoreTime end, Event *events, int count)
{
    printf("%.2f-%.2f: %d events:", start.scale(1), end.scale(1), count);
    for (int i = 0; i < count; i++) {
        printf(" (%.2f '%s')", events[i].start.scale(1),
            events[i].text ? events[i].text : "");
    }
    printf("\n");
}


static bool
is_empty(const char *str)
{
    if (!str)
        return true;
    bool all_white = true;
    while (*str) {
        if (*str != ' ')
            return false;
        str++;
    }
    return all_white;
}

static SymbolTable::Wrapped
wrap_text(const Event &event, int width)
{
    const EventStyle *event_style = StyleTable::get()->get(event.style_id);
    const SymbolTable::Style style(
        event_style->font, event_style->size, event_style->text_color.fl());

    if (is_empty(event.text))
        return SymbolTable::Wrapped();
    else
        return SymbolTable::get()->wrap(event.text, style, width);
}


static EventTrack::TextBox
compute_text_box(
    const Event &event, const SymbolTable::Wrapped &wrapped,
    int x, int y, int width, EventTrack::Align align,
    int track_min, int track_max)
{
    vector<pair<string, IRect>> lines;
    lines.reserve(wrapped.size());
    if (event.is_negative()) {
        // When going top-to-bottom, I add 1 for padding, so I have to do the
        // same when figuring out the top.
        for (const pair<string, DPoint> &line : wrapped)
            y -= ceil(line.second.y) + 1;
        y -= 2; // Up a couple pixels to avoid overlapping the trigger.
    } else {
        y++;
    }
    for (const pair<string, DPoint> &line : wrapped) {
        int tx = align == EventTrack::Right
            ? x + width - ceil(line.second.x)
            : x + 2;
        IRect box(tx, y, ceil(line.second.x), ceil(line.second.y));
        lines.push_back(std::make_pair(line.first, box));
        y += ceil(line.second.y) + 1;
    }
    // Avoid putting text past the unscrollable start and end of the track.
    if (!lines.empty()) {
        if (event.is_negative()) {
            int offset = track_min - lines.begin()->second.y;
            if (offset > 0) {
                // DEBUG("+line " << *lines.begin() << " offset: " << offset);
                for (auto &line : lines) {
                    line.second.translate(IPoint(0, offset));
                }
            }
        } else {
            int offset = lines.rbegin()->second.b() - track_max;
            if (offset > 0) {
                // DEBUG("-line " << *lines.rbegin() << " offset: " << offset);
                for (auto &line : lines) {
                    line.second.translate(IPoint(0, -offset));
                }
            }
        }
    }
    return EventTrack::TextBox(lines, align);
}


// Drawing order:
// EventTrack: bg -> events -> wave -> signal -> ruler -> text -> trigger -> sel
// RulerTrack: bg ->                             ruler ->                 -> sel
void
EventTrack::draw_area()
{
    IRect clip = f_util::clip_rect(f_util::rect(this));
    // Expand by a pixel, otherwise I miss little slivers on retina displays.
    clip.y--;
    clip.h++;
    const int y_start = this->track_start();
    const int wrap_width = w() - 3; // minus some padding to avoid the bevel

    // Calculate bounds.

    const ScoreTime start = this->zoom.to_time(clip.y - y_start) + zoom.offset;
    const ScoreTime end = start + this->zoom.to_time(clip.h);
    // DEBUG("START " << clip.y << " - " << y_start << " to time "
    //     << zoom.to_time(clip.y - y_start));
    // start = start + this->zoom.offset;
    // end = end + this->zoom.offset;

    // The results are sorted by (event_start, rank), so lower ranks always
    // come first.
    Event *events;
    int *ranks;
    int count = this->config.find_events(&start, &end, &events, &ranks);
    // If I comment it, I get an unused function warning.
    if (false)
        show_found_events(start, end, events, count);

    vector<TextBox> boxes(count);
    vector<int> triggers(count);

    const int track_min = track_start() - zoom.to_pixels(zoom.offset);
    const int track_max = std::max(
        this->y() + this->h(), track_min + zoom.to_pixels(time_end()));
    for (int i = 0; i < count; i++) {
        triggers[i] = y_start
            + this->zoom.to_pixels(events[i].start - this->zoom.offset);
        const SymbolTable::Wrapped wrapped(wrap_text(events[i], wrap_width));
        Align align = ranks[i] > 0 ? Right : Left;
        boxes[i] = compute_text_box(
            events[i], wrapped, x(), triggers[i], wrap_width, align,
            track_min, track_max);
    }

    // Actually start drawing.

    this->draw_event_boxes(events, ranks, count, triggers);
    this->draw_waveforms(clip.y, clip.b(), start);
    this->draw_signal(clip.y, clip.b(), start);

    IRect box(x(), track_start(), w(), h() - (y()-track_start()));
    this->ruler_overlay.draw(box, zoom, clip);

    for (int i = 0; i < count; i++) {
        Align align = ranks[i] > 0 ? Right : Left;
        draw_upper_layer(i, events, align, boxes, triggers);
    }
    selection_overlay.draw(x(), track_start(), w(), zoom);

    // Free text, allocated on the haskell side.
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
EventTrack::draw_event_boxes(
    const Event *events, const int *ranks, int count,
    const vector<int> &triggers)
{
    for (int i = 0; i < count; i++) {
        if (ranks[i])
            continue;
        const Event &event = events[i];
        int height = this->zoom.to_pixels(event.duration);
        // If this event touches the next one, make its box one pixel short,
        // so it's clearer which direction it's facing.
        if (height > 0) {
            int next_trigger = MAX_PIXEL;
            for (int j = i+1; j < count; j++) {
                if (!ranks[j]) {
                    next_trigger = triggers[j];
                    break;
                }
            }
            if (triggers[i] + height == next_trigger) {
                height--;
            }
        } else if (height < 0) {
            int prev_offset = MIN_PIXEL;
            for (int j = i-1; j >= 0; j--) {
                if (!ranks[j]) {
                    prev_offset = triggers[j];
                    break;
                }
            }
            if (triggers[i] + height == prev_offset) {
                height += 2;
            }
        }
        if (height != 0) {
            Color c = StyleTable::get()->get(event.style_id)
                ->event_color.brightness(this->brightness);
            if (event.duration != ScoreTime(0)) {
                int y0 = std::min(triggers[i], triggers[i] + height);
                int y1 = std::max(triggers[i], triggers[i] + height);

                if (event.duration < ScoreTime(0))
                    c = c.brightness(negative_duration_brightness);
                fl_color(c.fl());
                fl_rectf(this->x() + 1, y0, this->w() - 2, y1-y0);
            }
        }
    }
}


static const ScoreTime *
get_next_start(
    std::vector<std::shared_ptr<PeakCache::Entry>> &peaks,
    int chunknum)
{
    for (; chunknum < peaks.size(); chunknum++) {
        if (peaks[chunknum].get())
            return &peaks[chunknum]->start;
    }
    return nullptr;
}


// min_y and max_y are already in absolute pixels
void
EventTrack::draw_waveforms(int min_y, int max_y, ScoreTime start)
{
    const float amplitude_scale = max_peak == 0 ? 1 : 1 / max_peak;
    if (peaks.empty())
        return;

    const int min_x = x() + 2;
    const int max_x = x() + w() - 2;

    std::shared_ptr<const std::vector<float>> cache;
    double y = min_y;
    int chunknum = -1; // -1 means uninitialized
    int i = 0; // peak index within a chunk, reset when I reach a new chunk
    const ScoreTime *next_start = get_next_start(peaks, 0);
    const double pixels_per_peak = PeakCache::pixels_per_peak(zoom.factor);
    ScoreTime time = start;
    const ScoreTime time_per_peak = zoom.to_time_d(pixels_per_peak);
    // Trigger a transition from zero.  Useful when the first chunk starts >
    // min_y.  It also suppresses redundant vertices at 0.
    bool at_zero = true;

    fl_line_style(FL_SOLID | FL_CAP_ROUND, 1);
    fl_color(Config::waveform_color.fl());
    fl_begin_polygon();
    // Go over max_y for a little bit.  Otherwise, the slope on the last sample
    // seems to want to go to 0.
    for (; y < max_y+2; y += pixels_per_peak, i++, time = time+time_per_peak) {
        if (y < min_y)
            continue; // TODO just skip it in one step
        // if (next_start)
        //     DEBUG("y " << y << " time " << time << " next: " << *next_start);
        // else
        //     DEBUG("y " << y << " time " << time << " is last");
        while (chunknum == -1 || (next_start && time >= *next_start)) {
            chunknum++;
            i = 0;
            if (chunknum >= peaks.size())
                break;
            if (peaks[chunknum].get()) {
                time = peaks[chunknum]->start;
                y = zoom.to_pixels_d(time - zoom.offset) + track_start();
                next_start = get_next_start(peaks, chunknum+1);
                cache = peaks[chunknum]->at_zoom(zoom.factor);
            } else {
                // Someone put in waveform chunks out of order, and this one is
                // missing.
                cache = nullptr;
            }
        }
        // Out of peaks on the last chunk.
        if (!next_start && i >= cache->size())
            break;
        // i > cache->size() means I ran out of cached peaks before getting to
        // the next chunk start.  This can happen for a sample or two due to
        // pixel roundoff (TODO where exactly?)
        if (cache.get() && i < cache->size()) {
            float x = (*cache)[i];
            x *= amplitude_scale;
            x = x * (max_x - min_x) + min_x;
            // DEBUG("v: (" << x << ", " << y << ") " << x_);
            if (at_zero) {
                fl_vertex(min_x, y);
                at_zero = false;
            }
            fl_vertex(x, y);
        } else if (!at_zero) {
            // DEBUG("v: (" << min_x << ", " << y << ")");
            fl_vertex(min_x, y);
            at_zero = true;
        }
    }
    // DEBUG("v: (" << min_x << ", " << y << ")");
    fl_vertex(min_x, y);
    // DEBUG("v: (" << min_x << ", " << min_y << ")");
    fl_vertex(min_x, min_y);
    fl_end_polygon();
}


static void
draw_segment(RenderConfig::RenderStyle style, int min_x,
    int xpos, int next_xpos, int offset, int next_offset)
{
    switch (style) {
    case RenderConfig::render_line:
        fl_line_style(FL_SOLID | FL_CAP_ROUND, 2);
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
EventTrack::draw_signal(int min_y, int max_y, ScoreTime start)
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

    if (tsig.val_min < 0) {
        // Draw thin line at 0 to give some sense of the absolute value.
        // Without this, it can be hard to notice if the value goes <0, since
        // it will be automatically normalized to -1..1.
        fl_line_style(FL_SOLID, 1);
        fl_color(FL_GRAY);
        double val = util::normalize(tsig.val_min, tsig.val_max, 0.0);
        int xpos = floor(util::scale(double(min_x), double(max_x),
            util::clamp(0.0, 1.0, val)));
        fl_line(xpos, min_y, xpos, max_y);
    }

    for (int i = found; i < tsig.length; i++) {
        // I draw from offset to next_offset.
        // For the first sample, 'found' should be at or before start.

        int xpos = floor(util::scale(double(min_x), double(max_x),
            util::clamp(0.0, 1.0, tsig.val_at(i))));
        int next_xpos, next_offset;
        if (i+1 >= tsig.length) {
            // Out of signal, last sample goes to the bottom.
            next_xpos = xpos;
            next_offset = max_y;
        } else {
            next_xpos = floor(util::scale(double(min_x), double(max_x),
                util::clamp(0.0, 1.0, tsig.val_at(i+1))));
            next_offset = y + tsig.pixel_time_at(zoom, i+1);
        }

        int offset = y + tsig.pixel_time_at(zoom, i);
        // If the next sample is too close then don't draw this one.
        if (next_offset <= offset)
            continue;

        // DEBUG("sample " << i << "--" << i+1
        //     << " val " << xpos << "--" << next_xpos
        //     << ", " << offset << "--" << next_offset);
        fl_color(signal_color);
        draw_segment(config.render.style, min_x,
            xpos, next_xpos, offset, next_offset);
    }
    // Reset line style to not mess up other draw routines.
    fl_line_style(0);
}


// Draw trigger line.  This will draw ranked trigger lines on top of unranked
// ones, but it's ok because they coincide.
static void
draw_trigger(bool draw_text, int x, double y, int w, const Event &event,
    EventTrack::Align align, int prev_limit, int next_limit)
{
    Color color = draw_text || !event.text
        ? Config::event_trigger_color : Config::abbreviation_color;

    fl_color(color.fl());
    fl_line_style(FL_SOLID, 0);
    double h1 = trigger_height;
    double cx = 2;
    double cy = 2;
    double h2 = 0.5;
    if (event.is_negative()) {
        h1 *= -1;
        cy *= -1;
        h2 *= -1;
        // A -0 event at 0 will be invisible, so bump it down just enough to
        // see what it is.
        if (align == EventTrack::Left) {
            y = std::max(y, prev_limit + 3.0);
        }
    } else {
        // Same story as the negative event at 0.
        if (align == EventTrack::Left) {
            y = std::min(y, next_limit - 3.0);
        }
    }
    int r = x + w;
    int mid = x + w/2;

    //          x       x+cx    mid     r-cx    r
    // y        0               1
    // y+h2                     2
    // y+cy             3
    // y+h1     4
    if (align == EventTrack::Left) {
        fl_begin_complex_polygon();
        fl_vertex(x, y);            // 0
        fl_vertex(mid, y);          // 1
        fl_vertex(mid, y+h2);       // 2
        fl_vertex(x+cx, y+cy);      // 3
        fl_vertex(x, y+h1);         // 4
        fl_vertex(x, y);
        fl_end_complex_polygon();
    }

    //          x       x+cx    mid     r-cx    r
    // y                        0               1
    // y+h2                     4
    // y+cy                             3
    // y+h1                                     2
    fl_begin_complex_polygon();
    fl_vertex(mid, y);              // 0
    fl_vertex(r, y);                // 1
    fl_vertex(r, y+h1);             // 2
    fl_vertex(r-cx, y+cy);          // 3
    fl_vertex(mid, y+h2);           // 4
    fl_vertex(mid, y);
    fl_end_complex_polygon();
}


/*
    Find the number of drawable pixels above or below.
    This needs the TextBoxes because how much draw space is available depends
    on how they overlap.  It will return either 0, or >= the complete first
    line, since there's not much point displaying a chopped-off single line.

    Left events will ignore Right ones since Left has drawing priority.
*/
static int
drawable_pixels(
    int index, const Event *events, const vector<EventTrack::TextBox> &boxes)
{
    const Event &event = events[index];
    if (!event.text || !*event.text)
        return 0;

    bool is_left = boxes[index].align == EventTrack::Left;
    TEXT("---- calculate drawable pixels for " << event
        << " is_left: " << is_left);
    int pixels = 0;
    // For each event line, see if it can fit by iterating over each line
    // of each previous TextBox.

    // TODO the negative and positive cases are mostly copy-pastes, except
    // backwards.  How can I reduce the repetition?
    // iteration order for event_line
    // rewind for prev/next text and line
    // iteration order for event lines
    // negative has at_trigger
    if (event.is_negative()) {
        for (auto event_line = boxes[index].lines.crbegin();
            event_line != boxes[index].lines.crend();
            pixels += event_line->second.h, ++event_line)
        {
            IRect event_box = event_line->second;
            TEXT("event: " << event_box << " '" << event_line->first << "'");
            // Preserve some distance between Right and Left text.
            if (boxes[index].align == EventTrack::Right) {
                event_box.x -= 2;
                event_box.w += 2;
            }
            // Go forward to find all events starting here.
            // This has to be signed so I can check >=0.
            int prev = index + 1;
            while (prev < boxes.size() && events[prev].start == event.start)
                prev++;
            for (--prev; prev >=0; --prev) {
                if (prev == index)
                    continue;
                if (is_left && boxes[prev].align == EventTrack::Right)
                    continue;
                for (auto prev_line = boxes[prev].lines.crbegin();
                    prev_line != boxes[prev].lines.crend();
                    ++prev_line)
                {
                    IRect prev_box = prev_line->second;
                    // Add some padding to avoid touching the previous event's
                    // trigger.  Negative event text is bumped up a bit in
                    // compute_text_box and this counteracts that.
                    bool at_prev_trigger =
                        prev_line == boxes[prev].lines.crbegin()
                        && events[prev].is_negative();
                    // Unless this is the first event line, which is clipped
                    // all or nothing, because otherwise text that clearly fits
                    // will not be displayed at all.
                    // TODO this is way complicated, surely there's a simpler
                    // way to express this?
                    bool is_first = event_line == boxes[index].lines.crbegin();
                    if (at_prev_trigger && !is_first)
                        prev_box.h += 2;

                    TEXT("prev " << prev << ", box: " << prev_box
                        << " '" << prev_line->first << "'");
                    if (prev_box.intersects(event_box)) {
                        // If it's not the first line, a partial line is ok.
                        TEXT("intersect: prev " << prev_box << " with cur "
                            << event_box << " (" << event_line->first
                            << "), pixels += " << event_box.b()-prev_box.b());
                        if (pixels > 0) {
                            pixels += event_box.b() - prev_box.b();
                        }
                        return pixels;
                    } else if (prev_box.b() <= event_box.y) {
                        TEXT("fits, continue, pixels += " << event_box.h);
                        goto done; // break outer loop
                    }
                }
            }
            done: continue;
        }
    } else {
        for (auto event_line = boxes[index].lines.begin();
            event_line != boxes[index].lines.end();
            pixels += event_line->second.h, ++event_line)
        {
            TEXT("event_line " << *event_line << " pixels: " << pixels);
            IRect event_box = event_line->second;
            // Preserve some distance between Right and Left text.
            if (boxes[index].align == EventTrack::Right) {
                event_box.x -= 2;
                event_box.w += 2;
            }
            // Rewind to find all events starting here.
            int next = index - 1;
            while (next >= 0 && events[next].start == event.start)
                --next;
            for (++next; next < boxes.size(); ++next) {
                if (next == index)
                    continue;
                if (is_left && boxes[next].align == EventTrack::Right)
                    continue;
                for (auto next_line = boxes[next].lines.begin();
                    next_line != boxes[next].lines.end();
                    ++next_line)
                {
                    IRect next_box = next_line->second;
                    TEXT("next " << next << ", box: " << next_box
                        << " '" << next_line->first << "'");
                    if (next_box.intersects(event_box)) {
                        // If it's not the first line, a partial line is ok.
                        TEXT("intersect: " << next_box << " with "
                            << event_box << " (" << event_line->first
                            << ")");
                        if (pixels > 0) {
                            TEXT("add partial "
                                << next_box.y - event_box.y);
                            pixels += next_box.y - event_box.y;
                        }
                        return pixels;
                    } else if (next_box.y >= event_box.b()) {
                        TEXT("fits, continue, pixels += " << event_box.h);
                        next = boxes.size(); // break outer loop
                        break;
                    }
                }
            }
        }
    }
    // I never ran into another box so there's plenty of space.  Give some
    // extra to avoid chopping off descenders.
    return pixels + 10;
}


static void
draw_text_line(
    const string &line, const IRect &box, const SymbolTable::Style &style)
{
    // Drawing text that touches the bottom of a box means drawing one
    // above the bottom.  I don't totally understand this.
    IPoint draw_at(box.x, box.b() - 1);
    SymbolTable::get()->draw(line, draw_at, style);
}


// Draw the stuff that goes on top of the event boxes: trigger line and text.
void
EventTrack::draw_upper_layer(
    int index, const Event *events, Align align,
    const vector<TextBox> &boxes, const vector<int> &triggers)
{
    /* The overlap stuff is actually pretty tricky.  I want to hide
        overlapping text so it doesn't get into an unreadable jumble.  It's
        also important that the algorithm be consistent and not require
        context, because this will be called to redraw various small fragments.
        This is further complicated by the fact that text can be all different
        sizes and that there is text aligned to the left and lower priority
        text aligned to the right.  Also, text of negative events goes above
        the trigger line, while for positive events it goes below.

        So the current plan is to for positive events to hide text if it will
        overlap with the offset of the next ranked or unranked event, as
        appropriate.  Negative events are the same but for the previous offset.
        Ranked events have the additional restriction that they can't overlap
        with the previous text rect, so if they bump into it horizontally they
        won't be drawn.

        This scheme hides all text that might overlap with other text, so it
        hides a lot of text that could be displayed.  I experimented with a
        scheme that recorded where all text was drawn to try to display more,
        but in addition to being complicated it wound up looking cluttered and
        ugly, and I don't think knowing some random note in the middle of a run
        is actually that useful.  If I want to give hints about the contents of
        small notes I'll have to come up with some other mechanism, like color
        coding.
    */
    const Event &event = events[index];
    // TODO duplicated with wrap_text
    const EventStyle *event_style = StyleTable::get()->get(event.style_id);
    const SymbolTable::Style style(event_style->font, event_style->size,
        (align == Right
            ? event_style->text_color.brightness(rank_brightness)
            : event_style->text_color).fl());

    const int drawable = drawable_pixels(index, events, boxes);
    TEXT("drawable pixels for " << events[index] << ": " << drawable);

    const int track_min = this->track_start() - zoom.to_pixels(zoom.offset);
    const int track_max = track_min + zoom.to_pixels(time_end());

    // Don't draw above 0 since I can't scroll further up to see it.  Also
    // don't draw on top of the previous trigger line of a Left event.  Since
    // the limit only applies to negative events, which draw upwards, this
    // prevents an overlap for [-(0, 1), -(1, 0)].
    int prev_limit = track_min;
    for (int i = index - 1; i >= 0; i--) {
        if (boxes[i].align == EventTrack::Left) {
            prev_limit = std::max(prev_limit, triggers[i]);
            break;
        }
    }
    // Due to the drawing order, I'll only see a blue right-hand trigger if
    // the Right event comes after its conincident Left one.  The haskell side
    // does this for merged tracks.
    draw_trigger(
        drawable > 0, x()+1, triggers[index], w()-2, event, align,
        prev_limit, track_max);

    // for (int i = 0; i < boxes[index].lines.size(); i++) {
    //     TEXT("text box: " << i << ": " << boxes[index].lines[i]);
    // }
    if (drawable <= 0 || boxes[index].lines.empty()) {
        // drawable_pixels will return 0 if there isn't room for at least one
        // line.
        // Draw boxes for the omitted text to debug:
        // for (auto &line : boxes[index].lines)
        //     f_util::draw_rect(line.second, Color(0, 0, 0xff));
    } else if (event.is_negative()) {
        const int bottom = boxes[index].lines.crbegin()->second.b();
        // I'm not worried about drawing below the last box, because that's
        // where the trigger is, so give some extra space for descenders.
        // Ultimately this is because SymbolTable doesn't include descenders
        // in the bounding box.
        // static ColorCycle c;
        // f_util::draw_rect(
        //     IRect(x()+2, bottom - drawable, w()-2, drawable + 10), c.get());
        f_util::ClipArea clip(
            IRect(x(), bottom - drawable, w(), drawable + 10));

        int remaining = drawable;
        for (auto line = boxes[index].lines.crbegin();
            remaining >= 0 && line != boxes[index].lines.crend();
            remaining -= line->second.h, ++line)
        {
            // Even if remaining==0, I still have stuff left to draw, so I need
            // to draw the abbreviation bar at least.
            const auto &box = line->second;
            TEXT("NEGATIVE: " << box << ": " << line->first << " left "
                << remaining << " - " << box.h);
            draw_text_line(line->first, box, style);
            // f_util::draw_rect(box, Color(0, 0xff, 0xff));
            if (remaining < box.h) {
                f_util::draw_rectf(
                    IRect(x(), bottom - drawable, w(), 2),
                    Config::abbreviation_color);
            }
        }
    } else {
        const int top = boxes[index].lines[0].second.y;
        f_util::ClipArea clip(IRect(x(), top, w(), drawable));
        int remaining = drawable;
        for (auto line = boxes[index].lines.cbegin();
            remaining >= 0 && line != boxes[index].lines.cend();
            remaining -= line->second.h, ++line)
        {
            const auto &box = line->second;
            TEXT("POSITIVE: " << box << ": " << line->first << " left "
                << remaining << " - " << box.h);
            draw_text_line(line->first, box, style);
            // f_util::draw_rect(box, Color(0, 0xff, 0xff));
            if (remaining < box.h) {
                f_util::draw_rectf(
                    IRect(x(), top + drawable - 2, w(), 2),
                    Config::abbreviation_color);
            }
        }
    }
}

string
EventTrack::dump() const
{
    std::ostringstream out;
    out << "type event title " << f_util::show_string(this->get_title());
    return out.str();
}

std::ostream &
operator<<(std::ostream &os, const EventTrack::TextBox &box)
{
    os << "TextBox " << (box.align == EventTrack::Left ? "Left" : "Right")
        << '\n';
    for (auto const &line : box.lines) {
        os << "    '" << line.first << "' " << line.second << '\n';
    }
    return os;
}
