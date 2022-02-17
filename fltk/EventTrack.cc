// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <string>
#include <utility>
#include <vector>
#include <sstream>
#include <math.h>

#include <FL/fl_draw.H>

#include "SymbolTable.h"
#include "MsgCollector.h"
#include "config.h"
#include "f_util.h"
#include "util.h"

#include "EventTrack.h"
#include "WrappedInput.h"


using std::string;
using std::pair;
using std::vector;

// #define DEBUG(X) ;

// DEBUG text drawing.
// #define TEXT(X) DEBUG(X)
#define TEXT(X) ;

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

static const int minimum_suggested_width = 20;


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
    return zoom.to_pixels(from_real(signal[i].time));
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

EventTrack::EventTrack(
    int tracknum,
    const EventTrackConfig &config,
    const RulerConfig &ruler_config
) :
    Track("events"),
    // 40 is an arbitrary max_width, will be filled in when I know window width
    title_input(0, 0, 1, 1, true, 40),
    body_scroll(0, 0, 1, 1),
        body(tracknum, config, ruler_config)
{
    end(); // make sure no one else falls in
    this->add(body_scroll);
    // This will be owned by the parent TrackTile.
    this->remove(title_input);
    body_scroll.add(body);
    body_scroll.set_bg(body.config.bg_color.brightness(body.brightness));
    title_input.callback(title_input_cb, static_cast<void *>(this));
}


void
EventTrack::resize(int x, int y, int w, int h)
{
    bool changed = w != this->w() || h != this->h();
    Track::resize(x, y, w, h);
    // CachedScroll doesn't propagate size changes to its child, so I have to
    // do it manually for width.
    body.resize(x, y, w, body.h());
    if (changed)
        invalidate();
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
    title_input.take_focus();
}


void
EventTrack::set_selection(int selnum, const std::vector<Selection> &news)
{
    selection_overlay.set(selnum, news);
    // selection_overlay isn't a Fl_Widget, so I can't redraw() it.  Instead, I
    // add a special kind of damage.
    this->damage(Track::DAMAGE_SELECTION);
}


void
EventTrack::set_zoom(const Zoom &new_zoom)
{
    if (new_zoom == body.zoom)
        return;
    else if (new_zoom.factor != body.zoom.factor)
        invalidate();
    // Otherwise just offset changed and CachedScroll can avoid a redraw.
    body_scroll.set_offset(IPoint(0, -new_zoom.to_pixels(new_zoom.offset)));
    body.set_zoom(new_zoom);
}


void
EventTrack::set_event_brightness(double d)
{
    body.brightness = d;
    body_scroll.set_bg(body.config.bg_color.brightness(body.brightness));
    body_scroll.invalidate();
}


void
EventTrack::update(const Tracklike &track, ScoreTime start, ScoreTime end)
{
    ASSERT_MSG(track.track, "updated an event track with a non-event config");
    body.update(track);
    body_scroll.set_bg(body.config.bg_color.brightness(body.brightness));
    invalidate();
}


void
EventTrack::set_track_signal(const TrackSignal &tsig)
{
    if (body.config.track_signal.empty() && tsig.empty())
        return;
    body.config.track_signal.free_signals();
    // Copy over the pointers, I'm taking ownership now.
    body.config.track_signal = tsig;
    if (!body.config.track_signal.empty()
            && body.config.render.style == RenderConfig::render_none) {
        DEBUG("WARNING: got a track signal even though RenderStyle is none");
    }
    invalidate();
}


void
EventTrack::title_input_cb(Fl_Widget *w, void *arg)
{
    EventTrack *self = static_cast<EventTrack *>(arg);
    WrappedInput *input = static_cast<WrappedInput *>(w);

    // WrappedInput documents the things that trigger callback.
    switch (Fl::event()) {
    case FL_PUSH:
        self->title_focused();
        break;
    case FL_UNFOCUS:
        self->title_unfocused();
        break;
    default:
        input->update_size();
        self->do_callback();
        break;
    }
}


// Call after the title has gotten focused, to expand it if necessary.
void
EventTrack::title_focused()
{
    Fl_Window *win = window();
    // Can expand out to the right edge of the window.
    int max_w = win->w() - title_input.x();
    title_input.set_max_width(max_w);
    title_input.update_size();
    // If it expanded, TrackTile will need to redraw.
    do_callback();
}

// Call after the title has unfocused, to contract.
void
EventTrack::title_unfocused()
{
    // Just force the size back down.  It won't rewrap but that's fine.
    title_input.size(this->w(), Config::Block::track_title_height);
    const char *text = title_input.get_text();
    MsgCollector::get()->track_title(this, body.tracknum, text);
    title_input.value(text);
    // If it doesn't fit, always show the beginning.
    title_input.position(0);
    // Winds up at TrackTile::title_input_cb, which will redraw TrackTile
    // since it may have changed size.
    do_callback();
}


static float
get_max_peak(
    const std::vector<std::unique_ptr<PeakCache::MixedEntry>> &peak_entries)
{
    float max = 0;
    for (const auto &entry : peak_entries) {
        if (entry.get())
            max = std::max(max, entry->max_peak());
    }
    return max;
}


void
EventTrack::set_waveform(int chunknum, const PeakCache::Params &params)
{
    // chunknum=-1 means clear all.
    if (chunknum < 0) {
        body.peak_entries.clear();
    } else {
        if (chunknum >= util::ssize(body.peak_entries))
            body.peak_entries.resize(size_t(chunknum+1));
        if (!body.peak_entries[chunknum].get()) {
            body.peak_entries[chunknum].reset(
                new PeakCache::MixedEntry(params.start));
        }
        body.peak_entries[chunknum]->add(PeakCache::get()->load(params));
    }
    float new_peak = get_max_peak(body.peak_entries);
    if (new_peak != body.max_peak)
        body.max_peak = new_peak;
    invalidate();
}


void
EventTrack::finalize_callbacks()
{
    Config::free_haskell_fun_ptr(
        reinterpret_cast<void *>(body.config.find_events));
    body.config.track_signal.free_signals();
    body.ruler_overlay.delete_config();
}


void
EventTrack::draw()
{
    util::timing(2, "EventTrack::draw-start");
    // The selection moved, so get the cache to redraw, but not recache.
    if ((damage() & ~FL_DAMAGE_CHILD) == Track::DAMAGE_SELECTION) {
        this->clear_damage();
        body_scroll.damage(FL_DAMAGE_SCROLL);
    }
    Track::draw();
    util::timing(2, "EventTrack::Track::draw");
    selection_overlay.draw(
        x(), track_start(*this), w(), track_end(*this), body.zoom);
    util::timing(2, "EventTrack::selection_overlay");
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


// Body ////////////////////////////////

EventTrack::Body::Body(
    int tracknum,
    const EventTrackConfig &config,
    const RulerConfig &ruler_config
) :
    Fl_Widget(0, 0, 1, 1),
    tracknum(tracknum),
    suggested_width(0), // guarantee to emit msg_track_width on the first draw
    config(config), brightness(1),
    ruler_overlay(ruler_config, false)
{
    update_size();
}


ScoreTime
EventTrack::Body::time_end() const
{
    return std::max(config.time_end, ruler_overlay.time_end());
}

void
EventTrack::Body::update(const Tracklike &track)
{
    ASSERT_MSG(track.track, "updated an event track with a non-event config");

    if (track.ruler)
        ruler_overlay.set_config(false, *track.ruler);

    TrackSignal tsig = this->config.track_signal;
    Config::free_haskell_fun_ptr(
        reinterpret_cast<void *>(this->config.find_events));
    this->config = *track.track;
    update_size();
    // Copy the previous track signal over even though it might be out of date
    // now.  At the least I can't forget the pointers or there's a leak.
    this->config.track_signal = tsig;
}


void
EventTrack::Body::set_zoom(const Zoom &new_zoom)
{
    bool factor_changed = zoom.factor != new_zoom.factor;
    this->zoom = new_zoom;
    if (factor_changed)
        this->update_size();
}


void
EventTrack::Body::update_size()
{
    // I'm not sure why +4, but otherwise it's not quite long enough.
    // Maybe it has to do with the +2 in track_start()?
    this->resize(x(), y(), w(), zoom.to_pixels(time_end()) + 4);
}


// Drawing order:
// EventTrack: bg -> events -> wave -> signal -> ruler -> text -> trigger -> sel
// RulerTrack: bg ->                             ruler ->                 -> sel
void
EventTrack::Body::draw()
{
    util::timing(2, "EventTrack::Body::draw-start");
    fl_color(config.bg_color.brightness(this->brightness).fl());
    fl_rectf(x(), y(), w(), h());

    const ScoreTime t_start = ScoreTime(0);
    // TODO I can just ship over the events eagerly now, instead of a callback.
    const ScoreTime t_end = time_end();

    // The results are sorted by (event_start, rank), so lower ranks always
    // come first.
    Event *events;
    int *ranks;
    util::timing(2, "EventTrack::find_events-start");
    int count = this->config.find_events(&t_start, &t_end, &events, &ranks);
    util::timing(2, "EventTrack::find_events");

    vector<TextBox> boxes(count);
    vector<int> triggers(count);

    const int wrap_width = w() - 3; // minus some padding to avoid the edge

    const int start = track_start(*this);
    const int end = track_end(*this);
    for (int i = 0; i < count; i++) {
        triggers[i] = start + this->zoom.to_pixels(events[i].start);
        const SymbolTable::Wrapped wrapped(wrap_text(events[i], wrap_width));
        Align align = ranks[i] > 0 ? Right : Left;
        boxes[i] = compute_text_box(
            events[i], wrapped, x(), triggers[i], wrap_width, align,
            start, end);
    }
    update_suggested_width(boxes);
    util::timing(2, "EventTrack::compute_text_boxes");


    // Actually start drawing.

    this->draw_event_boxes(events, ranks, count, triggers);
    util::timing(2, "EventTrack::draw_event_boxes");
    this->draw_waveforms(start, end, t_start);
    util::timing(2, "EventTrack::draw_waveforms");
    this->draw_signal(start, end, t_start);
    util::timing(2, "EventTrack::draw_signal");

    IRect box(x(), start, w(), end - start);
    // TODO later ruler_overlay will always draw from 0
    this->ruler_overlay.draw(box, Zoom(ScoreTime(0), zoom.factor), box);
    util::timing(2, "EventTrack::ruler_overlay");

    for (int i = 0; i < count; i++) {
        Align align = ranks[i] > 0 ? Right : Left;
        draw_upper_layer(i, events, align, boxes, triggers);
    }
    util::timing(2, "EventTrack::draw_upper_layer");

    // Free text, allocated on the haskell side.
    if (count) {
        for (int i = 0; i < count; i++) {
            if (events[i].text)
                free(const_cast<char *>(events[i].text));
        }
        free(events);
        free(ranks);
        util::timing(2, "EventTrack::free_text");
    }
}


// Figure out the minimum width needed for all of the text boxes to fit.
// This is made tricky because they wrap, and because there are Left and
// Right ones.
static int
compute_suggested_width(const vector<EventTrack::TextBox> &boxes)
{
    const int gap = 5;
    // Right text doesn't line up flush with the right side of the track.
    const int gap_r = 3;
    int width = minimum_suggested_width;

    int right_w = 0;
    int left_w = 0;
    int left_b = 0;
    int right_b = 0;

    // This is conservative since it will do all Left wrapped words before
    // doing the right ones.  The effect should be that it considers them all
    // to overlap, so it'll take the longest ones.  If I wanted to be more
    // aggressive, I could try to iterate the lines in increasing y.
    for (const EventTrack::TextBox &box : boxes) {
        for (const auto &line : box.lines) {
            // DEBUG("line: " << line);
            if (box.align == EventTrack::Left) {
                left_w = line.second.w;
                left_b = line.second.b();
                if (right_b > line.second.y) {
                    // DEBUG("l over: " << (left_w + gap + right_w));
                    width = std::max(width, left_w + gap + right_w);
                } else {
                    // DEBUG("l: " << left_w);
                    width = std::max(width, left_w + gap);
                }
            } else {
                right_w = line.second.w + gap_r;
                right_b = line.second.b();
                if (left_b > line.second.y) {
                    // DEBUG("r over: " << (left_w + gap + right_w));
                    width = std::max(width, left_w + gap + right_w);
                } else {
                    // DEBUG("r: " << left_w);
                    width = std::max(width, right_w + gap);
                }
            }
            // DEBUG("BOX: " << width);
        };
    }
    // DEBUG("--- final: " << width);
    return width;
}


void
EventTrack::Body::update_suggested_width(const vector<TextBox> &boxes)
{
    int w = compute_suggested_width(boxes);
    if (w != this->suggested_width) {
        this->suggested_width = w;
        MsgCollector::get()->track(UiMsg::msg_track_width, this, tracknum);
    }
}


// Draw event boxes.  Rank >0 boxes are not drawn since I'd have to figure out
// overlaps and they're meant to be used with control tracks anyway.
void
EventTrack::Body::draw_event_boxes(
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
    std::vector<std::unique_ptr<PeakCache::MixedEntry>> &peak_entries,
    int chunknum)
{
    for (; chunknum < util::ssize(peak_entries); chunknum++) {
        if (peak_entries[chunknum].get())
            return &peak_entries[chunknum]->start;
    }
    return nullptr;
}


// min_y and max_y are already in absolute pixels
void
EventTrack::Body::draw_waveforms(int min_y, int max_y, ScoreTime start)
{
    const float amplitude_scale = max_peak == 0 ? 1 : 1 / max_peak;
    if (peak_entries.empty())
        return;

    const int min_x = x() + 2;
    const int max_x = x() + w() - 2;

    std::shared_ptr<const std::vector<float>> cache;
    double y = min_y;
    int chunknum = -1; // -1 means uninitialized
    int i = 0; // peak index within a chunk, reset when I reach a new chunk
    const ScoreTime *next_start = get_next_start(peak_entries, 0);
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
            if (chunknum >= util::ssize(peak_entries))
                break;
            if (peak_entries[chunknum].get()) {
                time = peak_entries[chunknum]->start;
                y = zoom.to_pixels_d(time) + track_start(*this);
                next_start = get_next_start(peak_entries, chunknum+1);
                cache = peak_entries[chunknum]->at_zoom(zoom.factor);
            } else {
                // Someone put in waveform chunks out of order, and this one is
                // missing.
                cache = nullptr;
            }
        }
        // Out of peaks on the last chunk.
        if (!next_start && cache.get() && i >= util::ssize(*cache))
            break;
        // i > cache->size() means I ran out of cached peaks before getting to
        // the next chunk start.  This can happen for a sample or two due to
        // pixel roundoff (TODO where exactly?)
        if (cache.get() && i < util::ssize(*cache)) {
            float x = (*cache)[i];
            x *= amplitude_scale;
            x = x * (max_x - min_x) + min_x;
            // DEBUG("v: (" << x << ", " << y << ")");
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
EventTrack::Body::draw_signal(int min_y, int max_y, ScoreTime start)
{
    if (config.render.style == RenderConfig::render_none)
        return;

    const TrackSignal &tsig = config.track_signal;
    const int found = tsig.find_sample(start);
    if (found == tsig.length)
        return;

    const int y = track_start(*this);
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
    Find the number of vertical pixels available to draw the event text at the
    given index.  It will be positive but is understood to extend upwards from
    the event start of a negative event and downwards for a positive one.
    0 is returned to prevent text from drawing at all.

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
            while (prev<util::ssize(boxes) && events[prev].start == event.start)
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
            for (++next; next < util::ssize(boxes); ++next) {
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
EventTrack::Body::draw_upper_layer(
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
    util::timing(2, "drawable_pixels");
    TEXT("drawable pixels for " << events[index] << ": " << drawable);

    const int track_min = track_start(*this);
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
    util::timing(2, "draw_trigger");

    // // Draw all boxes to debug:
    // for (auto &line : boxes[index].lines)
    //     f_util::draw_rect(line.second, Color(0, 0, 0xff));

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
        for (int i = boxes[index].lines.size() - 1;
            remaining >= 0 && i >= 0;
            remaining -= boxes[index].lines[i].second.h, --i)
        {
            const std::string &line = boxes[index].lines[i].first;
            const IRect &box = boxes[index].lines[i].second;
            // Even if remaining==0, I still have stuff left to draw, so I need
            // to draw the abbreviation bar at least.
            TEXT("NEGATIVE: " << box << ": " << line << " left "
                << remaining << " - " << box.h);
            draw_text_line(line, box, style);
            // f_util::draw_rect(box, Color(0, 0xff, 0xff));
            if (remaining < box.h) {
                f_util::draw_rectf(
                    IRect(x(), bottom - drawable, w(), 2),
                    Config::abbreviation_color);
            }
            if (i == 0 && line.back() == ' ') {
                f_util::draw_rectf(
                    IRect(box.r() - 3, box.y + 1, 2, box.h),
                    Config::trailing_space_color);
            }
        }
    } else { // event is positive
        const int top = boxes[index].lines[0].second.y;
        f_util::ClipArea clip(IRect(x(), top, w(), drawable));
        int remaining = drawable;
        for (int i = 0;
            remaining >= 0 && i < util::ssize(boxes[index].lines);
            remaining -= boxes[index].lines[i].second.h, ++i)
        {
            const std::string &line = boxes[index].lines[i].first;
            const IRect &box = boxes[index].lines[i].second;

            TEXT("POSITIVE: " << box << ": " << line << " left "
                << remaining << " - " << box.h);
            draw_text_line(line, box, style);
            // f_util::draw_rect(box, Color(0, 0xff, 0xff));
            if (remaining < box.h) {
                f_util::draw_rectf(
                    IRect(x(), top + drawable - 2, w(), 2),
                    Config::abbreviation_color);
            }
            if (i+1 == util::ssize(boxes[index].lines) && line.back() == ' ') {
                f_util::draw_rectf(
                    IRect(box.r() - 3, box.y + 1, 2, box.h),
                    Config::trailing_space_color);
            }
        }
    }
    util::timing(2, "draw_text_line");
}
