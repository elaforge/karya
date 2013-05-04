#include <sstream>
#include <math.h>
#include "config.h"
#include "util.h"
#include "alpha_draw.h"

#include "SeqInput.h"
#include "EventTrack.h"
#include "SymbolTable.h"
#include "MsgCollector.h"


// #define DEBUG(X) ;

// Hack for debugging.
#define SHOW_RANGE(r) (r).y << "--" << (r).b()

// The color of events at a non-zero rank is scaled by this.
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


int
TrackSignal::time_at(const ZoomInfo &zoom, int i) const
{
    ASSERT_MSG(signal, "time_at on empty track signal");
    return zoom.to_pixels(from_real(signal[i].time) - zoom.offset);
}


// Get the val at the given index, normalized between 0--1.
double
TrackSignal::val_at(int i) const
{
    ASSERT_MSG(signal, "val_at on empty track signal");
    return normalize(this->val_min, this->val_max, signal[i].val);
}


void
TrackSignal::calculate_val_bounds() {
    // Only automatically scale the bounds for pitch signals.
    if (this->is_pitch_signal) {
        val_min = 9999;
        val_max = 1;
        for (ControlSample *s = signal; s < signal + length; s++) {
            val_max = std::max(val_max, s->val);
            val_min = std::min(val_min, s->val);
        }
    } else {
        val_min = 0;
        val_max = 1;
        for (ControlSample *s = signal; s < signal + length; s++) {
            val_max = std::max(val_max, s->val);
        }
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
    title_input(NULL),
    bg_box(0, 0, 1, 1),
    overlay_ruler(ruler_config, false)
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
    // DEBUG("resize " << rect(this) << " -> " << IRect(x, y, w, h));
    // Don't call Fl_Group::resize because I just did the sizes myself.
    Fl_Widget::resize(x, y, w, h);
    this->overlay_ruler.resize(x, y, w, h);
    this->bg_box.resize(x, y, w, h);
}

void
EventTrackView::set_title(const char *title)
{
    this->title_input->set_text(title);
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
    if (this->config.render.style == RenderConfig::render_none) {
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


// I redraw the scroll revealed area separately, so pass a dummy to fl_scroll.
static void dummy_scroll_draw(void *, int, int, int, int) {}

void
EventTrackView::draw()
{
    IRect draw_area = rect(this);

    // DEBUG("event track damage " << show_damage(damage()));
    // Fast scrolling is disabled, because it's hard to get right, and it
    // doesn't actually seem to improve performance.
    // TODO either fix or remove
    if (false && this->damage() == FL_DAMAGE_SCROLL) {
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
            ScoreTime bottom = zoom.offset + zoom.to_time(draw_area.h);
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

    // This is more than one pixel, but otherwise I draw on top of the bevel on
    // retina displays.
    IRect inside_bevel = rect(this);
    inside_bevel.x += 2; inside_bevel.w -= 3;
    inside_bevel.y += 2; inside_bevel.h -= 3;
    ClipArea clip_area2(inside_bevel);

    this->draw_area();
    overlay_ruler.damaged_area.w = overlay_ruler.damaged_area.h = 0;
    this->last_offset = this->zoom.offset;
}


/*
static void
show_found_events(ScoreTime start, ScoreTime end, Event *events, int count)
{
    printf("%.2f-%.2f: %d events:", start.scale(1), end.scale(1), count);
    for (int i = 0; i < count; i++) {
        printf(" (%.2f %s)", events[i].start.scale(1), events[i].text);
    }
    printf("\n");
}
*/


void
EventTrackView::draw_area()
{
    IRect clip = clip_rect(rect(this));
    // Expand by a pixel, otherwise I miss little slivers on retina displays.
    clip.y--;
    clip.h++;
    int y = this->track_start();

    ScoreTime start = this->zoom.to_time(clip.y - y);
    ScoreTime end = start + this->zoom.to_time(clip.h);
    start = start + this->zoom.offset;
    end = end + this->zoom.offset;
    // DEBUG("TRACK CLIP: " << start << "--" << end << ", "
    //         << clip.y << "--" << clip.b()
    //         << " zoom " << zoom);

    Event *events;
    int *ranks;
    int count = this->config.find_events(&start, &end, &events, &ranks);
    // show_found_events(start, end, events, count);

    int *offsets = new int[count];
    for (int i = 0; i < count; i++) {
        offsets[i] = y + this->zoom.to_pixels(
            events[i].start - this->zoom.offset);
    }

    // Draw event boxes.  Rank >0 boxes are not drawn since I'd have to figure
    // out overlaps and they're meant to be used with control tracks anyway.
    for (int i = 0; i < count; i++) {
        if (ranks[i])
            continue;
        const Event &event = events[i];
        int height = this->zoom.to_pixels(event.duration);
        // If this event touches the next one, make it's box one pixel short,
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
        int y0 = std::min(offsets[i], offsets[i] + height);
        int y1 = std::max(offsets[i], offsets[i] + height);

        Color c = StyleTable::get()->get(event.style_id)
            ->event_color.brightness(this->brightness);
        if (event.duration < ScoreTime(0))
            c = c.brightness(negative_duration_brightness);
        fl_color(color_to_fl(c));
        fl_rectf(this->x() + 1, y0, this->w() - 2, y1-y0);
    }

    this->draw_signal(clip.y, clip.b(), start);

    IRect prev_unranked_rect(0, 0, 0, 0);
    // Draw the upper layer (event start line, text).
    for (int i = 0; i < count; i++) {
        int rank = ranks[i];
        int next_offset = MAX_PIXEL;
        int prev_offset = i == 0 ? MIN_PIXEL : offsets[i-1];
        // TODO negative events should do this for the prev_offset
        for (int j = i+1; j < count; j++) {
            if ((rank && ranks[j]) || (!rank && !ranks[j])) {
                next_offset = offsets[j];
                break;
            }
        }
        prev_unranked_rect = this->draw_upper_layer(offsets[i], events[i],
            rank, prev_offset, next_offset, prev_unranked_rect);
    }
    if (count) {
        for (int i = 0; i < count; i++) {
            if (events[i].text)
                free(const_cast<char *>(events[i].text));
        }
        free(events);
        free(ranks);
    }
    delete[] offsets;

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

    const int y = this->track_start();
    // TODO alpha not supported, I'd need a non-portable drawing routine for
    // it.
    Fl_Color signal_color =
        color_to_fl(config.render.color.brightness(brightness));

    // Account for both the 1 pixel track border and the width of the line.
    const int min_x = x() + 2;
    const int max_x = x() + w() - 2;
    int prev_xpos = -1;
    const SymbolTable::Style style(
        Config::font, Config::font_size::pitch_signal, FL_BLACK);

    for (int i = found; i < tsig.length; i++) {
        int offset = y + tsig.time_at(zoom, i);
        // Skip coincident samples, or at least ones that are too close.
        int next_offset;
        if (i+1 >= tsig.length) {
            // Out of signal, last sample goes to the bottom.
            next_offset = max_y;
        } else {
            next_offset = y + tsig.time_at(zoom, i+1);
        }
        // If the next sample is too close then don't draw this one.
        // TODO I'd also like to skip samples that are too close to make
        // much difference, but I still have to draw some or a series of
        // close samples would be omitted entirely, so it's quite a bit
        // more complicated.  This is a similar problem to the event text
        // display.
        if (next_offset <= offset)
            continue;
        double val = tsig.val_at(i);
        int xpos = floor(::scale(double(min_x), double(max_x),
            ::clamp(0.0, 1.0, val)));
        // DEBUG("sample " << i << " val " << val << " offset " << offset);

        // I originally wanted to draw the signal as one big line in a separate
        // pass, eliminating the jump from the previous xpos if the distance is
        // below a threshold, but it turned out it seems to be slower and
        // uglier, and ugly artifacts appear as seperate segments of the line
        // pop across the threshold.
        //
        // I'm already skipping samples when they are too close together so I
        // don't think I have to worry about dense signals.
        if (next_offset > min_y && offset <= max_y) {
            fl_color(signal_color);
            switch (config.render.style) {
            case RenderConfig::render_line:
                fl_line_style(FL_SOLID | FL_CAP_ROUND, 2);
                // Don't draw a jump from prev_xpos if it didn't exist.
                if (prev_xpos == -1) {
                    fl_line(xpos, offset, xpos, next_offset);
                } else {
                    fl_line(
                        prev_xpos, offset, xpos, offset, xpos, next_offset);
                }
                break;
            case RenderConfig::render_filled:
                fl_line_style(FL_SOLID | FL_CAP_ROUND, 0);
                // For some reason, on OS X at least, height 1 rects don't get
                // drawn.
                fl_rectf(min_x, offset, xpos - min_x,
                    (next_offset - offset) + 1);
                break;
            case RenderConfig::render_none:
                // shouldn't get here since the function returns early
                ASSERT_MSG(0, "tried to draw a signal with render_none");
                break;
            default:
                DEBUG("unknown render style: " << config.render.style);
            }
        }
        // DEBUG("draw " << i << " @ " << offset << "--" << next_offset);

        prev_xpos = xpos;
        if (offset > max_y)
            break;
    }
    // Reset line style to not mess up other draw routines.
    fl_line_style(0);
}


IRect
EventTrackView::draw_upper_layer(int offset, const Event &event, int rank,
        int prev_offset, int next_offset, const IRect &prev_unranked_rect)
{
    // So the overlap stuff is actually pretty tricky.  I want to hide
    // overlapping text so it doesn't get into an unreadable jumble.  It's also
    // important that the algorithm be consistent and not require context,
    // because this will be called to redraw various small fragments.  This
    // is further complicated by the fact that text can be all different sizes
    // and that there is text aligned to the left (unranked) and lower priority
    // text aligned to the right (ranked).  Also, text of negative events goes
    // above the trigger line, while for positive events it goes below.
    //
    // So the current plan is to for positive events to hide text if it will
    // overlap with the offset of the next ranked or unranked event, as
    // appropriate.  Negative events are the same but for the previous offset.
    // Ranked events have the additional restriction that they can't overlap
    // with the previous text rect, so if they bump into it horizontally they
    // won't be drawn.
    //
    // This draws negative ranked text incorrectly since I should be checking
    // the next_unranked_rect instead of the prev one, but I can fix that if
    // it ever becomes a problem.
    //
    // This scheme hides all text that might overlap with other text, so it
    // hides a lot of text that could be displayed.  I experimented with a
    // scheme that recorded where all text was drawn to try to display more,
    // but in addition to being complicated it wound up looking cluttered and
    // ugly, and I don't think knowing some random note in the middle of a run
    // is actually that useful.  If I want to give hints about the contents of
    // small notes I'll have to come up with some other mechanism, like color
    // coding.

    const EventStyle *event_style = StyleTable::get()->get(event.style_id);
    const SymbolTable::Style style(event_style->font, event_style->size,
        color_to_fl(rank ? event_style->text_color.brightness(rank_brightness)
            : event_style->text_color));

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
            // I think this should be next_ranked_rect, but that's too much of
            // a bother to get.
            draw_text = text_rect.y >= prev_offset;
            if (rank && text_rect.intersects(prev_unranked_rect))
                draw_text = false;
        } else {
            draw_text = text_rect.b() <= next_offset;
            if (rank && text_rect.intersects(prev_unranked_rect))
                draw_text = false;
            // DEBUG(offset << " d " << draw_text << " next " << next_offset
            //     << " prev_unranked_rect " << prev_unranked_rect);
            // DEBUG("text rect: " << text_rect);
        }
    }

    // The various pixel tweaks in here were determined by zooming in and
    // squinting.

    // DEBUG("offset " << offset << ", text_rect " << text_rect);
    // fl_color(FL_BLUE);
    // fl_rect(text_rect.x, text_rect.y, text_rect.w, text_rect.h);

    // Draw trigger line.  This will draw ranked trigger lines over unranked
    // ones, but it's ok because ranked ones start in the middle of the track.
    Color trigger_c;
    if (draw_text || !event.text)
        trigger_c = Config::event_trigger_color;
    else
        trigger_c = Config::abbreviation_color;
    fl_color(color_to_fl(trigger_c));
    fl_line_style(FL_SOLID, 0);
    if (rank) {
        fl_line(x() + w()/2, offset, x()+w() - 2, offset);
    } else {
        fl_line(x() + 1, offset, x()+w() - 2, offset);
    }

    if (draw_text) {
        // Rotation and word wrapping only apply to positive events.
        // I would need additional code to get them working for negative
        // events, since the text goes up rather than down.
        int track_width = w() - 3;
        bool too_wide = text_rect.w > track_width;
        bool vertical = event.is_positive()
            && config.text_wrap == EventTrackConfig::rotate
            && !rank && too_wide && text_rect.y + text_rect.w < next_offset;
        if (config.text_wrap == EventTrackConfig::wrap
            && event.is_positive() && !rank)
        {
            // The *_wrapped functions start at upper left, not lower left.
            IPoint draw_pos(text_rect.x, text_rect.y - 2);
            SymbolTable::get()->draw_wrapped(
                event.text, draw_pos, track_width,
                next_offset - draw_pos.y, style);
        } else {
            // Due to boundary issues, drawing text that touches the bottom of
            // a box means drawing one above the bottom.  I don't totally
            // understand this.
            IPoint draw_pos = vertical
                ? IPoint(x() + w()/2 - text_rect.h/2, text_rect.y)
                : IPoint(text_rect.x, text_rect.b() - 1);
            SymbolTable::get()->draw(event.text, draw_pos, style, vertical);
        }
    }
    if (rank) {
        return prev_unranked_rect;
    } else {
        return text_rect;
    }
}


std::string
EventTrackView::dump() const
{
    std::ostringstream out;
    out << "type event title " << show_string(this->get_title());
    return out.str();
}
