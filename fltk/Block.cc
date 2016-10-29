// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <sstream>
#include <vector>

#include "EventTrack.h"
#include "MsgCollector.h"
#include "RulerTrack.h"
#include "Track.h"
#include "f_util.h"
#include "utf8.h"
#include "util.h"

#include "Block.h"

// Try to avoid running into the little resize tab on the mac.
// Actually, it's ok if it overlaps, since it's just the status field.
static const int mac_resizer_width = 0;

// Multiply mousewheel scroll pixels by this.
static const double mousewheel_time_scale = 3;
static const double mousewheel_track_scale = 3;

// If you create windows that are too small, the initial widget placement gets
// messed up.  Rather than trying to figure all that out, just forbid windows
// below a certain size.
static const int min_height =
    Config::Block::block_title_height
    + Config::Block::track_title_height
    + Config::Block::skel_height + Config::Block::status_size
    + Config::Block::sb_size;
static const int min_width = Config::Block::sb_size + 10;


Block::Block(int x, int y, int w, int h,
        const BlockConfig &config, const char *window_title) :
    Fl_Group(x, y, w, h),

    title(0, 0, 1, 1, true),
    status_line(0, 0, 1, 1),
    body(0, 0, 1, 1),
        body_resize_group(0, 0, 1, 1, "resize group"),
        skel_box(0, 0, 1, 1),
        skel_display_scroll(0, 0, 1, 1),
            skel_display(0, 0, 1, 1),
        ruler_group(0, 0, 1, 1),
            track_box(0, 0, 1, 1),
            sb_box(0, 0, 1, 1),
            time_sb(0, 0, 1, 1),
            ruler_track(nullptr), // filled in later by insert_track
        track_group(0, 0, 1, 1),
            track_sb(0, 0, 1, 1),
            track_scroll(0, 0, 1, 1),
                track_tile(0, 0, 1, 1, Config::block_bg,
                        Config::Block::track_title_height)
{
    // The sizes of 1 are so that groups realize that their children are inside
    // of them.  The real resizing will be done in set_widget_sizes
    current(0); // done adding widgets
    // fltk's automatic group stuff gets most of the hierarchy above, but not
    // all of it.
    body.add(skel_box);
    body.add(skel_display_scroll);
    body.add(ruler_group);
    body.add(track_group);
    body_resize_group.hide();

    // Insert a placeholder for the ruler.  The size will be set again by
    // set_widget_sizes, but that's ok.
    this->no_ruler = new Divider(DividerConfig(Color::black));
    this->replace_ruler_track(no_ruler, 0);

    // Remove the status line from the tab focus list.  I bypass that anyway
    // so this doesn't have any effect.
    // status_line.visible_focus(false);
    skel_box.box(FL_FLAT_BOX);
    status_line.box(FL_FLAT_BOX);
    track_box.box(FL_FLAT_BOX);
    sb_box.box(FL_FLAT_BOX);
    time_sb.callback(Block::scrollbar_cb, static_cast<void *>(this));
    track_sb.callback(Block::scrollbar_cb, static_cast<void *>(this));
    body.callback(Block::track_tile_cb, static_cast<void *>(this));
    track_tile.callback(Block::track_tile_cb, static_cast<void *>(this));

    skel_display.color(Config::skeleton_display_bg.fl());
    skel_display.set_title(window_title);

    resizable(body);
    body.resizable(body_resize_group);
    track_group.resizable(track_scroll);

    title.callback(Block::title_cb_dispatch, static_cast<void *>(this));
    title.hide(); // It starts with no text.

    this->set_widget_sizes();
    this->set_config(config, true);

    // See dummy_ruler_size in set_widget_sizes.
    this->set_ruler_width(0);

    // Initialize zoom to default.  This will cause zooming children to
    // properly position their widgets.
    this->set_zoom(this->zoom);
    this->update_scrollbars();
}


Block::~Block()
{
    delete no_ruler; // drop no tea
}


int
Block::handle(int evt)
{
    if (evt == FL_MOUSEWHEEL) {
        if (Fl::event_dy()) {
            ScoreTime scroll = this->zoom.to_time(
                Fl::event_dy() * mousewheel_time_scale);
            ScoreTime old = this->zoom.offset;
            set_zoom(ZoomInfo(this->zoom.offset + scroll, this->zoom.factor));
            if (this->zoom.offset != old)
                MsgCollector::get()->block(UiMsg::msg_time_scroll, this);
        }
        if (Fl::event_dx()) {
            int scroll_y = Fl::event_dx() * mousewheel_track_scale;
            int old = this->get_track_scroll();
            this->set_track_scroll(old + scroll_y);
            if (this->get_track_scroll() != old) {
                MsgCollector::get()->block(UiMsg::msg_track_scroll, this);
            }

        }
        return 1;
    }
    return Fl_Group::handle(evt);
}


void
Block::resize(int X, int Y, int W, int H)
{
    int old_w = this->w();
    Fl_Group::resize(X, Y, W, H);
    status_line.size(w() - mac_resizer_width, status_line.h());

    // Only check for title height changes if width has changed.  Otherwise,
    // the position(0, 0) call in set_widget_sizes causes recursion.
    if (old_w != W && title.visible() && title.h() != title.text_height())
        set_widget_sizes();

    // If I'm growing, I will have changed the visible area, so recalculate
    // the scroll boundaries.
    this->set_track_scroll(this->get_track_scroll());
    this->update_scrollbars();
}


void
Block::set_widget_sizes()
{
    // I update everything, even if update_all is false.  It's probably not
    // worth trying to skip things that haven't changed for this.
    //
    // If I resize from parent to child, then children get a lot of spurious
    // resizes as their parents move them around.  On the other hand, if
    // we go the other way, parents mess up their children.
    // Spurious resizes it is.
    //
    // I don't totally understand what's going on here.  The window's x() and
    // y() are the values that are passed to the constructor.  However, all
    // widget positions are measured relative to 0, so using x() and y() here
    // will screw up placement.  In addition, using (0, 0) here screws up
    // resizing.
    //
    // Setting the position to 0 fixes all that and doesn't even seem to move
    // the window around.
    int wx = 0, wy = 0;
    this->position(0, 0);

    int title_h = title.visible() ? title.text_height() : 0;
    title.resize(wx, wy, w(), title_h);

    status_line.resize(
        wx, h() - Config::Block::status_size,
        w() - mac_resizer_width, Config::Block::status_size);
    body.resize(wx, wy + title_h, w(), h() - title_h - status_line.h());
    body_resize_group.resize(
        body.x() + Config::Block::sb_size, body.y(),
        body.w() - Config::Block::sb_size, body.h());

    int ruler_group_w = Config::Block::sb_size + ruler_track->w();
    skel_box.resize(
        body.x(), body.y(),
        ruler_group_w, Config::Block::skel_height);
    skel_display_scroll.resize(
        skel_box.x() + skel_box.w(), skel_box.y(),
        body.w() - ruler_group_w, Config::Block::skel_height);
    IRect p = f_util::rect(skel_display_scroll);
    skel_display.resize(p.x, p.y, p.w, p.h);

    // Re-use the existing ruler_track width so it is maintained across calls
    // to set_widget_sizes.  It has to be at least 1, or else the 'body' Fl_Tile
    // won't resize properly.
    ruler_group.resize(
        wx, body.y() + Config::Block::skel_height,
        ruler_group_w, body.h() - Config::Block::skel_height);
    p = f_util::rect(ruler_group);
    track_group.resize(p.r(), p.y, body.w() - p.w, p.h);

    // The track_box looks taller than just the track titles because it's
    // always the same color as the skel_box.
    track_box.resize(p.x, p.y, p.w, Config::Block::track_title_height);
    sb_box.resize(
        p.x, p.b() - Config::Block::sb_size,
        p.w, Config::Block::sb_size);

    time_sb.set_orientation(Scrollbar::vertical);
    track_sb.set_orientation(Scrollbar::horizontal);

    time_sb.resize(p.x, p.y + track_box.h(),
        Config::Block::sb_size, p.h - track_box.h() - sb_box.h());
    ruler_track->resize(p.x + time_sb.w(), p.y + track_box.h(),
        ruler_track->w(), time_sb.h());

    p = f_util::rect(track_group);
    track_sb.resize(p.x, p.b() - Config::Block::sb_size,
        p.w, Config::Block::sb_size);
    track_scroll.resize(p.x, p.y, p.w, p.h - track_sb.h());
    track_tile.resize(track_scroll.x(), track_scroll.y(),
            track_scroll.w(), track_scroll.h());

    this->track_tile.set_title_height(Config::Block::track_title_height);

    // This is overhead required by fltk when you resize anything manually.
    init_sizes();
    body.init_sizes();
    ruler_group.init_sizes();
    track_group.init_sizes();
    track_scroll.init_sizes();
    track_tile.init_sizes();

    this->update_scrollbars();
    this->redraw();
}


void
Block::set_ruler_width(int width)
{
    ASSERT(0 <= width);
    if (this->ruler_track->w() == width)
        return;

    int x = this->ruler_track->x();
    this->body.position(x + ruler_track->w(), body.y(), x + width, body.y());
    this->body.init_sizes();
}


static void
set_block_box(Fl_Box &box, const BlockBox &b)
{
        box.color(b.color.fl());
        if (b.c == ' ')
            box.copy_label(nullptr);
        else
            box.copy_label(utf8::encode(b.c));
        box.redraw();
}


void
Block::set_config(const BlockConfig &config, bool update_all)
{
    const BlockConfig &old = this->config;
    if (update_all || old.skel_box != config.skel_box)
        set_block_box(skel_box, config.skel_box);
    if (update_all || old.track_box != config.track_box)
        set_block_box(track_box, config.track_box);
    if (update_all || old.sb_box != config.sb_box)
        set_block_box(sb_box, config.sb_box);
    this->config = config;
}


void
Block::set_skeleton(const SkeletonConfig &skel)
{
    std::vector<int> widths(track_tile.tracks());
    for (int i = 0; i < track_tile.tracks(); i++) {
        widths[i] = track_tile.get_track_width(i);
    }
    skel_display.set_config(skel, widths);
}


void
Block::set_skeleton_display_bg(const Color &color)
{
    if (color.fl() != skel_display.color()) {
        skel_display.color(color.fl());
        skel_display.redraw();
    }
}


void
Block::set_zoom(const ZoomInfo &zoom)
{
    // This function, and hence set_zoom_attr below, is called from the outside
    // to set the zoom.  Therefore, no msg should be sent back out, since I
    // don't need to tell the outside what it already knows.
    this->set_zoom_attr(zoom);
    this->update_scrollbars();
}


void
Block::set_zoom_attr(const ZoomInfo &new_zoom)
{
    ZoomInfo clamped = new_zoom;
    // Clip offset to be positive, and quantize it to conform exactly to
    // a pixel boundary.  Otherwise, some events may move 1 pixel while others
    // move 2 pixels, which messes up the blit-oriented scrolling.
    clamped.offset = std::max(ScoreTime(0), clamped.offset);
    // -4 to pretend that the visible area is a bit smaller than it is.
    // This in turns lets me scroll down a little bit past the end.  Otherwise,
    // events right at the end are cut off.
    ScoreTime visible = clamped.to_time(track_tile.visible_pixels().y - 4);
    // make this a bit bigger
    ScoreTime max_offset = track_tile.time_end() - visible;
    clamped.offset = util::clamp(ScoreTime(0), max_offset, clamped.offset);
    if (clamped == this->zoom)
        return;
    this->zoom = clamped;
    this->track_tile.set_zoom(zoom);
    this->ruler_track->set_zoom(zoom);
}


int
Block::get_track_scroll() const
{
    return -track_scroll.get_offset().x;
}


void
Block::set_track_scroll(int offset)
{
    int track_end = this->track_tile.track_end();
    int max_offset = std::max(0, track_end - this->track_scroll.w());
    offset = util::clamp(0, max_offset, offset);
    // offset = std::min(max_offset, offset);

    // If you update this, also update scrollbar_cb!
    IPoint scroll_offset(-offset, 0);
    if (scroll_offset == this->track_scroll.get_offset())
        return;
    this->track_scroll.set_offset(scroll_offset);
    this->skel_display_scroll.set_offset(scroll_offset);
    this->update_scrollbars();
}


IPoint
Block::get_padding() const
{
    // Subtract, rather than try to remember every widget to add them up.
    IPoint p = track_tile.visible_pixels();
    return IPoint(w() - (p.x + ruler_track->w()), h() - p.y
        + Config::Block::extra_time_padding);
}


void
Block::set_selection(
    int selnum, int tracknum, const std::vector<Selection> &sels)
{
    if (!(0 <= tracknum && tracknum < tracks())) {
        DEBUG("Block::set_selection: tracknum out of range: " << tracknum);
        return;
    }
    if (tracknum == 0) {
        track_at(0)->set_selection(selnum, 0, sels);
    } else {
        track_at(tracknum)->set_selection(selnum, tracknum-1, sels);
    }
    // Since the selection counts toward time_end.
    this->update_scrollbars();
}


void
Block::set_title(const char *s)
{
    title.set_text(s);
    title_cb();
}


void
Block::insert_track(int tracknum, const Tracklike &track, int width)
{
    Track *t;

    // DEBUG("view insert at " << tracknum);
    if (track.track) {
        t = new EventTrack(*track.track, *track.ruler);
    } else if (track.ruler) {
        t = new RulerTrack(*track.ruler);
    } else {
        t = new Divider(*track.divider);
    }
    this->insert_track_view(tracknum, t, width);
}


// Even though the ruler track is not in the TrackTile, I go to some effort
// to create the illusion that it's just another track, which means some extra
// work to push other tracks over when another track is inserted into or
// removed from the ruler track.

void
Block::remove_track(int tracknum)
{
    if (tracknum != 0) {
        tracknum--; // adjust to be relative to the first non-ruler track
        Track *t = track_tile.remove_track(tracknum);
        t->finalize_callbacks();
        delete t;

        this->update_scrollbars();
        // I don't want to delete the track from the skeleton because if this
        // is a replace then the skeleton can be preserved.  This happens when
        // a track is collapsed.  Otherwise, the skeleton is out of date and
        // there should be a set_skeleton soon.
        this->skel_display.set_width(tracknum, 0);
    } else if (this->tracks() == 1) {
        if (this->ruler_track != this->no_ruler) {
            Track *t = this->replace_ruler_track(this->no_ruler, 0);
            t->finalize_callbacks();
            delete t;
        }
    } else {
        Track *t = track_tile.remove_track(0);
        Track *removed = this->replace_ruler_track(t, t->w());
        if (removed != this->no_ruler) {
            removed->finalize_callbacks();
            delete removed;
        }
    }
}

void
Block::set_display_track(int tracknum, const DisplayTrack &dtrack)
{
    ASSERT(0 <= tracknum && tracknum < this->tracks());
    if (tracknum > 0) {
        this->skel_display.set_status(
            tracknum-1, dtrack.status1, dtrack.status2, dtrack.status_color);
    }
    this->track_at(tracknum)->set_event_brightness(dtrack.event_brightness);
    this->set_track_width(tracknum, dtrack.width);
}


void
Block::floating_open(int tracknum, ScoreTime pos, const char *text,
    int select_start, int select_end)
{
    ASSERT(0 <= tracknum && tracknum < this->tracks());
    // Unlike all the other TrackTile methods, this one doesn't subtract 1
    // from the tracknum.  Documented in TrackTile::floating_open.
    track_tile.floating_open(tracknum, pos, text, select_start, select_end);
}


void
Block::floating_insert(const char *text)
{
    track_tile.floating_insert(text);
}


void
Block::insert_track_view(int tracknum, Track *track, int width)
{
    if (tracknum == 0) {
        Track *replaced = this->replace_ruler_track(track, width);
        // Manually emulate that inserting a track will bump the others over.
        if (replaced != this->no_ruler)
            track_tile.insert_track(0, replaced, replaced->w());
        this->ruler_track->set_zoom(this->zoom);
        // Changing the ruler will change the track area.
        MsgCollector::get()->block(UiMsg::msg_resize, this);
    } else {
        tracknum--; // adjust to be relative to the first non-ruler track
        track_tile.insert_track(tracknum, track, width);
        this->track_tile.set_zoom(this->zoom);
        // Restore the width as per the comment in 'remove_track'.
        this->skel_display.set_width(tracknum, width);
    }
    this->update_scrollbars();
}


Track *
Block::replace_ruler_track(Track *track, int width)
{
    Track *removed = nullptr;
    // The only time ruler_track is NULL is when replace_ruler_track is called
    // from the constructor on no_ruler.
    if (this->ruler_track) {
        // 0 width is ok for the ruler track, but causes problems elsewhere.
        // Since this track will be bumped into tracknum 1 it can't be 0 width.
        if (ruler_track->w() < 1)
            this->set_ruler_width(1);
        this->ruler_group.remove(ruler_track);
        removed = ruler_track;
    }
    this->ruler_track = track;
    // Initially the widths match, so the inserted track is layed out
    // correctly.
    IRect p = f_util::rect(this->ruler_group);
    ruler_track->resize(
        p.x + time_sb.w(), p.y + track_box.h(),
        removed ? removed->w() : 1, time_sb.h());
    ruler_group.add(track);
    ruler_group.resizable(ruler_track);
    this->set_ruler_width(width);
    return removed;
}


void
Block::update_track(int tracknum, const Tracklike &track,
    ScoreTime start, ScoreTime end)
{
    ASSERT_MSG(track.track || track.ruler || track.divider,
        "totally empty tracklike");
    this->track_at(tracknum)->update(track, start, end);
    this->update_scrollbars();
}


void
Block::set_track_signal(int tracknum, const TrackSignal &tsig)
{
    this->track_at(tracknum)->set_track_signal(tsig);
    // There may be a floating input box that needs to be redrawn.
    this->track_tile.redraw();
}


int
Block::get_track_width(int tracknum) const
{
    if (tracknum == 0)
        return this->ruler_track->w();
    else
        return track_tile.get_track_width(tracknum-1);
}


void
Block::set_track_width(int tracknum, int width)
{
    if (tracknum == 0) {
        this->set_ruler_width(width);
        MsgCollector::get()->block(UiMsg::msg_resize, this);
    } else {
        track_tile.set_track_width(tracknum-1, width);
        skel_display.set_width(tracknum-1, width);
    }
}

const char *
Block::dump() const
{
    std::ostringstream out;
    static std::string outs;

    Fl_Window *win = this->window();
    out << "x " << win->x() << " y " << win->y()
        << " w " << this->w() << " h " << this->h();
    out << " window-title " << f_util::show_string(this->window()->label())
        << " title " << f_util::show_string(this->get_title());
    for (int i = 0; i < this->tracks(); i++) {
        out << " track" << i << " ("
            << "width " << this->get_track_width(i)
            << ' ' << this->track_at(i)->dump() << ")";
    }

    outs = out.str();
    return outs.c_str();
}


// private

// Update scrollbar display based on the current zoom and scroll offset.
void
Block::update_scrollbars()
{
    this->track_sb.set_scroll_zoom(
        track_tile.track_end(), get_track_scroll(), track_scroll.w());

    const ZoomInfo &zoom = this->get_zoom();
    // The scale(1)s just convert a ScoreTime to a double.
    this->time_sb.set_scroll_zoom(
        track_tile.time_end().scale(1),
        zoom.offset.scale(1),
        track_tile.visible_time().scale(1));
}

// static callbacks

// Scrollbar callback.  Update the view window based on the scrollbar
// positions.
void
Block::scrollbar_cb(Fl_Widget *_unused_w, void *vp)
{
    Block *self = static_cast<Block *>(vp);

    double time_offset = self->time_sb.get_offset();
    ScoreTime end = self->track_tile.time_end();
    // This does the same stuff as Block::set_zoom, but doesn't call
    // update_scrollbars and collects the msg.
    ZoomInfo new_zoom(
        ScoreTime(end.scale(time_offset)), self->get_zoom().factor);
    if (new_zoom.offset != self->get_zoom().offset) {
        MsgCollector::get()->block(UiMsg::msg_time_scroll, self);
        self->set_zoom_attr(new_zoom);
    }

    // This is the same as Block::set_track_scroll, but can reuse track_end,
    // and doesn't call update_scrollbars.  Since they do the same thing, if
    // you update this, also update set_track_scroll!
    double track_offset = self->track_sb.get_offset();
    int track_end = self->track_tile.track_end();
    int max_offset = std::max(0, track_end - self->track_scroll.w());
    int offset = std::min(max_offset, int(track_offset * track_end));
    IPoint new_offset(-offset, 0);
    if (self->track_scroll.get_offset() != new_offset) {
        self->track_scroll.set_offset(new_offset);
        self->skel_display_scroll.set_offset(new_offset);
        MsgCollector::get()->block(UiMsg::msg_track_scroll, self);
    }

    if (Fl::event() == FL_RELEASE)
        self->update_scrollbars();
}


// The tracks or events changed, so fix up the scrollbars.
void
Block::update_scrollbars_cb(Fl_Widget *w, void *vp)
{
    Block *self = static_cast<Block *>(vp);
    self->update_scrollbars();
}

// This is called by both body and track_tile.  track_tile calls only on
// FL_RELEASE (TODO which I should probably fix).
void
Block::track_tile_cb(Fl_Widget *w, void *vp)
{
    Block *self = static_cast<Block *>(vp);
    self->update_scrollbars();
    int track = self->track_tile.get_dragged_track();
    // get_dragged_track is -1 if there is none, which means it must have been
    // the ruler, which is 0.
    track++;

    // TODO cache last track widths and only emit changes?
    // A track resize can affect all tracks to the left of it.
    for (int i = 0; i <= track; i++) {
        // This causes skel_display to call recalculate_centers a bunch of
        // times but it's fast.
        if (i != 0)
            self->skel_display.set_width(i-1, self->get_track_width(i));
        // Don't spam out updates until a release.
        if (Fl::event() == FL_RELEASE)
            MsgCollector::get()->track(UiMsg::msg_track_width, self, i);
    }
    // body tile drags could resize the skel_display, which will change the
    // visible track area.
    if (w == &self->body && Fl::event() == FL_RELEASE)
        MsgCollector::get()->block(UiMsg::msg_resize, self);
}


void
Block::title_cb_dispatch(Fl_Widget *_w, void *vp)
{
    Block *self = static_cast<Block *>(vp);
    self->title_cb();
}

void
Block::title_cb()
{
    BlockWindow *view = static_cast<BlockWindow *>(window());
    if (Fl::event() == FL_UNFOCUS)
        MsgCollector::get()->view(UiMsg::msg_input, view);
    bool changed = false;
    if (!*title.value()) {
        // Delay hiding the title until focus is lost.
        if (title.visible() && Fl::event() == FL_UNFOCUS) {
            changed = true;
            title.hide();
        }
    } else {
        if (!title.visible()) {
            changed = true;
            title.show();
        }
    }
    changed = changed || title.h() != title.text_height();
    if (changed) {
        set_widget_sizes();
        MsgCollector::get()->block(UiMsg::msg_resize, this);
    }
}


// BlockWindow ///////////

static void
block_view_window_cb(Fl_Window *win, void *p)
{
    BlockWindow *view = static_cast<BlockWindow *>(win);
    MsgCollector::get()->view(UiMsg::msg_close, view);
    if (view->testing) {
        view->hide();
    }
}


BlockWindow::BlockWindow(
        int x, int y, int w, int h,
        const char *label,
        const BlockConfig &config) :
    Fl_Double_Window(
        x, y, std::max(min_width, w), std::max(min_height, h), label),
    block(x, y, std::max(min_width, w), std::max(min_height, h), config,
        label),
    testing(false)
{
    this->callback((Fl_Callback *) block_view_window_cb);
    this->resizable(this);
    // Fl_Window::resize makes explicit resize()s set size_range to the given
    // size, and then the system won't let me make it any bigger.
    // Explicitly setting it to some big number that fits in a short seems
    // to work around the problem.
    // Contrary to the documentation, setting maxw and maxh to 0 does not
    // automatically pick screen-filling sizes.
    this->size_range(min_width, min_height, 10000, 10000);

    // Turn off some annoying defaults.
    Fl::dnd_text_ops(false); // don't do drag and drop text
    // Fl::visible_focus(false); // doesn't seem to do anything
    // this->border(false);
}


void
BlockWindow::resize(int x, int y, int w, int h)
{
    int sx, sy, sw, sh;
    Fl::screen_work_area(sx, sy, sw, sh, x, y);
    int titlebar = this->decorated_h() - this->h();
    // Don't make the window taller than will fit on the screen.
    h = std::min(h, sh - titlebar);
    Fl_Window::resize(x, y, w, h);
    // This sends tons of resize msgs, and I'd rather just send one on
    // mouse up.
    MsgCollector::get()->view(UiMsg::msg_resize, this);
}


void
BlockWindow::initialize(Config::FreeHaskellFunPtr free_haskell_fun_ptr)
{
    Config::_free_haskell_fun_ptr = free_haskell_fun_ptr;
    // Setup event notification when a screen is added or removed.
    Fl::add_handler(MsgCollector::event_handler);
    MsgCollector::get()->screen_update();
}


static void
highlight_focused(BlockWindow *focus)
{
    for (Fl_Window *w = Fl::first_window(); w; w = Fl::next_window(w)) {
        BlockWindow *block = dynamic_cast<BlockWindow *>(w);
        if (block) {
            block->block.set_skeleton_display_bg(
                block == focus ? Config::focus_skeleton_display_bg
                    : Config::skeleton_display_bg
            );
        }
    }
}


int
BlockWindow::handle(int evt)
{
    if (evt == FL_SHOW) {
        // Send an initial resize to inform the haskell layer about dimensions.
        MsgCollector::get()->view(UiMsg::msg_resize, this);
    } else if (evt == FL_FOCUS) {
        highlight_focused(this);
        // This is sent *before* the widget becomes Fl::focus().
        MsgCollector::get()->focus(this);
        return true;
    }
    if (this->testing && evt == FL_KEYDOWN && Fl::event_key() == FL_Escape) {
        // This is kind of dumb, but I'm used to using this to quit test_block.
        this->hide();
    }
    if (evt == FL_KEYDOWN || evt == FL_KEYUP) {
        // The fact I got it means I have focus.
        MsgCollector::get()->event(evt);
        return true;
    }

    bool accepted = false;
    if (evt == FL_PUSH || evt == FL_MOVE || evt == FL_MOUSEWHEEL) {
        // see if someone else wants it
        accepted = Fl_Group::handle(evt);
    }
    if (!accepted) {
        switch (evt) {
        case FL_PUSH:
            // They didn't want it, so they lose focus.
            Fl::focus(this);
            // fall through
        case FL_DRAG: case FL_RELEASE:
            // If it's in the track area but reached here it must be right
            // of the rightmost track.
            MsgCollector::get()->event(evt, block.event_in_track_area());
            break;
        }
        return true;
    }
    return accepted;
}
