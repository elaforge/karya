#include <sstream>
#include "util.h"
#include "f_util.h"

#include "MsgCollector.h"
#include "Track.h"
#include "EventTrack.h"
#include "Ruler.h"

#include "Block.h"

// Try to avoid running into the little resize tab on the mac.
// Actually, it's ok if it overlaps, since it's just the status field.
static const int mac_resizer_width = 0;

// Multiply mousewheel scroll pixels by this.
static const double mousewheel_time_scale = 3;
static const double mousewheel_track_scale = 3;


BlockView::BlockView(int X, int Y, int W, int H,
        const BlockModelConfig &model_config) :
    Fl_Group(X, Y, W, H),

    title(0, 0, 1, 1, false),
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
            ruler_track(NULL), // filled in later by insert_track
        track_group(0, 0, 1, 1),
            track_sb(0, 0, 1, 1),
            track_scroll(0, 0, 1, 1),
                track_tile(0, 0, 1, 1, model_config.bg,
                        Config::View::track_title_height)
{
    // The sizes of 1 are so that groups realize that their children are inside
    // of them.  The real resizing will be done in set_view_config
    current(0); // done adding widgets
    // fltk's automatic group stuff gets most of the hierarchy above, but not
    // all of it.
    body.add(skel_box);
    body.add(skel_display_scroll);
    body.add(ruler_group);
    body.add(track_group);
    body_resize_group.hide();

    // Insert a placeholder for the ruler.  The size will be set again by
    // set_view_config, but that's ok.
    this->no_ruler = new DividerView(DividerConfig(Color::black));
    this->replace_ruler_track(no_ruler, 0);

    // Remove the status line from the tab focus list.  I bypass that anyway
    // so this doesn't have any effect.
    // status_line.visible_focus(false);
    skel_box.box(FL_FLAT_BOX);
    status_line.box(FL_FLAT_BOX);
    track_box.box(FL_FLAT_BOX);
    sb_box.box(FL_FLAT_BOX);
    time_sb.callback(BlockView::scrollbar_cb, static_cast<void *>(this));
    track_sb.callback(BlockView::scrollbar_cb, static_cast<void *>(this));
    body.callback(BlockView::track_tile_cb, static_cast<void *>(this));
    track_tile.callback(BlockView::track_tile_cb, static_cast<void *>(this));

    resizable(body);
    body.resizable(body_resize_group);
    track_group.resizable(track_scroll);

    title.set_callback2(BlockView::title_cb, static_cast<void *>(this));
    title.hide(); // It starts with no text.

    this->set_view_config();
    this->set_model_config(model_config, true);

    // See dummy_ruler_size in set_view_config.
    this->set_ruler_width(0);

    // Initialize zoom to default.  This will cause zooming children to
    // properly position their widgets.
    this->set_zoom(this->zoom);
    this->update_scrollbars();
}


BlockView::~BlockView()
{
    delete no_ruler;
}


int
BlockView::handle(int evt)
{
    if (evt == FL_MOUSEWHEEL) {
        if (Fl::event_dy()) {
            ScoreTime scroll = this->zoom.to_time(
                    Fl::event_dy() * mousewheel_time_scale);
            ScoreTime old = this->zoom.offset;
            set_zoom(ZoomInfo(this->zoom.offset + scroll, this->zoom.factor));
            if (this->zoom.offset != old)
                MsgCollector::get()->block(UiMsg::msg_zoom, this);
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
BlockView::resize(int X, int Y, int W, int H)
{
    Fl_Group::resize(X, Y, W, H);
    status_line.size(w() - mac_resizer_width, status_line.h());
    // If I'm growing, I will have changed the visible area, so recalculate
    // the scroll boundaries.
    this->set_track_scroll(this->get_track_scroll());
    this->update_scrollbars();
}


void
BlockView::set_view_config()
{
    // I update everything, even if update_all is false.  It's probably not
    // worth trying to skip things that haven't changed for this.
    //
    // If I resize from parent to child, then children get a lot of spurious
    // resizes as their parents move them around.  on the other hand, if
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

    title.resize(wx, wy, w(), Config::View::block_title_height);
    int title_h;
    if (title.visible()) {
        title_h = title.h();
    } else {
        title_h = 0;
    }
    status_line.resize(wx, h() - Config::View::status_size,
            w() - mac_resizer_width, Config::View::status_size);
    body.resize(wx, wy + title_h, w(), h() - title_h - status_line.h());
    body_resize_group.resize(body.x() + Config::View::sb_size, body.y(),
            body.w() - Config::View::sb_size, body.h());

    int ruler_group_w = Config::View::sb_size + ruler_track->w();
    skel_box.resize(body.x(), body.y(),
            ruler_group_w, Config::View::skel_height);
    skel_display_scroll.resize(skel_box.x() + skel_box.w(), skel_box.y(),
            body.w() - ruler_group_w, Config::View::skel_height);
    IRect p = rect(skel_display_scroll);
    skel_display.resize(p.x, p.y, p.w, p.h);

    // Re-use the existing ruler_track width so it is maintained across calls
    // to set_view_config.  It has to be at least 1, or else the 'body' Fl_Tile
    // won't resize properly.
    ruler_group.resize(wx, body.y() + Config::View::skel_height,
        ruler_group_w, body.h() - Config::View::skel_height);
    p = rect(ruler_group);
    track_group.resize(p.r(), p.y, body.w() - p.w, p.h);

    // The track_box looks taller than just the track titles because it's
    // always the same color as the skel_box.
    track_box.resize(p.x, p.y, p.w, Config::View::track_title_height);
    sb_box.resize(p.x, p.b() - Config::View::sb_size,
        p.w, Config::View::sb_size);

    time_sb.set_orientation(P9Scrollbar::vertical);
    track_sb.set_orientation(P9Scrollbar::horizontal);

    time_sb.resize(p.x, p.y + track_box.h(),
        Config::View::sb_size, p.h - track_box.h() - sb_box.h());
    ruler_track->resize(p.x + time_sb.w(), p.y + track_box.h(),
        ruler_track->w(), time_sb.h());

    p = rect(track_group);
    track_sb.resize(p.x, p.b() - Config::View::sb_size,
        p.w, Config::View::sb_size);
    track_scroll.resize(p.x, p.y, p.w, p.h - track_sb.h());
    track_tile.resize(track_scroll.x(), track_scroll.y(),
            track_scroll.w(), track_scroll.h());

    this->track_tile.set_title_height(Config::View::track_title_height);

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
BlockView::set_ruler_width(int width)
{
    ASSERT(0 <= width);
    if (this->ruler_track->w() == width)
        return;

    int x = this->ruler_track->x();
    this->body.position(x + ruler_track->w(), body.y(), x + width, body.y());
}


void
BlockView::set_model_config(const BlockModelConfig &config, bool update_all)
{
    const BlockModelConfig &old = this->model_config;

    if (update_all || old.bg != config.bg) {
        track_tile.set_bg_color(config.bg);
        track_tile.redraw();
        skel_display.color(color_to_fl(config.bg.brightness(.9)));
        skel_display.redraw();
    }
    if (update_all || old.track_box != config.track_box) {
        track_box.color(color_to_fl(config.track_box));
        track_box.redraw();
        skel_box.color(color_to_fl(config.track_box));
        skel_box.redraw();
    }
    if (update_all || old.track_char != config.track_char) {
        if (config.track_char == ' ')
            track_box.copy_label(NULL);
        else {
            char s[2] = {config.track_char, '\0'};
            track_box.copy_label(s);
        }
    }
    if (update_all || old.sb_box != config.sb_box) {
        sb_box.color(color_to_fl(config.sb_box));
        sb_box.redraw();
    }
    if (update_all || old.sb_char != config.sb_char) {
        if (config.sb_char == ' ')
            sb_box.copy_label(NULL);
        else {
            char s[2] = {config.sb_char, '\0'};
            sb_box.copy_label(s);
        }
    }
    this->model_config = config;
}


void
BlockView::set_skeleton(const SkeletonConfig &skel)
{
    std::vector<int> widths(track_tile.tracks());
    for (int i = 0; i < track_tile.tracks(); i++) {
        widths[i] = track_tile.get_track_width(i);
    }
    skel_display.set_config(skel, widths);
}


void
BlockView::set_zoom(const ZoomInfo &zoom)
{
    // As with set_track_scroll, clamp the time scroll to time_end().
    // Actually, it's important to be able to scroll down to areas without
    // events, so I'm going to comment this out for now.
    // int track_h = track_tile.h() - view_config.track_title_height;
    // ScoreTime height = zoom.to_time(track_h);
    // ScoreTime max_pos = std::max(ScoreTime(0), track_tile.time_end() - height);
    //
    // this->zoom = ZoomInfo(clamp(ScoreTime(0), max_pos, zoom.offset),
    //         zoom.factor);

    // This function, and hence set_zoom_attr below, is called from the outside
    // to set the zoom.  Therefore, no msg should be sent back out, since I
    // don't need to tell the outside what it already knows.
    this->set_zoom_attr(zoom);
    this->update_scrollbars();
}


void
BlockView::set_zoom_attr(const ZoomInfo &new_zoom)
{
    ZoomInfo clamped = new_zoom;
    // Clip offset to be positive, and quantize it to conform exactly to
    // a pixel boundary.  Otherwise, some events may move 1 pixel while others
    // move 2 pixels, which messes up the blit-oriented scrolling.
    clamped.offset = std::max(ScoreTime(0), clamped.offset);
    ScoreTime visible = clamped.to_time(track_tile.visible_pixels().y);
    ScoreTime max_offset = track_tile.time_end() - visible;
    clamped.offset = clamp(ScoreTime(0), max_offset, clamped.offset);
    if (clamped == this->zoom)
        return;
    this->zoom = clamped;
    this->track_tile.set_zoom(zoom);
    this->ruler_track->set_zoom(zoom);
}


int
BlockView::get_track_scroll() const
{
    return -track_scroll.get_offset().x;
}


void
BlockView::set_track_scroll(int offset)
{
    int track_end = this->track_tile.track_end();
    int max_offset = std::max(0, track_end - this->track_scroll.w());
    offset = clamp(0, max_offset, offset);
    // offset = std::min(max_offset, offset);

    // If you update this, also update scrollbar_cb!
    IPoint scroll_offset(-offset, 0);
    if (scroll_offset == this->track_scroll.get_offset())
        return;
    this->track_scroll.set_offset(scroll_offset);
    this->skel_display_scroll.set_offset(scroll_offset);
    this->update_scrollbars();
}


void
BlockView::set_selection(int selnum, const Selection &sel)
{
    ASSERT(0 <= selnum && selnum < Config::max_selections);

    // This is a bit tricky because the tile tracks have a -1 view of tracknum.
    // track_at(0) is the ruler track.
    track_at(0)->set_selection(selnum, 0, sel);
    Selection track_sel(sel);
    track_sel.start_track--;
    track_sel.cur_track--;
    for (int i = 1; i < tracks(); i++)
        track_at(i)->set_selection(selnum, i-1, track_sel);
    // Since the selection counts toward time_end.
    this->update_scrollbars();
}


void
BlockView::set_track_selection(int selnum, int tracknum, const Selection &sel)
{
    ASSERT(0 <= selnum && selnum < Config::max_selections);
    // It's sort of hacky to reuse Selection (instead of say TrackSelection)
    // since I then totally ignore the tracknum, but fewer types means less
    // marshalling code.
    // TODO an "empty" Selection() should clear the sel, but here it just sets
    // it to 0.  It's ok for now because I use set_selection to clear for the
    // whole block.
    Selection track_sel = sel;
    if (tracknum == 0) {
        track_sel.start_track = track_sel.cur_track = tracknum;
        track_at(0)->set_selection(selnum, 0, track_sel);
    } else {
        track_sel.start_track = track_sel.cur_track = tracknum - 1;
        track_at(tracknum)->set_selection(selnum, tracknum-1, track_sel);
    }
}


void
BlockView::set_title(const char *s)
{
    title.set_text(s);
    title_cb(NULL, this);
}


void
BlockView::insert_track(int tracknum, const Tracklike &track, int width)
{
    TrackView *t;

    // DEBUG("view insert at " << tracknum);
    if (track.track) {
        t = new EventTrackView(*track.track, *track.ruler);
    } else if (track.ruler) {
        t = new RulerTrackView(*track.ruler);
    } else {
        t = new DividerView(*track.divider);
    }
    this->insert_track_view(tracknum, t, width);
}


// Even though the ruler track is not in the TrackTile, I go to some effort
// to create the illusion that it's just another track, which means some extra
// work to push other tracks over when another track is inserted into or
// removed from the ruler track.

void
BlockView::remove_track(int tracknum, FinalizeCallback finalizer)
{
    if (tracknum != 0) {
        TrackView *t = track_tile.remove_track(tracknum-1);
        t->finalize_callbacks(finalizer);
        delete t;

        this->update_scrollbars();
        // I don't want to delete the track from the skeleton because if this
        // is a replace then the skeleton can be preserved.  This happens when
        // a track is collapsed.  Otherwise, the skeleton is out of date and
        // there should be a set_skeleton soon.
        this->skel_display.set_width(tracknum-1, 0);
    } else if (this->tracks() == 1) {
        if (this->ruler_track != this->no_ruler) {
            TrackView *t = this->replace_ruler_track(this->no_ruler, 0);
            delete t;
        }
    } else {
        TrackView *t = track_tile.remove_track(0);
        TrackView *removed = this->replace_ruler_track(t, t->w());
        if (removed != this->no_ruler)
            delete removed;
    }
}

void
BlockView::set_display_track(int tracknum, const DisplayTrack &dtrack)
{
    ASSERT(0 <= tracknum && tracknum < this->tracks());
    if (tracknum > 0) {
        this->skel_display.set_status(tracknum-1, dtrack.status,
                dtrack.status_color);
    }
    this->track_at(tracknum)->set_event_brightness(dtrack.event_brightness);
    this->set_track_width(tracknum, dtrack.width);
}


void
BlockView::insert_track_view(int tracknum, TrackView *track, int width)
{
    if (tracknum == 0) {
        TrackView *replaced = this->replace_ruler_track(track, width);
        if (replaced != this->no_ruler)
            track_tile.insert_track(0, replaced, replaced->w());
        this->ruler_track->set_zoom(this->zoom);
        // Changing the ruler will change the track area.
        MsgCollector::get()->block(UiMsg::msg_resize, this);
    } else {
        track_tile.insert_track(tracknum - 1, track, width);
        this->track_tile.set_zoom(this->zoom);
        // Restore the width as per the comment in 'remove_track'.
        this->skel_display.set_width(tracknum-1, width);
    }
    this->update_scrollbars();
}


TrackView *
BlockView::replace_ruler_track(TrackView *track, int width)
{
    TrackView *removed = NULL;
    if (this->ruler_track) {
        // 0 width is ok for the ruler track, but causes problems elsewhere.
        if (ruler_track->w() < 1)
            this->set_ruler_width(1);
        this->ruler_group.remove(ruler_track);
        removed = ruler_track;
    }
    this->ruler_track = track;
    // Initially the widths match, so the inserted track is layed out
    // correctly.
    IRect p = rect(this->ruler_group);
    ruler_track->resize(p.x + time_sb.w(), p.y + track_box.h(),
            removed ? removed->w() : 1, time_sb.h());
    ruler_group.add(track);
    ruler_group.resizable(ruler_track);
    this->set_ruler_width(width);
    return removed;
}


void
BlockView::update_track(int tracknum, const Tracklike &track,
        FinalizeCallback finalizer, ScoreTime start, ScoreTime end)
{
    ASSERT_MSG(track.track || track.ruler || track.divider,
        "totally empty tracklike");
    this->track_at(tracknum)->update(track, finalizer, start, end);
    this->update_scrollbars();
}


void
BlockView::set_track_signal(int tracknum, const TrackSignal &tsig)
{
    this->track_at(tracknum)->set_track_signal(tsig);
}


int
BlockView::get_track_width(int tracknum) const
{
    if (tracknum == 0)
        return this->ruler_track->w();
    else
        return track_tile.get_track_width(tracknum-1);
}


void
BlockView::set_track_width(int tracknum, int width)
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
BlockView::dump() const
{
    std::ostringstream out;
    static std::string outs;

    Fl_Window *win = this->window();
    out << "x " << win->x() << " y " << win->y()
        << " w " << this->w() << " h " << this->h();
    out << " window-title " << show_string(this->window()->label())
        << " title " << show_string(this->get_title());
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
BlockView::update_scrollbars()
{
    this->track_sb.set_scroll_zoom(
        track_tile.track_end(), get_track_scroll(), track_scroll.w());

    const ZoomInfo &zoom = this->get_zoom();
    // The scale(1)s just convert a ScoreTime to a double.
    this->time_sb.set_scroll_zoom(track_tile.time_end().scale(1),
            zoom.offset.scale(1),
            track_tile.visible_time().scale(1));
}

// static callbacks

// Scrollbar callback.  Update the view window based on the scrollbar
// positions.
void
BlockView::scrollbar_cb(Fl_Widget *_unused_w, void *vp)
{
    BlockView *self = static_cast<BlockView *>(vp);

    double time_offset = self->time_sb.get_offset();
    ScoreTime end = self->track_tile.time_end();
    // This does the same stuff as BlockView::set_zoom, but doesn't call
    // update_scrollbars and collects the msg.
    ZoomInfo new_zoom(ScoreTime(end.scale(time_offset)),
            self->get_zoom().factor);
    if (new_zoom != self->get_zoom()) {
        MsgCollector::get()->block(UiMsg::msg_zoom, self);
        self->set_zoom_attr(new_zoom);
    }

    // This is the same as BlockView::set_track_scroll, but can reuse
    // track_end, and doesn't call update_scrollbars.  Since they do the same
    // thing, if you update this, also update set_track_scroll!
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
BlockView::update_scrollbars_cb(Fl_Widget *w, void *vp)
{
    BlockView *self = static_cast<BlockView *>(vp);
    self->update_scrollbars();
}

// This is called by both body and track_tile.  track_tile calls only on
// FL_RELEASE (TODO which I should probably fix).
void
BlockView::track_tile_cb(Fl_Widget *w, void *vp)
{
    BlockView *self = static_cast<BlockView *>(vp);
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
BlockView::title_cb(Fl_Widget *_w, void *vp)
{
    BlockView *self = static_cast<BlockView *>(vp);
    if (strlen(self->title.value()) == 0) {
        if (self->title.visible()) {
            self->title.hide();
            self->set_view_config();
        }
    } else {
        if (!self->title.visible()) {
            self->title.show();
            self->set_view_config();
        }
    }
}


// BlockViewWindow ///////////

static void
block_view_window_cb(Fl_Window *win, void *p)
{
    BlockViewWindow *view = static_cast<BlockViewWindow *>(win);
    MsgCollector::get()->view(UiMsg::msg_close, view);
    if (view->testing) {
        view->hide();
    }
}


BlockViewWindow::BlockViewWindow(int X, int Y, int W, int H,
        const char *label,
        const BlockModelConfig &model_config) :
    Fl_Double_Window(X, Y, W, H, label),
    block(X, Y, W, H, model_config), testing(false)
{
    this->callback((Fl_Callback *) block_view_window_cb);
    this->resizable(this);
    // Fl_Window::resize makes explicit resize()s set size_range to the given
    // size, and then the system won't let me make it any bigger.
    // Explicitly setting it to some big number that fits in a short seems
    // to work around the problem.
    // Contrary to the documentation, setting maxw and maxh to 0 does not
    // automatically pick screen-filling sizes.
    this->size_range(10, 10, 10000, 10000);

    // Turn off some annoying defaults.
    Fl::dnd_text_ops(false); // don't do drag and drop text
    // Fl::visible_focus(false); // doesn't seem to do anything
    // this->border(false);
}


void
BlockViewWindow::resize(int X, int Y, int W, int H)
{
    Fl_Window::resize(X, Y, W, H);
    MsgCollector::get()->view(UiMsg::msg_resize, this);
}


void
BlockViewWindow::show()
{
    IRect requested = rect(this);
    Fl_Double_Window::show();
    if (rect(this) != requested) {
        IRect screen;
        Fl::screen_xywh(screen.x, screen.y, screen.w, screen.h,
            requested.x, requested.y);
        if (screen.contains(IPoint(requested.x, requested.y)))
            this->position(requested.x, requested.y);
    }
}


void
BlockViewWindow::initialize()
{
    // Setup event notification when a screen is added or removed.
    Fl::add_handler(MsgCollector::event_handler);
    MsgCollector::get()->screen_update();
}


int
BlockViewWindow::handle(int evt)
{
    if (evt == FL_SHOW) {
        // Send an initial resize to inform the haskell layer about dimensions.
        MsgCollector::get()->view(UiMsg::msg_resize, this);
    }
    if (this->testing && evt == FL_KEYDOWN && Fl::event_key() == FL_Escape) {
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
            MsgCollector::get()->event(evt);
            break;
        case FL_FOCUS:
            // This is sent *before* the widget becomes Fl::focus().
            MsgCollector::get()->focus(this);
            break;
        }
        return true;
    }
    return accepted;
}
