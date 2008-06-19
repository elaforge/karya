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

BlockView::BlockView(int X, int Y, int W, int H,
        const BlockModelConfig &model_config,
        const BlockViewConfig &view_config) :
    Fl_Group(X, Y, W, H),

    title(0, 0, 1, 1, false),
    status_line(0, 0, 1, 1),
    body(0, 0, 1, 1),
        body_resize_group(0, 0, 1, 1, "resize group"),
        ruler_group(0, 0, 1, 1),
            track_box(0, 0, 1, 1),
            sb_box(0, 0, 1, 1),
            time_sb(0, 0, 1, 1),
            ruler_track(0), // filled in later by insert_track
        track_group(0, 0, 1, 1),
            track_sb(0, 0, 1, 1),
            track_scroll(0, 0, 1, 1),
                track_tile(0, 0, 1, 1, model_config.bg,
                        view_config.track_title_height)
{
    // The sizes of 1 are so that groups realize that their children are inside
    // of them.  The real resizing will be done in set_view_config
    current(0); // done adding widgets
    // fltk's automatic group stuff gets most of the hierarchy above, but not
    // all of it.
    body.add(ruler_group);
    body.add(track_group);
    body_resize_group.hide();

    // Insert a placeholder for the ruler.  The size will be set again by
    // set_view_config, but that's ok.
    DividerConfig *div = new DividerConfig(Color(0));
    this->insert_track(0, Tracklike(div), 0);

    // Remove the status line from the tab focus list.  I bypass that anyway
    // so this doesn't have any effect.
    // status_line.visible_focus(false);
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

    this->set_view_config(view_config, true);
    this->set_model_config(model_config, true);

    // See dummy_ruler_size in set_view_config.
    this->set_ruler_width(0);

    // Initialize zoom to default.  This will cause zooming children to
    // properly position their widgets.
    this->set_zoom(this->zoom);
    this->update_scrollbars();
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
BlockView::set_view_config(const BlockViewConfig &vconfig, bool always_update)
{
    // I update everything, even if always_update is false.  It's probably not
    // worth trying to skip things that haven't changed for this.

    // If I resize from parent to child, then children get a lot of spurious
    // resizes as their parents move them around.  on the other hand, if
    // we go the other way, parents mess up their children.
    // Spurious resizes it is.

    // I don't totally understand what's going on here.  The window's x() and
    // y() are the values that are passed to the constructor.  However, all
    // widget positions are measured relative to 0, so using x() and y() here
    // will screw up placement.  In addition, using (0, 0) here screws up
    // resizing.
    // Setting the position to 0 fixes all that and doesn't even seem to move
    // the window around.
    int wx = 0, wy = 0;
    this->position(0, 0);

    title.resize(wx, wy, w(), vconfig.block_title_height);
    status_line.resize(wx, h() - vconfig.status_size,
            w() - mac_resizer_width, vconfig.status_size);
    status_line.textsize(vconfig.status_size - 4);
    body.resize(wx, wy + title.h(),
            w(), h() - title.h() - status_line.h());
    body_resize_group.resize(body.x() + vconfig.sb_size, body.y(),
            body.w() - vconfig.sb_size, body.h());

    // Re-use the existing ruler_track width so it is maintained across calls
    // to set_view_config.  It has to be at least 1, or else the 'body' Fl_Tile
    // won't resize properly.
    ruler_group.resize(wx, body.y(),
            vconfig.sb_size + ruler_track->w(), body.h());
    Rect p = rect(ruler_group);
    track_group.resize(p.r(), body.y(), body.w() - p.w, body.h());

    track_box.resize(p.x, p.y, p.w, vconfig.block_title_height);
    sb_box.resize(p.x, p.b() - vconfig.sb_size, p.w, vconfig.sb_size);

    time_sb.set_orientation(P9Scrollbar::vertical);
    track_sb.set_orientation(P9Scrollbar::horizontal);

    time_sb.resize(p.x, p.y + track_box.h(),
            vconfig.sb_size, p.h - track_box.h() - sb_box.h());
    ruler_track->resize(p.x + time_sb.w(), p.y + track_box.h(),
            ruler_track->w(), time_sb.h());

    p = rect(track_group);
    track_sb.resize(p.x, p.b() - vconfig.sb_size, p.w, vconfig.sb_size);
    track_scroll.resize(p.x, p.y, p.w, p.h - track_sb.h());
    track_tile.resize(track_scroll.x(), track_scroll.y(),
            track_scroll.w(), track_scroll.h());

    this->track_tile.set_title_height(vconfig.track_title_height);

    // This is overhead required by fltk when you resize anything manually.
    init_sizes();
    body.init_sizes();
    ruler_group.init_sizes();
    track_group.init_sizes();
    track_scroll.init_sizes();
    track_tile.init_sizes();

    this->update_scrollbars();
    this->view_config = vconfig;
    this->redraw();
}


void
BlockView::set_ruler_width(int width)
{
    ASSERT(0 <= width);

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
    }
    if (update_all || old.track_box != config.track_box) {
        track_box.color(color_to_fl(config.track_box));
        track_box.redraw();
    }
    if (update_all || old.sb_box != config.sb_box) {
        sb_box.color(color_to_fl(config.sb_box));
        sb_box.redraw();
    }
    this->model_config = config;
}


void
BlockView::set_zoom(const ZoomInfo &zoom)
{
    // As with set_track_scroll, clamp the time scroll to time_end().
    // Actually, it's important to be able to scroll down to areas without
    // events, so I'm going to comment this out for now.
    // int track_h = track_tile.h() - view_config.track_title_height;
    // TrackPos height = zoom.to_trackpos(track_h);
    // TrackPos max_pos = std::max(TrackPos(0), track_tile.time_end() - height);
    //
    // this->zoom = ZoomInfo(clamp(TrackPos(0), max_pos, zoom.offset),
    //         zoom.factor);

    if (this->zoom != zoom) {
        this->zoom = zoom;
        this->track_tile.set_zoom(this->zoom);
        this->ruler_track->set_zoom(this->zoom);
        this->update_scrollbars();
    }
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
    int max_offset = std::max(0, track_end - this->track_tile.w());
    offset = std::min(max_offset, offset);
    this->track_scroll.set_offset(Point(-offset, 0));
    this->update_scrollbars();
}


void
BlockView::set_selection(int selnum, const Selection &sel)
{
    ASSERT(0 <= selnum && selnum < Config::max_selections);

    // This is a bit tricky because the tile tracks have a -1 view of tracknum.
    // track_at(0) is the ruler track.
    track_at(0)->set_selection(selnum, 0, sel);
    Selection track_sel = sel;
    track_sel.start_track--;
    for (int i = 1; i < tracks(); i++)
        track_at(i)->set_selection(selnum, i-1, track_sel);
    // Since the selection counts toward time_end.
    this->update_scrollbars();
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
    if (tracknum == 0) {
        // Set the width to a known non-zero size, clear the old track,
        // and replace it with the new one.
        if (this->ruler_track) {
            this->set_ruler_width(1);
            this->ruler_group.remove(ruler_track);
            delete ruler_track;
        }
        this->ruler_track = t;

        Rect p = rect(this->ruler_group);
        ruler_track->resize(p.x + time_sb.w(), p.y + track_box.h(),
                1, time_sb.h());

        ruler_group.add(t);
        ruler_group.resizable(ruler_track);
        this->set_ruler_width(width);
    } else {
        track_tile.insert_track(tracknum - 1, t, width);
    }
    this->update_scrollbars();
}


void
BlockView::remove_track(int tracknum, FinalizeCallback finalizer)
{
    if (tracknum != 0) {
        TrackView *t = track_tile.remove_track(tracknum-1);
        t->finalize_callbacks(finalizer);
        delete t;
        this->update_scrollbars();
    }
}


void
BlockView::update_track(int tracknum, const Tracklike &track,
        FinalizeCallback finalizer, TrackPos start, TrackPos end)
{
    this->track_at(tracknum)->update(track, finalizer, start, end);
    this->update_scrollbars();
}


// private

// Update scrollbar display based on the current zoom and scroll offset.
void
BlockView::update_scrollbars()
{
    this->track_sb.set_scroll_zoom(
        track_tile.track_end(), get_track_scroll(), track_tile.w());

    const ZoomInfo &zoom = this->get_zoom();
    // The scale(1)s just convert a TrackPos to a double.
    int track_h = track_tile.h() - view_config.track_title_height;
    this->time_sb.set_scroll_zoom(track_tile.time_end().scale(1),
            zoom.offset.scale(1),
            zoom.to_trackpos(track_h).scale(1));
}

// static callbacks

// Scrollbar callback.  Update the view window based on the scrollbar
// positions.
void
BlockView::scrollbar_cb(Fl_Widget *_unused_w, void *vp)
{
    BlockView *self = static_cast<BlockView *>(vp);

    double time_offset = self->time_sb.get_offset();
    TrackPos end = self->track_tile.time_end();
    // This does the same stuff as BlockView::set_zoom, but doesn't call
    // update_scrollbars and collects the msg.
    ZoomInfo new_zoom(TrackPos(end.scale(time_offset)),
            self->get_zoom().factor);
    if (new_zoom != self->get_zoom()) {
        self->zoom = new_zoom;
        self->ruler_track->set_zoom(self->zoom);
        self->track_tile.set_zoom(self->zoom);
        global_msg_collector()->block_update(self, UiMsg::msg_zoom);
    }

    // This is the same as BlockView::set_track_scroll, but can reuse
    // track_end, and doesn't call update_scrollbars.
    double track_offset = self->track_sb.get_offset();
    int track_end = self->track_tile.track_end();
    int max_offset = std::max(0, track_end - self->track_tile.w());
    int offset = std::min(max_offset, int(track_offset * track_end));
    Point new_offset(-offset, 0);
    if (self->track_scroll.get_offset() != new_offset) {
        self->track_scroll.set_offset(new_offset);
        global_msg_collector()->block_update(self, UiMsg::msg_track_scroll);
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

void
BlockView::track_tile_cb(Fl_Widget *w, void *vp)
{
    // Don't bother emitting width changes until mouse up.
    if (Fl::event() != FL_RELEASE)
        return;

    // TODO dragging a tile can actually resize multiple columns, so emit
    // changes for each

    BlockView *self = static_cast<BlockView *>(vp);
    self->update_scrollbars();
    int track = self->track_tile.get_dragged_track();
    // -1 means it must have been the ruler track, which is 0.
    track++;
    global_msg_collector()->block_update(self, UiMsg::msg_track_width, track);
}


// BlockViewWindow ///////////

static void
block_view_window_cb(Fl_Window *win, void *p)
{
    BlockViewWindow *view = static_cast<BlockViewWindow *>(win);
    global_msg_collector()->window_update(view, UiMsg::msg_close);
    if (view->testing) {
        view->hide();
    }
}


BlockViewWindow::BlockViewWindow(int X, int Y, int W, int H,
        const char *label,
        const BlockModelConfig &model_config,
        const BlockViewConfig &view_config) :
    Fl_Double_Window(X, Y, W, H, label),
    block(X, Y, W, H, model_config, view_config),
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
    this->size_range(10, 10, 10000, 10000);

    // Turn off some annoying defaults.
    Fl::dnd_text_ops(false); // don't do drag and drop text
    // Fl::visible_focus(false); // doesn't seem to do anything
    // this->border(false);
}

void
BlockViewWindow::resize(int X, int Y, int W, int H)
{
    global_msg_collector()->window_update_resize(this, Rect(X, Y, W, H));
    Fl_Window::resize(X, Y, W, H);
}


int
BlockViewWindow::handle(int evt)
{
    if (this->testing && evt == FL_KEYDOWN && Fl::event_key() == FL_Escape) {
        this->hide();
    }
    if (evt == FL_KEYDOWN || evt == FL_KEYUP) {
        // The fact I got it means I have focus.
        int key = Fl::event_key();
        if (evt == FL_KEYDOWN) {
            this->keys_down[key] = true;
        } else {
            // Actually, I get KEYUP even if I don't have focus, and also get
            // the KEYUP half of an event that defocuses.  Eat those up
            // silently.
            // if (!this->keys_down[key])
            //     return true;
            // Actually, I can't do this because I also get lone KEYUPs when
            // a new window was given focus during the key down.  I don't see
            // how I can tell the difference, so just live with the spurious
            // keyup.  The proper fix would be to put this map into fltk event
            // delivery so it doesn't split events like that.
            this->keys_down[key] = false;
        }
        global_msg_collector()->event(evt);
        if (this->testing && Fl::event_key() == FL_Escape)
            return false; // this will wind up closing the window
        return true;
    }

    // TODO turn FL_SCROLLWHEEL (also sent by two-finger drag) into scrolls

    bool accepted = false;
    if (evt == FL_PUSH || evt == FL_MOVE) {
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
            global_msg_collector()->event(evt);
            break;
        case FL_FOCUS:
            global_msg_collector()->event(evt, this);
        }
        return true;
    }
    return accepted;
}
