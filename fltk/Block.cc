#include "util.h"
#include "f_util.h"

#include "MsgCollector.h"
#include "Track.h"
#include "EventTrack.h"
#include "Ruler.h"

#include "Block.h"

// Try to avoid running into the little resize tab on the mac.
static const int mac_resizer_width = 15;

BlockView::BlockView(int X, int Y, int W, int H,
        const BlockModelConfig &model_config,
        const BlockViewConfig &view_config,
        const RulerConfig &ruler_config) :
    Fl_Group(X, Y, W, H),

    title(0, 0, 1, 1),
    status_line(0, 0, 1, 1),
    body(0, 0, 1, 1),
        body_resize_group(0, 0, 1, 1, "resize group"),
        ruler_group(0, 0, 1, 1),
            track_box(0, 0, 1, 1),
            sb_box(0, 0, 1, 1),
            time_sb(0, 0, 1, 1),
            ruler(ruler_config),
        track_group(0, 0, 1, 1),
            track_sb(0, 0, 1, 1),
            track_zoom(0, 0, 1, 1),
                track_scroll(0, 0, 1, 1),
                    track_tile(0, 0, 1, 1, model_config.bg,
                            view_config.track_title_height)
{
    // The sizes of 1 are so that groups realize that their children are inside
    // of them.  The real resizing will be done in set_view_config
    current(0); // done adding widgets
    body.add(ruler_group);
    body.add(track_group); // fix up hierarchy
    body_resize_group.hide();

    // Remove the status line from the tab focus list.  I bypass that anyway
    // so this doesn't have any effect.
    // status_line.visible_focus(false);
    status_line.box(FL_FLAT_BOX);
    track_box.box(FL_FLAT_BOX);
    sb_box.box(FL_FLAT_BOX);
    time_sb.callback(BlockView::scrollbar_cb, static_cast<void *>(this));
    track_sb.callback(BlockView::scrollbar_cb, static_cast<void *>(this));
    track_tile.callback(BlockView::track_tile_cb, static_cast<void *>(this));

    resizable(body);
    body.resizable(body_resize_group);
    ruler_group.resizable(ruler);
    track_group.resizable(track_zoom);
    // track_zoom.resizable(track_scroll);

    this->set_view_config(view_config, true);
    this->set_model_config(model_config, true);

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
    this->update_scrollbars();
    global_msg_collector()->block_changed(this, UiMsg::msg_view_resize);
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

    ruler_group.resize(wx, body.y(),
            vconfig.sb_size + vconfig.ruler_size, body.h());
    Rect p = rect(ruler_group);
    track_group.resize(p.r(), body.y(), body.w() - p.w, body.h());

    track_box.resize(p.x, p.y, p.w, vconfig.block_title_height);
    sb_box.resize(p.x, p.b() - vconfig.sb_size, p.w, vconfig.sb_size);

    time_sb.set_orientation(P9Scrollbar::vertical);
    track_sb.set_orientation(P9Scrollbar::horizontal);

    time_sb.resize(p.x, p.y + track_box.h(),
            vconfig.sb_size, p.h - track_box.h() - sb_box.h());
    ruler.resize(p.x + time_sb.w(), p.y + track_box.h(),
            vconfig.ruler_size, time_sb.h());

    p = rect(track_group);
    track_sb.resize(p.x, p.b() - vconfig.sb_size, p.w, vconfig.sb_size);
    track_zoom.resize(p.x, p.y, p.w, p.h - track_sb.h());
    track_scroll.resize(p.x, p.y, p.w, p.h - track_sb.h());
    track_tile.resize(track_zoom.x(), track_zoom.y(),
            track_zoom.w(), track_zoom.h());

    // This is overhead required by fltk when you resize anything manually.
    init_sizes();
    body.init_sizes();
    ruler_group.init_sizes();
    track_group.init_sizes();
    track_zoom.init_sizes();
    track_scroll.init_sizes();
    track_tile.init_sizes();

    this->update_scrollbars();

    this->view_config = vconfig;
}


void
BlockView::set_model_config(const BlockModelConfig &config, bool update_all)
{
    const BlockModelConfig &old = this->model_config;
    for (int i = 0; i < Config::max_selections; i++)
        this->set_selection(i, config.selections[i]);

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
    this->zoom = zoom;
    this->track_tile.set_zoom(this->zoom);
    this->ruler.set_zoom(this->zoom);
    this->update_scrollbars();
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


const Selection &
BlockView::get_selection(int selnum) const
{
    ASSERT(0 <= selnum && selnum < Config::max_selections);
    return this->model_config.selections[selnum];
}


void
BlockView::set_selection(int selnum, const Selection &sel)
{
    ASSERT(0 <= selnum && selnum < Config::max_selections);
    for (int i = sel.start_track;
            i < sel.start_track + sel.tracks && i < tracks(); i++)
    {
        track_at(i)->set_selection(selnum, sel);
    }

    this->model_config.selections[selnum] = sel;
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
    track_tile.insert_track(tracknum, t, width);
    this->update_scrollbars();
}


void
BlockView::remove_track(int tracknum, FinalizeCallback finalizer)
{
    TrackView *t = track_tile.remove_track(tracknum);
    t->finalize_callbacks(finalizer);
    delete t;
    this->update_scrollbars();
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
    // DEBUG("update scrolls " << track_tile.track_end());
    this->track_sb.set_scroll_zoom(track_tile.track_end(),
            get_track_scroll(), track_tile.w());

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
    // TODO consider putting the repeated code into their own functions.
    // This does the same stuff as BlockView::set_zoom, but naturally doesn't
    // call update_scrollbars, or we don't get anywhere.
    ZoomInfo new_zoom(end.scale(time_offset), self->get_zoom().factor);
    if (new_zoom != self->get_zoom()) {
        self->zoom = new_zoom;
        self->ruler.set_zoom(self->zoom);
        self->track_tile.set_zoom(self->zoom);
        global_msg_collector()->block_changed(self, UiMsg::msg_zoom);
    }

    // This is the same as set_track_scroll, but can reuse track_end, and
    // doesn't call update_scrollbars.
    double track_offset = self->track_sb.get_offset();
    int track_end = self->track_tile.track_end();
    int max_offset = std::max(0, track_end - self->track_tile.w());
    int offset = std::min(max_offset, int(track_offset * track_end));
    Point new_offset(-offset, 0);
    if (self->track_scroll.get_offset() != new_offset) {
        self->track_scroll.set_offset(new_offset);
        global_msg_collector()->block_changed(self, UiMsg::msg_track_scroll);
    }
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
    BlockView *self = static_cast<BlockView *>(vp);
    self->update_scrollbars();
    int track = self->track_tile.get_dragged_track();
    global_msg_collector()->block_changed(self, UiMsg::msg_track_width, track);
}


// BlockViewWindow ///////////

// Don't let escape kill the window.
static void
block_view_window_cb(Fl_Window *win, void *p)
{
    BlockViewWindow *view = static_cast<BlockViewWindow *>(win);
    global_msg_collector()->window_changed(view, UiMsg::msg_close);
    // TODO remove this, and add a event handling thread to test_block
    if (view->testing)
        Fl_Window::default_callback(win, p);
}


BlockViewWindow::BlockViewWindow(int X, int Y, int W, int H,
        const BlockModelConfig &model_config,
        const BlockViewConfig &view_config,
        const RulerConfig &ruler_config) :
    Fl_Double_Window(X, Y, W, H),
    block(X, Y, W, H, model_config, view_config, ruler_config),
    testing(false)
{
    callback((Fl_Callback *) block_view_window_cb);
    resizable(this);
    // turn off some annoying defaults
    Fl::dnd_text_ops(false); // don't do drag and drop text
    // Fl::visible_focus(false); // doesn't seem to do anything
}


int
BlockViewWindow::handle(int evt)
{
    if (evt == FL_KEYDOWN || evt == FL_KEYUP) {
        // The fact I got it means I have focus.
        int key = Fl::event_key();
        if (evt == FL_KEYDOWN) {
            this->keys_down[key] = true;
        } else {
            // Actually, I get KEYUP even if I don't have focus, and also get
            // the KEYUP half of an event that defocuses.  Eat those up
            // silently.
            if (!this->keys_down[key])
                return true;
            else
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
        case FL_PUSH: case FL_DRAG: case FL_RELEASE:
            global_msg_collector()->event(evt);
            break;
        case FL_FOCUS:
            global_msg_collector()->event(evt, this);
        }
        return true;
    }
    return accepted;
}
