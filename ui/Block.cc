#include "util.h"
#include "f_util.h"

#include "MsgCollector.h"
#include "Track.h"
#include "EventTrack.h"
#include "Ruler.h"

#include "Block.h"

// Try to avoid running into the little resize tab on the mac.
static const int mac_resizer_width = 15;

// BlockModel

int BlockModel::current_serial_number = 0;

BlockModel::~BlockModel()
{
    // Any remaining views should have kept this model alive.
    ASSERT(this->views.size() == 0);
    // DEBUG("destroy block model " << this->serial_number);
}

void
BlockModel::set_title(const char *s)
{
    this->title = s;
    for (int i = 0; i < views.size(); i++)
        views[i]->set_title(s);
}

void
BlockModel::insert_track(int at, const TrackModel &track, int width)
{
    ASSERT(0 <= at && at <= tracks());
    _tracks.insert(_tracks.begin() + at, std::make_pair(track, width));
    for (int i = 0; i < views.size(); i++)
        views[i]->insert_track(at, track, width);
}

void
BlockModel::remove_track(int at)
{
    ASSERT(0 <= at && at <= tracks());
    for (int i = 0; i < views.size(); i++)
        views[i]->remove_track(at);
    _tracks.erase(_tracks.begin() + at);
}

void
BlockModel::set_config(const BlockModelConfig &config)
{
    // TODO set config and update views if necessary
}


// BlockView

BlockView::BlockView(int X, int Y, int W, int H,
        boost::shared_ptr<BlockModel> model,
        boost::shared_ptr<const RulerTrackModel> ruler_model,
        const BlockViewConfig &config) :
    Fl_Group(X, Y, W, H),
    model(model),
    config(config),
    selections(Config::max_selections),

    title(0, 0, 1, 1),
    status_line(0, 0, 1, 1),
    body(0, 0, 1, 1),
        body_resize_group(0, 0, 1, 1, "resize group"),
        ruler_group(0, 0, 1, 1),
            track_box(0, 0, 1, 1),
            sb_box(0, 0, 1, 1),
            time_sb(0, 0, 1, 1),
            ruler(ruler_model),
        track_group(0, 0, 1, 1),
            track_sb(0, 0, 1, 1),
            track_zoom(0, 0, 1, 1),
                track_scroll(0, 0, 1, 1),
                    track_tile(0, 0, 1, 1, model->get_config().bg,
                            config.track_title_height)
{
    // The sizes of 1 are so that groups realize that their children are inside
    // of them.  The real resizing will be done in update_sizes
    current(0); // done adding widgets
    body.add(ruler_group);
    body.add(track_group); // fix up hierarchy
    body_resize_group.hide();

    status_line.visible_focus(false); // TODO remove from tabbing
    status_line.box(FL_FLAT_BOX);
    track_box.box(FL_FLAT_BOX);
    sb_box.box(FL_FLAT_BOX);
    // track_scroll.type(0); // no scrollbars
    time_sb.callback(BlockView::scrollbar_cb, static_cast<void *>(this));
    track_sb.callback(BlockView::scrollbar_cb, static_cast<void *>(this));
    track_tile.callback(BlockView::update_scrollbars_cb,
            static_cast<void *>(this));

    resizable(body);
    body.resizable(body_resize_group);
    ruler_group.resizable(ruler);
    track_group.resizable(track_zoom);
    // track_zoom.resizable(track_scroll);

    update_sizes();
    update_colors();

    // Initialize the view with the model's state.
    this->set_title(model->get_title());
    for (int i = 0; i < model->tracks(); i++) {
        std::pair<TrackModel, int> trackw = model->track_at(i);
        this->insert_track(i, trackw.first, trackw.second);
    }
    model->add_view(this);

    // Initialize zoom to default.  This will cause zooming children to
    // properly position their widgets.
    this->set_zoom(this->zoom);
    // Update the scrollbars last, after I've added all the tracks.
    update_scrollbars();
}

BlockView::~BlockView()
{
    model->remove_view(this);
}


void
BlockView::update_sizes()
{
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

    title.resize(wx, wy, w(), config.block_title_height);
    status_line.resize(wx, h() - config.status_size,
            w() - mac_resizer_width, config.status_size);
    status_line.textsize(config.status_size - 4);
    body.resize(wx, wy + title.h(),
            w(), h() - title.h() - status_line.h());
    body_resize_group.resize(body.x() + config.sb_size, body.y(),
            body.w() - config.sb_size, body.h());

    ruler_group.resize(wx, body.y(),
            config.sb_size + config.ruler_size, body.h());
    Rect p = rect(ruler_group);
    track_group.resize(p.r(), body.y(), body.w() - p.w, body.h());

    track_box.resize(p.x, p.y, p.w, config.block_title_height);
    sb_box.resize(p.x, p.b() - config.sb_size, p.w, config.sb_size);

    time_sb.set_orientation(P9Scrollbar::vertical);
    track_sb.set_orientation(P9Scrollbar::horizontal);

    time_sb.resize(p.x, p.y + track_box.h(),
            config.sb_size, p.h - track_box.h() - sb_box.h());
    ruler.resize(p.x + time_sb.w(), p.y + track_box.h(),
            config.ruler_size, time_sb.h());

    p = rect(track_group);
    track_sb.resize(p.x, p.b() - config.sb_size, p.w, config.sb_size);
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
}


void
BlockView::update_colors()
{
    const BlockModelConfig &c = this->model->get_config();
    this->track_box.color(color_to_fl(c.track_box));
    this->sb_box.color(color_to_fl(c.sb_box));
    // redraw?
}


// Update scrollbar display based on the current zoom and scroll offset.
void
BlockView::update_scrollbars()
{
    // DEBUG("update scrolls " << track_tile.track_end());
    this->track_sb.set_scroll_zoom(track_tile.track_end(),
            get_track_scroll(), track_tile.w());

    const ZoomInfo &zoom = this->get_zoom();
    // The scale(1)s just convert a TrackPos to a double.
    int track_h = track_tile.h() - config.track_title_height;
    this->time_sb.set_scroll_zoom(track_tile.time_end().scale(1),
            zoom.offset.scale(1),
            (zoom.to_trackpos(track_h) - zoom.offset).scale(1));
}


// fltk methods
void
BlockView::resize(int X, int Y, int W, int H)
{
    Fl_Group::resize(X, Y, W, H);
    status_line.size(w() - mac_resizer_width, status_line.h());
    this->update_scrollbars();
}


// api methods

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


void
BlockView::set_config(const BlockViewConfig &config)
{
    // TODO
}

const Selection &
BlockView::get_selection(int selnum) const
{
    ASSERT(0 <= selnum && selnum < Config::max_selections);
    return this->selections[selnum];
}

void
BlockView::set_selection(int selnum, const Selection &sel)
{
    ASSERT(0 <= selnum && selnum < Config::max_selections);
    // clear old selection
    const Selection &old = this->selections[selnum];
    const Color &color = model->get_config().select[selnum];
    Selection empty;
    for (int i = old.start_track;
            i < old.start_track + old.tracks && i < tracks(); i++)
    {
        track_at(i)->set_selection(selnum, color, empty);
    }

    // draw new one
    for (int i = sel.start_track;
            i < sel.start_track + sel.tracks && i < tracks(); i++)
    {
        track_at(i)->set_selection(selnum, color, sel);
    }

    this->selections[selnum] = sel;
}


void
BlockView::insert_track(int at, const TrackModel &track, int width)
{
    TrackView *t;

    // DEBUG("view insert at " << at);
    if (track.track) {
        t = new EventTrackView(track.track, track.ruler);
        t->callback(BlockView::update_scrollbars_cb, static_cast<void *>(this));
    } else if (track.ruler) {
        t = new RulerTrackView(track.ruler);
    } else {
        t = new DividerView(track.divider);
    }
    track_tile.insert_track(at, t, width);
    this->update_scrollbars();
}


void
BlockView::remove_track(int at)
{
    TrackView *t = track_tile.remove_track(at);
    delete t;
    this->update_scrollbars();
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
    self->zoom = ZoomInfo(end.scale(time_offset), self->get_zoom().factor);
    self->ruler.set_zoom(self->zoom);
    self->track_tile.set_zoom(self->zoom);

    // This is the same as set_track_scroll, but can reuse track_end, and
    // doesn't call update_scrollbars.
    double track_offset = self->track_sb.get_offset();
    int track_end = self->track_tile.track_end();
    int max_offset = std::max(0, track_end - self->track_tile.w());
    int offset = std::min(max_offset, int(track_offset * track_end));
    self->track_scroll.set_offset(Point(-offset, 0));
}


// The tracks or events changed, so fix up the scrollbars.
void
BlockView::update_scrollbars_cb(Fl_Widget *w, void *vp)
{
    BlockView *self = static_cast<BlockView *>(vp);
    self->update_scrollbars();
}


// BlockViewWindow ///////////

// Don't let escape kill the window.
static void
block_view_window_cb(Fl_Window *win, void *p)
{
    BlockViewWindow *view = static_cast<BlockViewWindow *>(win);
    global_msg_collector()->close(view);
    // TODO remove this, and add a event handling thread to test_block
    if (view->testing)
        Fl_Window::default_callback(win, p);
}

BlockViewWindow::BlockViewWindow(int X, int Y, int W, int H,
        boost::shared_ptr<BlockModel> model,
        boost::shared_ptr<const RulerTrackModel> ruler_model,
        const BlockViewConfig &config) :
    Fl_Double_Window(X, Y, W, H),
    block(X, Y, W, H, model, ruler_model, config),
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

    bool accepted = false;
    if (evt == FL_PUSH || evt == FL_MOVE) {
        // see if someone else wants it
        accepted = Fl_Group::handle(evt);
    }
    if (!accepted && (evt == FL_PUSH || evt == FL_DRAG)) {
        global_msg_collector()->event(evt);
        return true;
    }
    return accepted;
}
