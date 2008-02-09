#include "util.h"
#include "f_util.h"

#include "Block.h"

#include "Track.h"
#include "EventTrack.h"
#include "Ruler.h"


// BlockModel

BlockModel::~BlockModel()
{
    // Any remaining views should have kept this model alive.
    ASSERT(this->views.size() == 0);
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
    ASSERT(0 <= at && at <= tracks.size());
    tracks.insert(tracks.begin() + at, track);
    for (int i = 0; i < views.size(); i++)
        views[i]->insert_track(at, track, width);
}

void
BlockModel::remove_track(int at)
{
    ASSERT(0 <= at && at <= tracks.size());
    for (int i = 0; i < views.size(); i++)
        views[i]->remove_track(at);
    tracks.erase(tracks.begin() + at);
}

void
BlockModel::set_config(const BlockModelConfig &config)
{
    // set config and update views if necessary
}


// BlockView

BlockView::BlockView(int X, int Y, int W, int H,
        boost::shared_ptr<BlockModel> model,
        boost::shared_ptr<const RulerTrackModel> ruler_model,
        const BlockViewConfig &config) :
    Fl_Group(X, Y, W, H),
    model(model),
    config(config),

    title(0, 0, 1, 1),
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
                track_tile(0, 0, 1, 1, model->get_config().bg,
                        config.track_title_height)
{
    // The sizes of 1 are so that groups realize that their children are inside
    // of them.  The real resizing will be done in update_sizes
    current(0); // done adding widgets
    body.add(ruler_group);
    body.add(track_group); // fix up hierarchy
    body_resize_group.hide();

    track_box.box(FL_FLAT_BOX);
    sb_box.box(FL_FLAT_BOX);

    resizable(body);
    body.resizable(body_resize_group);
    ruler_group.resizable(ruler);
    track_group.resizable(track_zoom);

    update_sizes();
    update_colors();

    model->add_view(this);
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
    int wx = 0, wy = 0;

    title.resize(wx, wy, w(), config.block_title_height);
    body.resize(wx, wy + config.block_title_height,
            w(), h() - config.block_title_height);
    body_resize_group.resize(body.x() + config.sb_size, body.y(),
            body.w() - config.sb_size, body.h());

    ruler_group.resize(wx, body.y(),
            config.sb_size + config.ruler_size, body.h());
    Rect p = rect(ruler_group);
    track_group.resize(p.r(), body.y(), body.w() - p.w, body.h());

    track_box.resize(p.x, p.y, p.w, config.block_title_height);
    sb_box.resize(p.x, p.b() - config.sb_size, p.w, config.sb_size);

    time_sb.type(FL_VERTICAL);
    track_sb.type(FL_HORIZONTAL);

    time_sb.resize(p.x, p.y + track_box.h(),
            config.sb_size, p.h - track_box.h() - sb_box.h());
    ruler.resize(p.x + time_sb.w(), p.y + track_box.h(),
            config.ruler_size, time_sb.h());

    p = rect(track_group);
    track_sb.resize(p.x, p.b() - config.sb_size, p.w, config.sb_size);
    track_zoom.resize(p.x, p.y, p.w, p.h - track_sb.h());
    track_tile.resize(track_zoom.x(), track_zoom.y(),
            track_zoom.w(), track_zoom.h());

    // This is overhead required by fltk when you resize anything manually.
    init_sizes();
    body.init_sizes();
    ruler_group.init_sizes();
    track_group.init_sizes();
    track_zoom.init_sizes();
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


// fltk methods
void
BlockView::resize(int X, int Y, int W, int H)
{
    Fl_Group::resize(X, Y, W, H);
}

void
BlockView::redraw()
{
    Fl_Group::redraw();
}

// called by fltk
int
BlockView::handle(int evt)
{
    return Fl_Group::handle(evt);
}


// api methods
// const ZoomInfo &BlockView::get_zoom() // inline

void
BlockView::set_zoom(const ZoomInfo &zoom)
{
}

// const BlockViewConfig &BlockView::get_config() // inline

void
BlockView::set_config(const BlockViewConfig &config)
{
}

const Selection &
BlockView::get_selection() const
{
}

void
BlockView::set_selection(int selnum, const Selection &sel)
{
}

void
BlockView::set_title(const char *s)
{
    title.value(s);
}


void
BlockView::insert_track(int at, const TrackModel &track, int width)
{
    TrackView *t;

    if (track.track)
        t = new EventTrackView(track.track);
    else if (track.ruler)
        t = new RulerTrackView(track.ruler);
    else
        t = new DividerView(track.divider);
    track_tile.insert_track(at, t, width);
}


void
BlockView::remove_track(int at)
{
    TrackView *t = track_tile.remove_track(at);
    delete t;
}
