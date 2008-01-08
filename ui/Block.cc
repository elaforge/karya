#include "util.h"
#include "f_util.h"

#include "Block.h"


// BlockModel

// const char *BlockModel::get_title() const // inline

void
BlockModel::set_title(const char *s)
{
    this->title = s;
    // for view in this->views: view.set_title(this->title);
}

void
BlockModel::add_track(int tracknum, const TrackModel &track, int width)
{
}

void
BlockModel::remove_track(int tracknum)
{
}

const TrackModel *
BlockModel::track_at(int tracknum)
{
    return 0;
}

void
BlockModel::set_color_config(const BlockColorConfig &color_config)
{
    // set color_config and update views if necessary
}

void
BlockModel::add_view(BlockView *view)
{
}

void
BlockModel::remove_view(BlockView *view)
{
}


// BlockView

BlockView::BlockView(int X, int Y, int W, int H, BlockModel &model,
        const BlockConfig &config) :
    Fl_Group(X, Y, W, H),
    model(model),
    config(config),

    title(0, 0, 1, 1),
    body(0, 0, 1, 1),
        body_resize_group(0, 0, 1, 1),
        ruler_group(0, 0, 1, 1),
            track_box(0, 0, 1, 1),
            sb_box(0, 0, 1, 1),
            time_sb(0, 0, 1, 1),
            ruler(0, 0, 1, 1),
        track_group(0, 0, 1, 1),
            track_sb(0, 0, 1, 1),
            track_zoom(0, 0, 1, 1),
                track_tile(0, 0, 1, 1)
{
    // The sizes of 1 are so that groups realize that their children are inside
    // of them.  The real resizing will be done in update_sizes
    current(0); // done adding widgets
    body.add(ruler_group);
    body.add(track_group); // fix up hierarchy

    track_box.box(FL_THIN_DOWN_BOX);
    sb_box.box(FL_THIN_DOWN_BOX);

    resizable(body);
    body.resizable(body_resize_group);
    ruler_group.resizable(ruler);
    track_group.resizable(track_zoom);

    update_sizes();
    update_colors();
}

void
BlockView::update_sizes()
{
    // If I resize from parent to child, then children get a lot of spurious
    // resizes as their parents move them around.  on the other hand, if
    // we go the other way, parents mess up their children.
    // Spurious resizes it is.

    title.resize(x(), y(), w(), config.title_size);
    body.resize(x(), y() + config.title_size, w(), h() - config.title_size);
    body_resize_group.resize(body.x() + config.sb_size, body.y(),
            body.w() - config.sb_size, body.h());

    ruler_group.resize(x(), body.y(),
            config.sb_size + config.ruler_size, body.h());
    Rect p = rect(ruler_group);
    track_group.resize(p.r(), body.y(), body.w() - p.w, body.h());

    track_box.resize(p.x, p.y, p.w, config.title_size);
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
    const BlockColorConfig &c = this->model.get_color_config();
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

// const BlockConfig &BlockView::get_config() // inline

void
BlockView::set_config(const BlockConfig &config)
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
