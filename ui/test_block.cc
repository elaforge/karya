#include <FL/Fl.H>
#include <FL/Fl_Double_Window.H>

#include "f_util.h"

#include "Block.h"

BlockConfig block_config()
{
    BlockConfig c;
    c.orientation = HorizontalTime;
    c.zoom_speed = 1;
    c.title_size = 20;
    c.sb_size = 18;
    c.ruler_size = 26;
    return c;
}

BlockColorConfig color_config()
{
    BlockColorConfig c;
    c.select.push_back(Color(0xffff00));
    c.bg = Color(0xdddddd);
    c.track_box = Color(0x44ffff);
    c.sb_box = Color(0x00ffff);
    return c;
}

int
main(int argc, char **argv)
{

    BlockColorConfig cconfig = color_config();
    BlockConfig config = block_config();
    BlockModel *model = new BlockModel(cconfig);
    BlockViewWindow view(0, 0, 200, 200, *model, config);

    model->set_title("hi there");

    TrackModel *t = new TrackModel();
    DividerModel *d = new DividerModel(Color(0x0000ff));
    RulerModel *r = new RulerModel();

    TracklikeModel track(t, 0, 0);
    TracklikeModel ruler(0, r, 0);
    TracklikeModel divider(0, 0, d);
    model->insert_track(0, track, 40);
    model->insert_track(1, ruler, 25);
    model->insert_track(1, divider, 4);

    print_children(&view);

    view.show(argc, argv);
    Fl::run();
}
