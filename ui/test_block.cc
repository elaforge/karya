#include <FL/Fl.H>
#include <FL/Fl_Double_Window.H>

#include "f_util.h"

#include "Block.h"

#include "EventTrack.h"
#include "Ruler.h"


BlockConfig block_config()
{
    BlockConfig c;
    c.orientation = HorizontalTime;
    c.zoom_speed = 1;
    c.block_title_height = 20;
    c.track_title_height = 20;
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

static Marklist *
m44_marklist()
{
    static Marklist *mlist = new Marklist();
    char *name = "";

    for (int i = 0; i < 600; i++) {
        TrackPos t = i*8;
        if (i % 4 == 0) {
            Mark m(1, 3, Color(116, 70, 0), NULL, 0, 0);
            mlist->push_back(std::pair<TrackPos, Mark>(t, m));
        } else {
            Mark m(2, 2, Color(255, 100, 50), NULL, 0, 0);
            mlist->push_back(std::pair<TrackPos, Mark>(t, m));
        }
    }
    return mlist;
}

int
main(int argc, char **argv)
{
    BlockColorConfig cconfig = color_config();
    BlockConfig config = block_config();
    BlockModel *model = new BlockModel(cconfig);

    // static const Marklists no_marks = Marklists();
    Color ruler_bg = Color(255, 220, 128);

    Marklists mlists;
    Marklist *ms = m44_marklist();
    mlists.push_back(ms);
    RulerTrackModel *r = new RulerTrackModel(mlists, ruler_bg);
    BlockViewWindow view(0, 0, 200, 200, *model, *r, config);

    model->set_title("hi there");

    EventTrackModel *t = new EventTrackModel();
    DividerModel *d = new DividerModel(Color(0x0000ff));

    TrackModel track(t, 0, 0);
    TrackModel ruler(0, r, 0);
    TrackModel divider(0, 0, d);

    model->insert_track(0, track, 40);
    model->insert_track(1, track, 60);
    // model->insert_track(1, divider, 20);
    // model->insert_track(2, ruler, 25);
    // model->insert_track(3, track, 60);

    print_children(&view);

    // internal to right
    // view.block.drag_tile(Point(84, 0), Point(88, 0));
    // view.block.drag_tile(Point(84, 0), Point(96, 0));

    // internal to left
    // view.block.drag_tile(Point(84, 0), Point(64, 0));

    // rightmost to left
    // view.block.drag_tile(Point(144, 0), Point(134, 0));

    // rightmost overlaps middle
    // view.block.drag_tile(Point(144, 0), Point(60, 0));
    view.block.drag_tile(Point(144, 0), Point(70, 0));
    view.block.drag_tile(Point(70, 0), Point(144, 0));
    print_children(&view);

    view.show(argc, argv);
    Fl::run();
}
