#include <boost/shared_ptr.hpp>

#include <FL/Fl.H>
#include <FL/Fl_Double_Window.H>

#include "f_util.h"

#include "Block.h"

#include "EventTrack.h"
#include "Ruler.h"


BlockViewConfig block_view_config()
{
    BlockViewConfig c;
    c.orientation = VerticalTime;
    c.zoom_speed = 1;
    c.block_title_height = 20;
    c.track_title_height = 20;
    c.sb_size = 12;
    c.ruler_size = 26;
    return c;
}

BlockModelConfig block_model_config()
{
    BlockModelConfig c;
    c.select[0] = Color(0xffff00);
    c.select[1] = Color(0xffff00);
    c.select[2] = Color(0xffff00);
    c.bg = Color(0xdddddd);
    c.track_box = Color(0x44ffff);
    c.sb_box = Color(0x00ffff);
    return c;
}

static boost::shared_ptr<Marklist>
m44_marklist()
{
    boost::shared_ptr<Marklist> mlist(new Marklist());
    // char *name = "";

    for (int i = 0; i < 600; i++) {
        TrackPos t = i*8;
        if (i % 4 == 0) {
            Mark m(1, 3, Color(116, 70, 0, 90), "", 0, 0);
            mlist->push_back(std::pair<TrackPos, Mark>(t, m));
        } else {
            Mark m(2, 2, Color(255, 100, 50, 90), "", 0, 0);
            mlist->push_back(std::pair<TrackPos, Mark>(t, m));
        }
    }
    return mlist;
}

int
main(int argc, char **argv)
{
    BlockViewConfig view_config = block_view_config();
    BlockModelConfig config = block_model_config();
    boost::shared_ptr<BlockModel> model(new BlockModel(config));

    // static const Marklists no_marks = Marklists();
    Color ruler_bg = Color(255, 220, 128);

    Marklists mlists;
    mlists.push_back(m44_marklist());

    model->set_title("hi there");

    Color eventc = Color(240, 220, 200);
    // Color event_bg = Color(127, 127, 127);
    Color event_bg = Color(255, 255, 255);

    boost::shared_ptr<EventTrackModel> t(new EventTrackModel(event_bg));
    boost::shared_ptr<DividerModel> d(new DividerModel(Color(0x0000ff)));
    boost::shared_ptr<RulerTrackModel> r(new RulerTrackModel(mlists, ruler_bg,
                true, false, false));
    boost::shared_ptr<RulerTrackModel> tr(new RulerTrackModel(mlists, ruler_bg,
                false, true, true));

    t->insert_event(TrackPos(0), EventModel(TrackPos(16), eventc));
    t->insert_event(TrackPos(32), EventModel(TrackPos(64), eventc));

    TrackModel track(t, tr);
    TrackModel ruler(r);
    TrackModel divider(d);

    model->insert_track(0, track, 30);
    model->insert_track(1, divider, 10);
    model->insert_track(2, ruler, 30);
    model->insert_track(3, track, 60);

    BlockViewWindow view(0, 0, 200, 400, model, r, view_config);

    t->insert_event(TrackPos(128), EventModel(TrackPos(32), eventc));

    print_children(&view);
    // DEBUG(1);
    // view.resize(0, 0, 100, 100);
    // print_children(&view);
    // DEBUG(2);
    // view.resize(0, 0, 300, 300);
    // print_children(&view);

    // drag over and back
    // view.block.drag_tile(Point(104, 0), Point(60, 0));
    // view.block.drag_tile(Point(104, 0), Point(101, 0));
    // view.block.tile_init();

    // internal to right
    // view.block.drag_tile(Point(84, 0), Point(88, 0));
    // view.block.drag_tile(Point(84, 0), Point(96, 0));

    // internal to left
    // view.block.drag_tile(Point(84, 0), Point(64, 0));

    // rightmost to left
    // view.block.drag_tile(Point(144, 0), Point(134, 0));

    // rightmost overlaps middle
    // view.block.drag_tile(Point(144, 0), Point(60, 0));
    // view.block.drag_tile(Point(144, 0), Point(70, 0));
    // view.block.drag_tile(Point(70, 0), Point(144, 0));
    // print_children(&view);

    view.show(argc, argv);
    Fl::run();
    printf("complete\n");
}
