#include <FL/Fl.H>
#include <FL/Fl_Double_Window.H>

#include "f_util.h"

#include "Block.h"

#include "EventTrack.h"
#include "Ruler.h"


BlockViewConfig block_view_config()
{
    BlockViewConfig c;
    c.orientation = HorizontalTime;
    c.zoom_speed = 1;
    c.block_title_height = 20;
    c.track_title_height = 20;
    c.sb_size = 18;
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
    BlockViewConfig view_config = block_view_config();
    BlockModelConfig config = block_model_config();
    boost::shared_ptr<BlockModel> model(new BlockModel(config));

    // static const Marklists no_marks = Marklists();
    Color ruler_bg = Color(255, 220, 128);

    Marklists mlists;
    const Marklist *ms(m44_marklist());
    mlists.push_back(ms);

    model->set_title("hi there");

    boost::shared_ptr<EventTrackModel> t(new EventTrackModel());
    boost::shared_ptr<DividerModel> d(new DividerModel(Color(0x0000ff)));
    boost::shared_ptr<RulerTrackModel> r(new RulerTrackModel(mlists, ruler_bg));

    boost::shared_ptr<EventTrackModel> null_track;
    boost::shared_ptr<DividerModel> null_divider;
    boost::shared_ptr<RulerTrackModel> null_ruler;

    TrackModel track(t, null_ruler, null_divider);
    TrackModel ruler(null_track, r, null_divider);
    TrackModel divider(null_track, null_ruler, d);

    BlockViewWindow view(0, 0, 200, 200, model, r, view_config);

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
    printf("complete\n");
}
