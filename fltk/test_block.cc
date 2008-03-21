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
    c.ruler_size = 18;
    c.status_size = 16;
    return c;
}

BlockModelConfig block_model_config()
{
    BlockModelConfig c;
    c.select[0] = Color(0, 0, 255, 45);
    c.select[1] = Color(255, 0, 255, 90);
    c.select[2] = Color(0, 255, 255, 90);
    c.bg = Color(0xdddddd);
    c.track_box = Color(0x44ffff);
    c.sb_box = Color(0x00ffff);
    return c;
}

static boost::shared_ptr<Marklist>
m44_marklist()
{
    boost::shared_ptr<Marklist> mlist(new Marklist());
    char name[32];
    Color major = Color(116, 70, 0, 90);
    Color minor = Color(225, 100, 50, 90);

    for (int i = 0; i < 600; i++) {
        TrackPos t = i*8;
        if (i % 4 == 0) {
            sprintf(name, "%d", i / 4);
            Mark m(1, 3, major, name, 0, 0);
            mlist->push_back(std::pair<TrackPos, Mark>(t, m));
        } else {
            // sprintf(name, "%d.%d", i / 4, i % 4);
            Mark m(2, 2, minor, "", 0, 0);
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

    Color reddish(255, 230, 230);
    Color greenish(230, 255, 230);
    Color blueish(230, 230, 255);
    Color white(255, 255, 255);

    // static const Marklists no_marks = Marklists();
    Color ruler_bg = Color(255, 230, 160);

    Marklists mlists;
    mlists.push_back(m44_marklist());
    Marklists nomarks;

    model->set_title("hi there");

    Color eventc = Color(200, 200, 170);

    boost::shared_ptr<EventTrackModel> t1(new EventTrackModel(white));
    boost::shared_ptr<EventTrackModel> t2(new EventTrackModel(reddish));
    boost::shared_ptr<EventTrackModel> t3(new EventTrackModel(greenish));

    boost::shared_ptr<DividerModel> d(new DividerModel(Color(0x0000ff)));
    boost::shared_ptr<RulerTrackModel> r(new RulerTrackModel(mlists, ruler_bg,
                true, false, false));
    boost::shared_ptr<RulerTrackModel> tr(new RulerTrackModel(mlists, ruler_bg,
                false, true, true));
    boost::shared_ptr<RulerTrackModel> tr2(
            new RulerTrackModel(nomarks, ruler_bg, false, true, true));

    TextStyle style;

    t1->insert_event(TrackPos(0),
            EventModel("4c#", TrackPos(16), eventc, style));
    t1->insert_event(TrackPos(32),
            EventModel("4d-", TrackPos(64), eventc, style));

    TrackModel track(t1, tr);
    TrackModel track2(t1, tr2);
    TrackModel ruler(r);
    TrackModel divider(d);

    model->insert_track(0, track, 70);
    model->insert_track(1, divider, 4);
    model->insert_track(2, ruler, 50);
    model->insert_track(3, track, 30);
    model->insert_track(4, track2, 30);

    BlockViewWindow view(300, 250, 200, 200, model, r, view_config);
    view.testing = true;
    view.block.set_status("no status yet");

    t1->insert_event(TrackPos(128),
        EventModel("0.7", TrackPos(32), eventc, style));
    t1->insert_event(TrackPos(175),
        EventModel("0.4", TrackPos(8), eventc, style));

    t1->insert_event(TrackPos(0),
            EventModel("pwned!", TrackPos(8), eventc, style));

    view.block.set_selection(0, Selection(0, TrackPos(60), 4, TrackPos(56)));
    view.block.set_selection(0, Selection(0, TrackPos(0), 4, TrackPos(56)));
    view.block.set_selection(1, Selection(0, TrackPos(64), 4, TrackPos(0)));

    // print_children(&view);
    // DEBUG(1);
    // view.resize(0, 0, 100, 100);
    // print_children(&view);
    // DEBUG(2);
    // view.resize(0, 0, 300, 300);
    // print_children(&view);

    view.show(argc, argv);
    Fl::run();
    printf("complete\n");
}
