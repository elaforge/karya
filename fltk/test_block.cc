#include <FL/Fl.H>
#include <FL/Fl_Double_Window.H>

#include "f_util.h"

#include "Block.h"

#include "EventTrack.h"
#include "Ruler.h"


Color selection_colors[] = {
    Color(0, 0, 255, 45),
    Color(255, 0, 255, 90),
    Color(0, 255, 255, 90)
};

BlockViewConfig block_view_config()
{
    BlockViewConfig c;
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
    c.bg = Color(0xdddddd);
    c.track_box = Color(0x44ffff);
    c.sb_box = Color(0x00ffff);
    return c;
}

typedef static std::vector<std::pair<TrackPos, Mark> > MarkData;
static MarkData m44_marks;

void m44_set()
{
    MarkData &mlist = m44_marks;
    char name[32];
    Color major = Color(116, 70, 0, 90);
    Color minor = Color(225, 100, 50, 90);

    for (int i = 0; i < 100; i++) {
        TrackPos t = TrackPos(i*8);
        if (i % 4 == 0) {
            sprintf(name, "%d", i / 4);
            Mark m(1, 3, major, strdup(name), 0, 0);
            mlist.push_back(std::make_pair(t, m));
        } else {
            // sprintf(name, "%d.%d", i / 4, i % 4);
            Mark m(2, 2, minor, 0, 0, 0);
            mlist.push_back(std::make_pair(t, m));
        }
    }
}

int
m44_find_marks(TrackPos *start_pos, TrackPos *end_pos,
        TrackPos **ret_tps, Mark **ret_marks)
{
    MarkData &mlist = m44_marks;
    int count = 0;
    int start = 0;
    for (; start < mlist.size(); start++) {
        if (mlist[start].first >= *start_pos)
            break;
    }
    while (start + count < mlist.size()) {
        if (mlist[start+count].first >= *end_pos)
            break;
        count++;
    }

    // One extra on the top and bottom so they get drawn when partially
    // offscreen.  This will still clip if marks are close.
    if (start)
        start--;
    if (count < mlist.size())
        count += 2;

    *ret_tps = (TrackPos *) calloc(count, sizeof(TrackPos));
    *ret_marks = (Mark *) calloc(count, sizeof(Mark));
    for (int i = 0; i < count; i++) {
        // Placement new since malloced space is uninitialized.
        new((*ret_tps) + i) TrackPos(mlist[start+i].first);
        new((*ret_marks) + i) Mark(mlist[start+i].second);
        char **namep = &(*ret_marks)[i].name;
        if (*namep)
            *namep = strdup(*namep);
    }
    return count;
}


typedef static std::vector<std::pair<TrackPos, Event> > TrackData;
static TrackData t1_events;

void t1_set()
{
    TrackData &e = t1_events;
    Color eventc = Color(200, 200, 170);
    TextStyle style;

    e.push_back(std::make_pair(TrackPos(0),
        Event("4c#@$", TrackPos(16), eventc, style)));
    e.push_back(std::make_pair(TrackPos(32),
        Event("4d-", TrackPos(4), eventc, style)));
    e.push_back(std::make_pair(TrackPos(38),
        Event("5cb", TrackPos(4), eventc, style)));
    e.push_back(std::make_pair(TrackPos(44),
        Event("6--", TrackPos(4), eventc, style)));
    e.push_back(std::make_pair(TrackPos(50),
        Event("7--", TrackPos(4), eventc, style)));
}

int
t1_find_events(TrackPos *start_pos, TrackPos *end_pos,
        TrackPos **ret_tps, Event **ret_events)
{
    int count = 0;
    int start = 0;
    for (; start < t1_events.size(); start++) {
        if (t1_events[start].first + t1_events[start].second.duration
                >= *start_pos)
            break;
    }
    while (start + count < t1_events.size()) {
        if (t1_events[start+count].first >= *end_pos)
            break;
        count++;
    }

    *ret_tps = (TrackPos *) calloc(count, sizeof(TrackPos));
    *ret_events = (Event *) calloc(count, sizeof(Event));
    for (int i = 0; i < count; i++) {
        // Placement new since malloced space is uninitialized.
        new((*ret_tps) + i) TrackPos(t1_events[start+i].first);
        new((*ret_events) + i) Event(t1_events[start+i].second);
        char **textp = &(*ret_events)[i].text;
        if (*textp)
            *textp = strdup(*textp);
    }
    return count;
}

int
t1_last_track_pos(TrackPos *pos)
{
    int i = t1_events.size() - 1;
    *pos = t1_events[i].first + t1_events[i].second.duration;
    return 1;
}

int
main(int argc, char **argv)
{
    BlockViewConfig view_config = block_view_config();
    BlockModelConfig config = block_model_config();

    Marklists mlists;
    mlists.push_back(Marklist(m44_find_marks));
    Marklists nomarks;

    Color ruler_bg = Color(255, 230, 160);
    Color track_bg = Color(255, 255, 255);

    t1_set();
    m44_set();

    RulerConfig ruler(ruler_bg, true, false, false);
    ruler.marklists = mlists;
    RulerConfig truler(ruler_bg, false, true, true);
    truler.marklists = mlists;
    DividerConfig divider(Color(0x0000ff));
    EventTrackConfig track(track_bg, t1_find_events, t1_last_track_pos);
    EventTrackConfig track2(track_bg, t1_find_events, t1_last_track_pos);

    BlockViewWindow view(300, 250, 200, 200, config, view_config,
            Tracklike(&ruler));
    // view.border(0);

    view.testing = true;
    view.block.set_status("no status yet");
    view.block.set_title("hi there");

    view.block.insert_track(0, Tracklike(&divider), 8);
    view.block.insert_track(1, Tracklike(&ruler), 25);
    view.block.insert_track(2, Tracklike(&track, &truler), 30);
    view.block.insert_track(3, Tracklike(&track2, &truler), 50);

    /*
    view.block.set_selection(0, Selection(selection_colors[0],
                1, TrackPos(60), 4, TrackPos(56)));
    view.block.set_selection(0, Selection(selection_colors[0],
                1, TrackPos(60), 4, TrackPos(46)));
    view.block.set_selection(0, Selection(selection_colors[0],
                1, TrackPos(0), 4, TrackPos(56)));
    view.block.set_selection(1, Selection(selection_colors[1],
                1, TrackPos(64), 4, TrackPos(0)));
    */

    // print_children(&view);
    // DEBUG(1);
    // view.resize(0, 0, 100, 100);
    // print_children(&view);
    // DEBUG(2);
    // view.resize(0, 0, 300, 300);
    print_children(&view);

    view.show(argc, argv);
    Fl::run();
    printf("complete\n");
}
