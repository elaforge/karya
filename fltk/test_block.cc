#include <iostream>
#include <FL/Fl.H>
#include <FL/Fl_Double_Window.H>

#include "f_util.h"

#include "Block.h"
#include "EventTrack.h"
#include "Ruler.h"
#include "SkeletonDisplay.h"
#include "MsgCollector.h"


static const bool arrival_beats = false;


Color selection_colors[] = {
    Color(0, 0, 255, 90),
    Color(255, 0, 255, 90),
    Color(0, 255, 255, 90)
};

BlockViewConfig block_view_config()
{
    BlockViewConfig c;
    c.block_title_height = 20;
    c.track_title_height = 20;
    c.skel_height = 16;
    c.sb_size = 12;
    c.status_size = 16;
    return c;
}

BlockModelConfig block_model_config()
{
    BlockModelConfig c;
    c.bg = Color(0xdddddd);
    c.track_box = Color(0x44ffff);
    c.sb_box = Color(0x00ffff);
    c.track_char = 'K';
    c.sb_char = 'S';
    return c;
}

SkeletonConfig skeleton_config(int *pairs, int len)
{
    SkeletonConfig skel;
    skel.len = len;
    skel.parents = (int *) calloc(len, sizeof(int));
    skel.children = (int *) calloc(len, sizeof(int));
    for (int i = 0; i < len; i++) {
        skel.parents[i] = *pairs;
        pairs++;
        skel.children[i] = *pairs;
        pairs++;
    }
    return skel;
}

typedef static std::vector<std::pair<ScoreTime, Mark> > MarkData;
static MarkData m44_marks;

static ScoreTime m44_last_pos;
void m44_set()
{
    MarkData &mlist = m44_marks;
    char name[32];
    Color major = Color(116, 70, 0, 90);
    Color minor = Color(225, 100, 50, 90);

    const int nmarks = 200;
    for (int i = 0; i < nmarks; i++) {
        ScoreTime t = ScoreTime(i*8);
        if (i % 4 == 0) {
            sprintf(name, "%d", i / 4);
            Mark m(1, 3, major, strdup(name), 0, 0);
            mlist.push_back(std::make_pair(t, m));
        } else {
            sprintf(name, "long %d.%d", i / 4, i % 4);
            Mark m(2, 2, minor, strdup(name), 0, 0);
            mlist.push_back(std::make_pair(t, m));
        }
    }
    m44_last_pos = ScoreTime((nmarks-1) * 8);
}

int
m44_find_marks(ScoreTime *start_pos, ScoreTime *end_pos,
        ScoreTime **ret_tps, Mark **ret_marks)
{
    MarkData &mlist = m44_marks;
    size_t count = 0;
    size_t start = 0;
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

    *ret_tps = (ScoreTime *) calloc(count, sizeof(ScoreTime));
    *ret_marks = (Mark *) calloc(count, sizeof(Mark));
    for (size_t i = 0; i < count; i++) {
        // Placement new since malloced space is uninitialized.
        new((*ret_tps) + i) ScoreTime(mlist[start+i].first);
        new((*ret_marks) + i) Mark(mlist[start+i].second);
        char **namep = &(*ret_marks)[i].name;
        if (*namep)
            *namep = strdup(*namep);
    }
    return count;
}

struct EventInfo {
    EventInfo(ScoreTime pos, Event event, int rank) :
        pos(pos), event(event), rank(rank)
    {}
    ScoreTime pos;
    Event event;
    int rank;
    bool operator<(const EventInfo &o) const {
        if (pos == o.pos)
            return rank < o.rank;
        else
            return pos < o.pos;
    }
};


typedef std::vector<EventInfo> TrackData;
static TrackData t1_events;
typedef std::vector<std::pair<ScoreTime, double> > SampleData;
static SampleData t1_samples;

void t1_set()
{
    TrackData &e = t1_events;
    Color eventc = Color(200, 200, 170);
    TextStyle style;
    style.font = FL_HELVETICA;
    style.size = 9;

    e.push_back(EventInfo(ScoreTime(0),
        Event("4c#@$", ScoreTime(16), eventc, style), 0));
    e.push_back(EventInfo(ScoreTime(32),
        Event("4d-", ScoreTime(4), eventc, style), 0));
    e.push_back(EventInfo(ScoreTime(38),
        Event("5cb", ScoreTime(4), eventc, style), 0));
    e.push_back(EventInfo(ScoreTime(44),
        Event("6--", ScoreTime(4), eventc, style), 0));
    e.push_back(EventInfo(ScoreTime(50),
        Event("7--", ScoreTime(4), eventc, style), 0));
    e.push_back(EventInfo(ScoreTime(128),
        Event("late!", ScoreTime(64), eventc, style), 0));
    // coincident with rank 0
    e.push_back(EventInfo(ScoreTime(128),
        Event("bg1", ScoreTime(8), eventc, style), 1));
    // overlaps with rank 0
    e.push_back(EventInfo(ScoreTime(160),
        Event("bg2", ScoreTime(8), eventc, style), 1));
    e.push_back(EventInfo(ScoreTime(164),
        Event("bg2.5", ScoreTime(8), eventc, style), 1));
    // coincedent with end of rank 0
    e.push_back(EventInfo(ScoreTime(128+64),
        Event("bg3", ScoreTime(0), eventc, style), 1));
    // doesn't overlap rank 0
    e.push_back(EventInfo(ScoreTime(230),
        Event("bg4", ScoreTime(0), eventc, style), 0));

    SampleData &s = t1_samples;
    for (int i = 0; i < 145; i++) {
        s.push_back(std::make_pair(ScoreTime(i), fmod(i / 60.0, 1)));
    }
    /*
    e.push_back(EventInfo(ScoreTime(0*8),
        Event("main", ScoreTime(8), eventc, style), 0));
    for (int i = 0; i < 100; i++) {
        char buf[32];
        sprintf(buf, "e%d", i);
        e.push_back(EventInfo(ScoreTime(i*8),
            Event(strdup(buf), ScoreTime(8), eventc, style), 1));
    }
    */

    if (arrival_beats) {
        for (size_t i = 0; i < e.size(); i++) {
            ScoreTime p = e[i].pos;
            ScoreTime dur = e[i].event.duration;
            e[i].pos = p + dur;
            e[i].event.duration = -dur;
        }
        std::sort(e.begin(), e.end());
    }
}

int
t1_find_events(ScoreTime *start_pos, ScoreTime *end_pos,
        ScoreTime **ret_tps, Event **ret_events, int **ret_ranks)
{
    size_t count = 0;
    size_t start = 0;
    for (; start < t1_events.size(); start++) {
        if (t1_events[start].pos + t1_events[start].event.duration
                >= *start_pos)
            break;
    }
    while (start + count < t1_events.size()) {
        if (t1_events[start+count].pos >= *end_pos)
            break;
        count++;
    }
    start = 0;
    count = t1_events.size();

    *ret_tps = (ScoreTime *) calloc(count, sizeof(ScoreTime));
    *ret_events = (Event *) calloc(count, sizeof(Event));
    *ret_ranks = (int *) calloc(count, sizeof(int));
    for (size_t i = 0; i < count; i++) {
        // Placement new since malloced space is uninitialized.
        new((*ret_tps) + i) ScoreTime(t1_events[start+i].pos);
        new((*ret_events) + i) Event(t1_events[start+i].event);
        char **textp = &(*ret_events)[i].text;
        if (*textp)
            *textp = strdup(*textp);
        (*ret_ranks)[i] = t1_events[start+i].rank;
    }
    return count;
}

int
t1_no_events(ScoreTime *start_pos, ScoreTime *end_pos,
        ScoreTime **ret_tps, Event **ret_events, int **ret_ranks)
{
    return 0;
}

int
t1_find_samples(ScoreTime *start_pos, ScoreTime *end_pos,
        ScoreTime **ret_tps, double **ret_samples)
{
    size_t count = 0;
    size_t start = 0;
    SampleData &a = t1_samples;
    for (; start < a.size(); start++) {
        if (start + 1 == a.size())
            break;
        else if (a[start+1].first >= *start_pos)
            break;
    }
    while (start + count < a.size()) {
        if (a[start+count].first >= *end_pos)
            break;
        count++;
    }
    if (start + count < a.size())
        count++; // should get until one sample after the cutoff

    *ret_tps = (ScoreTime *) calloc(count, sizeof(ScoreTime));
    *ret_samples = (double *) calloc(count, sizeof(double));
    for (size_t i = 0; i < count; i++) {
        // Placement new since malloced space is uninitialized.
        new((*ret_tps) + i) ScoreTime(a[start+i].first);
        (*ret_samples)[i] = a[start+i].second;
    }
    return count;
}

// Of course I don't actually need to finalize any FunPtrs here...
void t1_finalizer(void *p) {}

static const Color ruler_bg = Color(255, 230, 160);
static const Color track_bg = Color(255, 255, 255);
static const Color render_color = Color(200, 100, 100, 128);

void
timeout_func(void *vp)
{
    BlockViewWindow &view = *((BlockViewWindow *) vp);
    static int n;

    // copy paste from main()
    static int i = t1_events.size() - 1;
    static ScoreTime t1_time_end = t1_events[i].pos
        + t1_events[i].event.duration;
    static RenderConfig render_config(RenderConfig::render_filled,
        t1_find_samples, render_color);
    static EventTrackConfig track1(track_bg, t1_no_events, t1_time_end,
        render_config);
    static RulerConfig truler(ruler_bg, false, true, true, arrival_beats,
        m44_last_pos);

    std::cout << n << "------------\n";
    switch (n) {
    case 0:
        view.block.set_selection(0, Selection(selection_colors[0],
                    1, ScoreTime(64), 1, ScoreTime(64)));
        view.block.set_zoom(ZoomInfo(ScoreTime(16), 1.6));
        break;
    case 1:
        return;
        view.block.insert_track(2, Tracklike(&track1, &truler), 30);
        break;
    case 2:
        // print_children(&view);
        break;
    default:
        return;
    }
    n++;
    Fl::repeat_timeout(1, timeout_func, vp);
}

void
handle_argv(int argc, char **argv)
{
    if (argc > 1 && strcmp(argv[1], "log") == 0)
        global_msg_collector()->log_collected = true;
}

int
main(int argc, char **argv)
{
    handle_argv(argc, argv);
    BlockViewConfig view_config = block_view_config();
    BlockModelConfig config = block_model_config();

    Marklists mlists;
    mlists.push_back(Marklist(m44_find_marks));
    Marklists nomarks;

    t1_set();
    m44_set();

    RulerConfig ruler(ruler_bg, true, false, false, arrival_beats,
        m44_last_pos);
    ruler.marklists = mlists;
    RulerConfig truler(ruler_bg, false, true, true, arrival_beats,
        m44_last_pos);
    truler.marklists = mlists;
    DividerConfig divider(Color(0x00ff00));

    int i = t1_events.size() - 1;
    ScoreTime t1_time_end = t1_events[i].pos + t1_events[i].event.duration;

    RenderConfig render_config(RenderConfig::render_line,
        t1_find_samples, render_color);

    EventTrackConfig empty_track(track_bg, t1_no_events, t1_time_end,
            render_config);
    EventTrackConfig track1(track_bg, t1_find_events, t1_time_end,
        render_config);
    EventTrackConfig track2(track_bg, t1_find_events, t1_time_end,
        render_config);

    BlockViewWindow view(0, 100, 200, 500, "view1", config, view_config);
    view.testing = true;
    // view.border(0);
    // BlockViewWindow view2(300, 100, 200, 500, "view2", config, view_config);
    // view2.testing = true;
    // view2.show();

    view.block.set_status("no status yet");
    view.block.set_title("hi there");

    // view.block.insert_track(0, Tracklike(&ruler), 20);
    // view.block.insert_track(1, Tracklike(&divider), 10);
    view.block.insert_track(1, Tracklike(&empty_track, &truler), 100);
    view.block.insert_track(2, Tracklike(&track2, &truler), 40);
    view.block.insert_track(3, Tracklike(&empty_track, &truler), 60);
    view.block.insert_track(4, Tracklike(&empty_track, &truler), 40);
    // view.block.insert_track(5, Tracklike(&track2, &truler), 80);

    // int pairs[] = {0, 5, 2, 4, 3, 4};
    // SkeletonConfig skel = skeleton_config(pairs, 3);
    // view.block.set_skeleton(skel);

    DisplayTrack dtrack;
    dtrack.status = 'M';
    // dtrack.status_color = Color(255, 150, 150);
    dtrack.status_color = Color(150, 150, 150);
    dtrack.event_brightness = .75;
    // view.block.set_display_track(3, dtrack);
    // print_children(&view);

    // Fl::add_timeout(1, timeout_func, (void*) &view);

    // view_config.block_title_height = 40;
    // view_config.track_title_height = 40;
    // view.block.set_view_config(view_config);

    view.block.set_zoom(ZoomInfo(ScoreTime(0), 1.6));

    view.block.set_selection(0, Selection(selection_colors[0],
                1, ScoreTime(32), 1, ScoreTime(32)));
    /*
    view.block.set_selection(0, Selection(selection_colors[0],
                1, ScoreTime(60), 4, ScoreTime(46)));
    view.block.set_selection(0, Selection(selection_colors[0],
                1, ScoreTime(0), 4, ScoreTime(56)));
    view.block.set_selection(1, Selection(selection_colors[1],
                1, ScoreTime(64), 4, ScoreTime(0)));
    */

    view.show();
    Fl::run();
}
