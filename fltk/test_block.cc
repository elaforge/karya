#include <iostream>
#include <FL/Fl.H>
#include <FL/Fl_Double_Window.H>

#include "f_util.h"

#include "Block.h"
#include "EventTrack.h"
#include "Ruler.h"
#include "SkeletonDisplay.h"
#include "MsgCollector.h"
#include "SymbolTable.h"


static const bool arrival_beats = false;
// Turn this off just draw a single event.
static const bool draw_lots_of_stuff = true;


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

typedef std::vector<std::pair<ScoreTime, Mark> > MarkData;
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

void t1_set()
{
    TrackData &e = t1_events;
    StyleId style = 0;

    // e.push_back(EventInfo(ScoreTime(5),
    //     Event("`tamil-i`", ScoreTime(16), style), 0));
    // e.push_back(EventInfo(ScoreTime(0),
    //     Event("15`1^`", ScoreTime(16), style), 0));
    // e.push_back(EventInfo(ScoreTime(32),
    //     Event("15`1^`m", ScoreTime(-16), style), 0));

    e.push_back(EventInfo(ScoreTime(0),
        Event("`1^`", ScoreTime(16), style), 0));
    e.push_back(EventInfo(ScoreTime(16),
        Event("a`tamil-i``xie`", ScoreTime(16), style), 0));
    e.push_back(EventInfo(ScoreTime(32),
        Event("`nosym`", ScoreTime(4), style), 0));
    e.push_back(EventInfo(ScoreTime(36),
        Event("overlap", ScoreTime(4), style), 0));
    e.push_back(EventInfo(ScoreTime(44),
        Event("6--", ScoreTime(4), style), 0));
    e.push_back(EventInfo(ScoreTime(50),
        Event("mis`match", ScoreTime(4), style), 0));
    e.push_back(EventInfo(ScoreTime(128),
        Event("late!", ScoreTime(64), style), 0));
    // coincident with rank 0
    e.push_back(EventInfo(ScoreTime(128),
        Event("bg1", ScoreTime(8), style), 1));
    // overlaps with rank 0
    e.push_back(EventInfo(ScoreTime(160),
        Event("bg2", ScoreTime(8), style), 1));
    e.push_back(EventInfo(ScoreTime(164),
        Event("bg2.5", ScoreTime(8), style), 1));
    // coincedent with end of rank 0
    e.push_back(EventInfo(ScoreTime(128+64),
        Event("bg3", ScoreTime(0), style), 1));
    // doesn't overlap rank 0
    e.push_back(EventInfo(ScoreTime(230),
        Event("bg4", ScoreTime(0), style), 0));

    /*
    e.push_back(EventInfo(ScoreTime(0*8),
        Event("main", ScoreTime(8), style), 0));
    for (int i = 0; i < 100; i++) {
        char buf[32];
        sprintf(buf, "e%d", i);
        e.push_back(EventInfo(ScoreTime(i*8),
            Event(strdup(buf), ScoreTime(8), style), 1));
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
        const char **textp = &(*ret_events)[i].text;
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

// Of course I don't actually need to finalize any FunPtrs here...
void
dummy_finalizer(void *p)
{
    DEBUG("FINALIZE " << p);
}


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
    // static EventTrackConfig track1(track_bg, t1_no_events, t1_time_end);
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
        // view.block.insert_track(2, Tracklike(&track1, &truler), 30);
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

static void
handle_argv(int argc, char **argv)
{
    if (argc > 1 && strcmp(argv[1], "log") == 0)
        MsgCollector::get()->log_collected = true;
}

static TrackSignal *
control_track_signal()
{
    TrackSignal *ts = new TrackSignal();

    const int length = 145;
    TrackSignal::ControlSample *samples = (TrackSignal::ControlSample *)
        calloc(length, sizeof(TrackSignal::ControlSample));
    for (int i = 0; i < length; i++) {
        samples[i].time = ScoreTime(i).to_real();
        samples[i].val = fmod(i / 60.0, 1);
    }
    ts->signal = samples;
    ts->pitch_signal = NULL;
    ts->length = length;
    ts->val_names = NULL;
    ts->val_names_length = 0;

    ts->shift = ScoreTime(0);
    ts->stretch = ScoreTime(1);

    return ts;
}

static TrackSignal *
pitch_track_signal()
{
    TrackSignal *ts = new TrackSignal();

    /*
    const int length = 4;
    TrackSignal::PitchSample *samples = (TrackSignal::PitchSample *)
        calloc(length, sizeof(TrackSignal::PitchSample));
    samples[0] = TrackSignal::PitchSample(ScoreTime(0), 2, 4, 0.2);
    samples[1] = TrackSignal::PitchSample(ScoreTime(20), 2, 4, 0.75);
    samples[2] = TrackSignal::PitchSample(ScoreTime(40), 1, 3, 0.5);
    samples[3] = TrackSignal::PitchSample(ScoreTime(60), 1.75, 3.5, 0.5);
    */

    const int length = 80;
    int i = 0;
    TrackSignal::PitchSample *samples = (TrackSignal::PitchSample *)
        calloc(length, sizeof(TrackSignal::PitchSample));
    for (; i < 20; i++) {
        samples[i] = TrackSignal::PitchSample(
            ScoreTime(i).to_real(), 2, 4, i / 20.0);
    }
    for (; i < 40; i++) {
        samples[i] = TrackSignal::PitchSample(
            ScoreTime(i).to_real(), 1, 3, (i-20) / 20.0);
    }
    for (; i < 80; i++) {
        samples[i] = TrackSignal::PitchSample(
            ScoreTime(i).to_real(), 1.5, 3.5, (i-40) / 40.0);
    }

    ts->signal = NULL;
    ts->pitch_signal = samples;
    ts->length = length;

    ts->val_names = new ValName[5];
    ts->val_names[0] = ValName(0, "a");
    ts->val_names[1] = ValName(1, "`1.`");
    ts->val_names[2] = ValName(2, "c");
    ts->val_names[3] = ValName(3, "d");
    ts->val_names[4] = ValName(4, "e");
    ts->val_names_length = 5;

    ts->shift = ScoreTime(0);
    ts->stretch = ScoreTime(1);

    return ts;
}

static void
show_fonts()
{
    SymbolTable *t = SymbolTable::get();
    char **fonts = t->fonts();
    for (int i = 0; fonts[i]; i++) {
        printf("%s\n", fonts[i]);
        free(fonts[i]);
    }
    free(fonts);
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

    EventTrackConfig empty_track(track_bg, t1_no_events, t1_time_end,
            RenderConfig(RenderConfig::render_line, render_color));
    EventTrackConfig track1(track_bg, t1_find_events, t1_time_end,
            RenderConfig(RenderConfig::render_line, render_color));
    EventTrackConfig track2(track_bg, t1_find_events, t1_time_end,
            RenderConfig(RenderConfig::render_filled, render_color));

    BlockViewWindow view(1100, 40, 200, 500, "view1", config, view_config);
    view.testing = true;
    // view.border(0);
    // BlockViewWindow view2(300, 100, 200, 500, "view2", config, view_config);
    // view2.testing = true;
    // view2.show();

    if (draw_lots_of_stuff) {
        // view.block.insert_track(0, Tracklike(&ruler), 20);
        // view.block.insert_track(1, Tracklike(&divider), 10);
        view.block.insert_track(1, Tracklike(&empty_track, &truler), 60);
        view.block.insert_track(2, Tracklike(&track1, &truler), 130);
        view.block.insert_track(3, Tracklike(&track2, &truler), 40);
        view.block.insert_track(4, Tracklike(&empty_track, &truler), 40);
        view.block.insert_track(5, Tracklike(&track2, &truler), 80);

        view.block.set_status("ABC`tamil-i` ABC `xie`");
        view.block.set_title("hi there");

        TrackSignal *pitch_tsig = pitch_track_signal();
        view.block.set_track_signal(1, *pitch_tsig);
        TrackSignal *control_tsig = control_track_signal();
        view.block.set_track_signal(2, *control_tsig);
        view.block.set_track_signal(3, *control_tsig);

        int pairs[] = {0, 3, 3, 2, 2, 1};
        SkeletonConfig skel = skeleton_config(pairs, 3);
        view.block.set_skeleton(skel);
    } else {
        view.block.insert_track(1, Tracklike(&track1, &truler), 130);
    }

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
                1, ScoreTime(80), 1, ScoreTime(80)));
    /*
    view.block.set_selection(0, Selection(selection_colors[0],
                1, ScoreTime(60), 4, ScoreTime(46)));
    view.block.set_selection(0, Selection(selection_colors[0],
                1, ScoreTime(0), 4, ScoreTime(56)));
    view.block.set_selection(1, Selection(selection_colors[1],
                1, ScoreTime(64), 4, ScoreTime(0)));
    */

    StyleTable::get()->put(0, EventStyle(FL_HELVETICA, 12, Color::black,
        Color(200, 200, 170)));

    // Technically the Glyphs should use heap space, not constants, since they
    // will be freed if there is a duplicate symbol.  But I don't care for a
    // test.
    SymbolTable *t = SymbolTable::get();
    t->insert("tamil-i", SymbolTable::Symbol(
        SymbolTable::Glyph("\xe0\xae\x87", NULL, 4)));
    // t->load("yen", "\xc2\xa5");
    // t->load("coda", "\xef\x80\xa5");

    // xie2 radical, slant of dai4, CJK STROKE XG
    // radicals are at +31c0
    t->insert("xie", SymbolTable::Symbol(
        SymbolTable::Glyph("\xe3\x87\x82", t->font("LiSong Pro"), 4)));

    t->insert("1.", SymbolTable::Symbol(
        SymbolTable::Glyph("1"),
        SymbolTable::Glyph("\xe2\x80\xa2", Config::font, 0, DPoint(.5, .2))));
    t->insert("1..", SymbolTable::Symbol(
        SymbolTable::Glyph("1"),
        SymbolTable::Glyph("\xe2\x80\xa2", Config::font, 0, DPoint(-.3, .2)),
        SymbolTable::Glyph("\xe2\x80\xa2", Config::font, 0, DPoint(.5, .2))));

    Fl_Font em = t->font("Emmentaler 11");
    t->insert("sharp", SymbolTable::Symbol(
        SymbolTable::Glyph("a"),
        SymbolTable::Glyph("\xee\x84\x8e", em, 2, DPoint(.5, -.4))));
    t->insert("flat", SymbolTable::Symbol(
        SymbolTable::Glyph("a"),
        SymbolTable::Glyph("\xee\x84\x8e", em, 2, DPoint(.5, -.4))));
    t->insert("mordent", SymbolTable::Symbol(
        SymbolTable::Glyph("\xee\x86\x8c", em, 4, DPoint(0, 0))));
    t->insert("arp-up", SymbolTable::Symbol(
        SymbolTable::Glyph("\xee\x86\x8a", em, 8, DPoint(-.14, -.62), 0),
        SymbolTable::Glyph("\xee\x86\x8e", em, 4, DPoint(0, 0), 90)));

    t->insert("arp-down", SymbolTable::Symbol(
        SymbolTable::Glyph("\xee\x86\x89", em, 8, DPoint(-.14, .25), 0),
        SymbolTable::Glyph("\xee\x86\x8e", em, 4, DPoint(0, 0), 90)));

    // dots: DOT OPERATOR e2 8b 85, bullet e2 80 a2
    // t->load("v-angle-double", "\xef\xb8\xbd", "LiSong Pro", 4);

    // t->load("ding", "M", NULL, 10, IPoint(0, 0), IPoint(0, 0));
    // t->load("ding", "i", "Bali-Simbar-B", 28, IPoint(12, 18), IPoint(12, 10));
    // t->load("ding", "i", "Bali-Simbar-B", 10, IPoint(0, 0), IPoint(0, 0));
    // t->load("dong", "o", "Bali-Simbar-B", 26, IPoint(0, 8), IPoint(0, 10));
    // t->load("deng", "e", "Bali-Simbar-B", 16, IPoint(0, -6), IPoint(0, 0));
    // t->load("dung", "u", "Bali-Simbar-B", 16, IPoint(7, -14), IPoint(7, 0));
    // t->load("dang", "*", "Bali-Simbar-B", 16, IPoint(12, 2), IPoint(8, 0));
    // t->load("pepet", ")", "Bali-Simbar-B", 16);

    view.show();

    std::cout << view.block.dump() << '\n';

    Fl::run();
}
