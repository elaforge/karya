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

BlockModelConfig block_model_config()
{
    BlockModelConfig c;
    c.skel_box = BlockBox(Color(0x99, 0x99, 0xff), 'a');
    c.track_box = BlockBox(Color(0x44, 0xff, 0xff), 'K');
    c.sb_box = BlockBox(Color(0x00, 0xff, 0xff), 'S');
    return c;
}

Marklist *m44_set(ScoreTime *last_pos)
{
    char name[32];
    Color major = Color(116, 70, 0, 90);
    Color minor = Color(225, 100, 50, 90);

    int length = 200;
    PosMark *marks = (PosMark *) calloc(sizeof(PosMark), length);
    for (int i = 0; i < length; i++) {
        ScoreTime t = ScoreTime(i*8);
        if (i % 4 == 0) {
            sprintf(name, "%d", i / 4);
            Mark m(1, 3, major, strdup(name), 0, 0);
            marks[i] = PosMark(t, m);
        } else {
            sprintf(name, "long %d.%d", i / 4, i % 4);
            Mark m(2, 2, minor, strdup(name), 0, 0);
            marks[i] = PosMark(t, m);
        }
    }
    *last_pos = ScoreTime((length-1) * 8);
    return new Marklist(marks, length);
}

struct EventInfo {
    EventInfo(int rank, Event event) : rank(rank), event(event)
    {}
    int rank;
    Event event;
    bool operator<(const EventInfo &o) const {
        if (event.start == o.event.start)
            return rank < o.rank;
        else
            return event.start < o.event.start;
    }
};


typedef std::vector<EventInfo> TrackData;
static TrackData t1_events;

void t1_set()
{
    TrackData &e = t1_events;
    StyleId style = 0;
    StyleId style2 = 1;

    if (arrival_beats) {
        e.push_back(EventInfo(0,
            Event(ScoreTime(8), ScoreTime(-8), "a", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(16), ScoreTime(-8), "b", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(24), ScoreTime(-8), "c", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(32), ScoreTime(-8), "d", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(40), ScoreTime(-8), "e", style)));
    } else {
        e.push_back(EventInfo(0,
            Event(ScoreTime(0), ScoreTime(16), "`arp-down`", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(16), ScoreTime(16), "a`tamil-i``xie`", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(32), ScoreTime(4), "`nosym`", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(36), ScoreTime(4), "overlap", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(44), ScoreTime(4), "6--", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(50), ScoreTime(4), "mis`match", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(128), ScoreTime(64), "`0x`ff", style2)));
        // coincident with rank 0
        e.push_back(EventInfo(1,
            Event(ScoreTime(128), ScoreTime(8), "bg1", style)));
        // overlaps with rank 0
        e.push_back(EventInfo(1,
            Event(ScoreTime(160), ScoreTime(8), "bg2", style)));
        e.push_back(EventInfo(1,
            Event(ScoreTime(164), ScoreTime(8), "bg2.5", style)));
        // coincedent with end of rank 0
        e.push_back(EventInfo(1,
            Event(ScoreTime(128+64), ScoreTime(0), "bg3", style)));
        // doesn't overlap rank 0
        e.push_back(EventInfo(0,
            Event(ScoreTime(230), ScoreTime(0), "bg4", style)));
    }
}

int
t1_find_events(ScoreTime *start_pos, ScoreTime *end_pos,
        Event **ret_events, int **ret_ranks)
{
    size_t count = 0;
    size_t start = 0;
    for (; start < t1_events.size(); start++) {
        if (t1_events[start].event.start + t1_events[start].event.duration
                >= *start_pos)
            break;
    }
    while (start + count < t1_events.size()) {
        if (t1_events[start+count].event.start >= *end_pos)
            break;
        count++;
    }
    start = 0;
    count = t1_events.size();

    *ret_events = (Event *) calloc(count, sizeof(Event));
    *ret_ranks = (int *) calloc(count, sizeof(int));
    for (size_t i = 0; i < count; i++) {
        // Placement new since malloced space is uninitialized.
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
        Event **ret_events, int **ret_ranks)
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
static const Color render_color = Color(166, 166, 205, 127);

static void
timeout_func(void *vp)
{
    BlockViewWindow &view = *((BlockViewWindow *) vp);
    static int n;

    static int i = t1_events.size() - 1;
    static ScoreTime t1_time_end =
        t1_events[i].event.start + t1_events[i].event.duration;
    static ScoreTime m44_last_pos;
    m44_set(&m44_last_pos);

    static EventTrackConfig empty_track(track_bg, t1_no_events, t1_time_end,
            RenderConfig(RenderConfig::render_line, render_color));
    static RulerConfig ruler(
            ruler_bg, false, true, true, arrival_beats, m44_last_pos);

    std::cout << n << "------------\n";
    switch (n) {
    case 0:
        view.block.insert_track(2, Tracklike(&empty_track, &ruler), 30);
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
creep_selection(void *vp)
{
    BlockViewWindow &view = *((BlockViewWindow *) vp);
    static int i;
    if (++i >= 100)
        return;

    ScoreTime pos(double(i) / 2);
    view.block.set_selection(0,
        Selection(selection_colors[0], 1, pos, 4, pos));
    Fl::repeat_timeout(.1, creep_selection, vp);
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
    ControlSample *samples = (ControlSample *)
        calloc(length, sizeof(ControlSample));
    for (int i = 0; i < length; i++) {
        samples[i].time = ScoreTime(i).to_real();
        samples[i].val = fmod(i / 20.0, 5);
    }
    ts->signal = samples;
    ts->length = length;
    ts->shift = ScoreTime(0);
    ts->stretch = ScoreTime(1);
    ts->calculate_val_bounds();
    return ts;
}

static TrackSignal *
pitch_track_signal()
{
    TrackSignal *ts = new TrackSignal();

    /*
    const int length = 4;
    PitchSample *samples = (PitchSample *)
        calloc(length, sizeof(PitchSample));
    samples[0] = PitchSample(ScoreTime(0), 2, 4, 0.2);
    samples[1] = PitchSample(ScoreTime(20), 2, 4, 0.75);
    samples[2] = PitchSample(ScoreTime(40), 1, 3, 0.5);
    samples[3] = PitchSample(ScoreTime(60), 1.75, 3.5, 0.5);

    const int length = 80;
    int i = 0;
    PitchSample *samples = (PitchSample *) calloc(length, sizeof(PitchSample));
    for (; i < 20; i++) {
        samples[i] = PitchSample(ScoreTime(i).to_real(), 2, 4, i / 20.0);
    }
    for (; i < 40; i++) {
        samples[i] = PitchSample(ScoreTime(i).to_real(), 1, 3, (i-20) / 20.0);
    }
    for (; i < 80; i++) {
        samples[i] = PitchSample(
            ScoreTime(i).to_real(), 1.5, 3.5, (i-40) / 40.0);
    }
    */

    ts->signal = NULL;
    ts->length = 0;
    ts->shift = ScoreTime(0);
    ts->stretch = ScoreTime(1);
    return ts;
}

void
show_fonts()
{
    SymbolTable *t = SymbolTable::get();
    char **fonts = t->fonts();
    for (int i = 0; fonts[i]; i++) {
        printf("'%s'\n", fonts[i]);
        free(fonts[i]);
    }
    free(fonts);
}

int
main(int argc, char **argv)
{
    handle_argv(argc, argv);
    BlockModelConfig config = block_model_config();

    BlockViewWindow::initialize(NULL);
    t1_set();
    ScoreTime m44_last_pos;
    Marklist *m44_marks = m44_set(&m44_last_pos);

    Marklists mlists;
    mlists.push_back(m44_marks);
    Marklists nomarks;

    RulerConfig ruler(ruler_bg, false, true, true, arrival_beats, m44_last_pos);
    ruler.marklists = mlists;
    DividerConfig divider(Color(0x00, 0xff, 0x00));

    int i = t1_events.size() - 1;
    ScoreTime t1_time_end =
        t1_events[i].event.start + t1_events[i].event.duration;

    EventTrackConfig empty_track(track_bg, t1_no_events, t1_time_end,
            RenderConfig(RenderConfig::render_line, render_color));
    EventTrackConfig track1(track_bg, t1_find_events, t1_time_end,
            RenderConfig(RenderConfig::render_line, render_color));
    EventTrackConfig track2(track_bg, t1_find_events, t1_time_end,
            RenderConfig(RenderConfig::render_filled, render_color));

    BlockViewWindow view(1100, 40, 300, 500, "view1", config);
    view.testing = true;
    // view.border(0);
    // BlockViewWindow view2(300, 100, 200, 500, "view2", config);
    // view2.testing = true;
    // view2.show();

    if (draw_lots_of_stuff) {
        view.block.insert_track(0, Tracklike(&ruler), 20);
        view.block.insert_track(1, Tracklike(&empty_track, &ruler), 60);
        view.block.insert_track(2, Tracklike(&track1, &ruler), 130);

        view.block.insert_track(3, Tracklike(&track2, &ruler), 40);
        view.block.insert_track(4, Tracklike(&empty_track, &ruler), 40);
        view.block.insert_track(5, Tracklike(&track2, &ruler), 80);

        view.block.set_status("ABC`tamil-i` ABC `xie`", Color::white);
        view.block.set_title("hi there");

        TrackSignal *pitch_tsig = pitch_track_signal();
        view.block.set_track_signal(1, *pitch_tsig);
        TrackSignal *control_tsig = control_track_signal();
        view.block.set_track_signal(2, *control_tsig);
        view.block.set_track_signal(3, *control_tsig);

        SkeletonEdge edges[] = {
            SkeletonEdge(0, 1, 0, Color::black),
            SkeletonEdge(1, 2, 0, Color::black),
            SkeletonEdge(1, 3, 0, Color::black),
            SkeletonEdge(0, 4, 2, Color(0xff, 0, 0))
        };
        SkeletonConfig skel(4, edges);
        view.block.set_skeleton(skel);
    } else {
        view.block.insert_track(1, Tracklike(&track1, &ruler), 60);
        view.block.set_track_signal(1, *control_track_signal());
    }

    DisplayTrack dtrack;
    dtrack.status = 'M';
    // dtrack.status_color = Color(255, 150, 150);
    dtrack.status_color = Color(150, 150, 150);
    dtrack.event_brightness = .75;
    // view.block.set_display_track(3, dtrack);
    // print_children(&view);

    // Fl::add_timeout(1, creep_selection, (void*) &view);

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
        Color::rgb_normalized(0.9, 0.9, 0.7)));
    StyleTable::get()->put(1, EventStyle(FL_HELVETICA, 12, Color::black,
        Color::rgb_normalized(0.8, 1, 0.9)));

    // Technically the Glyphs should use heap space, not constants, since they
    // will be freed if there is a duplicate symbol.  But I don't care for a
    // test.
    SymbolTable *t = SymbolTable::get();

#ifdef __linux__
    Fl_Font music = t->font(" Emmentaler");
    Fl_Font chinese = t->font(" AR PL UKai TW");
    Fl_Font tamil = t->font(" Lohit Tamil");
#endif
#ifdef __APPLE__
    Fl_Font music = t->font("Emmentaler 11");
    Fl_Font chinese = t->font("LiSong Pro");
    Fl_Font tamil = Config::font; // thanks to OS X font substitution I guess
#endif

    SymbolTable::Symbol zerox = SymbolTable::Symbol(
        SymbolTable::Glyph("x", Config::font, -2, DPoint(0, -.4)));
    zerox.absolute_y = true;
    t->insert("0x", zerox);
    t->insert("tamil-i", SymbolTable::Symbol(
        SymbolTable::Glyph("\xe0\xae\x87", tamil, 4)));
    // t->load("yen", "\xc2\xa5");
    // t->load("coda", "\xef\x80\xa5");

    // xie2 radical, slant of dai4, CJK STROKE XG
    // radicals are at +31c0
    t->insert("xie", SymbolTable::Symbol(
        SymbolTable::Glyph("\xe3\x87\x82", chinese, 4)));

    t->insert("1.", SymbolTable::Symbol(
        SymbolTable::Glyph("1"),
        SymbolTable::Glyph("\xe2\x80\xa2", Config::font, 0, DPoint(.5, .2))));
    t->insert("1..", SymbolTable::Symbol(
        SymbolTable::Glyph("1"),
        SymbolTable::Glyph("\xe2\x80\xa2", Config::font, 0, DPoint(-.3, .2)),
        SymbolTable::Glyph("\xe2\x80\xa2", Config::font, 0, DPoint(.5, .2))));

    t->insert("sharp", SymbolTable::Symbol(
        SymbolTable::Glyph("a"),
        SymbolTable::Glyph("\xee\x84\x8e", music, 2, DPoint(.5, -.4))));
    t->insert("flat", SymbolTable::Symbol(
        SymbolTable::Glyph("a"),
        SymbolTable::Glyph("\xee\x84\x8e", music, 2, DPoint(.5, -.4))));
    t->insert("mordent", SymbolTable::Symbol(
        SymbolTable::Glyph("\xee\x86\x8c", music, 4, DPoint(0, 0))));
    t->insert("arp-up", SymbolTable::Symbol(
        SymbolTable::Glyph("\xee\x86\x8a", music, 8, DPoint(-.14, -.25), 0),
        SymbolTable::Glyph("\xee\x86\x8e", music, 4, DPoint(0, 0), 90)));
    t->insert("arp-down", SymbolTable::Symbol(
        SymbolTable::Glyph("\xee\x86\x89", music, 8, DPoint(-.14, .5), 0),
        SymbolTable::Glyph("\xee\x86\x8e", music, 4, DPoint(0, 0), 90)));

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
