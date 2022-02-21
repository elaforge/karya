// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <iostream>
#include <FL/Fl.H>
#include <FL/Fl_Double_Window.H>
#include <FL/Fl_Sys_Menu_Bar.H>

#include "Block.h"
#include "EventTrack.h"
#include "Keycaps.h"
#include "MsgCollector.h"
#include "RulerTrack.h"
#include "SkeletonDisplay.h"
#include "SymbolTable.h"
#include "f_util.h"


enum Events {
    Normal,
    Negative,
    Symbols,
    Many,
    Waveform
};
static const Events t1_use_events = Normal;
// Turn this off just draw a single track.
static const bool many_tracks = true;
static const bool show_keycaps = false;

// Visible windows.
static std::vector<BlockWindow *> windows;

const char *audio_chunk0 = "data/000.wav";
const char *audio_chunk1 = "data/001.wav";
const char *audio_chunk1_rev = "data/001-rev.wav";

Color selection_colors[] = {
    Color(0, 0, 255, 90),
    Color(255, 0, 255, 90),
    Color(0, 255, 255, 90)
};

BlockConfig block_config()
{
    BlockConfig c;
    c.skel_box = BlockBox(Color(0x99, 0x99, 0xff), 'a');
    c.track_box = BlockBox(Color(0x44, 0xff, 0xff), 'K');
    c.sb_box = BlockBox(Color(0x00, 0xff, 0xff), ' ');
    return c;
}

Marklist *m44_set(ScoreTime *last_pos)
{
    char name[32];
    Color major = Color(116, 70, 0, 90);
    Color minor = Color(225, 100, 50, 90);
    ScoreTime each_mark(8);

    int length = 64;
    PosMark *marks = (PosMark *) calloc(sizeof(PosMark), length);
    for (int i = 0; i < length; i++) {
        ScoreTime t = ScoreTime(i).multiply(each_mark);
        if (i % 4 == 0) {
            sprintf(name, "`+2/%d`", i / 4);
            Mark m(1, 3, major, strdup(name), 0, 0);
            marks[i] = PosMark(t, m);
        } else {
            sprintf(name, "%d.%d", i / 4, i % 4);
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

    switch (t1_use_events) {
    case Negative:
        e.push_back(EventInfo(0,
            Event(ScoreTime(8), ScoreTime(-8), "a", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(16), ScoreTime(-8), "b", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(24), ScoreTime(-8), "c", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(32), ScoreTime(-8), "d", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(40), ScoreTime(-8), "e ", style)));
        // Demonstrate upwards wrapping.
        e.push_back(EventInfo(0,
            Event(ScoreTime(80), ScoreTime(-8), "abc d ef", style)));
        break;
    case Symbols:
        e.push_back(EventInfo(0,
            Event(ScoreTime(0), ScoreTime(16), "`arp-down`", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(16), ScoreTime(0), "mis`match", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(32), ScoreTime(0), "`nosym`", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(48), ScoreTime(0), "`+8/big`norm", style)));

        e.push_back(EventInfo(0,
            Event(ScoreTime(64), ScoreTime(0), "`bold+italic/bold`", style)));
        break;
    case Normal:
        e.push_back(EventInfo(0,
            Event(ScoreTime(0), ScoreTime(16), "trail ", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(16), ScoreTime(16), "a`tamil-i``xie`", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(32), ScoreTime(4), "`nosym`", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(36), ScoreTime(4), "overlap", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(44), ScoreTime(4), "6--", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(72), ScoreTime(8), "漢字", style)));
        e.push_back(EventInfo(0,
            Event(ScoreTime(82), ScoreTime(8), "兩個 分開", style)));
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
        e.push_back(EventInfo(0,
            Event(ScoreTime(64*8), ScoreTime(0), "end", style)));
        break;
    case Many:
        for (int i = 0; i < 128; i++) {
            e.push_back(EventInfo(0,
                Event(ScoreTime(i*8), ScoreTime(8), "c4", style)));
        }
        break;
    case Waveform:
        break;
    }
}

int
t1_find_events(const ScoreTime *start_pos, const ScoreTime *end_pos,
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
t1_no_events(const ScoreTime *start_pos, const ScoreTime *end_pos,
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
creep_selection(void *vp)
{
    BlockWindow &view = *((BlockWindow *) vp);
    static int i;
    if (++i >= 100)
        return;

    ScoreTime pos(double(i) / 2);
    std::vector<Selection> sels;
    sels.push_back(
        Selection(selection_colors[0], pos, pos, Selection::Positive));
    view.block.set_selection(0, 1, sels);
    Fl::repeat_timeout(.1, creep_selection, vp);
}


static void
fail(const char *msg)
{
    fprintf(stderr, "%s\n", msg);
    exit(1);
}

static void
handle_argv(int argc, char **argv)
{
    const char *usage = "usage: test_block [ log ]";
    if (argc == 1) {
    } else if (argc == 2) {
        if (strcmp(argv[1], "log") == 0)
            MsgCollector::get()->log_collected = true;
        else
            fail(usage);
    } else {
        fail(usage);
    }
}

static TrackSignal *
control_track_signal()
{
    TrackSignal *ts = new TrackSignal();

    // linear ramps
    const int length = 145;
    ControlSample *samples = (ControlSample *)
        calloc(length, sizeof(ControlSample));
    for (int i = 0; i < length; i++) {
        samples[i].time = ScoreTime(i).to_real();
        samples[i].val = fmod(i / 20.0, 5) - 0.15;
    }

    // // flat steps
    // const int length = 1;
    // ControlSample *samples = (ControlSample *)
    //     calloc(length, sizeof(ControlSample));
    // samples[0] = ControlSample(4, 0.5);
    // samples[1] = ControlSample(10, 0);
    // samples[2] = ControlSample(20, 1);
    // samples[3] = ControlSample(25, 0.5);

    // for (int i = 0; i < length; i++) {
    //     DEBUG("sample " << i << ": " << samples[i].time << ", "
    //         << samples[i].val);
    // }

    ts->signal = samples;
    ts->length = length;
    ts->shift = ScoreTime(0);
    ts->stretch = ScoreTime(1);
    ts->calculate_val_bounds("test");
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

void
menu_cb(Fl_Widget *w, void *arg)
{
}

static void
add_symbols()
{
    // Technically the Glyphs should use heap space, not constants, since they
    // will be freed if there is a duplicate symbol.  But I don't care for a
    // test.
    SymbolTable *t = SymbolTable::get();

#ifdef __linux__
    Fl_Font chinese = t->font("Noto Serif CJK TC");
    Fl_Font tamil = t->font("Noto Serif Tamil");
    Fl_Font bali = t->font("Noto Sans Balinese");
#endif
#ifdef __APPLE__
    // NotoMono, Noto{Sans,Serif}-{Bold,BoldItalic,Italic}
    // OS X has font character substitution, so it can figure out which font
    // has the glyphs.
    // It seems new noto versions changed the names.
    bool new_names =
        t->font("NotoSerif-Regular") != SymbolTable::font_not_found;
    Fl_Font chinese = t->font(new_names ? "NotoSerif-Regular" : "NotoSerif");
    Fl_Font tamil = t->font(new_names ? "NotoSerif-Regular" : "NotoSerif");
    Fl_Font bali = t->font(new_names ? "NotoSans-Regular" : "NotoSans");
#endif
    Fl_Font bravura = t->font("Bravura");

    SymbolTable::Symbol zerox = SymbolTable::Symbol(
        SymbolTable::Glyph("x", Config::font, -2, DPoint(0, -.4)));
    zerox.absolute_y = true;
    t->insert("0x", zerox);
    t->insert("tamil-i", SymbolTable::Symbol(
        SymbolTable::Glyph("\xe0\xae\x87", tamil, 4)));
    //෴  looks useful
    t->insert("sinhala-stop", SymbolTable::Symbol(
        SymbolTable::Glyph("\xe0\xb7\xb4", Config::font, 4)));
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

    t->insert("arp-up", SymbolTable::Symbol(
        // arrow
        SymbolTable::Glyph("\xee\xaa\xad", bravura, 8, DPoint(0, -0.25), 90),
        // wiggle
        SymbolTable::Glyph("\xee\xaa\xa9", bravura, 8, DPoint(0, 0), 90)));
    t->insert("arp-down", SymbolTable::Symbol(
        // arrow
        SymbolTable::Glyph("\xee\xaa\xae", bravura, 8, DPoint(0, 0.25), -90),
        // wiggle
        SymbolTable::Glyph("\xee\xaa\xaa", bravura, 8, DPoint(0, 0), -90)));


    t->insert("ding", SymbolTable::Symbol(
        SymbolTable::Glyph("\xe1\xad\xa6", bali, 8)));
    t->insert("dong", SymbolTable::Symbol(
        SymbolTable::Glyph("\xe1\xad\xa1", bali, 8)));
    t->insert("deng", SymbolTable::Symbol(
        SymbolTable::Glyph("\xe1\xad\xa2", bali, 8)));
    t->insert("dung", SymbolTable::Symbol(
        SymbolTable::Glyph("\xe1\xad\xa3", bali, 8)));
    t->insert("dang", SymbolTable::Symbol(
        SymbolTable::Glyph("\xe1\xad\xa4", bali, 8)));

    // dots: DOT OPERATOR e2 8b 85, bullet e2 80 a2
    // t->load("v-angle-double", "\xef\xb8\xbd", "LiSong Pro", 4);
}

static void
set_suggested_track_widths(Block *block)
{
    for (int i = 1; i < block->tracks(); i++) {
        int w = block->track_at(i)->get_suggested_width();
        DEBUG("track " << i << ": " << w);
        block->set_track_width(i, w);
    }
}

static void
timeout_func(void *unused)
{
    static int n;
    Block &block = windows[0]->block;

    std::cout << n << "------------\n";
    if (n == 0) {
        // std::vector<double> ratios;
        // block.clear_waveforms();
        // block.set_waveform(
        //     1, 0, PeakCache::Params(audio_chunk0, ScoreTime(0), ratios));
        // PeakCache::get()->gc();

        // block.floating_open(1, ScoreTime(16), "floaty boaty", 20, 20);
        // set_suggested_track_widths(&block);
        return;
    } else if (n == 1) {
    } else if (n == 2) {
    } else {
        return;
    }
    n++;
    Fl::repeat_timeout(1, timeout_func, nullptr);
}

static Keycaps::Layout *
keycaps_layout()
{
    Keycaps::Layout *layout = new Keycaps::Layout();
    auto &a = *layout;
    a.bg_color = Color::white;
    a.keycap_color = Color::white.brightness(0.75);
    a.highlight_color = Color(0xa0, 0xa0, 0xff);
    a.label_color = Color::black;
    a.binding_color = Color::black;

    a.rects_len = 2;
    a.rects = (IRect *) calloc(a.rects_len, sizeof(IRect));
    a.rects[0] = IRect(10, 10, 25, 25);
    a.rects[1] = IRect(40, 10, 25, 25);

    a.labels_len = 2;
    a.labels_points = (IPoint *) calloc(a.labels_len, sizeof(IPoint));
    a.labels_texts = (const char **) calloc(a.labels_len, sizeof(char *));
    a.labels_points[0] = IPoint(15, 20);
    a.labels_texts[0] = strdup("q");
    a.labels_points[1] = IPoint(45, 20);
    a.labels_texts[1] = strdup("w");

    return layout;
}

static std::vector<Keycaps::Binding *>
keycaps_bindings()
{
    std::vector<Keycaps::Binding *> bs;
    bs.push_back(new Keycaps::Binding(
        IPoint(20, 25),
        strdup("4c"),
        strdup("pitch 4c"),
        Color::black
    ));
    bs.push_back(new Keycaps::Binding(
        IPoint(50, 25),
        strdup("4d"),
        strdup("pitch 4d"),
        Color(0xff, 0xa0, 0xa0)
    ));
    return bs;
}

int
main(int argc, char **argv)
{
    handle_argv(argc, argv);
    Fl_Sys_Menu_Bar menu(0, 0, 0, 0, nullptr);
    menu.add("Window/blorp", 0, menu_cb, nullptr, FL_MENU_TOGGLE);
    menu.show();
    // check for focused, diamond for minimized

    BlockConfig config = block_config();

    BlockWindow::initialize(nullptr);
    t1_set();
    ScoreTime m44_last_pos;
    Marklist *m44_marks = m44_set(&m44_last_pos);

    Marklists mlists;
    mlists.push_back(m44_marks);
    Marklists nomarks;

    RulerConfig ruler(
        ruler_bg, false, true, true, t1_use_events == Negative, m44_last_pos);
    ruler.marklists = mlists;
    RulerConfig no_ruler(
        ruler_bg, false, true, true, t1_use_events == Negative, ScoreTime(0));
    DividerConfig divider(Color(0x00, 0xff, 0x00));

    int i = t1_events.size() - 1;
    ScoreTime t1_time_end = t1_events.size() == 0
        ? ScoreTime(0) : t1_events[i].event.start + t1_events[i].event.duration;

    EventTrackConfig empty_track(track_bg, t1_no_events, t1_time_end,
        RenderConfig(RenderConfig::render_line, render_color));
    EventTrackConfig track1(track_bg, t1_find_events, t1_time_end,
        RenderConfig(RenderConfig::render_line, render_color));
    EventTrackConfig track2(track_bg, t1_find_events, t1_time_end,
        RenderConfig(RenderConfig::render_filled, render_color));

    config.skeleton_editable = true;
    BlockWindow *w = new BlockWindow(1100, 40, 300, 500, "view1", config);
    w->show();
    windows.push_back(w);
    BlockWindow &view = *w;
    view.testing = true;
    // view.border(0);

    view.block.insert_track(0, Tracklike(&ruler), 20);
    if (t1_use_events == Waveform) {
        view.block.insert_track(1, Tracklike(&track1, &no_ruler), 40);
        std::vector<double> ratios;
        ratios.push_back(1/64.0);
        view.block.set_waveform(
            1, 0, PeakCache::Params(audio_chunk0, ScoreTime(0), ratios));
        view.block.set_waveform(
            1, 1, PeakCache::Params(audio_chunk1, ScoreTime(4*64), ratios));

        view.block.insert_track(2, Tracklike(&track1, &no_ruler), 40);
        std::vector<double> ratios2;
        ratios2.push_back(1);
        view.block.set_waveform(
            2, 0, PeakCache::Params(audio_chunk0, ScoreTime(4*64), ratios2));

        // view.block.set_waveform(
        //     1, 1, PeakCache::Params(audio_chunk1, ScoreTime(4), ratios));
        // view.block.set_waveform(
        //     1, 1, PeakCache::Params(audio_chunk1_rev, ScoreTime(4), ratios));

        PeakCache::get()->gc();
    } else if (t1_use_events == Many) {
        for (int i = 1; i < 24; i++) {
            view.block.insert_track(i, Tracklike(&track1, &ruler), 30);
        }
    } else if (many_tracks) {
        view.block.insert_track(1, Tracklike(&empty_track, &ruler), 60);
        view.block.insert_track(2, Tracklike(&track1, &ruler), 130);

        view.block.insert_track(3, Tracklike(&track2, &ruler), 40);
        view.block.insert_track(4, Tracklike(&empty_track, &ruler), 40);
        view.block.insert_track(5, Tracklike(&track2, &ruler), 80);

        view.block.set_status("ABC`tamil-i` ABC `xie`", Color::white);

        TrackSignal *control_tsig = control_track_signal();
        view.block.set_track_signal(3, *control_tsig);
        view.block.set_track_signal(4, *control_tsig);

        SkeletonEdge edges[] = {
            SkeletonEdge(0, 1, 0, Color::black),
            SkeletonEdge(1, 2, 0, Color::black),
            SkeletonEdge(1, 3, 0, Color::black),
            SkeletonEdge(0, 4, 2, Color(0xff, 0, 0))
        };
        SkeletonConfig skel(sizeof edges / sizeof(*edges), edges);
        view.block.set_skeleton(skel);

        DisplayTrack dtrack;
        dtrack.status = SkeletonStatus(Color(255, 150, 150), 'M', '\0');
        dtrack.width = 30;
        dtrack.event_brightness = .75;
        view.block.set_display_track(2, dtrack);
    } else {
        view.block.insert_track(1, Tracklike(&track1, &no_ruler), 40);
        view.block.track_at(1)->set_title("track title");
        view.block.set_track_signal(1, *control_track_signal());
    }
    view.block.set_title("clocky blocky");

    Fl::add_timeout(1, timeout_func, nullptr);

    view.block.set_zoom(Zoom(ScoreTime(0), 1.6));

    std::vector<Selection> sels;
    sels.push_back(
        Selection(selection_colors[0], ScoreTime(80), ScoreTime(80),
            Selection::Positive));
    sels.push_back(
        Selection(selection_colors[0], ScoreTime(90), ScoreTime(100),
            Selection::Positive));
    view.block.set_selection(0, 1, sels);

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

    // show_fonts();
    // return 0;
    add_symbols();

    // keycaps
    if (show_keycaps) {
        KeycapsWindow *k = new KeycapsWindow(
            200, 200, 200, 100, "keycaps", keycaps_layout());
        k->set_bindings(keycaps_bindings());
        k->set_bindings(keycaps_bindings());
        k->show();
    }

    // view.show();
    //
    // std::cout << view.block.dump() << '\n';
    // f_util::print_children(&view, 3);

    Fl::run();
}
