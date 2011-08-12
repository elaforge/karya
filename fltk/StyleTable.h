#ifndef __STYLE_TABLE_H
#define __STYLE_TABLE_H


// Index into a table of preset styles.  This is intentionally small since
// every event has one.
typedef unsigned char StyleId;

// How to draw an event.
struct EventStyle {
    EventStyle(int font, int size, Color text_color, Color event_color)
        : font(font), size(size), text_color(text_color),
            event_color(event_color)
    {}
    int font; // font and face as an fltk font index
    int size;
    Color text_color;
    Color event_color;
};

// For efficiency, styles are actually stored as small numbers.
class StyleTable {
public:
    const EventStyle *get(StyleId id) const;
    void put(StyleId id, const EventStyle &style);
    static StyleTable *get();

private:
    std::vector<EventStyle> stable;
};

#endif
