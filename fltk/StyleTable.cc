#include <FL/Enumerations.H>
#include "util.h"
#include "StyleTable.h"


const EventStyle *
StyleTable::get(StyleId id) const
{
    ASSERT(id <= stable.size());
    return &stable[id];
}


void
StyleTable::put(StyleId id, const EventStyle &style)
{
    // The default is really big to make it obviously wrong.
    static EventStyle deflt(FL_HELVETICA, 24, Color::black, Color::white);
    while (id >= stable.size()) {
        stable.push_back(deflt);
    }
    stable[id] = style;
}


StyleTable *
StyleTable::table()
{
    static StyleTable table;
    return &table;
}
