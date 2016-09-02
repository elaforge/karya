// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <FL/Enumerations.H>

#include "util.h"

#include "StyleTable.h"


const EventStyle *
StyleTable::get(StyleId id) const
{
    ASSERT(id < stable.size());
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
StyleTable::get()
{
    static StyleTable table;
    return &table;
}
