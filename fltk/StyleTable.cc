// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <FL/Enumerations.H>

#include "util.h"

#include "StyleTable.h"

// The default is really big to make it obviously wrong.
static const EventStyle missing_style(
    FL_HELVETICA, 24, Color::black, Color::white);

const EventStyle *
StyleTable::get(StyleId id) const
{
    if (id < stable.size())
        return &stable[id];
    else
        return &missing_style;
}


void
StyleTable::put(StyleId id, const EventStyle &style)
{
    while (id >= stable.size()) {
        stable.push_back(missing_style);
    }
    stable[id] = style;
}


StyleTable *
StyleTable::get()
{
    static StyleTable table;
    return &table;
}
