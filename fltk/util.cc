// Copyright 2020 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "util.h"


namespace util {

Timing *
Timing::get()
{
    static Timing timing;
    return &timing;
}

void
Timing::timing(const char *name, int level)
{
    // Make a copy, but intern it, because they'll mostly be the same strings.
    // This is probably a pointless micro-optimization.
    name = intern_table.insert(std::string(name)).first->c_str();
    events.push_back(Event(std::chrono::steady_clock::now(), name, level));
}


void
Timing::flush()
{
    if (events.empty())
        return;
    if (!fp.is_open()) {
        fp.open("seq.events", std::ofstream::out | std::ofstream::app);
        ASSERT(fp.is_open());
        this->start = std::chrono::steady_clock::now();
    }
    for (Event &event : events) {
        // auto now = std::chrono::steady_clock::now();
        std::chrono::duration<double> dur = event.time - start;
        fp << dur.count() << ' ' << event.name << ' ' << event.val << '\n';
    }
    fp.flush();
    events.clear();
    events.reserve(256);
}

}
