// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

// OS X doesn't support POSIX unnamed semaphores, C++11 doesn't include one
// for some reason, so here's one from stackoverflow.
//
// I originally used OS X mach semaphores or POSIX, depending on the platform,
// but I don't trust the OS X one and don't want to deal with #ifdefs.
//
// This one takes a lock in post(), which is against the rules for realtime,
// but it's a really short lock.  For all I know POSIX semaphores do that too.

#include <mutex>
#include <condition_variable>

class Semaphore {
public:
    Semaphore(int count = 0) : count(count) {}

    void post() {
        std::unique_lock<std::mutex> lock(mutex);
        ++count;
        condition.notify_one();
    }

    void wait() {
        std::unique_lock<std::mutex> lock(mutex);
        while (!count) // Handle spurious wake-ups.
            condition.wait(lock);
        --count;
    }

private:
    std::mutex mutex;
    std::condition_variable condition;
    int count;
};
