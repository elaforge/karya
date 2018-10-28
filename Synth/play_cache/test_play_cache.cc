// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

// Exercise PlayCache internals for manual testing.
#include <memory>
#include <iostream>
#include <thread>
#include <unistd.h>

#include <sndfile.h>

#include "Streamer.h"
#include "Semaphore.h"


static void
nap(double seconds)
{
    usleep(seconds/2 * 1000000);
    // Try to avoid getting mixed up output from threads.
    std::cout << "nap for " << seconds << '\n';
    usleep(seconds/2 * 1000000);
}


static void
waiter(Semaphore *sem, int arg)
{
    for (int i = 0; i < 2; i++) {
        sem->wait();
        std::cout << "thread " << arg << '\n';
    }
}


static void
semaphore()
{
    Semaphore sem(0);
    std::thread t1(waiter, &sem, 1);
    std::thread t2(waiter, &sem, 2);
    nap(0.5);
    sem.post();
    nap(0.5);
    sem.post();
    nap(0.5);
    sem.post();
    nap(0.5);
    sem.post();

    t1.join();
    t2.join();
}


static void
stream(const char *dir)
{
    if (!*dir) {
        std::cout << "no dir";
        return;
    }
    sf_count_t maxFrames = 512;
    sf_count_t startOffset = 0;
    std::vector<std::string> mutes;

    Streamer streamer(std::cout, 2, 44100, maxFrames, true);
    streamer.start(dir, startOffset, mutes);

    float *samples;

    for (int n = 0; n < 4; n++) {
        streamer.read(256, &samples);
        std::cout << "smp: " << samples[0] << '\n';
        nap(1);
    }
}


int
main(int argc, const char **argv)
{
    std::string cmd = argc >= 2 ? argv[1] : "";
    if (argc == 2 && cmd == "semaphore") {
        semaphore();
    } else if (argc == 3 && cmd == "stream") {
        stream(argv[2]);
    } else {
        std::cout << "test_play_cache [ semaphore | stream dir ]\n";
        return 1;
    }
    return 0;
}
