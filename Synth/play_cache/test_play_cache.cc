// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

// Exercise PlayCache internals for manual testing.
#include <memory>
#include <iostream>
#include <thread>
#include <unistd.h>

#include <sndfile.h>

#include "Thru.h"
#include "Semaphore.h"
#include "Streamer.h"
#include "Wav.h"


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
    sf_count_t max_frames = 512;
    sf_count_t start_offset = 0;
    std::vector<std::string> mutes;

    TracksStreamer streamer(std::cout, 2, 44100, max_frames);
    streamer.start(dir, start_offset, mutes);

    float *samples;

    for (int n = 0; n < 4; n++) {
        streamer.read(2, 256, &samples);
        std::cout << "smp: " << samples[0] << '\n';
        nap(1);
    }
}


static void
thru()
{
    Thru thru(std::cout, 2, 44100, 512);
    std::cout << "thread started, 'q' to quit\n";
    for (;;) {
        std::string input;
        std::cout << "wait..." << std::flush;
        std::getline(std::cin, input);
        if (input == "q")
            break;

        // float left[8], right[8];
        // float *samples[] = { left, right };
        float *samples = nullptr;
        bool done = thru.read(2, 8, &samples);
        if (done) {
            std::cout << "done\n";
        } else {
            std::cout << "samples:";
            for (int i = 0; i < 8; i++) {
                std::cout << ' ' << samples[i*2] << ',' << samples[i*2+1];
            }
            std::cout << '\n';
        }
    }
}


static void
test_wav(const char *fname)
{
    wav::Wav *wav;
    wav::Error err;

    err = wav::open_read(fname, &wav, 2, 44100);
    if (err) {
        std::cout << fname << ": " << err << "\n";
        return;
    }
    SF_INFO info = {0};
    SNDFILE *sndfile = sf_open(fname, SFM_READ, &info);

    const int frames = 64;
    float samples1[frames*2], samples2[frames*2];
    int unequal = 0, equal = 0;
    for (;;) {
        wav::Frames read1 = wav::read(wav, frames, samples1);
        wav::Frames read2 = sf_readf_float(sndfile, samples2, frames);
        if (read1 != read2) {
            std::cout << read1 << " != " << read2 << "\n";
            return;
        }
        if (read1 == 0)
            break;

        for (int i = 0; i < read1; i++) {
            if (samples1[i] != samples2[i]) {
                std::cout << i << ": " << samples1[i] << " != " << samples2[i]
                    << "\n";
                unequal++;
            } else {
                equal++;
                // std::cout << i << ": == " << samples1[i] << "\n";
            }
        }
    }
    std::cout << "equal: " << equal << " unequal: " << unequal << "\n";
    wav::close(wav);
    sf_close(sndfile);
}


int
main(int argc, const char **argv)
{
    std::string cmd = argc >= 2 ? argv[1] : "";
    if (argc == 2 && cmd == "semaphore") {
        semaphore();
    } else if (argc == 3 && cmd == "stream") {
        stream(argv[2]);
    } else if (argc == 2 && cmd == "thru") {
        thru();
    } else if (argc == 3 && cmd == "wav") {
        test_wav(argv[2]);
    } else {
        std::cout << "test_play_cache"
            " [ semaphore | stream dir | thru | wav file.wav ]\n";
        return 1;
    }
    return 0;
}
