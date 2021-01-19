// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <fstream>
#include <atomic>
#include <thread>

#include <sndfile.h>

#include "Streamer.h"


// This is a thread that listens for OSC messages, and streams samples when it
// gets them.
class Thru {
public:
    Thru(std::ostream &log, int channels, int sample_rate, int max_frames);
    ~Thru();
    bool read(int channels, sf_count_t frames, float **out);

private:
    std::ostream &log;

    std::unique_ptr<std::thread> thread;
    std::atomic<bool> thread_quit;
    std::unique_ptr<MixStreamer> streamer;
    int socket_fd;

    void loop();
};
