// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <fstream>
#include <atomic>
#include <thread>

#include <lo/lo.h>
#include <sndfile.h>

#include "Streamer.h"


// This is a thread that listens for OSC messages, and streams samples when it
// gets them.
class Osc {
public:
    Osc(std::ostream &log, int channels, int sample_rate, int max_block_frames);
    ~Osc();
    bool read(int channels, sf_count_t frames, float **out);

private:
    std::ostream &log;

    lo_server server;
    std::unique_ptr<std::thread> thread;
    std::atomic<bool> thread_quit;
    std::unique_ptr<ResampleStreamer> streamer;
    double volume;

    lo_server new_server();
    void loop();

    static int handle_play(
        const char *path, const char *types, lo_arg **argv,
        int argc, void *data, void *self);
    void play(const char *path, int64_t offset, double ratio, double vol);

    static int handle_stop(
        const char *path, const char *types, lo_arg **argv,
        int argc, void *data, void *self);
    void stop();
};
