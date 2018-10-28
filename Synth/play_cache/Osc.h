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
    Osc(std::ostream &log, int channels, int sampleRate, int maxBlockFrames);
    ~Osc();
    bool read(sf_count_t frames, float **out);

private:
    std::ostream &log;

    lo_server server;
    std::unique_ptr<std::thread> thread;
    std::atomic<bool> threadQuit;
    std::unique_ptr<Streamer> streamer;
    float volume;
    // Resample ratio.  Not implemented yet.
    float ratio;
    void loop();

    static int handlePlay(
        const char *path, const char *types, lo_arg **argv,
        int argc, void *data, void *self);
    void play(const char *path, float ratio, float vol);

    static int handleStop(
        const char *path, const char *types, lo_arg **argv,
        int argc, void *data, void *self);
    void stop();
};
