// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <iostream>
#include <fstream>
#include <string>
#include <stdio.h>
#include <lo/lo.h>

#include "Osc.h"
#include "Synth/Shared/config.h"
#include "log.h"


static void
handleError(int num, const char *msg, const char *where)
{
    printf("ERROR: %s %s\n", msg, where);
}


Osc::Osc(std::ostream &log, int channels, int sampleRate, int maxBlockFrames)
    : log(log), threadQuit(false), volume(1), ratio(1)
{
    std::string port = std::to_string(OSC_PORT);
    this->server = lo_server_new(port.c_str(), handleError);
    lo_server_add_method(server, "/play", "sff", Osc::handlePlay, this);
    lo_server_add_method(server, "/stop", "", Osc::handleStop, this);
    streamer.reset(
        new Streamer(log, channels, sampleRate, maxBlockFrames, false));
    thread.reset(new std::thread(&Osc::loop, this));
}


Osc::~Osc()
{
    threadQuit.store(true);
    // TODO I might have to get it out of lo_server_recv.
    // The internal one uses polling and short timeouts, but maybe I can
    // send it a msg.
    thread->join();
    lo_server_free(server);
}

bool
Osc::read(sf_count_t frames, float **out)
{
    LOG("read");
    bool done = streamer->read(frames, out);
    LOG("done? " << done);
    if (done)
        return true;
    if (volume != 1) {
        for (int i = 0; i < streamer->channels * frames; i++) {
            (*out)[i] *= this->volume;
        }
    }
    return false;
}

void
Osc::loop()
{
    while (!threadQuit.load()) {
        int bytes = lo_server_recv(this->server);
        printf("recv bytes %d\n", bytes);
    }
}


int
Osc::handlePlay(
    const char *path, const char *types, lo_arg **argv,
    int argc, void *data, void *user_data)
{
    Osc *self = static_cast<Osc *>(user_data);
    self->play(&argv[0]->s, argv[1]->f, argv[2]->f);
    return 0;
}


void
Osc::play(const char *path, float ratio, float vol)
{
    std::vector<std::string> mutes;
    this->volume = vol;
    this->ratio = ratio;
    streamer->start(path, 0, mutes);
}


int
Osc::handleStop(
    const char *path, const char *types, lo_arg **argv,
    int argc, void *data, void *user_data)
{
    Osc *self = static_cast<Osc *>(user_data);
    self->stop();
    return 0;
}


void
Osc::stop()
{
    std::vector<std::string> mutes;
    streamer->start("", 0, mutes);
}


int
main(int argc, char **argv)
{
    Osc *osc = new Osc(std::cout, 2, 44100, 512);
    for (;;) {
        std::string input;
        std::cout << "wait..." << std::flush;
        std::getline(std::cin, input);
        if (input == "q")
            break;

        // float left[8], right[8];
        // float *samples[] = { left, right };
        float *samples = nullptr;
        bool done = osc->read(8, &samples);
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
    exit(0);
}
