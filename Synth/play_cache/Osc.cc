// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <string>
#include <unistd.h>
#include <lo/lo.h>

#include "Osc.h"
#include "Synth/Shared/config.h"
#include "log.h"


// Seriously?
#define TO_STR(x) #x
#define STR(x) TO_STR(x)


// liblo somehow forgot to add a context argument, and here in 2018 C++ somehow
// still has no way to easily create an explicit closure wrapper.
//
// Actually it has lo_server_set_error_context, but of course that has to be
// called after lo_server_new, at which point it's too late to notice creation
// errors.
static std::ostream *errorLog;

static void
handleError(int num, const char *msg, const char *where)
{
    std::ostream &log = *errorLog;
    LOG("OSC error: " << msg << ", at: " << (where ? where : "<nowhere>"));
}



Osc::Osc(std::ostream &log, int channels, int sampleRate, int maxBlockFrames)
    : log(log), threadQuit(false), volume(1)
{
    errorLog = &log;
    this->server = new_server();
    streamer.reset(
        new ResampleStreamer(log, channels, sampleRate, maxBlockFrames));
    thread.reset(new std::thread(&Osc::loop, this));
}


Osc::~Osc()
{
    threadQuit.store(true);
    LOG("Osc quit");
    {
        // Send myself a random message to get lo_server_recv to return.
        lo_address self = lo_address_new(nullptr, STR(OSC_PORT));
        lo_send(self, "/quit", "");
        lo_address_free(self);
    }
    thread->join();
    if (this->server)
        lo_server_free(server);
}


bool
Osc::read(int channels, sf_count_t frames, float **out)
{
    bool done = streamer->read(channels, frames, out);
    if (!done && volume != 1) {
        for (int i = 0; i < channels * frames; i++) {
            (*out)[i] *= this->volume;
        }
    }
    return done;
}


// Sometimes I get "cannot find free port".  The liblo source is byzantine, but
// it looks like I got EADDRINUSE which is related to the more byzantine BSD
// socket rules, but perhaps the old connection is lingering.  I think
// SO_REUSEADDR should prevent this, but it seems like liblo only uses that for
// TCP.
lo_server
Osc::new_server()
{
    lo_server server =
        lo_server_new_with_proto(STR(OSC_PORT), LO_UDP, handleError);
    if (server) {
        lo_server_add_method(server, "/play", "sdd", Osc::handlePlay, this);
        lo_server_add_method(server, "/stop", "", Osc::handleStop, this);
    }
    return server;
}


void
Osc::loop()
{
    while (!threadQuit.load()) {
        while (!this->server) {
            // see Osc::new_server
            LOG("server failed to connect, retrying...");
            this->server = new_server();
            if (!server)
                sleep(1);

        }
        lo_server_recv(server);
    }
}


int
Osc::handlePlay(
    const char *path, const char *types, lo_arg **argv,
    int argc, void *data, void *user_data)
{
    Osc *self = static_cast<Osc *>(user_data);
    self->play(&argv[0]->s, argv[1]->d, argv[2]->d);
    return 0;
}


void
Osc::play(const char *path, double ratio, double vol)
{
    LOG("play: " << path << " ratio:" << ratio << " vol:" << vol);
    this->volume = vol;
    streamer->start(path, ratio);
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
    streamer->stop();
}
