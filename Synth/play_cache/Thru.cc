// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <arpa/inet.h>
#include <netinet/in.h>
#include <string>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include "Thru.h"
#include "Synth/Shared/config.h"
#include "log.h"

#include <iostream> // DEBUG


enum {
    max_voices = 3 // Max simultaneous voices.
};


struct Play {
    const char *sample;
    int offset;
    double ratio;
    double volume;
};


struct Message {
    Message(bool stop, std::vector<Play> plays) : stop(stop), plays(plays) {}
    Message(bool stop) : stop(stop), plays() {}
    Message() : stop(false) {} // This means there was an error.
    bool stop;
    std::vector<Play> plays;
};


static const char *
parse_play(const char *message, Play *play)
{
    if (*message == '\0' || *message == '\n')
        return nullptr;
    play->sample = message;
    message += strlen(message) + 1;
    play->offset = strtol(message, nullptr, 10);
    message += strlen(message) + 1;
    play->ratio = strtod(message, nullptr);
    message += strlen(message) + 1;
    play->volume = strtod(message, nullptr);
    message += strlen(message) + 1;
    return message;
}

// Parse the protocol emitted by Synth/Shared/Thru.hs
static std::vector<Play>
parse_plays(const char *message)
{
    std::vector<Play> plays;
    for (;;) {
        Play play;
        message = parse_play(message, &play);
        if (message)
            plays.push_back(play);
        else
            break;
    }
    return plays;
}

static Message
read_message(FILE *fp)
{
    // The read buffer is statically allocated in here, and the strings are
    // just pointers to it.  This is ok because the server is single-threaded.
    static char *line = nullptr;
    static size_t line_size = 0;
    ssize_t read = getline(&line, &line_size, fp);
    if (read > 0) {
        if (strcmp(line, "stop\n") == 0) {
            return Message(true);
        } else {
            return Message(false, parse_plays(line));
        }
    } else {
        // Socket closed?
        return Message();
    }
}


static int
listen(std::ostream &log)
{
    int fd = socket(PF_INET, SOCK_STREAM, 0);
    if (fd == -1) {
        LOG("socket(): " << strerror(errno));
        return -1;
    }
    int opt = 1;
    if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)) == -1) {
        LOG("setsockopt(): " << strerror(errno));
        return -1;
    }

    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(THRU_PORT);
    addr.sin_addr.s_addr = INADDR_ANY;

    // I will get EADDRINUSE if the port is already bound.  I tried
    // SO_REUSEPORT, but only one of them gets the connection, where I would
    // like it to broadcast to them all.
    if (bind(fd, (struct sockaddr *) &addr, sizeof(addr)) == -1) {
        LOG("bind(): " << strerror(errno));
        return -1;
    }
    if (listen(fd, 1) == -1) {
        LOG("listen(): " << strerror(errno));
        return -1;
    }
    return fd;
}


static Message
accept(std::ostream &log, int socket_fd)
{
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    int addr_len = sizeof(addr);
    // Will block.
    int fd = accept(
        socket_fd, (struct sockaddr *) &addr, (socklen_t *) &addr_len);
    if (fd == -1) {
        LOG("accept(): " << strerror(errno));
        return Message();
    }
    FILE *fp = fdopen(fd, "r");
    if (fp == nullptr) {
        LOG("fdopen(): " << strerror(errno));
        return Message();
    }
    Message message = read_message(fp);
    fclose(fp);
    return message;
}


Thru::Thru(std::ostream &log, int channels, int sample_rate, int max_frames)
    : log(log), thread_quit(false), volume(1)
{
    socket_fd = listen(log);
    streamer.reset(
        new MixStreamer(max_voices, log, channels, sample_rate, max_frames));
    thread.reset(new std::thread(&Thru::loop, this));
}


Thru::~Thru()
{
    thread_quit.store(true);
    LOG("Thru quit");
    // Send myself as empty message to get accept() to return.
    {
        int fd = socket(PF_INET, SOCK_STREAM, 0);
        struct sockaddr_in addr;
        memset(&addr, 0, sizeof(addr));
        addr.sin_family = AF_INET;
        addr.sin_port = htons(THRU_PORT);
        addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        if (connect(fd, (struct sockaddr *) &addr, sizeof(addr)) == -1) {
            LOG("connect(): " << strerror(errno));
        }
        write(fd, "\n", 2);
        close(fd);
    }
    thread->join();
    if (socket_fd > 0)
        close(socket_fd);
}


bool
Thru::read(int channels, sf_count_t frames, float **out)
{
    bool done = streamer->read(channels, frames, out);
    if (!done && volume != 1) {
        for (int i = 0; i < channels * frames; i++) {
            (*out)[i] *= this->volume;
        }
    }
    return done;
}


void
Thru::loop()
{
    while (!thread_quit.load()) {
        if (socket_fd < 0) {
            LOG("socket not allocated");
            break;
        }
        Message message(accept(log, socket_fd));
        if (message.stop) {
            LOG("stop");
            streamer->stop();
        } else {
            for (const Play &play : message.plays) {
                LOG("play: " << play.sample << " offset: " << play.offset
                    << " ratio:" << play.ratio << " vol:" << play.volume);
            }
            // Stop old notes.
            streamer->stop();
            int voice = 0;
            for (const Play &play : message.plays) {
                // TODO incorrect for multiple Plays, move volume to streamer
                this->volume = play.volume;
                streamer->start(voice, play.sample, play.offset, play.ratio);
                voice++;
            }
        }
    }
}
