// Copyright 2013 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <pthread.h>
#include <semaphore.h>

#include <jack/jack.h>
#include <jack/midiport.h>
#include <jack/ringbuffer.h>


#define MAX_PORTS 64

class Client;
typedef void (*NotifyCallback)(
    Client *client, const char *port, int is_add, int is_read);

// write_message looks up the port name, puts it on the ringbuffer with a ptr.
// The msgs contain the port
struct Client {
    Client();
    ~Client();
    jack_client_t *client;
    NotifyCallback notify;

    void add_read_port(jack_port_t *port);
    void remove_read_port(jack_port_t *port);
    void add_write_port(jack_port_t *port);

    jack_port_t *read_ports[MAX_PORTS];
    // The only reason I need these is to clear them on each process cycle.
    // Too bad JACK doesn't have a way to ask for my ports in process().
    jack_port_t *write_ports[MAX_PORTS];
    sem_t available; // How many events are on the input buffer.

    jack_ringbuffer_t *immediate_output;
    jack_ringbuffer_t *output;
    jack_ringbuffer_t *input;
};

enum {
    output_buffer_size = 4 * 1024,
    // VL1 sysex dumps can get up to 400K, but I guess I can deal with that
    // if it becomes a problem.
    immediate_output_buffer_size = 8 * 1024,
    input_buffer_size = 8 * 1024
};

struct midi_event {
    jack_port_t *port;
    jack_midi_event_t event;
};

extern "C" {

const char *create_client(const char *client_name, NotifyCallback notify,
    Client **out_client);

// ports
const char *create_read_port(Client *client, const char *remote_name);
const char *remove_read_port(Client *client, const char *remote_name);
const char *create_write_port(Client *client, const char *remote_name);
const char **get_midi_ports(Client *client, unsigned long flags);

// read and write
const char *write_message(Client *client, const char *port, uint64_t time,
    void *bytes, int size);

// Block until an event arrives, then fill it in.
int read_event(Client *client, const char **port, uint64_t *time,
    void **mevent);
void jack_abort(Client *client);
uint64_t now(Client *client);
}
