#include <pthread.h>
#include <semaphore.h>

#include <jack/jack.h>
#include <jack/midiport.h>
#include <jack/ringbuffer.h>


#define MAX_PORTS 64

// write_message looks up the port name, puts it on the ringbuffer with a ptr.
// The msgs contain the port
struct Client {
    Client();
    ~Client();
    jack_client_t *client;

    void add_read_port(jack_port_t *port);
    void remove_read_port(jack_port_t *port);
    void add_write_port(jack_port_t *port);

    jack_port_t *read_ports[MAX_PORTS];
    pthread_mutex_t processing; // Whether the process() function is working.
    sem_t available; // How many events are on the input buffer.

    jack_ringbuffer_t *immediate_output;
    jack_ringbuffer_t *output;
    jack_ringbuffer_t *input;

    jack_nframes_t current_frame;
};

enum {
    output_buffer_size = 4096,
    immediate_output_buffer_size = 1024,
    input_buffer_size = 1024
};

struct midi_event {
    jack_port_t *port;
    jack_midi_event_t event;
};

extern "C" {

const char *create_client(const char *client_name, Client **out_client);

// ports
const char *create_read_port(Client *client, const char *remote_name);
const char *remove_read_port(Client *client, const char *remote_name);
const char **get_midi_ports(Client *client, unsigned long flags);

// Block until an event arrives, then fill it in.
int read_event(Client *client, const char **port, uint64_t *time,
    void **mevent);
uint64_t now(Client *client);
}
