// Binding to the MIDI part of JACK.
#include <string>
#include "jack.h"

// just for DEBUG
#include "fltk/util.h"


// create_client

Client::Client()
{
    for (int i = 0; i < MAX_PORTS; i++) {
        read_ports[i] = 0;
    }
    pthread_mutex_init(&processing, NULL);
    sem_init(&available, 0, 0);
    current_frame = 0;

    // TODO memset(ring->buf, 0, ring->size) to avoid page faults?
    // failed = jack_ringbuffer_mlock(client.immediate_output_ring);
    immediate_output = jack_ringbuffer_create(immediate_output_buffer_size);
    output = jack_ringbuffer_create(output_buffer_size);
    input = jack_ringbuffer_create(input_buffer_size);
}

Client::~Client()
{
    pthread_mutex_destroy(&this->processing);
    jack_ringbuffer_free(immediate_output);
    jack_ringbuffer_free(output);
    jack_ringbuffer_free(input);
}

void
Client::add_read_port(jack_port_t *port)
{
    for (int i = 0; i < MAX_PORTS; i++) {
        if (__sync_bool_compare_and_swap(&read_ports[i], 0, port))
            break;
    }
}

void
Client::remove_read_port(jack_port_t *port)
{
    for (int i = 0; i < MAX_PORTS; i++) {
        if (__sync_bool_compare_and_swap(&read_ports[i], port, 0))
            break;
    }
}

void
Client::add_write_port(jack_port_t *port)
{
    for (int i = 0; i < MAX_PORTS; i++) {
    }
}


// Called when a new port is registered or unregistered.
// When a new MIDI input appears, I should have haskell connect it up
// to a local port if I want it, likewise for a MIDI output.
static void
registration_callback(jack_port_id_t port_id, int register_, void *arg)
{
    Client *client = static_cast<Client *>(arg);
    jack_port_t *port = jack_port_by_id(client->client, port_id);
    if (port) {
        const char *name = jack_port_name(port);
        DEBUG("saw a registration: '" << name << "' register " << register_);
    } else {
        DEBUG("registered unknown port");
    }
}


// whenever a port is connected or disconnected
static void
port_connect_callback(jack_port_id_t a, jack_port_id_t b, int connect,
    void *arg)
{
}


static void
write_midi_event(jack_nframes_t nframes, const midi_event &event)
{
    void *buf = jack_port_get_buffer(event.port, nframes);
    jack_midi_data_t *midi = jack_midi_event_reserve(
        buf, event.event.time, event.event.size);
    if (!midi) {
        DEBUG("no space in output port");
    } else {
        memcpy(midi, event.event.buffer, event.event.size);
    }
}

// This runs in a high priority thread.
static int
process(jack_nframes_t nframes, void *arg)
{
    Client *client = static_cast<Client *>(arg);

    // This should never be locked for process().  It's used so other threads
    // can wait for the process thread to complete.
    pthread_mutex_trylock(&client->processing);

    // Read incoming MIDI.  To guarantee all the ports are still valid, I
    // disconnect, wait for process() to complete with the 'processing' lock,
    // and then unregister.
    for (int i = 0; i < MAX_PORTS; i++) {
        jack_port_t *port = client->read_ports[i];
        if (!port)
            continue;
        void *buf = jack_port_get_buffer(port, nframes);

        jack_nframes_t count = jack_midi_get_event_count(buf);
        for (jack_nframes_t j = 0; j < count; j++) {
            midi_event event;
            jack_midi_event_get(&event.event, buf, j);
            event.port = port;
            event.event.time += jack_last_frame_time(client->client);
            if (!jack_ringbuffer_write(
                client->input, (char *) &event, sizeof(event)))
            {
                DEBUG("input buffer overrun");
            } else {
                sem_post(&client->available);
            }
        }
    }

    // Write outgoing MIDI.  To guarantee all the ports are still valid, I
    // never unregister ports, only disconnect them.  The problem is that it's
    // hard to tell if any instances of the old port are still in the
    // output ringbuffer.
    midi_event event;
    while (jack_ringbuffer_read(client->immediate_output, (char *) &event,
        sizeof(event)))
    {
        write_midi_event(nframes, event);
    }
    while (jack_ringbuffer_peek(client->output, (char *) &event, sizeof(event)))
    {
        if (event.event.time > nframes)
            break;
        jack_ringbuffer_read_advance(client->output, sizeof(event));
        write_midi_event(nframes, event);
    }
    client->current_frame += nframes;
    pthread_mutex_unlock(&client->processing);
    return 0; // no error, but who knows what returning an error would do
}


// Provide C functions for the FFI.
extern "C" {

// Create a Client and return it into the 'client' parameter, or NULL and
// return an error message.
const char *
create_client(const char *client_name, Client **out_client)
{
    jack_status_t status;
    int failed;
    static char errorbuf[127];

    *out_client = NULL;
    Client *client = new Client();

    // ensure client_name < jack_client_name_size()
    client->client = jack_client_open(client_name, JackNullOption, &status);
    if (!client->client) {
        // Yeah I know it's a bitmask, but these seem mutually exclusive.
        if (status & JackServerFailed)
            return "unable to connect to JACK server";
        else if (status & JackServerError)
            return "communication error with JACK server";
        else if (status & JackInitFailure)
            return "unable to initialize client";
        else if (status & JackShmFailure)
            return "unable to access shared memory";
        else if (status & JackVersionError)
            return "client's protocol version does not match";
        else {
            sprintf(errorbuf, "unknown error: 0x%x\n", status);
            return errorbuf;
        }
    }

    // jack_set_port_connect_callback
    // This is called called whenever a port is connected or disconnected
    failed = jack_set_port_registration_callback(
        client->client, registration_callback, client);
    if (failed)
        return "jack_set_port_registration_callback() failed";

    failed = jack_set_port_connect_callback(
        client->client, port_connect_callback, client);
    if (failed)
        return "jack_set_port_connect_callback() failed";

    failed = jack_set_process_callback(client->client, process, client);
    if (failed)
        return "jack_set_process_callback failed";

    failed = jack_activate(client->client);
    if (failed) {
        return "jack_activate() failed";
    }
    *out_client = client;
    return NULL;
}


// ports

const char *
create_read_port(Client *client, const char *remote_name)
{
    // I create a local name with the same name as the full remote name.  So
    // if the remote name is 'system:out', then the local name will be
    // 'client:system:out'.  Jack seems happy to accept names with more than
    // one colon.
    std::string local_name = remote_name;
    std::string local_long_name = jack_get_client_name(client->client);
    local_long_name += ':';
    local_long_name += local_name;

    DEBUG("connect read: " << local_long_name << " <- " << remote_name);
    jack_port_t *port = jack_port_by_name(
        client->client, local_long_name.c_str());
    if (!port) {
        port = jack_port_register(
            client->client, local_name.c_str(), JACK_DEFAULT_MIDI_TYPE,
            JackPortIsInput | JackPortIsTerminal, 0);
        if (!port)
            return "can't create local port";
    }

    if (jack_connect(client->client, remote_name, local_long_name.c_str())) {
        // jack_port_unregister(client->client, port);
        return "can't connect to remote port";
    }
    client->add_read_port(port);
    return NULL;
}

const char *
remove_read_port(Client *client, const char *remote_name)
{
    std::string local_name = jack_get_client_name(client->client);
    local_name += ':';
    local_name += remote_name;

    jack_port_t *port = jack_port_by_name(client->client, local_name.c_str());
    if (!port)
        return "local port not found";
    DEBUG("disconnect read: " << local_name << " <- " << remote_name);
    client->remove_read_port(port);
    jack_port_disconnect(client->client, port);
    // I never unregister ports.  This ensures that any read ports still on
    // the input ringbuffer won't cause jack_port_short_name to crash.
    //
    // I could unregister with a guarentee that process() has completed (wait
    // for the processing lock to open, then don't take it) if I used a safe
    // map<jack_port_t *, string> instead of jack functions.
    // pthread_mutex_lock(&client->processing);
    // pthread_mutex_unlock(&client->processing);
    // jack_port_unregister(client->client, port);
    return NULL;
}

/*
int
create_write_port(const char *dest)
{
    const char *write_name = strchr(dest, ':') + 1;
    jack_port_t *port = jack_port_register(
        client.client, write_name, JACK_DEFAULT_MIDI_TYPE,
        JackPortIsOutput | JackPortIsTerminal, 0);
    if (!port)
        return 1;
    return jack_connect(client.client, jack_port_name(port), dest);
}
*/

const char **
get_midi_ports(Client *client, unsigned long flags)
{
    return jack_get_ports(client->client, NULL, JACK_DEFAULT_MIDI_TYPE, flags);
}

// write

// int
// write_message(const char *port_name, jack_time_t timestamp, midi_message msg)
// {
//     // lookup port, create a write_message, put it on the ringbuffer
//     jack_port_t *port = jack_port_by_name(client.client, port_name);
//     if (!port)
//         return 1;
//     // struct read_msg msg;
//     return 0;
// }

int
read_event(Client *client, const char **port, uint64_t *time, void **mevent)
{
    midi_event event;
try_again:
    // Incremented for every msg put on the ringbuffer.
    sem_wait(&client->available);

    size_t read = jack_ringbuffer_read(
        client->input, (char *) &event, sizeof(event));
    if (read < sizeof(event)) {
        // This should never happen!
        goto try_again;
    }
    *port = jack_port_short_name(event.port);
    *time = jack_frames_to_time(client->client, event.event.time);
    *mevent = event.event.buffer;
    return event.event.size;
}

uint64_t
now(Client *client)
{
    return jack_frames_to_time(
        client->client, jack_frame_time(client->client));
}

};
