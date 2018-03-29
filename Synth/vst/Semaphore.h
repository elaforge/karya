// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

// OS X doesn't support POSIX unnamed semaphores, but it has its own semaphore
// API which is close enough for me.

#ifdef __APPLE__

#include <mach/task.h>
#include <mach/semaphore.h>
#include <mach/mach_init.h>


// Docs at:
// https://developer.apple.com/library/content/documentation/Darwin/Conceptual/KernelProgramming/synchronization/synchronization.html
class Semaphore {
public:
    Semaphore() : Semaphore(0) {}
    Semaphore(int value) : task(mach_task_self()) {
        semaphore_create(task, &semaphore, SYNC_POLICY_FIFO, value);
    }
    ~Semaphore() {
        semaphore_destroy(task, semaphore);
    }
    void post() { semaphore_signal(semaphore); }
    void wait() { semaphore_wait(semaphore); }
private:
    const task_t task;
    semaphore_t semaphore;
};

#else

#include <semaphore.h>
#include <stdlib.h>

class Semaphore {
public:
    Semaphore(int value) { sem_init(&semaphore, 0, value); }
    ~Semaphore() { sem_destroy(&semaphore); }
    void post() { sem_post(&semaphore); }
    void wait() {
        for (;;) {
            if (sem_wait(&semaphore) == -1) {
                if (errno != EINTR)
                    abort();
            }
        }
    }
private:
    sem_t semaphore;
}

#endif
