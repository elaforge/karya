// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include "Osc.h"
#include <iostream>
#include <fstream>


// Hand test the Osc class.
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
