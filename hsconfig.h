/* This configures the haskell compilation.

    It's in a separate file so that the relevant haskell files can include
    it.  This way only those files will recompile when the config changes.

    Unlike the other headers, this is strictly C, not C++.  The haskell CPP
    will choke on C++ comments.
*/

/* All the include guard does is avoid some "redefined" warnings from CPP. */
#ifndef __HSCONFIG_H
#define __HSCONFIG_H

/* These are automatically rewritten by the shakefile. */
#undef INTERPRETER_HINT
#undef INTERPRETER_GHC

#if defined(__linux__)

#define JACK_MIDI

#elif defined(__APPLE__)

#define CORE_MIDI

#endif

#endif
