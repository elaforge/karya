/* This configures the haskell compilation.

    It's in a separate file so that the relevant haskell files can include
    it.  This way only those files will recompile when the config changes.

    Unlike the other headers, this is strictly C, not C++.  The haskell CPP
    will choke on C++ comments.
*/

/* These are automatically rewritten by the shakefile. */
#undef INTERPRETER_HINT
#undef INTERPRETER_GHC

#if defined(__linux__)

#elif defined(__APPLE__)

#define CORE_MIDI

#endif
