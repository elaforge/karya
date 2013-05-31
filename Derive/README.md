The derivation subsystem is concerned with converting the generic high level
UI score to a medium-level stream of 'Derive.Score.Event's, which then goes to
the performance backend.

The core library of functions for use in the Deriver monad is exported from
'Derive.Derive'.  This is actual a re-export of 'Derive.Deriver.Monad', which
implements the core monad and types, 'Derive.Deriver.Internal', which exports
vaguely-defined "internal" functions, and 'Derive.Deriver.Lib', which
implements functions which are of general use but still somewhat low level.

The derivation starts at 'Derive.Call.Block.eval_root_block' and proceeds from
there.  The language that appears in the tracks is defined by
'Derive.ParseBs' and 'Derive.TrackLang', the basic evaluation machinery is in
'Derive.Call', and the library of calls is in the `Derive/Call` subdirectory.
'Derive.Call.Util' is a library of functions for calls, sort of like a higher
level 'Derive.Deriver.Lib'.

Scales are implemented as a namespace of calls which return pitches, and are
brought into scope when the scale is set.  The built-in set of scales and
their attendant libraries are in `Derive/Scale`.
