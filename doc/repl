repl is a simple command line interface.  It uses haskeline for input editing,
which is similar to readline, only with different bugs.  Haskeline can be
configured in ~/.haskeline.

The repl accepts haskell which is evaluated in a CmdL monad, which is a synonym
for 'Cmd.CmdT IO a'.  The result is displayed directly if it's a String, or
given to 'show' and pretty printed.  There's also a 'pp' function which will
format the result with the Util.Pretty.Pretty typeclass, which is easier to
read but may omit data.

The Cmd.Lang.Environ module is in scope, which in turn imports just about
everything interesting.  Notably, it imports Cmd.Lang.Global unqualified, which
provides a basic vocabulary of functions, and all the Cmd.Lang.L* modules,
which provide functions intended to be used from the REPL.

The REPL also has a simple macro feature to make it easier to write IDs.
`@name` will become `(*Id "ns/name")` where `*` is BlockId, RulerId, TrackId,
or ViewId, whatever is appropriate, and `ns` is the
'Ui.State.config_namespace'.
