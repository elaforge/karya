`repl` is a simple command line interface.  It uses haskeline for input
editing, which is similar to readline, only with different bugs.  Haskeline
can be configured in ~/.haskeline, but basically what you want in there is
`editMode: Vi` and `historyDuplicates: IgnoreConsecutive`.  Full documentation
is at <http://trac.haskell.org/haskeline>.

The repl accepts haskell which is evaluated in a 'Cmd.Cmd.CmdL' monad, which is
just a synonym for `CmdT IO a`.  The result is displayed directly if it's a
String, or otherwise given to `show` and pretty printed.  There's also a `pp`
function which will format the result with the 'Util.Pretty.Pretty' typeclass,
which is easier to read but may omit data.

The 'Cmd.Repl.Environ' module is in scope, which in turn imports just about
everything interesting.  Notably, it imports 'Cmd.Repl.Global' unqualified,
which provides a basic vocabulary of functions, and all the Cmd.Repl.L*
modules, which provide functions intended to be used from the REPL.  In case
you're wondering about the L prefix, it's because the directory used to be
called Lang, and I got used to the L.  And there has to be some prefix,
otherwise all those module names clash with other module names.

The REPL will load all the modules when the app starts.  Like ghci, if the `.o`
files are missing or out of date, it will load modules as bytecode.  This is
fine if it's a few modules in `Cmd/Repl`, but if some low level module has been
touched it will want to byte-compile everything, and usually crash on the spot.
Not sure why.  Anyway, make sure you did a fresh build.  If you just modified
high-level modules like the ones in `Cmd/Repl`, you can type `:r` in the REPL
and it will reload them.

The REPL also has a simple macro feature to make it easier to write IDs.
`@name` will become `(*Id "ns/name")` where `*` is BlockId, RulerId, TrackId,
or ViewId, whatever is appropriate, and `ns` is the
'Ui.State.config_namespace'.  `@"a b c"` will let you put spaces or symbols or
other non-letter non-dash letters in.

You can do tons of stuff from the REPL, and in fact most operations can only be
done from the REPL.  Read through the `Cmd/Repl` directory to get some ideas.
If an expression is too large to type directly at the REPL, you can edit a
`Cmd/Repl` module, or open a new module in `Local/Repl`, and use `:r` to
reload.
