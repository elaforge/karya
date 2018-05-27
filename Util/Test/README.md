The test framework is home-grown and sort of hacked up, but fairly simple.

Shake will use `Util.Test.GenerateRunTests` to gather functions starting with
`test_` from `**/*_test.hs`, and generate a module which passes them to
`Util.Test.RunTests`.

RunTests has some features for parallelism and whatnot, but ultimately it just
runs the tests.  They are expected to print to stdout, where a line starting
with `__->` is a failure, and `++->` is a success.

Either RunTests itself or a wrapper script greps the outputs for failures.
