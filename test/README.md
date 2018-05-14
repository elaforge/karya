The test framework is home-grown and sort of hacked up, but fairly simple.

`Util.Test.GenerateRunTests` is given a set of test modules.  By convention,
these have the form *_test.hs.  It extracts functions beginning with `test_`,
and generates a `RunTests` program that calls all of them.  `RunTests`
actually takes a regex and only runs tests that match.

Test names are prefixed with `interactive-` for tests that want to have a chat
with you, `gui-` that are going to be popping up windows right and left (more
accurately, ones that require a special initialization function, which means
gui in practice), and `normal-` for well-behaved tests that do none of that.

Tests are run by `test/run_tests`.  It runs all of the `normal-` tests and
redirects their stdout to `build/test.output`.

Failing tests print lines beginning with `__->`, so failures are reported by
grepping test.output.  After running the tests, the coverage report goes in
`build/hpc/index.html`.
