- Local style is in doc/STYLE.

- After cloning the repo, run git-hooks/install to check some local checks.
If you're not using vim and fast-tags, you don't need post-merge.

### tests

Karya uses a custom testing framework.  Sorry.  Back in the day, there weren't
other test frameworks, or they were missing features I wanted, like
file:linenumber information, and by now I'm too used to doing it my way.

Test modules end with `_test.hs` and tests are functions inside that start with
`test_`.  The shakefile collects them to build a single
`build/test/RunTests.hs` and from there `build/test/RunTests`, so `mk
build/test/RunTests` builds all the tests.  You can call with `--list` to
see the tests, or a regex to run those tests, or with no args to run them all.

Test failures are lines that start wtih `__`, `++` is passes.

If you want you can build a suite from a single module with the `_test` suffix
omitted, e.g. `run build/test/RunTests-Cmd.Integrate` to build and run just
those tests.  There usually isn't much reason to do this, because of ghci,
but tests with C dependencies don't like to run from ghci due to no `.so`
version of the C / C++ deps.  I think.

If you `mk tests-normal` this will run all the tests with `tools/run_tests`
which is a wrapper that runs them in parallel, checks the results, and
generates hpc.

The easiest way to run individual tests is from ghci.  Just load the test
module, and type in the test name.  Then you can `:r` and up-arrow as you try
to get it to pass.
