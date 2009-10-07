#!/usr/bin/env python
"""usage: output.hs test_mod1.hs test_mod2.hs ...

Collect tests from the given modules and generate a haskell module that calls
the tests.

The generated haskell module takes a set of string prefixes, and will run
tests starting with one of those prefixes.  If the first argument is '-list',
it will just print the tests instead of running them.

Tests are divided into init- / direct- and plain- / interactive-, depending on
whether they define an 'initialize' function, or use certain interactive
assertions.
"""

import sys, os, re
import hs_pp


def main():
    out_fn = sys.argv[1]
    test_fns = sys.argv[2:]

    init_func = re.compile(r'^initialize .*=', re.MULTILINE)
    test_defs = {}
    init_funcs = {}
    for fn in test_fns:
        src = open(fn).read()
        lines = list(open(fn))
        test_defs[fn] = get_defs(list(enumerate(lines)))
        if not test_defs[fn]:
            print >>sys.stderr, 'Warning: no test_* defs in %r' % fn
        if init_func.search(''.join(lines)):
            init_funcs[fn] = '%s.initialize' % path_to_module(fn)

    out = open(out_fn, 'w')
    out.write(hs_template % {
        'argv0': sys.argv[0],
        'imports': '\n'.join(map(make_import, test_fns)),
        'all_tests': ',\n    '.join(make_tests(test_defs, init_funcs)),
    })


def get_defs(lines):
    # regexes are not liking me, so functional it is
    if not lines:
        return []
    i, line = lines[0]
    i += 1 # enumerate starts from 0
    m = re.match(r'^test_[a-zA-Z0-9_]+ \=', line)
    if m:
        body, rest = span(
            lambda (_, line): line.startswith(' ') or line == '\n', lines[1:])
        body = ''.join(line for (_, line) in body)
        head = line.split(None, 1)
        return [(i, head[0], head[1]+body)] + get_defs(rest)
    else:
        return get_defs(lines[1:])

def span(f, xs):
    pre = []
    for i, x in enumerate(xs):
        if f(x):
            pre.append(x)
        else:
            break
    return pre, xs[i:]

def make_import(fn):
    return 'import qualified %s' % path_to_module(fn)

def path_to_module(path):
    return os.path.splitext(path)[0].replace('/', '.')

def make_tests(test_defs, init_funcs):
    out = []
    for fn, defs in test_defs.items():
        for (lineno, test_name, body) in defs:
            name = ''
            if fn in init_funcs:
                init = '(Just %s)' % init_funcs[fn]
                name += 'init-'
            else:
                name += 'direct-'
                init = 'Nothing'
            if has_interactive(body):
                name += 'interactive-'
            else:
                name += 'plain-'
            sym = '%s.%s' % (path_to_module(fn), test_name)
            name += sym
            out.append('Test %s (%s >> return ()) %s %d %s'
                % (hs_str(name), sym, hs_str(fn), lineno, init))
    return out

def has_interactive(func_body):
    for (src, dest, is_interactive) in hs_pp.test_macros:
        if is_interactive and re.search(r'\b(%s|%s)\b'
            % (src, dest), func_body):
            return True
    return False

def hs_str(s):
    return '"%s"' % s.replace('"', '\\"').replace('\n', '\\n')


hs_template = r'''-- automatically generated by %(argv0)s --
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.IORef as IORef
import qualified System.Environment
import qualified System.Console.GetOpt as GetOpt

import qualified Util.Seq as Seq
import qualified Util.Test as Test

%(imports)s

data Test = Test
    { test_name :: String
    , test_test :: IO ()
    , test_file :: String
    , test_line :: Int
    , test_initialize :: Maybe (IO () -> IO ())
    }

all_tests = [
    %(all_tests)s
    ]

data Flag = List | Skip deriving (Eq, Show)

options =
    [ GetOpt.Option [] ["list"] (GetOpt.NoArg List) "display but don't run"
    , GetOpt.Option [] ["skip"] (GetOpt.NoArg Skip)
        "run though interactive tests without asking"
    ]

main :: IO ()
main = do
    args <- System.Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (opts, n, []) -> return (opts, n)
        (_, _, errs) -> error $ "errors:\n" ++ concat errs
    run flags args

run flags args
    | List `elem` flags = print_tests
    | Skip `elem` flags = do
        IORef.writeIORef Test.skip_human True
        print_tests
        mapM_ run_test tests
    | otherwise = do
        print_tests
        mapM_ run_test tests
    where
    tests = matching_tests args
    print_tests = print_sorted (map test_name tests)

print_sorted vals = mapM_ putStrLn (List.sort vals)

matching_tests :: [String] -> [Test]
matching_tests [] = all_tests
matching_tests prefixes =
    filter (\t -> any (`List.isInfixOf` test_name t) prefixes) all_tests

run_test test = do
    putStrLn $ "---------- run test "
        ++ test_file test ++ ": " ++ test_name test
    let name = last (Seq.split "." (test_name test))
    maybe id id (test_initialize test) $
        Test.catch_srcpos
            (Just (test_file test, Just name, test_line test)) (test_test test)
'''


if __name__ == '__main__':
    main()
