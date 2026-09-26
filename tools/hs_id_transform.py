#!/usr/bin/env python3
# SLOP but seems to work
"""Transform identifiers in Haskell source, e.g. replace 'return' with 'pure'.

This lexes just enough Haskell to skip string literals, char literals, line
comments, (nested) block comments, numeric literals, and hsc2hs directives,
then applies `transform` to each identifier in the code.  There are no flags
for the transformation, just edit `transform`.
"""

import argparse
import re
import sys


# The symbol chars that can make up an operator.  A run of them is lexed
# together, so '-->' is an operator, while '--' or '---' start a comment.
SYMBOL = set('!#$%&*+./<=>?@\\^|-~:')
SYMBOLS = re.compile(r'[!#$%&*+./<=>?@\\^|~:-]+')
DASHES = re.compile(r'--+$')
# Unicode letters are allowed, but an identifier can't start with a digit.
# \w includes _ and unicode letters and digits.
IDENT = re.compile(r"[^\W\d][\w']*")
# Lexed separately since they aren't identifiers, but '0x1f' looks like one
# once the leading digit is consumed.
NUMBER = re.compile(r'''
    0[xX][0-9a-fA-F_]+ | 0[oO][0-7_]+ | 0[bB][01_]+
    # The fraction requires a digit after the '.', so '[1..2]' isn't a float.
    | \d[\d_]* (?: \. \d[\d_]* )? (?: [eE][-+]?\d[\d_]* )?
''', re.VERBOSE)
# Pragmas whose body is Haskell, and hence gets transformed like any other
# code.  Unlisted ones such as LANGUAGE or OPTIONS_GHC are skipped along with
# the other block comments.  SCC labels aren't really identifiers, but they're
# written as ones, and conventionally match the name of what they annotate.
PRAGMA = re.compile(r'''
    \{-\# \s*
    (?: INLINEABLE | INLINABLE | INLINE | NOINLINE | SPECIALIZE | SPECIALISE
        | MINIMAL | COMPLETE | DEPRECATED | WARNING | RULES | ANN | SCC
    ) \b
''', re.VERBOSE | re.IGNORECASE)
# hsc2hs directives, whose arguments are C, not Haskell, e.g.
# '(#poke Struct, some_field)' or '#size Struct'.  '##ifdef' etc. are cpp
# directives passed through by hsc2hs.  '#enum's leading type and constructor
# args are actually Haskell, but they're left alone along with the rest.
DIRECTIVE = re.compile(r'''
    \#\#? \{?
    (?: peek | poke | ptr | offset | size | alignment | type | const_str
        | const | enum | def | let | include | undef
        | ifdef | ifndef | if | elif | else | endif | error | warning
    ) \b
''', re.VERBOSE)
# 'x', '\n', '\DEL', '\65'.  Not a prime, which is handled separately.
CHAR = re.compile(r"'(?:\\[^&]?[0-9A-Za-z]*|\\&|[^\\'])'")
# A string gap: backslash, whitespace, backslash.
GAP = re.compile(r'\\\s+\\')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('-i', '--in-place', action='store_true',
        help='modify files in place, instead of writing to stdout')
    parser.add_argument('file', nargs='*', help='default: stdin')
    args = parser.parse_args()

    if not args.file:
        if args.in_place:
            parser.error('--in-place requires files')
        sys.stdout.write(map_idents(transform, sys.stdin.read()))
        return
    for fname in args.file:
        with open(fname) as fp:
            source = fp.read()
        out = map_idents(transform, source)
        if not args.in_place:
            sys.stdout.write(out)
        elif out != source:
            with open(fname, 'w') as fp:
                fp.write(out)


def transform(name, qualified):
    """Called for each identifier in code, return its replacement.

    'qualified' is True if the name is preceded by a module qualifier, as the
    'x' in 'M.x'.  The 'M' itself is a separate unqualified identifier.
    """
    # return replace_word(name, 'return', 'pure') if not qualified else name
    return to_camel(name)


def replace_word(name, word, replacement):
    return replacement if name == word else name


def to_camel(name):
    """some_name -> someName, leaving leading and trailing _ alone."""
    if name[:1].isupper(): # Don't mess with module and constructor names.
        return name
    if name in not_to_camel:
        return name
    if name.startswith('test_'):
        return name
    stripped = name.strip('_')
    if not stripped:
        return name
    lead = name[:len(name) - len(name.lstrip('_'))]
    trail = name[len(name.rstrip('_')):]
    words = stripped.split('_')
    return (
        lead + words[0] + ''.join(w[:1].upper() + w[1:] for w in words[1:])
        + trail
    )

not_to_camel = set([
    # Process uses snake for some reason.
    'std_in', 'std_out', 'std_err',
])


def map_idents(transform, source):
    """Apply transform to each identifier in code, but not in strings or
    comments.
    """
    out = []
    i = 0
    end = len(source)
    while i < end:
        c = source[i]
        if c == '"':
            j = string_end(source, i)
            out.append(source[i:j])
            i = j
        elif c == "'" and not is_prime(source, i):
            m = CHAR.match(source, i)
            # If it's not a char literal it's TH quoting, e.g. 'name or ''Type,
            # so just emit the quote and continue with the name.
            j = m.end() if m else i + 1
            out.append(source[i:j])
            i = j
        elif m := PRAGMA.match(source, i):
            j = block_comment_end(source, i)
            # Transform the body, but leave the pragma word itself alone.
            body = j - 3 if source[j-3:j] == '#-}' else j
            out.append(m.group())
            out.append(map_idents(transform, source[m.end():body]))
            out.append(source[body:j])
            i = j
        elif source.startswith('{-', i):
            j = block_comment_end(source, i)
            out.append(source[i:j])
            i = j
        elif c == '#' and (m := DIRECTIVE.match(source, i)):
            j = directive_end(source, m.end(), brace='{' in m.group())
            out.append(source[i:j])
            i = j
        elif c in SYMBOL:
            m = SYMBOLS.match(source, i)
            j = m.end()
            if DASHES.match(m.group()):
                j = source.find('\n', i)
                j = end if j == -1 else j
            out.append(source[i:j])
            i = j
        elif m := NUMBER.match(source, i):
            out.append(m.group())
            i = m.end()
        elif m := IDENT.match(source, i):
            out.append(transform(m.group(), is_qualified(source, i)))
            i = m.end()
        else:
            out.append(c)
            i += 1
    return ''.join(out)


def directive_end(source, i, brace):
    """Index after the arguments of an hsc2hs directive, which start at i.

    They extend to the end of the line, or to a bracket that was opened
    outside of the directive, as the ')' in '(#const X)'.  The '#{...}' form
    instead ends at its own close brace.
    """
    depth = 1 if brace else 0
    while i < len(source):
        c = source[i]
        if c == '"':
            i = string_end(source, i)
        elif c == '\\' and source[i+1:i+2] == '\n': # cpp line continuation
            i += 2
        elif c == '\n':
            return i
        elif c in '([{':
            depth += 1
            i += 1
        elif c in ')]}':
            if depth == 0: # Not mine, so the directive ended before it.
                return i
            depth -= 1
            i += 1
            if brace and depth == 0:
                return i
        else:
            i += 1
    return len(source)


def string_end(source, i):
    """Index after the closing quote of the string literal starting at i."""
    i += 1
    while i < len(source):
        c = source[i]
        if c == '"':
            return i + 1
        elif c == '\\':
            m = GAP.match(source, i)
            # \\n, \\&, \\DEL etc: skipping two chars is enough to not be
            # confused by \\" or \\\\.
            i = m.end() if m else i + 2
        else:
            i += 1
    return len(source) # Unterminated string.


def block_comment_end(source, i):
    """Index after the closing '-}', counting nesting."""
    depth = 0
    while i < len(source):
        if source.startswith('{-', i):
            depth += 1
            i += 2
        elif source.startswith('-}', i):
            depth -= 1
            i += 2
            if depth == 0:
                return i
        else:
            i += 1
    return len(source) # Unterminated comment.


def is_prime(source, i):
    """True if the ' at i continues an identifier, e.g. x'."""
    return i > 0 and is_ident(source[i-1])


def is_qualified(source, i):
    """True if the identifier at i is preceded by a module qualification."""
    if i < 2 or source[i-1] != '.':
        return False
    j = i - 1
    while j > 0 and is_ident(source[j-1]):
        j -= 1
    return j < i - 1 and source[j].isupper()


def is_ident(c):
    return c.isalnum() or c in "_'"


if __name__ == '__main__':
    main()
