from __future__ import print_function

import re
import sys

print("Loading Nim Runtime support.", file=sys.stderr)
#http://python3porting.com/differences.html

if sys.version > '3':
  xrange = range

# allow to manually reload while developing

nimobjfile = gdb.current_objfile() or gdb.objfiles()[0]
nimobjfile.pretty_printers = []

import gdb

class NimStringPrinter:
    "pretty print Nim strings"

    pattern = re.compile('^NimStringDesc \*$')

    def __init__(self, val):
        self.val = val

    def display_hint(self):
        return 'string'

    def to_string(self):
        if self.val:
            l = int(self.val['Sup']['len'])
            return self.val['data'][0].address.string("utf-8", "ignore", l)
        else:
            return None

class SeqValue:
    "Wrapper for slice values."

    def __init__(self, val):
        self.val = val

    @property
    def len(self):
        return int(self.val['Sup']['len'])

    @property
    def reserved(self):
        return int(self.val['Sup']['reserved'])

    def __getitem__(self, i):
        if i < 0 or i >= self.len:
            raise IndexError(i)
        ptr = self.val["data"][0].address
        return (ptr + i).dereference()

class NimSeqPrinter:
    pattern = re.compile(r'^TY_.*$') # well not very specific here

    def __init__(self, val):
        self.val = val

    def display_hint(self):
        return 'array'

    def to_string(self):
        return "seq"

    def children(self):
        sval = SeqValue(self.val)

        if sval.len > sval.reserved:
            return

        for idx, item in enumerate(sval):
            yield ('[{0}]'.format(idx), item)

def makematcher(klass):
    def matcher(val):
        try:
            if klass.pattern.match(str(val.type)):
                return klass(val)
        except Exception:
            pass
    return matcher


nimobjfile.pretty_printers.extend([makematcher(var) for var in vars().values() if hasattr(var, 'pattern')])
