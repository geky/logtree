#!/usr/bin/env python3

# exhaustively search for smallest bugged tree

import itertools as it
import sys

from logtree import LogTree

def main(N=None):
    worst_height = 0
    worst_perm = ()
    try:
        for n in it.islice(it.count(1), N):
            for perm in it.permutations(range(n)):
                try:
                    # build tree
                    tree = LogTree()
                    for x in perm:
                        tree.append(x, 'n%d'%x)

                    # test that lookups work on tree
                    for x in range(n):
                        q = tree.lookup(x)
                        if q != 'n%d'%x:
                            print('found bad tree')
                            print('lookup(%s) => %s' % (x, q))
                            print('should be => %s' % ('n%d'%x))
                            print('perm %s' % (perm,))
                            sys.exit(1)

                    # worst tree we've seen?
                    if tree.height() > worst_height:
                        worst_height = tree.height()
                        worst_perm = perm
                except AssertionError:
                    print('assertion failed on perm %s' % (perm,))
                    print()
                    raise

            print('searched all trees of size %s, worst height %d' % (
                n, worst_height))
            print('worst perm %s' % (worst_perm,))

        print('done')

    except KeyboardInterrupt:
        print('terminated, worst height %d' % worst_height)
        print('worst perm %s' % (worst_perm,))

if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
