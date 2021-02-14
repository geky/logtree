#!/usr/bin/env python3

# exhaustively search for smallest bugged tree

import itertools as it
import sys

from logtree import LogTree

def main(N=None):
    # track built trees
    seen = {(): LogTree()}
    height = 0
    for n in it.islice(it.count(1), N):
        for perm in it.permutations(range(n)):
            # find location of new x, we should have the tree
            # before this in our seen trees
            i = perm.index(n-1)
            tree = seen[perm[:i]].clone()
            for j, x in enumerate(perm[i:]):
                tree.append(x, 'n%d'%x)

                # add each to seen trees
                seen[perm[:i+j+1]] = tree.clone()
                if tree.height() > height:
                    height = tree.height()

            # test that lookups work on tree
            for x in range(n):
                q = tree.lookup(x)
                if q != 'n%d'%x:
                    print('found bad tree')
                    print('lookup(%s) => %s' % (x, q))
                    print('should be => %s' % ('n%d'%x))
                    print('perm %s' % (perm,))
                    sys.exit(1)

        print('searched all trees of size %s, worst height %d' % (n, height))

    print('done')

if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
