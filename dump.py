#!/usr/bin/env python3

from logtree import LogTree
import string
import itertools as it
import random

#XS = [4,1,2,5,7,2,3,4,5,0,3,2,1]
#XS = list(range(10))
#XS = list(reversed(range(10)))
#XS = list(range(10))
#random.shuffle(XS)
#XS = it.repeat(0, 10)
#XS = [0,1,2,3,4,0,0,0,0,0]
#XS = [0,1,2,3,4,5]

#XS = [0,1,1,0,3,2,3,1,0,9]
#XS = [3,8,6,1,7,4,5,2,0,9]

#XS = [0,0,1,0,1,4,1,2,8,5]
#K = 6
#XS = [0,0,1,0,1,4] # ,1,2,8,5]
#K = 3
#XS = [0,0,1,3]
#K = 0
#XS = [0,1,2,3,4,5,6,7,8,9]
#K = 2
#XS = [0,0,1,0,1,3,0,7,8,2]
#K = 7
#XS = [0,0,2,1,3,2,3,5,0,0]
#K = 4
XS = [0,0,1,3]
K = 1

def main(output):
    # create tree
    tree = LogTree()
#    for i, c in cs:
#        tree.append(i, c)
#    for i, x in enumerate(XS):
#        tree.create(x, string.ascii_lowercase[i])
#    for i, x in enumerate(XS):
#        tree.append(x, string.ascii_lowercase[i])
    for i, x in enumerate(XS):
        tree.append2(x, repr(x))
    print('lookup2(%s) => ' % K, tree.lookup2(K))
#    tree.create(0, 'a')
#    tree.create(1, 'b')
#    tree.create(2, 'c')
#    tree.delete(1)
#    tree.create(2, 'd')
#    tree.create(3, 'e')
    #tree.create(0, 'f')
    #tree.create(1, 'g')
    #tree.create(0, 'h')
    #tree.create(0, 'i')
    #tree.create(0, 'j')
    #tree.create(0, 'k')
    #tree.create(0, 'l')

    # dump for later processing (ugh, python2/3 issues)
    with open(output, 'w') as f:
        for i, node in enumerate(tree.nodes):
            f.write('node,%s,%s,%s\n' % (i, node.key, node.value))
            for j, alt in enumerate(node.alts):
                f.write('alt,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' % (
                    j, i, 1 if alt.lt else 0,
                    alt.key, alt.weight, alt.iweight, alt.off, alt.skip, alt.delta))

if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
