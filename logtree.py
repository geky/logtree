#!/usr/bin/env python3

import random
import itertools as it

class LogTree:
    class Node:
        def __init__(self, key, value, alts=[]):
            self.key = key
            self.value = value
            self.alts = alts

        def __str__(self):
            return '(%r: %r%s)' % (
                self.key, self.value,
                '; %s' % ','.join('%s:%s' % (k, v) for (k, v) in self.alts)
                    if self.alts else '')

        def __repr__(self):
            return 'LogTree.Node%s' % self

    def __init__(self, nodes=[]):
        self.nodes = []
        for k, v in nodes:
            self.append(k, v)

    def __str__(self):
        return '[%s]' % ', '.join(str(node) for node in self.nodes)

    def __repr__(self):
        return 'LogTree%s' % self

    def append(self, key, value):
        # build alts
        alts = []
        off = len(self.nodes)-1
        lo, hi = float('-inf'), float('inf')

        while off >= 0:
            node = self.nodes[off]
            # found key?
            # TODO can we base this off lo/hi?
            if node.key == key:
                break

            if key < node.key and node.key < hi:
                hi = node.key
            elif key > node.key and node.key > lo:
                lo = node.key

            noff = -1
            for altkey, altoff in node.alts:
                if altkey > lo and altkey < hi:
                    if key < altkey and altkey < node.key:
                        noff = altoff
                        break
                    elif key > altkey and altkey > node.key:
                        noff = altoff
                        break
                    else:
                        alts.append((altkey, altoff))

#            # build new alt-pointers
#            noff = -1
#            nkey = self.nodes[off].key
#            for altkey, altoff in node.alts:
#                if key < altkey and altkey > node.key:
#                    alts.append((altkey, off))
#                    noff = altoff
#                elif key > altkey and altkey < node.key:
#                    alts.append((altkey, off))
#                    noff = altoff
#                else:
#                    alts.append((altkey, altoff))
#            else if :
#                alts.append((node.key, off))
                
                    


#                if altkey > lo and altkey < hi:
#                    
#
#
#
#
#                    alts.append((altkey, altoff))
#
#                    if key <= altkey and altkey < node.key:
#                        hi = altkey
#                        noff = altoff
#                        break
#                    elif key >= altkey and altkey > node.key:
#                        lo = altkey
#                        noff = altoff
#                        break
#                    elif key <= altkey:
#                        hi = altkey
#                        pass
#                    elif key >= altkey:
#                        lo = altkey
#                        pass
##
#
#                    if key <= altkey:
#                        hi = altkey+1
#                        if altkey < node.key:
#                            alts.append((altkey+1, off))
#                            noff = altoff
#                            break
#                        else:
#                            alts.append((altkey, altoff))
#                    elif key >= altkey:
#                        lo = altkey-1
#                        if altkey > node.key:
#                            alts.append((altkey-1, off))
#                            noff = altoff
#                            break
#                        else:
#                            alts.append((altkey, altoff))
#            else:
#            if not alts:

            alts.append((node.key, off))
            off = noff

        # append
        self.nodes.append(LogTree.Node(key, value, alts=alts))

    def lookup(self, key):
        off = len(self.nodes)-1
        lo, hi = float('-inf'), float('inf')

        while off >= 0:
            node = self.nodes[off]
            # found key?
            # TODO can we base this off lo/hi?
            if node.key == key:
                return node.value

            if key < node.key and node.key < hi:
                hi = node.key
            elif key > node.key and node.key > lo:
                lo = node.key

            # build new alt-pointers
            for altkey, altoff in node.alts:
                if altkey > lo and altkey < hi:
                    if key == altkey:
                        off = altoff
                        break
                    elif key < altkey and altkey < node.key:
                        off = altoff
                        break
                    elif key > altkey and altkey > node.key:
                        off = altoff
                        break
            else:
                return None

    def height(self):
        return max(len(node.alts) for node in self.nodes)

def main():
    tree = LogTree()
    tree.append(3, 'a')
    tree.append(5, 'b')
    tree.append(1, 'c')
    tree.append(7, 'd')
    tree.append(2, 'd')
    print(tree)

    tree = LogTree()
    tree.append(1, 'a')
    tree.append(2, 'b')
    tree.append(3, 'c')
    tree.append(4, 'c')
    tree.append(5, 'five')
    tree.append(1, 'c')
    tree.append(2, 'two')
    tree.append(3, 'c')
    tree.append(0, 'zero')
    print(tree)
    print('0 = ', tree.lookup(0))
    print('5 = ', tree.lookup(5))
    print('2 = ', tree.lookup(2))
    print('-1 = ', tree.lookup(-1))

    print('test in order')
    pass_ = True
    tree = LogTree()
    for i in range(1000):
        tree.append(i, repr(i))
    for i in range(1000):
        if tree.lookup(i) != repr(i):
#            print('could not find %s' % i)
            pass_ = False
    print('test %s' % ('passed' if pass_ else 'failed'))
#    if not pass_:
#        print(tree)
    print('height = %d' % tree.height())

    print('test in reverse order')
    pass_ = True
    tree = LogTree()
    for i in reversed(range(1000)):
        tree.append(i, repr(i))
    for i in range(1000):
        if tree.lookup(i) != repr(i):
#            print('could not find %s' % i)
            pass_ = False
    print('test %s' % ('passed' if pass_ else 'failed'))
#    if not pass_:
#        print(tree)
    print('height = %d' % tree.height())

    print('test in random order')
    pass_ = True
    tree = LogTree()
    is_ = list(range(1000))
    random.shuffle(is_)
    for i in is_:
        tree.append(i, repr(i))
    for i in range(1000):
        if tree.lookup(i) != repr(i):
#            print('could not find %s' % i)
            pass_ = False
    print('test %s' % ('passed' if pass_ else 'failed'))
#    if not pass_:
#        print(tree)
    print('height = %d' % tree.height())


if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
