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
            if hasattr(self, 'iters'):
                self.iters += 1

            node = self.nodes[off]
            if node.key == key:
                # found key!
                break

            for altkey, altoff in node.alts:
                if lo < altkey and altkey < hi:
                    if key < altkey:
                        hi = altkey
                        if altkey <= node.key:
                            alts.append((altkey, off))
                            off = altoff
                            break
                        else:
                            alts.append((altkey, altoff))
                    elif key >= altkey:
                        lo = altkey
                        if altkey > node.key:
                            alts.append((altkey, off))
                            off = altoff
                            break
                        else:
                            alts.append((altkey, altoff))
            else:
                # did not find key
                alts.append((max(node.key, key), off))
                break

#        if off != -1 and self.nodes[off].key != key:
#            alts.append((max(self.nodes[off].key, key), off))

#            # remove random suffix? TODO this good? TODO answer is no
#            if len(alts) > 1:
#                #alts = alts[:random.randrange(len(alts))+1]
#                #alts = alts[:1]
#                #alts = [(alts[0][0], max(x for _, x in alts))]
#                pass

#           TODO oh, move this out of loop?
#            # rotate shenanigans?
#            nalts = []
#            i = 0
#            while i < len(alts):
#                if i < len(alts)-1 and (
#                        key > alts[i+1][0] and alts[i+1][0] > alts[i+0][0] and
#                        alts[i+1][1] > alts[i+0][1] and
#                        random.randint(0, 1)):
#                    nalts.append(alts[i+1])
#                    i += 2
#                elif i < len(alts)-1 and (
#                        key < alts[i+1][0] and alts[i+1][0] < alts[i+0][0] and
#                        alts[i+1][1] > alts[i+0][1] and
#                        random.randint(0, 1)):
#                    nalts.append(alts[i+1])
#                    i += 2
#                else:
#                    nalts.append(alts[i])
#                    i += 1
#            alts = nalts



        # append
        self.nodes.append(LogTree.Node(key, value, alts=alts))

    def lookup(self, key):
        off = len(self.nodes)-1
        lo, hi = float('-inf'), float('inf')

        while off >= 0:
            if hasattr(self, 'iters'):
                self.iters += 1

            node = self.nodes[off]
            if node.key == key:
                return node.value

            # build new alt-pointers
            for altkey, altoff in node.alts:
                if lo < altkey and altkey < hi:
                    if key < altkey:
                        hi = altkey
                        if altkey <= node.key:
                            off = altoff
                            break
                    elif key >= altkey:
                        lo = altkey
                        if altkey > node.key:
                            off = altoff
                            break
            else:
                return None

    def height(self):
        return max(len(node.alts) for node in self.nodes)

def main():
    tree = LogTree()
    tree.append(1, 'a')
    tree.append(2, 'b')
    tree.append(3, 'c')
    print(tree)
    print('1 = ', tree.lookup(1))
    print('2 = ', tree.lookup(2))
    print('3 = ', tree.lookup(3))

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

    def test(input):
        pass_ = True
        tree = LogTree()
        append_iters = []
        lookup_iters = []
        input = list(input)
        baseline = {}
        x = 0
        for i in input:
            tree.iters = 0
            tree.append(i, repr(x))
            baseline[i] = repr(x)
            x += 1
            append_iters.append(tree.iters)
        for i in input:
            tree.iters = 0
            if tree.lookup(i) != baseline.get(i):
                if pass_ == True:
                    print('could not find %s (expected %s)' % (
                        i, baseline.get(i)))
                pass_ = False
            lookup_iters.append(tree.iters)
        print('test %s' % ('passed' if pass_ else 'FAILED'))
    #    if not pass_:
    #        print(tree)
    
        print('height = %d' % tree.height())
        print('max append iters = %d' % max(append_iters))
        print('avg append iters = %d' % (sum(append_iters) / len(append_iters)))
        print('max lookup iters = %d' % max(lookup_iters))
        print('avg lookup iters = %d' % (sum(lookup_iters) / len(append_iters)))

    print('test in order')
    test(range(1000))
    print('test in reverse order')
    test(reversed(range(1000)))
    print('test in random order')
    x = list(range(1000))
    random.shuffle(x)
    test(x)
    print('test in order overlapping')
    test(it.chain(range(1000), range(1000)))
    print('test in reverse order overlapping')
    test(it.chain(reversed(range(1000)), reversed(range(1000))))
    print('test in random order overlapping')
    x = list(range(1000))
    random.shuffle(x)
    y = list(range(1000))
    random.shuffle(y)
    test(it.chain(x, y))


if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
