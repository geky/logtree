#!/usr/bin/env python3

import random
import itertools as it
import collections as co

# 32-bit bit reverse
def brev(n):
    return int(''.join(reversed('{:032b}'.format(n))), 2)

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

    def __init__(self, nodes=[], rotate_pred='brev'):
        self.nodes = []
        for k, v in nodes:
            self.append(k, v)

        if rotate_pred == None or rotate_pred == False:
            # never rotate
            self.rotate_pred = lambda a, b: False
        elif rotate_pred == True:
            self.rotate_pred = lambda a, b: True
        elif rotate_pred == 'random':
            self.rotate_pred = lambda a, b: random.randint(0, 1)
        elif rotate_pred == 'brev':
            self.rotate_pred = lambda a, b: brev(a[1]) > brev(b[1])
        else:
            self.rotate_pred = rotate_pred

    def __str__(self):
        return '[%s]' % ', '.join(str(node) for node in self.nodes)

    def __repr__(self):
        return 'LogTree%s' % self

    def append(self, key, value):
        # build alts
        alts = []
        off = len(self.nodes)-1
        lo, hi = float('-inf'), float('inf')

        # keep track of past alt to see if we should rotate
        #
        # Note we just access the alt end here, but we would
        # actually need to keep the previous alt in RAM. Annoying
        # but not a deal-breaker.
        def appendalt(alt):
            altkey, altoff = alt
            # rotate?
            if (alts and ((key >= altkey and altkey >= alts[-1][0]) or
                    (key < altkey and altkey < alts[-1][0])) and
                    altoff > alts[-1][1] and
                    self.rotate_pred((altkey, altoff), alts[-1])):
                alts.pop()

            alts.append((altkey, altoff))

        while off >= 0:
            if hasattr(self, 'iters'):
                self.iters += 1

            node = self.nodes[off]
            if node.key == key:
                # found key!
                break

            for altkey, altoff in node.alts:
                if hasattr(self, 'iters2'):
                    self.iters2 += 1
                if altkey > lo and altkey < hi:
                    if key < altkey:
                        hi = altkey
                        if altkey <= node.key:
                            appendalt((altkey, off))
                            off = altoff
                            break
                        else:
                            appendalt((altkey, altoff))
                    elif key >= altkey:
                        lo = altkey
                        if altkey > node.key:
                            appendalt((altkey, off))
                            off = altoff
                            break
                        else:
                            appendalt((altkey, altoff))
            else:
                # did not find key
                appendalt((max(node.key, key), off))
                break

        # should not have duplicates
        alt_uniq = co.defaultdict(lambda: 0)
        for alt in alts:
            alt_uniq[alt[0]] += 1
        for alt in alts:
            assert alt_uniq[alt[0]] == 1, "alts not uniqe!? %s" % alt

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
                if hasattr(self, 'iters2'):
                    self.iters2 += 1
                if altkey > lo and altkey < hi:
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
    tree.append(4, 'd')
    print(tree)
    print('1 = ', tree.lookup(1))
    print('2 = ', tree.lookup(2))
    print('3 = ', tree.lookup(3))
    print('4 = ', tree.lookup(4))

    tree = LogTree()
    tree.append(4, 'd')
    tree.append(3, 'c')
    tree.append(2, 'b')
    tree.append(1, 'a')
    print(tree)
    print('4 = ', tree.lookup(4))
    print('3 = ', tree.lookup(3))
    print('2 = ', tree.lookup(2))
    print('1 = ', tree.lookup(1))

    tree = LogTree()
    tree.append(3, 'a')
    tree.append(5, 'b')
    tree.append(1, 'c')
    tree.append(7, 'd')
    tree.append(2, 'd')
    print(tree)
    print('3 = ', tree.lookup(3))

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
        append_iters2 = []
        lookup_iters = []
        lookup_iters2 = []
        input = list(input)
        baseline = {}
        x = 0
        for i in input:
            tree.iters = 0
            tree.iters2 = 0
            tree.append(i, repr(x))
            baseline[i] = repr(x)
            x += 1
            append_iters.append(tree.iters)
            append_iters2.append(tree.iters2)
        for i in input:
            tree.iters = 0
            tree.iters2 = 0
            if tree.lookup(i) != baseline.get(i):
                if pass_ == True:
                    print('could not find %s (expected %s)' % (
                        i, baseline.get(i)))
                pass_ = False
            lookup_iters.append(tree.iters)
            lookup_iters2.append(tree.iters2)
        print('test %s' % ('passed' if pass_ else 'FAILED'))
    #    if not pass_:
    #        print(tree)
    
        print('height = %d' % tree.height())
        print('max append iters  = %d' % max(append_iters))
        print('avg append iters  = %d' % (
            sum(append_iters) / len(append_iters)))
        print('max append iters2 = %d' % max(append_iters2))
        print('avg append iters2 = %d' % (
            sum(append_iters2) / len(append_iters2)))
        print('max lookup iters  = %d' % max(lookup_iters))
        print('avg lookup iters  = %d' % (
            sum(lookup_iters) / len(lookup_iters)))
        print('max lookup iters2 = %d' % max(lookup_iters2))
        print('avg lookup iters2 = %d' % (
            sum(lookup_iters2) / len(lookup_iters2)))

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
