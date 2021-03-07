#!/usr/bin/env python3

import random
import itertools as it
import collections as co
import binascii
import struct
import os
import copy

LOG_APPEND = os.environ.get('LOG_APPEND', None)
if LOG_APPEND:
    def log_append(*args):
        print(*args)
else:
    def log_append(*args):
        pass

# 32-bit bit reverse
def brev(n):
    return int(''.join(reversed('{:032b}'.format(n))), 2)

class LogTree:
    class Node:
        def __init__(self, key, value, type=None, alts=[]):
            self.key = key
            self.value = value
            self.type = type
            self.alts = alts

        def __str__(self, off=None):
            return '(%s%r: %r%s)%s' % (
                'c' if self.type == 'create' else
                    'd' if self.type == 'delete' else
                    '',
                self.key, self.value,
                '; %s' % ','.join(str(alt) for alt in self.alts)
                    if self.alts else '',
                '@%d' % off if off is not None else '')

        def __repr__(self):
            return 'LogTree.Node%s' % self

    class Alt:
        def __init__(self, lt, key, off, skip, color='b', weight=1):
            self.lt = lt
            self.key = key
            # in practice, alt.skip + alt.off can be combined into one offset
            self.off = off
            self.skip = skip
            self.color = color
            self.weight = weight

        def follow(self, key):
            return (
                (self.lt and key < self.key) or
                (not self.lt and key >= self.key))

        def inbounds(self, lo, hi):
            return self.key > lo and self.key < hi

        def clone(self):
            return copy.copy(self)

        def flip(self, off, skip):
            off, self.off = self.off, off
            skip, self.skip = self.skip, skip
            self.lt = not self.lt
            return off, skip

        def __str__(self):
            return '%s%s@%s.%s%c' % (
                '<' if self.lt else '≥',
                self.key,
                self.off, self.skip,
                self.color)

        def __repr__(self):
            return 'LogTree.Alt(%s)' % self

    def __init__(self, nodes=[]):
        self.nodes = []
        self.count = 0
        for k, v in nodes:
            self.append(k, v)

    def clone(self):
        # as a functional structure this is easy!
        clone = copy.copy(self)
        clone.nodes = self.nodes.copy()
        return clone

    def __str__(self):
        return '[%s]' % ', '.join(
            node.__str__(i) for i, node in enumerate(self.nodes))

    def __repr__(self):
        return 'LogTree%s' % self

    def append(self, key, value, type=None):
        if not self.nodes:
            self.count += 1
            self.nodes.append(LogTree.Node(key, value, type=type, alts=[]))
            log_append('N %s' % self.nodes[-1])
            return

        # extra helper functions
        def swap(alts, a, b):
            alts[a], alts[b] = alts[b], alts[a]
            # keep colors as they were
            alts[a].color, alts[b].color = alts[b].color, alts[a].color

        def recolor(alts):
            # push an alt up into a 2/3 node, recoloring and rotating
            # as necessary
            alts[-1].color = 'b'
            if len(alts) >= 2:
                assert alts[-2].color == 'b'
                alts[-2].color = 'r'
                if len(alts) >= 3 and alts[-3].color == 'r':
                    alts[-3].color = 'y'

                    # rotate to prepare split?
                    if alts[-3].lt != alts[-2].lt:
                        if alts[-3].lt == alts[-1].lt:
                            swap(alts, -1, -2)
                            log_append('RLR %s %s %s' % (
                                alts[-3], alts[-2], alts[-1]))
                        elif alts[-2].lt == alts[-1].lt:
                            swap(alts, -2, -3)
                            swap(alts, -1, -2)
                            log_append('RLL %s %s %s' % (
                                alts[-3], alts[-2], alts[-1]))
                        else:
                            assert False

        # build alts, in theory we only need a bounded number of these
        # in RAM to handle rebalancing
        alts = []
        off = len(self.nodes) - 1
        skip = 0
        lo, hi = float('-inf'), float('inf')
        yellow_edge = None

        while True:
            if hasattr(self, 'iters'):
                self.iters += 1

            node = self.nodes[off]

            for skip, alt in zip(
                    it.count(skip+1),
                    it.islice(node.alts, skip, None)):
                off_, skip_ = off, skip+1
                if hasattr(self, 'iters2'):
                    self.iters2 += 1

                # build alt
                alts.append(alt.clone())
                log_append('A %s' % alts[-1])

                # prune?
                if not alts[-1].inbounds(lo, hi):
                    if len(alts) >= 2:
                        assert alts[-2].color == 'y'
                        alts[-2].color = 'b'
                    off, skip = alts[-1].off, alts[-1].skip
                    alts.pop(-1)
                    log_append('P lo=%s hi=%s' % (lo, hi))

                # split?
                if len(alts) >= 2 and alts[-2].color == 'y':
                    assert yellow_edge is not None
                    if alts[-2].follow(key) or alts[-1].follow(key):
                        # swap so central branch is split, but by following
                        # this split forms naturally
                        swap(alts, -2, -1)
                        alts[-2].flip(off, skip)
                        off, skip = yellow_edge
                        yellow_edge = None
                        alts.pop(-1)
                        log_append('S %s' % (alts[-1]))
                    else:
                        # we need to force a split in this case, but we can
                        # reuse our history as long as we prune during later
                        # appends
                        swap(alts, -2, -1)
                        alts[-2].off, alts[-2].skip = yellow_edge
                        yellow_edge = None
                        alts.pop(-1)
                        log_append('S %s' % (alts[-1]))

                    # recolor?
                    recolor(alts)

                # follow single alt?
                if alts[-1].color == 'b' and alts[-1].follow(key):
                    # flip
                    off, skip = alts[-1].flip(off, skip)

                # follow red alt?
                if len(alts) >= 2 and alts[-2].follow(key):
                    assert alts[-2].color != 'b'
                    # flip
                    off, skip = alts[-2].flip(off, skip)
                    # swap
                    swap(alts, -1, -2)
                    log_append('R %s %s' % (alts[-2], alts[-1]))

                # reduce bounds based on all edges we could have taken
                if alts[-1].color == 'b':
                    for alt in alts[-3:]:
                        if not alt.lt:
                            lo, hi = lo, min(hi, alt.key)
                        else:
                            lo, hi = max(lo, alt.key), hi

                # track yellow offset in case of split
                if alts[-1].color == 'y':
                    yellow_edge = (off, skip-1)

                # this could be simplified but we want to track iters/iters2
                if off != off_ or skip != skip_:
                    break
            else:
                # did not find key, split leaf?
                if node.key != key:
                    alts.append(
                        LogTree.Alt(
                            lt=node.key < key,
                            key=key if node.key < key else node.key,
                            off=off,
                            skip=len(node.alts),
                            color='b'))
                    log_append('A %s' % alts[-1])
                    self.count += 1

                    # recolor?
                    recolor(alts)

                break

        # append
        self.nodes.append(LogTree.Node(key, value, type=type, alts=alts))
        log_append('N %s' % self.nodes[-1])

    def lookup(self, key):
        if not self.nodes:
            return None

        off = len(self.nodes)-1
        skip = 0
        while True:
            if hasattr(self, 'iters'):
                self.iters += 1

            node = self.nodes[off]
            if node.key == key and node.type != 'delete':
                # found key
                return node.value

            for alt in it.islice(node.alts, skip, None):
                if hasattr(self, 'iters2'):
                    self.iters2 += 1

                if not alt.lt:
                    if key >= alt.key:
                        off = alt.off
                        skip = alt.skip
                        break
                elif alt.lt:
                    if key < alt.key:
                        off = alt.off
                        skip = alt.skip
                        break
            else:
                # did not find key
                return None

    def traverse(self):
        if not self.nodes:
            return

        # traversal is like lookup, but we keep track of
        # hi, and use that for our next lookup, convenient
        # that we already track this
        key = float('-inf')
        while key != float('inf'):
            off = len(self.nodes)-1
            skip = 0
            lo, hi = float('-inf'), float('inf')
            while True:
                if hasattr(self, 'iters'):
                    self.iters += 1

                node = self.nodes[off]
                for alt in it.islice(node.alts, skip, None):
                    if hasattr(self, 'iters2'):
                        self.iters2 += 1

                    if not alt.lt:
                        # need to trim, otherwise we can end up with
                        # outdated keys
                        if key >= alt.key:
                            lo = max(lo, alt.key)
                            off = alt.off
                            skip = alt.skip
                            break
                        else:
                            hi = min(hi, alt.key)
                    elif alt.lt:
                        if key < alt.key:
                            hi = min(hi, alt.key)
                            off = alt.off
                            skip = alt.skip
                            break
                        else:
                            lo = max(lo, alt.key)
                else:
                    key = hi
                    # skip deletes
                    if node.value:
                        yield (node.key, node.value)
                    break

    # TODO can removes be implicit similarly to deletes?
    # note this causes problems for fetch... though could be
    # worked around by using 0=delete for the fetch-time attrs as
    # special case?
    def remove(self, key):
        # Just use a stub to mark key as "removed", the
        # key may or may not be cleaned up in later appends.
        # Worst case we cleanup removals during compactions.
        #
        # Note the delete algorithm in Dhara could work here, but
        # then we would potentially be copying over a large kv pair
        # when deleting a small pair. Figuring out the tree to
        # append also requires either backtracking or multiple passes,
        # which gets complicated. Oh and we avoid the can't-delete-root
        # issue this way.
        self.append(key, None)

    def create(self, key, value):
        # let append do most of the work
        self.append(key, value, type='create')

    def delete(self, key):
        # let append do most of the work
        self.append(key, None, type='delete')

    def height(self):
        return max(len(node.alts) for node in self.nodes)

    def heights(self):
        for node in self.nodes:
            yield len(node.alts)
        

def main():
    tree = LogTree()
    tree.append(1, 'a')
    tree.append(2, 'b')
    tree.append(3, 'c')
    tree.append(4, 'd')
    tree.create(2, 'b\'')
    tree.delete(3)
    print(tree)
    print('1 = %s' % tree.lookup(1))
    print('2 = %s' % tree.lookup(2))
    print('3 = %s' % tree.lookup(3))
    print('4 = %s' % tree.lookup(4))
    print('5 = %s' % tree.lookup(5))
    print('traverse = %s' % list(tree.traverse()))
    tree.remove(1)
    tree.remove(2)
    tree.remove(3)
    tree.remove(4)
    print(tree)
    print('1 = %s' % tree.lookup(1))
    print('2 = %s' % tree.lookup(2))
    print('3 = %s' % tree.lookup(3))
    print('4 = %s' % tree.lookup(4))
    print('5 = %s' % tree.lookup(5))
    print('traverse = %s' % list(tree.traverse()))

    tree = LogTree()
    tree.append(4, 'd')
    tree.append(3, 'c')
    tree.append(2, 'b')
    tree.append(1, 'a')
    tree.create(2, 'b\'')
    tree.delete(3)
    print(tree)
    print('1 = %s' % tree.lookup(1))
    print('2 = %s' % tree.lookup(2))
    print('3 = %s' % tree.lookup(3))
    print('4 = %s' % tree.lookup(4))
    print('5 = %s' % tree.lookup(5))
    print('traverse = %s' % list(tree.traverse()))
    tree.remove(4)
    tree.remove(3)
    tree.remove(2)
    tree.remove(1)
    print(tree)
    print('1 = %s' % tree.lookup(1))
    print('2 = %s' % tree.lookup(2))
    print('3 = %s' % tree.lookup(3))
    print('4 = %s' % tree.lookup(4))
    print('5 = %s' % tree.lookup(5))
    print('traverse = %s' % list(tree.traverse()))

    tree = LogTree()
    tree.append(3, 'a')
    tree.append(5, 'b')
    tree.append(1, 'c')
    tree.append(7, 'd')
    tree.append(2, 'd')
    print(tree)
    print('3 = ', tree.lookup(3))
    print('traverse = ', list(tree.traverse()))

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
    print('traverse = ', list(tree.traverse()))

    print("testing...")
    maxheight = 0
    for n in [2, 3, 4, 5, 6, 7, 8, 9, 10, 100, 1000]:
        for case in ['appends', 'updates']: #, 'removes', 'creates', 'deletes']:
            for order in ['in_order', 'reversed', 'random']:
                if order == 'in_order':
                    xs = list(range(n))
                    ys = list(range(n))
                    zs = list(range(n))
                elif order == 'reversed':
                    xs = list(reversed(range(n)))
                    ys = list(reversed(range(n)))
                    zs = list(it.repeat(0, n))
                elif order == 'random':
                    xs = list(range(n))
                    random.shuffle(xs)
                    ys = list(range(n))
                    random.shuffle(ys)
                    zs = list(random.randrange(y+1) for y in range(n))

                if case == 'appends':
                    tree = LogTree()
                    baseline = {}
                    for x in xs:
                        # testing appends
                        tree.append(x, repr(x))
                        baseline[x] = repr(x)
                    try:
                        for x in xs:
                            # testing lookups
                            assert tree.lookup(x) == baseline.get(x), (
                                "test %s %s %s FAILED\n"
                                "tree.lookup(%s) => %s\n"
                                "baseline[%s] => %s%s" % (
                                    case, order, n,
                                    x, tree.lookup(x),
                                    x, baseline.get(x),
                                    '\n%s' % tree if n <= 10 else ''))
                        # testing traversal
                        traversal = list(tree.traverse())
                        baseline_traversal = sorted(baseline.items())
                        assert traversal == baseline_traversal, (
                                "test %s %s %s FAILED\n"
                                "tree.traversal() => %s\n"
                                "sorted(baseline) => %s%s" % (
                                    case, order, n,
                                    traversal,
                                    baseline_traversal,
                                    '\n%s' % tree if n <= 10 else ''))
                    except AssertionError as e:
                        print(e)
                        if n < 100:
                            sys.exit(2)
                        else:
                            sys.exit(1)

                elif case == 'updates':
                    tree = LogTree()
                    baseline = {}
                    for x in xs:
                        # testing appends
                        tree.append(x, 'bad')
                        baseline[x] = 'bad'
                    for y in ys:
                        # testing updates
                        tree.append(y, repr(y))
                        baseline[y] = repr(y)
                    for x in xs:
                        # testing lookups
                        assert tree.lookup(x) == baseline.get(x), (
                            "test %s %s %s FAILED\n"
                            "tree.lookup(%s) => %s\n"
                            "baseline[%s] => %s%s" % (
                                case, order, n,
                                x, tree.lookup(x),
                                x, baseline.get(x),
                                '\n%s' % tree if n <= 10 else ''))
                    # testing traversal
                    traversal = list(tree.traverse())
                    baseline_traversal = sorted(baseline.items())
                    assert traversal == baseline_traversal, (
                            "test %s %s %s FAILED\n"
                            "tree.traversal() => %s\n"
                            "sorted(baseline) => %s%s" % (
                                case, order, n,
                                traversal,
                                baseline_traversal,
                                '\n%s' % tree if n <= 10 else ''))

                elif case == 'removes':
                    tree = LogTree()
                    baseline = {}
                    for x in xs:
                        # testing appends
                        tree.append(x, 'bad')
                        baseline[x] = 'bad'
                    for y in ys:
                        # testing removes
                        tree.remove(y)
                        del baseline[y]
                    for x in xs:
                        # testing lookups
                        assert tree.lookup(x) == baseline.get(x), (
                            "test %s %s %s FAILED\n"
                            "tree.lookup(%s) => %s\n"
                            "baseline.get(%s) => %s%s" % (
                                case, order, n,
                                x, tree.lookup(x),
                                x, baseline.get(x),
                                '\n%s' % tree if n <= 10 else ''))
                    # testing traversal
                    traversal = list(tree.traverse())
                    baseline_traversal = sorted(baseline.items())
                    assert traversal == baseline_traversal, (
                            "test %s %s %s FAILED\n"
                            "tree.traversal() => %s\n"
                            "sorted(baseline) => %s%s" % (
                                case, order, n,
                                traversal,
                                baseline_traversal,
                                '\n%s' % tree if n <= 10 else ''))

                elif case == 'creates':
                    tree = LogTree()
                    baseline = {}
                    for z in zs:
                        # testing creates
                        tree.create(z, repr(z))
                        baseline = {(k+1 if k >= z else k): v for k, v in baseline.items()}
                        baseline[z] = repr(z)
                    for x in xs:
                        # testing lookups
                        assert tree.lookup(x) == baseline.get(x), (
                            "test %s %s %s FAILED\n"
                            "tree.lookup(%s) => %s\n"
                            "baseline[%s] => %s%s" % (
                                case, order, n,
                                x, tree.lookup(x),
                                x, baseline.get(x),
                                '\n%s' % tree if n <= 10 else ''))
                    # testing traversal
                    traversal = list(tree.traverse())
                    baseline_traversal = sorted(baseline.items())
                    assert traversal == baseline_traversal, (
                            "test %s %s %s FAILED\n"
                            "tree.traversal() => %s\n"
                            "sorted(baseline) => %s%s" % (
                                case, order, n,
                                traversal,
                                baseline_traversal,
                                '\n%s' % tree if n <= 10 else ''))

                elif case == 'deletes':
                    tree = LogTree()
                    baseline = {}
                    for x in xs:
                        # testing appends
                        tree.append(x, 'bad')
                        baseline[x] = 'bad'
                    for z in reversed(zs):
                        # testing deletes
                        tree.delete(z)
                        baseline = {(k-1 if k > z else k): v for k, v in baseline.items() if k != z}
                    for x in xs:
                        # testing lookups
                        assert tree.lookup(x) == baseline.get(x), (
                            "test %s %s %s FAILED\n"
                            "tree.lookup(%s) => %s\n"
                            "baseline[%s] => %s%s" % (
                                case, order, n,
                                x, tree.lookup(x),
                                x, baseline.get(x),
                                '\n%s' % tree if n <= 10 else ''))
                    # testing traversal
                    traversal = list(tree.traverse())
                    baseline_traversal = sorted(baseline.items())
                    assert traversal == baseline_traversal, (
                            "test %s %s %s FAILED\n"
                            "tree.traversal() => %s\n"
                            "sorted(baseline) => %s%s" % (
                                case, order, n,
                                traversal,
                                baseline_traversal,
                                '\n%s' % tree if n <= 10 else ''))

                maxheight = max(maxheight, tree.height())

    print('max tree height = %s' % maxheight)
    print('tests passed!')


if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
