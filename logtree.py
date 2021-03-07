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
        def __init__(self, key, value, type=None, alts=[], delta2=0, root=0):
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
        def __init__(self, lt, key, weight, off, skip, delta, random,
                colors=('b','b'), rotates=(False,False), dont=False, iweight=0):
            self.lt = lt
            self.key = key
            self.weight = weight
            self.iweight = iweight
            # in practice, alt.skip + alt.off can be combined into one offset
            self.off = off
            self.skip = skip
            self.delta = delta
            self.random = random
            self.colors = colors
            self.rotates = rotates
            self.dont = dont

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
            return '%s%s%s@%s.%s%s' % (
                '<' if self.lt else '≥',
                self.key,
                '%+d' % self.delta if self.delta else '',
                self.off, self.skip,
                ''.join(self.colors))

        def __repr__(self):
            return 'LogTree.Alt(%s)' % self

    def __init__(self, nodes=[], rotate_pred='crc_key'):
        self.nodes = []
        self.count = 0
        for k, v in nodes:
            self.append(k, v)

        if rotate_pred == None or rotate_pred == False:
            # never rotate
            self.rotate_pred = lambda a, b: False
        elif rotate_pred == True:
            self.rotate_pred = lambda a, b: True
        elif rotate_pred == 'random':
            self.rotate_pred = lambda a, b: random.randint(0, 1)
        elif rotate_pred == 'brev_off':
            self.rotate_pred = lambda a, b: brev(a.off) > brev(b.off)
        elif rotate_pred == 'brev_key':
            self.rotate_pred = lambda a, b: brev(a.key) > brev(b.key)
        elif rotate_pred == 'brev_key_and_count':
            self.rotate_pred = lambda a, b: (
                brev(a.key+self.count) > brev(b.key+self.count))
        elif rotate_pred == 'crc_off':
            self.rotate_pred = lambda a, b: (
                binascii.crc32(struct.pack('<I', a.off))
                > binascii.crc32(struct.pack('<I', b.off)))
        # TODO make sure we crc key after delta, otherwise we end up with
        # collisions in the repeat create(0) case
        elif rotate_pred == 'crc_key':
            self.rotate_pred = lambda a, b: (
                binascii.crc32(struct.pack('<I', a.key))
                > binascii.crc32(struct.pack('<I', b.key)))
        elif rotate_pred == 'crc_key_and_count':
            self.rotate_pred = lambda a, b: (
                binascii.crc32(struct.pack('<I', a.key+self.count))
                > binascii.crc32(struct.pack('<I', b.key+self.count)))
        else:
            self.rotate_pred = rotate_pred

    def clone(self):
        # as a functional structure this is easy!
        clone = LogTree()
        clone.nodes = self.nodes.copy()
        clone.count = self.count
        clone.rotate_pred = self.rotate_pred
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
            alts[a].colors, alts[b].colors = alts[b].colors, alts[a].colors

        # build alts, in theory we only need a bounded number of these
        # in RAM to handle rebalancing
        alts = []
        off = len(self.nodes) - 1
        skip = 0
        lo, hi = float('-inf'), float('inf')
        yellow_edge = None

        while True:
            if hasattr(self, 'iters'):
                # TODO increment iters/iters2 correctly?
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
                        assert alts[-2].colors[0] == 'y'
                        alts[-2].colors = ('b', 'b')
                    off, skip = alts[-1].off, alts[-1].skip
                    alts.pop(-1)
                    log_append('P lo=%s hi=%s' % (lo, hi))

                # TODO are these all exclusive?
                # split?
                if len(alts) >= 2 and alts[-2].colors[0] == 'y':
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
                    alts[-1].colors = ('b','b')
                    if len(alts) >= 2:
                        assert alts[-2].colors[0] == 'b'
                        alts[-2].colors = ('r','b')
                        if len(alts) >= 3 and alts[-3].colors[0] == 'r':
                            alts[-3].colors = ('y', 'b')

                # follow single alt?
                if alts[-1].colors[0] == 'b' and alts[-1].follow(key):
                    # flip
                    off, skip = alts[-1].flip(off, skip)

                # follow red alt?
                if len(alts) >= 2 and alts[-2].follow(key):
                    assert alts[-2].colors[0] != 'b'
                    # flip
                    off, skip = alts[-2].flip(off, skip)
                    # swap
                    swap(alts, -1, -2)
                    log_append('R %s %s' % (alts[-2], alts[-1]))

                # rotate to prepare split?
                if len(alts) >= 3 and alts[-3].colors[0] == 'y':
                    assert alts[-2].colors[0] == 'r'
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

                # track yellow offset in case of split
                if alts[-1].colors[0] == 'y':
                    yellow_edge = (off, skip-1)

                # reduce bounds based on all edges we could have taken
                # TODO but this is real ugly, can we simplify this
                # somehow?
                if alts[-1].colors[0] == 'b':
                    if not alts[-1].lt:
                        lo, hi = lo, min(hi, alts[-1].key)
                    else:
                        lo, hi = max(lo, alts[-1].key), hi

                    if len(alts) >= 2:
                        if not alts[-2].lt:
                            lo, hi = lo, min(hi, alts[-2].key)
                        else:
                            lo, hi = max(lo, alts[-2].key), hi

                        if len(alts) >= 3:
                            if not alts[-3].lt:
                                lo, hi = lo, min(hi, alts[-3].key)
                            else:
                                lo, hi = max(lo, alts[-3].key), hi

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
                            weight=1,
                            off=off,
                            skip=len(node.alts),
                            # TODO remove these two
                            delta=0,
                            random=0,
                            colors=('b','b')))
                    log_append('A %s' % alts[-1])

                    # note, this could be deduplicated with the recoloring
                    # above, but then we couldn't measure the iters/iters2
                    # separation easily

                    # recolor?
                    alts[-1].colors = ('b','b')
                    if len(alts) >= 2:
                        assert alts[-2].colors[0] == 'b'
                        alts[-2].colors = ('r','b')
                        if len(alts) >= 3 and alts[-3].colors[0] == 'r':
                            alts[-3].colors = ('y', 'b')

                    # rotate to prepare split?
                    if len(alts) >= 3 and alts[-3].colors[0] == 'y':
                        assert alts[-2].colors[0] == 'r'
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

                    self.count += 1

                break

        # append
        self.nodes.append(LogTree.Node(key, value, type=type, alts=alts))
        log_append('N %s' % self.nodes[-1])

    def lookup(self, key):
        if not self.nodes:
            return None

        off = len(self.nodes)-1
        skip = 0
        delta = 0
        lo, hi = float('-inf'), float('inf')
        while True:
            if hasattr(self, 'iters'):
                self.iters += 1

            node = self.nodes[off]
            if node.key+delta == key and node.type != 'delete':
                # found key
                return node.value

            for alt in it.islice(node.alts, skip, None):
                if hasattr(self, 'iters2'):
                    self.iters2 += 1

                #print('l %s %s lo=%s hi=%s' % (key, alt, lo, hi))
                if not alt.lt:
                    if key >= alt.key+delta:
                        # TODO can we get rid of this for lookups?
                        #if alt.key+delta < hi:
                        lo = max(lo, alt.key+delta)
                        delta += alt.delta
                        off = alt.off
                        skip = alt.skip
                        break
                    else:
                        #if alt.key+delta < hi:
                        hi = min(hi, alt.key+delta)
                elif alt.lt:
                    if key < alt.key+delta:
                        #if alt.key+delta > lo:
                        hi = min(hi, alt.key+delta)
                        delta += alt.delta
                        off = alt.off
                        skip = alt.skip
                        break
                    else:
                        #if alt.key+delta > lo:
                        lo = max(lo, alt.key+delta)
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
            delta = 0
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
                        if key >= alt.key+delta:
                            #if alt.key+delta < hi:
                            lo = max(lo, alt.key+delta)
                            delta += alt.delta
                            off = alt.off
                            skip = alt.skip
                            break
                        else:
                            #if alt.key+delta < hi:
                            # TODO it's interesting we need this min here
                            hi = min(hi, alt.key+delta)
                    elif alt.lt: # and alt.key+delta > lo:
                        if key < alt.key+delta:
                            #if alt.key+delta > lo:
                            hi = min(hi, alt.key+delta)
                            delta += alt.delta
                            off = alt.off
                            skip = alt.skip
                            break
                        else:
                            #if alt.key+delta > lo:
                            lo = max(lo, alt.key+delta)
                else:
                    key = hi
                    # skip deletes
                    if node.value:
                        yield (node.key+delta, node.value)
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
