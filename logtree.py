#!/usr/bin/env python3

import random
import itertools as it
import collections as co
import binascii
import struct

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
        def __init__(self, lt, key, weight, off, skip, delta, random, iweight=0):
            self.lt = lt
            self.key = key
            self.weight = weight
            self.iweight = iweight
            # in practice, alt.skip + alt.off can be combined into one offset
            self.off = off
            self.skip = skip
            self.delta = delta
            self.random = random

        def __str__(self):
            return '%s%s%s@%s.%s' % (
                '<' if self.lt else '≥',
                self.key,
                '%+d' % self.delta if self.delta else '',
                self.off, self.skip)

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
            return

        # build alts, in theory we only need a bounded number of these
        # in RAM to rebalance correctly (in theory at least...)
        alts = []
        altoffs = []
        altskips = []
        altdeltas = []
        altweights = []

        weight = self.count
        prevwasdeleted = False

        # keep track of past alt to see if we should rotate
        #
        # Note we just access the alt end here, but we would
        # actually need to keep the previous alt in RAM. Annoying
        # but not a deal-breaker.
        def appendalt(alt, off, skip, delta, weight):
            if alt.lt:
                assert alt.key <= key, (
                    "alt.lt pred does not hold %s <= %s, alt.lt = %s" % (
                        alt.key, key, alt.lt))
            if not alt.lt:
                assert alt.key >= key, (
                    "alt.lt pred does not hold %s >= %s, alt.lt = %s" % (
                        alt.key, key, alt.lt))

            # rotate?
            if (len(alts) >= 1 and alt.lt == alts[-1].lt and
                    # TODO do we really need to check for removes?
                    value is not None and
                    alts[-1].random < alt.random
                    #alts[-1].weight < weight+1
                    ):
                #print('R %s %s' % (alts[-1], alt))
                # TODO check these names
                # LL and RR rotations
                alt.off = altoffs[-1]
                alt.skip = altskips[-1]
                alt.delta = altdeltas[-1]
                alt.weight += alts[-1].weight
                alt.iweight = alts[-1].weight+weight+1
                alts[-1] = alt
                #print('R -> %s' % alts[-1])
            elif (len(alts) >= 2 and 
                    # TODO make sure we aren't backtracking?
                    alts[-2].lt == alt.lt and alts[-2].lt != alts[-1].lt and
                    # TODO do we really need to check for removes?
                    value is not None and
                    alts[-1].random < alt.random
                    #alts[-2].weight < alts[-1].weight+weight+1
                    ):
                #print('R %s %s %s' % (alts[-2], alts[-1], alt))
                # TODO I don't think this is quite the right rotation
                # TODO check these names
                # LR and RL rotations
                alt.off = altoffs[-2]
                alt.skip = altskips[-2]
                alt.delta = altdeltas[-2]
                alt.weight += alts[-2].weight
                alt.iweight = alts[-2].weight+alts[-1].weight+weight+1
                alts[-2] = alt
#                alts[-2], alts[-1] = alts[-1], alts[-2]
#                altoffs[-2], altoffs[-1] = altoffs[-1], altoffs[-2]
#                altskips[-2], altskips[-1] = altskips[-1], altskips[-2]
#                altdeltas[-2], altdeltas[-1] = altdeltas[-1], altdeltas[-2]
#                altweights[-2], altweights[-1] = altweights[-1], altweights[-2]
#                alt.off = altoffs[-1]
#                alt.skip = altskips[-1]
#                alt.delta = altdeltas[-1]
#                alt.weight += alts[-1].weight
#                alts[-1] = alt
#                alts.append(alt)
#                altoffs.append(off)
#                altskips.append(skip)
#                altdeltas.append(delta)
#                altweights.append(weight)
                #print('R -> %s %s' % (alts[-2], alts[-1]))
            else:
                #print('A %s' % alt)
                alt.iweight = weight+1
                alts.append(alt)
                altoffs.append(off)
                altskips.append(skip)
                altdeltas.append(delta)
                altweights.append(weight)

        # TODO hm, root?
        delta = 0
        splice = +1 if type == 'create' else 0
        dsplice = -1 if type == 'delete' else 0
        off = len(self.nodes)-1
        skip = 0
        lo, hi = float('-inf'), float('inf')
        while True:
            if hasattr(self, 'iters'):
                self.iters += 1

            node = self.nodes[off]

            for i, alt in it.islice(enumerate(node.alts), skip, None):
                if hasattr(self, 'iters2'):
                    self.iters2 += 1

                if not alt.lt:
                    # need to trim, otherwise height grows indefinitely
                    if key >= alt.key+delta:
                        if alt.key+delta < hi and alt.key+delta > lo:
                            appendalt(
                                LogTree.Alt(
                                    lt=True,
                                    key=alt.key+delta,
                                    weight=weight-alt.weight,
                                    off=off,
                                    skip=i+1,
                                    delta=delta,
                                    random=alt.random),
                                off, i, delta, weight)
                            weight = alt.weight
                        #else:
                        #    print('s %s %s+%s/[%s %s]' % (alt,alt.key+delta, splice, lo, hi))
                        # TODO it's interesting we need this min here, but
                        # only with LR/RL rotates
                        lo = max(lo, alt.key+delta)
                        delta += alt.delta
                        off = alt.off
                        skip = alt.skip
                        break
                    else:
                        if alt.key+delta < hi and alt.key+delta > lo:
                            appendalt(
                                LogTree.Alt(
                                    lt=False,
                                    key=alt.key+delta+splice+dsplice,
                                    weight=alt.weight,
                                    off=alt.off,
                                    skip=alt.skip,
                                    delta=delta+alt.delta+splice+dsplice,
                                    random=alt.random),
                                off, i, delta+splice+dsplice, weight)
                            weight -= alt.weight
                        hi = min(hi, alt.key+delta+splice)
                elif alt.lt:
                    if key < alt.key+delta:
                        if alt.key+delta > lo and alt.key+delta < hi:
                            appendalt(LogTree.Alt(
                                    lt=False,
                                    key=alt.key+delta+splice+dsplice,
                                    weight=weight-alt.weight,
                                    off=off,
                                    skip=i+1,
                                    delta=delta+splice+dsplice,
                                    random=alt.random),
                                off, i, delta+splice+dsplice, weight)
                            weight = alt.weight
                        hi = min(hi, alt.key+delta+splice)
                        delta += alt.delta
                        off = alt.off
                        skip = alt.skip
                        break
                    else:
                        if alt.key+delta > lo and alt.key+delta < hi:
                            appendalt(
                                LogTree.Alt(
                                    lt=True,
                                    key=alt.key+delta,
                                    weight=alt.weight,
                                    off=alt.off,
                                    skip=alt.skip,
                                    delta=delta+alt.delta,
                                    random=alt.random),
                                off, i, delta, weight)
                            weight -= alt.weight
                        lo = max(lo, alt.key+delta)
            else:
                if node.key+delta != key or type == 'create' or type == 'create2':
                    # did not find key, split leaf?
                    appendalt(
                        LogTree.Alt(
                            lt=False if node.key+delta >= key else True,
                            key=node.key+delta+splice
                                if node.key+delta >= key
                                else key,
                            weight=1,
                            off=off,
                            skip=len(node.alts),
                            delta=delta+splice
                                if node.key+delta >= key
                                else delta,
                            random=random.random()),
                        off, len(node.alts), 0, weight)
                    self.count += 1
                # omit deletes
                if prevwasdeleted:
                    alts = alts[:-1]
                break

            # TODO is there a better way to do this?
            prevwasdeleted = node.value is None and node.type != 'delete'

        # append
        self.nodes.append(LogTree.Node(key, value, type=type, alts=alts))
        #print('N %s' % self.nodes[-1])

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
        for case in ['appends', 'updates', 'removes', 'creates', 'deletes']:
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
                            sys.exit(0)
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
