#!/usr/bin/env python3

import random
import itertools as it
import collections as co

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
                '; %s' % ','.join(
                    '%s%s@%s%s' % (
                        '<' if lt else '≥',
                        k, v,
                        '%+d'%d if d else '')
                    for (lt, k, v, d) in self.alts)
                    if self.alts else '',
                '@%d' % off if off is not None else '')

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
        return '[%s]' % ', '.join(
            node.__str__(i) for i, node in enumerate(self.nodes))

    def __repr__(self):
        return 'LogTree%s' % self

    def append(self, key, value, type=None):
        if not self.nodes:
            self.nodes.append(LogTree.Node(key, value, type=type, alts=[]))
            return

        # build alts
        alts = []

        # keep track of past alt to see if we should rotate
        #
        # Note we just access the alt end here, but we would
        # actually need to keep the previous alt in RAM. Annoying
        # but not a deal-breaker.
        prev = (None, None, None, None)
        def appendalt(alt, end=False):
            altlt, altkey, altoff, altdelta = alt

            if altlt:
                assert altkey <= key, (
                    "altlt pred does not hold %s <= %s, altlt = %s" % (
                        altkey, key, altlt))
            if not altlt:
                assert altkey >= key, (
                    "altlt pred does not hold %s >= %s, altlt = %s" % (
                        altkey, key, altlt))

            # rotate?
            nonlocal prev
            prevaltlt, prevaltkey, prevaltoff, prevaltdelta = prev
            if (prevaltkey and (
                    (altlt     and altkey >= prevaltkey) or
                    (not altlt and altkey <  prevaltkey)) and
#                    (key >= altkey and altkey >= prevaltkey) or
#                    (key < altkey and altkey < prevaltkey)) and
                    altoff > prevaltoff and
                    value is not None and
                    self.rotate_pred((altkey, altoff),
                        (prevaltkey, prevaltoff))):
                alts.pop()
                prev = (None, None, None, None)

            alts.append((altlt, altkey, altoff, altdelta))
            prev = (altlt, altkey, altoff, altdelta)

        prevwasdeleted = False
        delta = 0
        splice = +1 if type == 'create' else 0
        dsplice = -1 if type == 'delete' else 0
        off = len(self.nodes)-1
        lo, hi = float('-inf'), float('inf')
        while True:
            if hasattr(self, 'iters'):
                self.iters += 1

            node = self.nodes[off]
#            if node.key+delta == key and type != 'create':
#                if prevwasdeleted:
#                    alts = alts[:-1]
#                # found key
#                break

            for altlt, altkey, altoff, altdelta in node.alts:
                if hasattr(self, 'iters2'):
                    self.iters2 += 1
                #if altkey+delta > lo and altkey+delta+splice < hi:
                if not altlt and altkey+delta+splice < hi:
                    if key >= altkey+delta:
                        appendalt((True, altkey+delta, off, delta))
                        lo = altkey+delta
                        delta += altdelta
                        off = altoff
                        break
                    else:
                        appendalt((False, altkey+delta+splice+dsplice, altoff, delta+altdelta+splice+dsplice))
                        hi = altkey+delta+splice
                elif altlt and altkey+delta > lo:
                    if key < altkey+delta+splice:
                        appendalt((False, altkey+delta+splice+dsplice, off, delta+splice+dsplice))
                        hi = altkey+delta+splice
                        delta += altdelta
                        off = altoff
                        break
                    else:
                        appendalt((True, altkey+delta, altoff, delta+altdelta))
                        lo = altkey+delta


#                    if key >= altkey+delta:
#                        lo = altkey+delta
#                        if not altlt: # altkey > node.key:
#                            appendalt((True, altkey+delta, off, delta))
#                            delta += altdelta
#                            off = altoff
#                            break
#                        else:
#                            appendalt((True, altkey+delta, altoff, delta+altdelta))
#                    elif key < altkey+delta+splice:
#                        hi = altkey+delta+splice
#                        if altlt: # altkey <= node.key:
#                            appendalt((False, altkey+delta+splice+dsplice, off, delta+splice+dsplice))
#                            delta += altdelta
#                            off = altoff
#                            break
#                        else:
#                            appendalt((False, altkey+delta+splice+dsplice, altoff, delta+altdelta+splice+dsplice))
            else:
                if node.key+delta != key or type == 'create':
                    # did not find key, split leaf?
                    appendalt((
                        False if node.key+delta >= key else True,
                        node.key+delta+splice if node.key+delta >= key else key,
                        off,
                        delta+splice if node.key+delta >= key else delta))
                # omit deletes
                if prevwasdeleted:
                    alts = alts[:-1]
                break

            # TODO hm, can we drop deletes over time?
            prevwasdeleted = node.value is None and node.type != 'delete'

        # append
        self.nodes.append(LogTree.Node(key, value, type=type, alts=alts))

    def lookup(self, key):
        if not self.nodes:
            return None

        delta = 0
        off = len(self.nodes)-1
        lo, hi = float('-inf'), float('inf')
        while True:
            if hasattr(self, 'iters'):
                self.iters += 1

            node = self.nodes[off]
            if node.key+delta == key and node.type != 'delete':
                # found key
                return node.value

            for altlt, altkey, altoff, altdelta in node.alts:
                #print('%s? %s@%s %s..%s %+d :: %s%s%+d' % (key, node.key, off, lo, hi, delta, '<' if altlt else '≥', altkey, delta))
                if hasattr(self, 'iters2'):
                    self.iters2 += 1
                #if altkey+delta > lo and altkey+delta < hi:
                if not altlt and altkey+delta < hi:
                    if key >= altkey+delta:
                        lo = altkey+delta
                        delta += altdelta
                        off = altoff
                        break
                    else:
                        hi = altkey+delta
                elif altlt and altkey+delta > lo:
                    if key < altkey+delta:
                        hi = altkey+delta
                        delta += altdelta
                        off = altoff
                        break
                    else:
                        lo = altkey+delta
#
#
#                    if key >= altkey+delta:
#                        lo = altkey+delta
#                        if not altlt: # altkey > node.key:
#                            delta += altdelta
#                            off = altoff
#                            break
#                    elif key < altkey+delta:
#                        hi = altkey+delta
#                        if altlt: # altkey <= node.key:
#                            delta += altdelta
#                            off = altoff
#                            break
            else:
                # did not find key
                return None

    # TODO scrutinize this more?
    # TODO bring up to date with delete impl
    def traverse(self):
        if not self.nodes:
            return

        # traversal is like lookup, but we keep track of
        # hi, and use that for our next lookup, convenient
        # that we already track this
        key = float('-inf')
        while key != float('inf'):
            off = len(self.nodes)-1
            lo, hi = float('-inf'), float('inf')
            delta = 0
            while True:
                if hasattr(self, 'iters'):
                    self.iters += 1

                node = self.nodes[off]
                for altlt, altkey, altoff, altdelta in node.alts:
                    if hasattr(self, 'iters2'):
                        self.iters2 += 1
#                    if altkey+delta > lo and altkey+delta < hi:
#                        if key >= altkey+delta:
#                            lo = altkey+delta
#                            if not altlt: # altkey > node.key:
#                                delta += altdelta
#                                off = altoff
#                                break
#                        elif key < altkey+delta:
#                            hi = altkey+delta
#                            if altlt: # altkey <= node.key:
#                                delta += altdelta
#                                off = altoff
#                                break
                    if not altlt and altkey+delta < hi:
                        if key >= altkey+delta:
                            lo = altkey+delta
                            delta += altdelta
                            off = altoff
                            break
                        else:
                            hi = altkey+delta
                    elif altlt and altkey+delta > lo:
                        if key < altkey+delta:
                            hi = altkey+delta
                            delta += altdelta
                            off = altoff
                            break
                        else:
                            lo = altkey+delta
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
    for n in [2, 3, 4, 10, 100, 1000]:
        for case in ['appends', 'updates', 'removes', 'creates', 'deletes']:
            for order in ['in_order', 'reversed', 'random']:
                if order == 'in_order':
                    xs = list(range(n))
                    ys = xs
                elif order == 'reversed':
                    xs = list(reversed(range(n)))
                    ys = xs
                elif order == 'random':
                    xs = list(range(n))
                    random.shuffle(xs)
                    ys = list(range(n))
                    random.shuffle(ys)

                if case == 'appends':
                    tree = LogTree()
                    baseline = {}
                    for x in xs:
                        # testing appends
                        tree.append(x, repr(x))
                        baseline[x] = repr(x)
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
                    for x in xs:
                        # testing appends
                        tree.append(x, repr(x))
                        baseline[x] = repr(x)
                    for y in ys:
                        # testing updates
                        tree.create(y, '%d\''%y)
                        baseline = {(k+1 if k >= y else k): v for k, v in baseline.items()}
                        baseline[y] = '%d\''%y
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
                    for y in ys:
                        # testing deletes
                        if y in baseline:
                            tree.delete(y)
                            baseline = {(k-1 if k > y else k): v for k, v in baseline.items() if k != y}
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

    print('tests passed!')


if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
