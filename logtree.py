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
                'c' if self.type == 'create' else '',
                self.key, self.value,
                '; %s' % ','.join(
                    '%s%s%s@%s' % (
                        '≥' if k > self.key else '<',
                        k,
                        '%+d'%d if d else '',
                        v)
                    for (k, v, d) in self.alts)
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
        prevaltkey = None
        prevaltoff = None
        def appendalt(alt, end=False):
            altkey, altoff, altdelta = alt

            # rotate?
            nonlocal prevaltkey
            nonlocal prevaltoff
            if (prevaltkey and (
                    (key >= altkey and altkey >= prevaltkey) or
                    (key < altkey and altkey < prevaltkey)) and
                    altoff > prevaltoff and
                    value is not None and
                    self.rotate_pred((altkey, altoff),
                        (prevaltkey, prevaltoff))):
                alts.pop()
                prevaltkey = None
                prevaltoff = None

            alts.append((altkey, altoff, altdelta))
            prevaltkey = altkey
            prevaltoff = altoff

        prevwasdeleted = False
        delta = 0
        splice = +1 if type == 'create' else 0
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

            for altkey, altoff, altdelta in node.alts:
                if hasattr(self, 'iters2'):
                    self.iters2 += 1
                if altkey+delta > lo and altkey+delta+splice < hi:
                    if key >= altkey+delta:
                        lo = altkey+delta
                        if altkey > node.key:
                            appendalt((altkey+delta, off, delta))
                            delta += altdelta
                            off = altoff
                            break
                        else:
                            appendalt((altkey+delta, altoff, delta+altdelta))
                    elif key < altkey+delta+splice:
                        hi = altkey+delta+splice
                        if altkey <= node.key:
                            appendalt((altkey+delta+splice, off, delta+splice))
                            delta += altdelta
                            off = altoff
                            break
                        else:
                            appendalt((altkey+delta+splice, altoff, delta+altdelta+splice))
            else:
                if node.key+delta != key or type == 'create':
                    # did not find key, split leaf?
                    appendalt(
                        (max(node.key+delta, key)+(
                            splice if node.key+delta >= key else 0),
                        off,
                        delta+(splice if node.key+delta >= key else 0)))
                # omit deletes
                if prevwasdeleted:
                    alts = alts[:-1]
                break

            prevwasdeleted = node.value is None

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
            if node.key+delta == key:
                # found key
                return node.value

            for altkey, altoff, altdelta in node.alts:
                if hasattr(self, 'iters2'):
                    self.iters2 += 1
                if altkey+delta > lo and altkey+delta < hi:
                    if key >= altkey+delta:
                        lo = altkey+delta
                        if altkey > node.key:
                            delta += altdelta
                            off = altoff
                            break
                    elif key < altkey+delta:
                        hi = altkey+delta
                        if altkey <= node.key:
                            delta += altdelta
                            off = altoff
                            break
            else:
                # did not find key
                return None

    # TODO scrutinize this more?
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
                for altkey, altoff, altdelta in node.alts:
                    if hasattr(self, 'iters2'):
                        self.iters2 += 1
                    if altkey+delta > lo and altkey+delta < hi:
                        if key >= altkey+delta:
                            lo = altkey+delta
                            if altkey > node.key:
                                delta += altdelta
                                off = altoff
                                break
                        elif key < altkey+delta:
                            hi = altkey+delta
                            if altkey <= node.key:
                                delta += altdelta
                                off = altoff
                                break
                else:
                    key = hi
                    # skip deletes
                    if node.value:
                        yield (node.key+delta, node.value)
                    break

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
        for case in ['appends', 'updates', 'removes', 'creates']:
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

    print('tests passed!')


if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
