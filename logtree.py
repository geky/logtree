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

        def __str__(self):
            return '%s(%r: %r%s)' % (
                self.type.title() if self.type else '',
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
        def appendalt(alt):
            altkey, altoff = alt

            # rotate?
            nonlocal prevaltkey
            nonlocal prevaltoff
            if (prevaltkey and (
                    (key >= altkey and altkey >= alts[-1][0]) or
                    (key < altkey and altkey < alts[-1][0])) and
                    altoff > prevaltoff and
                    self.rotate_pred((altkey, altoff),
                        (prevaltkey, prevaltoff))):
                alts.pop()
                prevaltkey = None
                prevaltoff = None

            alts.append((altkey, altoff))
            prevaltkey = altkey
            prevaltoff = altoff

        prevwasdeleted = False
        off = len(self.nodes)-1
        lo, hi = float('-inf'), float('inf')
        while True:
            if hasattr(self, 'iters'):
                self.iters += 1

            node = self.nodes[off]
            # TODO wait, should we continue to grab offs here??
            if node.key == key:
                if prevwasdeleted:
                    alts = alts[:-1]
                # found key
                break

            for altkey, altoff in node.alts:
                if hasattr(self, 'iters2'):
                    self.iters2 += 1
                if altkey > lo and altkey < hi:
                    if key < altkey:
                        hi = altkey
                        if altkey <= node.key:
                            # omit deletes
                            # TODO ugh, this doesn't work because we don't
                            # adjust our hi/lo range correctly...
#                            if not (node.value is None and
#                                    altkey == node.alts[-1][0]):
                            appendalt((altkey, off))
                            off = altoff
                            break
                        else:
                            appendalt((altkey, altoff))
                    elif key >= altkey:
                        lo = altkey
                        if altkey > node.key:
                            # omit deletes
#                            if not (node.value is None and
#                                    altkey == node.alts[-1][0]):
                            appendalt((altkey, off))
                            off = altoff
                            break
                        else:
                            appendalt((altkey, altoff))
            else:
                # did not find key, split leaf?
#                # omit deletes
#                #if node.key != key and not node.value is None:
                appendalt((max(node.key, key), off))
                # omit deletion?
                if prevwasdeleted:
                    alts = alts[:-1]
                break

            prevwasdeleted = node.value is None

        # append
        self.nodes.append(LogTree.Node(key, value, type=type, alts=alts))

    def lookup(self, key, givenode=False):
        if not self.nodes:
            return None

        off = len(self.nodes)-1
        lo, hi = float('-inf'), float('inf')
        while True:
            if hasattr(self, 'iters'):
                self.iters += 1

            node = self.nodes[off]
            if node.key == key:
                # found key
                if givenode:
                    return node
                else:
                    return node.value

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
                # did not find key
                return None

    # TODO scrutinize this more?
    def traverse(self):
        if not self.nodes:
            return

        # traversal is like lookup, but we keep track of
        # hi, and use that for our next lookup, convenient
        # that we already track this
        prev = float('-inf')
        while prev != float('inf'):
            off = len(self.nodes)-1
            lo, hi = float('-inf'), float('inf')
            while True:
                if hasattr(self, 'iters'):
                    self.iters += 1

                node = self.nodes[off]
                for altkey, altoff in node.alts:
                    if hasattr(self, 'iters2'):
                        self.iters2 += 1
                    if altkey > lo and altkey < hi:
                        if prev < altkey:
                            hi = altkey
                            if altkey <= node.key:
                                off = altoff
                                break
                        elif prev >= altkey:
                            lo = altkey
                            if altkey > node.key:
                                off = altoff
                                break
                else:
                    prev = hi
                    # skip deletes # TODO can this be better?
                    if node.value:
                        yield (node.key, node.value)
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
    print(tree)
    print('1 = ', tree.lookup(1))
    print('2 = ', tree.lookup(2))
    print('3 = ', tree.lookup(3))
    print('4 = ', tree.lookup(4))
    print('traverse = ', list(tree.traverse()))
    tree.remove(1)
    tree.remove(2)
    tree.remove(3)
    tree.remove(4)
    print(tree)
    print('1 = ', tree.lookup(1))
    print('2 = ', tree.lookup(2))
    print('3 = ', tree.lookup(3))
    print('4 = ', tree.lookup(4))
    print('traverse = ', list(tree.traverse()))

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
    print('traverse = ', list(tree.traverse()))
    tree.remove(4)
    tree.remove(3)
    tree.remove(2)
    tree.remove(1)
    print(tree)
    print('1 = ', tree.lookup(1))
    print('2 = ', tree.lookup(2))
    print('3 = ', tree.lookup(3))
    print('4 = ', tree.lookup(4))
    print('traverse = ', list(tree.traverse()))

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
    for n in [10, 100, 1000]:
        for case in ['appends', 'updates', 'removes']:
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
                            "baseline[%s] => %s" % (
                                case, order, n,
                                x, tree.lookup(x),
                                x, baseline.get(x)))
                    # testing traversal
                    traversal = list(tree.traverse())
                    baseline_traversal = sorted(baseline.items())
                    assert traversal == baseline_traversal, (
                            "test %s %s %s FAILED\n"
                            "tree.traversal() => %s\n"
                            "sorted(baseline) => %s" % (
                                case, order, n,
                                traversal,
                                baseline_traversal))

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
                            "baseline[%s] => %s" % (
                                case, order, n,
                                x, tree.lookup(x),
                                x, baseline.get(x)))
                    # testing traversal
                    traversal = list(tree.traverse())
                    baseline_traversal = sorted(baseline.items())
                    assert traversal == baseline_traversal, (
                            "test %s %s %s FAILED\n"
                            "tree.traversal() => %s\n"
                            "sorted(baseline) => %s" % (
                                case, order, n,
                                traversal,
                                baseline_traversal))

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
                            "baseline.get(%s) => %s" % (
                                case, order, n,
                                x, tree.lookup(x),
                                x, baseline.get(x)))
                    # testing traversal
                    traversal = list(tree.traverse())
                    baseline_traversal = sorted(baseline.items())
                    assert traversal == baseline_traversal, (
                            "test %s %s %s FAILED\n"
                            "tree.traversal() => %s\n"
                            "sorted(baseline) => %s" % (
                                case, order, n,
                                traversal,
                                baseline_traversal))

    print('tests passed!')

if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
