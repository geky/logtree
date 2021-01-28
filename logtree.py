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
                    '%s%s@%s[%s]%s' % (
                        '<' if lt else '≥',
                        k, o, s,
                        '%+d'%d if d else '')
                    for (lt, k, o, s, d) in self.alts)
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
        #print('append')
        if not self.nodes:
            self.nodes.append(LogTree.Node(key, value, type=type, alts=[]))
            return

        # build alts
        alts = []
        prevwasdeleted = False
        prevwasrotated = False

        # keep track of past alt to see if we should rotate
        #
        # Note we just access the alt end here, but we would
        # actually need to keep the previous alt in RAM. Annoying
        # but not a deal-breaker.
        prev = (None, None, None, None, None)
        prevnot = (None, None)
        prevoff = None
        prevskip = None
        def appendalt(alt, notoff, notskip, off, skip, end=False):
            altlt, altkey, altoff, altskip, altdelta = alt
            #print(altkey)

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
            nonlocal prevnot
            nonlocal prevoff
            nonlocal prevskip
            nonlocal prevwasrotated
            prevaltlt, prevaltkey, prevaltoff, prevaltskip, prevaltdelta = prev
            prevnotoff, prevnotskip = prevnot
            if (prevaltkey and (
                    (altlt     and altkey >= prevaltkey) or
                    (not altlt and altkey <  prevaltkey)) and
#                    (key >= altkey and altkey >= prevaltkey) or
#                    (key < altkey and altkey < prevaltkey)) and
                    altoff > prevaltoff and
                    # TODO is this acceptable?
                    #prevnotoff == altoff and
                    value is not None and
                    self.rotate_pred((altkey, altoff),
                        (prevaltkey, prevaltoff))):
                #print(prevnotoff, prevnotskip, '~', altoff, altskip)
                assert prevnotoff > altoff or (prevnotoff == altoff and prevnotskip < altskip), (
                    "prevnotskip bad?\n"
                    "prevnotoff=%s >= altoff=%s\n"
                    "prevnotskip=%s < altskip=%s\n\n%s\n%s\n%s %s" % (
                    prevnotoff, altoff, prevnotskip, altskip,
                    self.nodes[prevnotoff].__str__(prevnotoff),
                    self.nodes[altoff].__str__(altoff), key, alts))

                #print('rotate %s %s' % (prevaltkey, altkey))
                alts.pop()
#                altoff = prevnotoff            
#                altskip = prevnotskip
#                altskip -= 1
#                while altskip > 0 and self.nodes[altoff].alts[altskip][1] != prevaltkey:
#                    print(self.nodes[altoff], altskip, '?', prevaltkey, altkey)
#                    altskip -= 1
                #altskip = altskip-1
                altskip = 0
                prevwasrotated = True
                # can only go back once with bounded RAM
                prev = (None, None, None, None, None)
                prevnot = (None, None)
            else:
                prevwasrotated = False
                prev = (altlt, altkey, altoff, altskip, altdelta)
                prevnot = (notoff, notskip)

#            if (prevaltkey and (
#                    (altlt     and altkey >= prevaltkey) or
#                    (not altlt and altkey <  prevaltkey)) and
#                    #altoff > prevaltoff and 
#                    off == altoff and off == prevoff and skip == prevskip+1 and
#                    value is not None and
#                    self.rotate_pred((altkey, altoff),
#                        (prevaltkey, prevaltoff))):
#                alts.pop()
#                #altoff = prevnotoff
#                #altskip = prevnotskip
#                #altskip = 0
##                altskip -= 1
##                assert altskip-1 == prevaltskip, (
##                    "not expected skip? %d-1 == %d" % (altskip, prevaltskip))
#                altoff = prevoff
#                altskip = prevskip
#                #print('rotate!')
#                # can only go back once with bounded RAM
#                prev = (None, None, None, None, None)
#                prevnot = (None, None)
#            else:
#                # TODO can we actually do more than one?
#                prev = (altlt, altkey, altoff, altskip, altdelta)
#                prevnot = (notoff, notskip)
#                prevoff = off
#                prevskip = skip
            
            alts.append((altlt, altkey, altoff, altskip, altdelta))

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
            #prev = (None, None, None, None, None)
#            if node.key+delta == key and type != 'create':
#                if prevwasdeleted:
#                    alts = alts[:-1]
#                # found key
#                break

            # in practice, altskip + altoff can be combined into one offset
            for i, (altlt, altkey, altoff, altskip, altdelta) in (
                    enumerate(node.alts)):
                if hasattr(self, 'iters2'):
                    self.iters2 += 1
                #if altkey+delta > lo and altkey+delta+splice < hi:
                if not altlt and altkey+delta+splice < hi:
                    if key >= altkey+delta:
                        appendalt((
                            True,
                            altkey+delta,
                            off,
                            i,
                            delta), None, None, off, i)
                        lo = altkey+delta
                        delta += altdelta
                        off = altoff
                        break
                    else:
                        appendalt((
                            False,
                            altkey+delta+splice+dsplice,
                            altoff,
                            altskip,
                            delta+altdelta+splice+dsplice), off, i, off, i)
                        hi = altkey+delta+splice
                elif altlt and altkey+delta > lo:
                    if key < altkey+delta+splice:
                        appendalt((
                            False,
                            altkey+delta+splice+dsplice,
                            off,
                            i,
                            delta+splice+dsplice), None, None, off, i)
                        hi = altkey+delta+splice
                        delta += altdelta
                        off = altoff
                        break
                    else:
                        appendalt((
                            True,
                            altkey+delta,
                            altoff,
                            altskip,
                            delta+altdelta), off, i, off, i)
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
                        len(node.alts),
                        delta+splice if node.key+delta >= key else delta), None, None, off, len(node.alts))
                # omit deletes
                if prevwasdeleted and not prevwasrotated:
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
        skip = 0
        lo, hi = float('-inf'), float('inf')
        while True:
            if hasattr(self, 'iters'):
                self.iters += 1

            node = self.nodes[off]
            if node.key+delta == key and node.type != 'delete':
                # found key
                return node.value

            for altlt, altkey, altoff, altskip, altdelta in node.alts[skip:]:
#                print(
#                    '%s?\n'
#                    '    %s[%s]\n'
#                    '    %s < (%s%s%+d?) ≤ %s -> %s[%s]' % (
#                        key, node.__str__(off), skip,
#                        lo, '<' if altlt else '≥', altkey, delta, hi,
#                        altoff, altskip))

                #print('%s? %s@%s %s..%s %+d :: %s%s%+d' % (key, node.key, off, lo, hi, delta, '<' if altlt else '≥', altkey, delta))
                if hasattr(self, 'iters2'):
                    self.iters2 += 1
                #if altkey+delta > lo and altkey+delta < hi:
                if not altlt and altkey+delta < hi:
                    if key >= altkey+delta:
                        lo = altkey+delta
                        delta += altdelta
                        off = altoff
                        skip = altskip
                        break
                    else:
                        hi = altkey+delta
                elif altlt  and altkey+delta > lo:
                    if key < altkey+delta:
                        hi = altkey+delta
                        delta += altdelta
                        off = altoff
                        skip = altskip
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
            skip = 0
            lo, hi = float('-inf'), float('inf')
            delta = 0
            while True:
                if hasattr(self, 'iters'):
                    self.iters += 1

                node = self.nodes[off]
                for altlt, altkey, altoff, altskip, altdelta in node.alts: #[skip:]:
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
                            skip = altskip
                            break
                        else:
                            hi = altkey+delta
                    elif altlt and altkey+delta > lo:
                        if key < altkey+delta:
                            hi = altkey+delta
                            delta += altdelta
                            off = altoff
                            skip = altskip
                            break
                        else:
                            lo = altkey+delta
                    #else:
                    #    print('huh %s? %s < %s%+d ≤ %s (%s in %s)' % (key, lo, altkey, delta, hi, skip, len(node.alts)))
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
        for case in ['appends', 'updates', 'removes']: #, 'creates', 'deletes']:
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

    print('tree height = %s' % tree.height())
    print('tests passed!')


if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
