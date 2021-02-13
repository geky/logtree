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
            self.delta2 = delta2
            self.root = root

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
        def __init__(self, lt, key, weight, iweight, off, skip, delta, delta2=0, rotate=False):
            self.lt = lt
            self.key = key
            self.weight = weight
            self.iweight = iweight
            # in practice, alt.skip + alt.off can be combined into one offset
            self.off = off
            self.skip = skip
            self.delta = delta
            self.rotate = rotate
            self.delta2 = delta2

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
            def pred(a, b):
                #print(a.key+self.count, b.key+self.count, self.count, binascii.crc32(struct.pack('<I', a.key+self.count)), binascii.crc32(struct.pack('<I', b.key+self.count)))
                return (
                    binascii.crc32(struct.pack('<I', a.key+self.count))
                    > binascii.crc32(struct.pack('<I', b.key+self.count)))
#            self.rotate_pred = lambda a, b: (
#                binascii.crc32(struct.pack('<I', a.key+self.count))
#                > binascii.crc32(struct.pack('<I', b.key+self.count)))
            self.rotate_pred = pred
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
            self.count += 1
            self.nodes.append(LogTree.Node(key, value, type=type, alts=[], root=key))
            return

        # build alts
        alts = []
        weight = self.count
        range = (0, self.count)
        prevwasdeleted = False
        prevwasrotated = False

        # keep track of past alt to see if we should rotate
        #
        # Note we just access the alt end here, but we would
        # actually need to keep the previous alt in RAM. Annoying
        # but not a deal-breaker.
        nroot = 0
        prevalt = None
        prevnot = (None, None)
        prevoff = None
        prevskip = None
        prevdelta = None
        def appendalt(alt, notoff, notskip, off, skip, delta, weight):
            #print(alt.key)

            # TODO wait we need this?
            # skip if weight == 0?
            # TODO this happens for some reason, is this a calculation mistake?
#            if alt.weight == 0:
#                return

            if alt.lt:
                assert alt.key <= key, (
                    "alt.lt pred does not hold %s <= %s, alt.lt = %s" % (
                        alt.key, key, alt.lt))
            if not alt.lt:
                assert alt.key >= key, (
                    "alt.lt pred does not hold %s >= %s, alt.lt = %s" % (
                        alt.key, key, alt.lt))

            # rotate?
            nonlocal nroot
            nonlocal prevalt
            nonlocal prevnot
            nonlocal prevoff
            nonlocal prevdelta
            nonlocal prevskip
            nonlocal prevwasrotated
            prevnotoff, prevnotskip = prevnot
#            if (prevaltkey and (
#                    (alt.lt     and alt.key >= prevaltkey) or
#                    (not alt.lt and alt.key <  prevaltkey)) and
##                    (key >= alt.key and alt.key >= prevaltkey) or
##                    (key < alt.key and alt.key < prevaltkey)) and
#                    alt.off > prevaltoff and
#                    # TODO is this acceptable?
#                    #prevnotoff == alt.off and
#                    value is not None and
#                    self.rotate_pred((alt.key, alt.off),
#                        (prevaltkey, prevaltoff))):
#                #print(prevnotoff, prevnotskip, '~', alt.off, alt.skip)
#                assert prevnotoff > alt.off or (prevnotoff == alt.off and prevnotskip < alt.skip), (
#                    "prevnotskip bad?\n"
#                    "prevnotoff=%s >= alt.off=%s\n"
#                    "prevnotskip=%s < alt.skip=%s\n\n%s\n%s\n%s %s" % (
#                    prevnotoff, alt.off, prevnotskip, alt.skip,
#                    self.nodes[prevnotoff].__str__(prevnotoff),
#                    self.nodes[alt.off].__str__(alt.off), key, alts))
#
#                #print('rotate %s %s' % (prevaltkey, alt.key))
#                alts.pop()
##                alt.off = prevnotoff            
##                alt.skip = prevnotskip
##                alt.skip -= 1
##                while alt.skip > 0 and self.nodes[alt.off].alts[alt.skip][1] != prevaltkey:
##                    print(self.nodes[alt.off], alt.skip, '?', prevaltkey, alt.key)
##                    alt.skip -= 1
#                #alt.skip = alt.skip-1
#                alt.skip = 0
#                prevwasrotated = True
#                # can only go back once with bounded RAM
#                prev = (None, None, None, None, None)
#                prevnot = (None, None)
#            else:
#                prevwasrotated = False
#                prev = (alt.lt, alt.key, alt.off, alt.skip, alt.delta)
#                prevnot = (notoff, notskip)

            if (prevalt and alt.lt == prevalt.lt and
#(
#                    (alt.lt     and alt.key >= prevalt.key) or
#                    (not alt.lt and alt.key <  prevalt.key)) and
                    #alt.off > prevaltoff and 
                    #off == alt.off and off == prevoff and skip == prevskip+1 and
                    value is not None and
                    # +1 because we assume we're inserting, and +1 because we're
                    # finding the the ceil(weight/2)
                    prevalt.weight <= (weight+1)/2
                    #True
                    #self.rotate_pred(alt, prevalt)
                    ):
                #assert alt.lt == prevalt.lt
                alts.pop()
#                if len(self.nodes) == 6:
#                    print('pop')
#                    if len(self.nodes) == 6:
#                        print(alt, prevskip, skip)
#                alt.off = prevnotoff
#                alt.skip = prevnotskip
                #alt.skip = 0
#                alt.skip -= 1
#                assert alt.skip-1 == prevaltskip, (
#                    "not expected skip? %d-1 == %d" % (alt.skip, prevaltskip))
                #print('rotating %s <-> %s' % (alt.key, prevaltkey))
                alt.off = prevoff
                alt.skip = prevskip
                alt.delta = prevdelta
                alt.weight += prevalt.weight
                #alt.delta = prevalt.delta
                # can only go back once with bounded RAM
                prevalt = None
                prevnot = (None, None)
                #prevwasrotated = True
            else:
                #assert not prevalt or alt.lt != prevalt.lt or value is None or not self.rotate_pred(alt, prevalt)
                # TODO can we actually do more than one?
                prevalt = alt
                prevnot = (notoff, notskip)
                prevoff = off
                prevskip = skip
                prevdelta = delta
                #prevwasrotated = False

#            if len(self.nodes) == 6:
#                print(alt, prevskip, skip)
            if not alts:
                alt.delta2 = (alt.delta2+nroot)-alt.key
                nroot = alt.key
            alts.append(alt)

        # TODO hm, root?
        root = self.nodes[-1].root
        delta = 0
        dkey = 0
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

            #print(skip)
            for i, alt in it.islice(enumerate(node.alts), skip, None):
                if hasattr(self, 'iters2'):
                    self.iters2 += 1
                # TODO WE NEED TO TRIM HERE
                # TODO Otherwise the height grows indefinitely!
                #if alt.key+delta > lo and alt.key+delta+splice < hi:
                if not alt.lt: # and alt.key+delta+splice < hi:
                  if alt.key+delta+splice < hi:
                    ndkey = dkey + alt.delta2
                    if key >= alt.key+delta:
                        appendalt(
                            LogTree.Alt(
                                lt=True,
                                key=alt.key+delta,
                                weight=weight-alt.weight,
                                iweight=weight,
                                off=off,
                                skip=i+1,
                                delta=delta,
                                delta2=alt.key-nroot),
                            None, None, off, i, delta, weight)
                        weight = alt.weight
                        lo = alt.key+delta
                        delta += alt.delta
                        off = alt.off
                        skip = alt.skip
                        dkey = ndkey
                        root = root+alt.delta2
                        break
                    else:
                        appendalt(
                            LogTree.Alt(
                                lt=False,
                                key=alt.key+delta+splice+dsplice,
                                weight=alt.weight,
                                iweight=weight,
                                off=alt.off,
                                skip=alt.skip,
                                delta=delta+alt.delta+splice+dsplice,
                                delta2=alt.key-nroot),
                            off, i, off, i, delta+splice+dsplice, weight)
                        weight -= alt.weight
                        hi = alt.key+delta+splice
                elif alt.lt: # and alt.key+delta > lo:
                  if alt.key+delta > lo:
                    ndkey = dkey - alt.delta2
                    if key < alt.key+delta+splice:
                        appendalt(LogTree.Alt(
                                lt=False,
                                key=alt.key+delta+splice+dsplice,
                                weight=weight-alt.weight,
                                iweight=weight,
                                off=off,
                                skip=i+1,
                                delta=delta+splice+dsplice,
                                delta2=alt.key-nroot),
                            None, None, off, i, delta+splice+dsplice, weight)
                        weight = alt.weight
                        hi = alt.key+delta+splice
                        delta += alt.delta
                        off = alt.off
                        skip = alt.skip
                        dkey = ndkey
                        root = root+alt.delta2
                        break
                    else:
                        appendalt(
                            LogTree.Alt(
                                lt=True,
                                key=alt.key+delta,
                                weight=alt.weight,
                                iweight=weight,
                                off=alt.off,
                                skip=alt.skip,
                                delta=delta+alt.delta,
                                delta2=alt.key-nroot),
                            off, i, off, i, delta, weight)
                        weight -= alt.weight
                        lo = alt.key+delta


#                    if key >= alt.key+delta:
#                        lo = alt.key+delta
#                        if not alt.lt: # alt.key > node.key:
#                            appendalt((True, alt.key+delta, off, delta))
#                            delta += alt.delta
#                            off = alt.off
#                            break
#                        else:
#                            appendalt((True, alt.key+delta, alt.off, delta+alt.delta))
#                    elif key < alt.key+delta+splice:
#                        hi = alt.key+delta+splice
#                        if alt.lt: # alt.key <= node.key:
#                            appendalt((False, alt.key+delta+splice+dsplice, off, delta+splice+dsplice))
#                            delta += alt.delta
#                            off = alt.off
#                            break
#                        else:
#                            appendalt((False, alt.key+delta+splice+dsplice, alt.off, delta+alt.delta+splice+dsplice))
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
                            iweight=weight,
                            off=off,
                            skip=len(node.alts),
                            delta=delta+splice
                                if node.key+delta >= key
                                else delta,
                            delta2=(node.key+delta+splice
                                if node.key+delta >= key else
                                key) - nroot),
                        None, None, off, len(node.alts), 0, weight)
                    self.count += 1
                # omit deletes
                if prevwasdeleted and not prevwasrotated:
                    alts = alts[:-1]
                break

            # TODO hm, can we drop deletes over time?
            # TODO create 0, 1, 2, delete 1, create 2, 3 ends up with
            # extra node?
            prevwasdeleted = node.value is None and node.type != 'delete'
            #prevalt = None

        # append
        self.nodes.append(LogTree.Node(key, value, type=type, alts=alts))

    def lookup(self, key):
        if not self.nodes:
            return None

        delta = 0
        off = len(self.nodes)-1
        skip = 0
        dkey = 0
        lo, hi = float('-inf'), float('inf')
        while True:
            if hasattr(self, 'iters'):
                self.iters += 1

            node = self.nodes[off]
            if node.key+delta == key and node.type != 'delete':
                # found key
                return node.value

            for alt in it.islice(node.alts, skip, None):
#                print(
#                    '%s?\n'
#                    '    %s[%s]\n'
#                    '    %s < (%s%s%+d?) ≤ %s -> %s[%s]' % (
#                        key, node.__str__(off), skip,
#                        lo, '<' if alt.lt else '≥', alt.key, delta, hi,
#                        alt.off, alt.skip))

                #print('%s? %s@%s %s..%s %+d :: %s%s%+d' % (key, node.key, off, lo, hi, delta, '<' if alt.lt else '≥', alt.key, delta))
                if hasattr(self, 'iters2'):
                    self.iters2 += 1
                #if alt.key+delta > lo and alt.key+delta < hi:
                if not alt.lt: # and alt.key+delta < hi:
                    ndkey = dkey + alt.delta2
                    if key >= alt.key+delta:
                        #assert key >= ndkey, "%d >= %d, %d >= %d%+d\n%s" % (key, alt.key+delta, key, dkey, alt.delta2, self)
                        lo = alt.key+delta
                        delta += alt.delta
                        off = alt.off
                        skip = alt.skip
                        dkey = ndkey
                        break
                    else:
                        hi = alt.key+delta
                elif alt.lt: # and alt.key+delta > lo:
                    ndkey = dkey - alt.delta2
                    if key < alt.key+delta:
                        #assert ndkey <= alt.key
                        hi = alt.key+delta
                        delta += alt.delta
                        off = alt.off
                        skip = alt.skip
                        dkey = ndkey
                        break
                    else:
                        lo = alt.key+delta
#
#
#                    if key >= alt.key+delta:
#                        lo = alt.key+delta
#                        if not alt.lt: # alt.key > node.key:
#                            delta += alt.delta
#                            off = alt.off
#                            break
#                    elif key < alt.key+delta:
#                        hi = alt.key+delta
#                        if alt.lt: # alt.key <= node.key:
#                            delta += alt.delta
#                            off = alt.off
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
                for alt in it.islice(node.alts, skip, None):
                    if hasattr(self, 'iters2'):
                        self.iters2 += 1
#                    if alt.key+delta > lo and alt.key+delta < hi:
#                        if key >= alt.key+delta:
#                            lo = alt.key+delta
#                            if not alt.lt: # alt.key > node.key:
#                                delta += alt.delta
#                                off = alt.off
#                                break
#                        elif key < alt.key+delta:
#                            hi = alt.key+delta
#                            if alt.lt: # alt.key <= node.key:
#                                delta += alt.delta
#                                off = alt.off
#                                break
                    if not alt.lt: # and alt.key+delta < hi:
                      if alt.key+delta < hi:
                        if key >= alt.key+delta:
                            lo = alt.key+delta
                            delta += alt.delta
                            off = alt.off
                            skip = alt.skip
                            break
                        else:
                            hi = alt.key+delta
                    elif alt.lt: # and alt.key+delta > lo:
                      if alt.key+delta > lo:
                        if key < alt.key+delta:
                            hi = alt.key+delta
                            delta += alt.delta
                            off = alt.off
                            skip = alt.skip
                            break
                        else:
                            lo = alt.key+delta
                    #else:
                    #    print('huh %s? %s < %s%+d ≤ %s (%s in %s)' % (key, lo, alt.key, delta, hi, skip, len(node.alts)))
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
    for n in [2, 3, 4, 10, 100, 1000]:
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
