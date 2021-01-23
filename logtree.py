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

    def append(self, key, value, omit=None):
        if not self.nodes:
            self.nodes.append(LogTree.Node(key, value, alts=[]))
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

            # skip omit keys, used for deletes, note requires care
            # to not corrupt tree!
            if altoff == omit:
                #print('omitting', altkey, altoff)
                return

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
        self.nodes.append(LogTree.Node(key, value, alts=alts))

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
        
    def remove2(self, key):
        # TODO this... doesn't work?
#        print('removing', key, self)
#        # Note that here we just copy the sibling of our deleted
#        # node, in practice we probably want to use indirection to
#        # avoid a copy... Maybe? Hmm, maybe both are worth benchmarking
#        # when implemented
#
#        # first find our sibling
#        # TODO can this be deduplicated?
#        node = self.lookup(key, givenode=True)
#        if not node:
#            return False
#
#        sibling = None
#        off = len(self.nodes)-1
#        lo, hi = float('-inf'), float('inf')
#        while True:
#            if hasattr(self, 'iters'):
#                self.iters += 1
#
#            node = self.nodes[off]
#            for altkey, altoff in node.alts:
#                if hasattr(self, 'iters2'):
#                    self.iters2 += 1
#                if altkey > lo and altkey < hi:
#                    if key < altkey:
#                        hi = altkey
#                        if altkey <= node.key:
#                            sibling = (node.key, off)
#                            off = altoff
#                            break
#                        else:
#                            sibling = (altkey, altoff)
#                    elif key >= altkey:
#                        lo = altkey
#                        if altkey > node.key:
#                            sibling = (node.key, off)
#                            off = altoff
#                            break
#                        else:
#                            sibling = (altkey, altoff)
#            else:
#                if node.key != key:
#                    # key not in tree?
#                    return False
#                break
#
#        # can't delete last node in the tree
#        assert sibling
#
#        # copy over sibling, removing our key from the tree
#        # TODO can this be deduplicated more cleanly?
#        #print('removing', key, off, 'via', sibling[0], sibling[1])
#        # TODO can we search directly for sibling?
#        self.append(self.nodes[sibling[1]].key, self.nodes[sibling[1]].value, omit=off)
#        return True
        pass

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
