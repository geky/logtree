#!/usr/bin/env python3

import random
import itertools as it
import collections as co
import binascii
import struct
import os

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

        def __str__(self):
            return '%s%s%s@%s.%s%s%s%s' % (
                '<' if self.lt else '≥',
                self.key,
                '%+d' % self.delta if self.delta else '',
                self.off, self.skip,
                ''.join(self.colors),
                ''.join('R' if r==2 else 'r' if r else '.' for r in self.rotates),
                'd' if self.dont else '.')

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
        skipped = 0
        prevwasred = False
        rotates = (False,False)
        rotate = False
        lo, hi = float('-inf'), float('inf')

        # help for swapping alts because we have extra metadata to manage
        def swapalts(a, b):
            alts[a], alts[b] = alts[b], alts[a]
            altoffs[a], altoffs[b] = altoffs[b], altoffs[a]
            altskips[a], altskips[b] = altskips[b], altskips[a]
            altdeltas[a], altdeltas[b] = altdeltas[b], altdeltas[a]
            # TODO get rid or fix weights, they are broken
            #altweights[a], altweights[b] = altweights[b], altweights[a]

        # keep track of past alt to see if we should rotate
        #
        # Note we just access the alt end here, but we would
        # actually need to keep the previous alt in RAM. Annoying
        # but not a deal-breaker.
        def appendalt(alt, off, skip, delta, weight):
#            if alt.lt:
#                assert alt.key <= key, (
#                    "alt.lt pred does not hold %s <= %s, alt.lt = %s" % (
#                        alt.key, key, alt.lt))
#            if not alt.lt:
#                assert alt.key >= key, (
#                    "alt.lt pred does not hold %s >= %s, alt.lt = %s" % (
#                        alt.key, key, alt.lt))

            nonlocal skipped
            #assert skipped in [0, 1], "skipped == %d, %s, %s" % (skipped, self, key)

#            # red-red indicates skipped node
#            # TODO does this work?
#            nonlocal prevwasred
##            if alt.colors[0] == 'r' and prevwasred:
##                alt.colors = ('b', alt.colors[1])
##            if alt.colors[1] == 'r' and prevwasred:
##                alt.colors = (alt.colors[0], 'b')
#            if prevwasred:
#                alt.colors = ('b', 'b')
#            prevwasred = (alt.colors[0] == 'r')

            # recolor?
            assert alt.colors != ('y','r')
            assert alt.colors != ('r','y')
#            if alt.colors == ('y','b'):
#                if len(alts) >= 1:
#                    alts[-1].colors = ('r', alts[-1].colors[1])
##                    # split red-yellow-red?
##                    if alts[-1].colors ==  ('r', 'y'):
##                        alts[-1].colors = ('b', 'b')
            if alt.colors == ('r','r'):
                # recolor
                alt.colors = ('b','b')
                if len(alts) >= 1:
                    alts[-1].colors = ('r', alts[-1].colors[1])
#                    # split red-yellow-red?
#                    if alts[-1].colors ==  ('r', 'y'):
#                        alts[-1].colors = ('b', 'b')


            # rotate?
            # TODO how do we remove outdated branches in yellow nodes?

#            if (len(alts) >= 1 and alt.lt == alts[-1].lt and
#                    # TODO do we really need to check for removes?
#                    value is not None and
#                    False
#                    #alts[-1].random < alt.random
#                    #alts[-1].weight < weight+1
#                    ):
#                #print('R %s %s' % (alts[-1], alt))
#                # TODO check these names
#                # LL and RR rotations
#                alt.off = altoffs[-1]
#                alt.skip = altskips[-1]
#                alt.delta = altdeltas[-1]
#                alt.weight += alts[-1].weight
#                alt.iweight = alts[-1].weight+weight+1
#                alts[-1] = alt
#                #print('R -> %s' % alts[-1])

            if (len(alts) >= 1 and
                    value is not None and
                    alts[-1].colors[0] == 'y' and not (alt.key > lo and alt.key < hi)
                    ):
                log_append('YD %s %s lo=%s hi=%s' % (alts[-1], alt, lo, hi))
                # TODO always black?
                alts[-1].colors = ('b', alts[-1].colors[1])
                log_append('YD -> %s' % (alts[-1]))
            elif (len(alts) >= 1 and
                    value is not None and
                    # TODO reorder to condition is less strict?
                    alts[-1].colors[0] == 'y' and alt.colors[0] == 'r'
                    ):
                log_append('YRR %s %s' % (alts[-1], alt))
                # RR/LL yellow-reds
                assert alts[-1].colors[1] == 'b'
                assert alt.colors[1] == 'b'
                alt.colors = ('b', 'b')

                alt.off = altoffs[-1]
                alt.skip = altskips[-1]
                alt.delta = altdeltas[-1]

                alts.pop(-1)
                altoffs.pop(-1)
                altskips.pop(-1)
                altdeltas.pop(-1)
                
                alts.append(alt)
                altoffs.append(off)
                altskips.append(skip)
                altdeltas.append(delta)

                if len(alts) >= 2:
                    alts[-2].colors = ('r', alts[-1].colors[1])

                log_append('YRR -> %s' % (alts[-1]))

#            elif (len(alts) >= 2 and
#                    value is not None and
#                    not (alt.key > lo and alt.key < hi)
#                    ):
#                log_append('YP %s %s' % (alts[-1], alt))
#                # yellow-red post-split cleanup
#                #assert alts[-1].colors[1] == 'b'
#                #assert alt.colors[1] == 'r', '%s %s' % (tuple([n.key for n in self.nodes]), key)
#                #alt.colors = ('b', 'b')
#                alts[-1].colors = ('b', 'b')
#
##                alts.insert(-1, alt)
##                altoffs.insert(-1, off)
##                altskips.insert(-1, skip)
##                altdeltas.insert(-1, delta)
#
#                log_append('YP -> %s' % (alts[-1]))

            elif (len(alts) >= 1 and
                    value is not None and
                    alts[-1].colors[0] == 'y' and alt.colors[0] == 'b'
                    ):
                log_append('YRL %s %s' % (alts[-1], alt))
                # RL/LR yellow-reds
                assert alts[-1].colors[1] == 'b'
                #assert alt.colors[1] == 'r', '%s %s' % (tuple([n.key for n in self.nodes]), key)
                alt.colors = ('b', 'b')
                alts[-1].colors = ('b', 'b')

                alts.insert(-1, alt)
                altoffs.insert(-1, off)
                altskips.insert(-1, skip)
                altdeltas.insert(-1, delta)

                if len(alts) >= 3:
                    alts[-3].colors = ('r', alts[-1].colors[1])

                log_append('YRL -> %s %s' % (alts[-2], alts[-1]))
            else:
#                else:
#                    # TODO hm, same?
#                    if len(alts) >= 1:
#                        alts[-1].colors = ('r', alts[-1].colors[1])
                log_append('A %s' % alt)
                alt.iweight = weight+1
                alts.append(alt)
                altoffs.append(off)
                altskips.append(skip)
                altdeltas.append(delta)
                altweights.append(weight)

            # TODO deduplicate red-reds in -4..-2 and -3..-1?

            if (len(alts) >= 4 and
                    alts[-4].lt == alts[-3].lt and
                    value is not None and
                    #False
                    #alts[-2].random < alts[-1].random
                    alts[-4].colors[0] == 'r' and alts[-3].colors[0] == 'r'
                    ):
                log_append('RR %s %s %s %s' % (alts[-4], alts[-3], alts[-2], alts[-1]))
                # RR/LL red-reds
                # TODO assert colors[1] is b?
                assert alts[-4].colors[1] == 'b'
                assert alts[-3].colors[1] == 'b'
                alts[-4].colors = ('y', 'b')

#                alts.append(alt)
#                altoffs.append(off)
#                altskips.append(skip)
#                altdeltas.append(delta)

#                alts[-1].off = altoffs[-2]
#                alts[-1].skip = altskips[-2]
#                alts[-1].delta = altdeltas[-2]
#                alts[-1].weight += alts[-2].weight
#                alts[-1].iweight = alts[-2].weight+alts[-1].weight+weight+1
#                alts[-1].colors = ('r', 'r') # LL/RR recolor
#                if not alts[-2].lt:
#                    alts[-1].rotates = (True, alts[-2].rotates[1])
#                else:
#                    alts[-1].rotates = (alts[-2].rotates[0], True)
#                alts.pop(-2)
#                altoffs[-1] = altoffs[-2] ; altoffs.pop(-2)
#                altskips[-1] = altskips[-2] ; altskips.pop(-2)
#                altdeltas[-1] = altdeltas[-2] ; altdeltas.pop(-2)
#                altweights[-1] = altweights[-2] ; altweights.pop(-2)
#                alts.append(alt)
#                altoffs.append(off)
#                altskips.append(skip)
#                altdeltas.append(delta)
#                altweights.append(weight)
                log_append('RR -> %s %s %s %s' % (alts[-4], alts[-3], alts[-2], alts[-1]))
            elif (len(alts) >= 4 and 
                    # TODO make sure we aren't backtracking?
                    alts[-4].lt != alts[-3].lt and alts[-4].lt == alts[-2].lt and
                    # TODO do we really need to check for removes?
                    value is not None and
                    #False
                    #alts[-1].random < alt.random
                    #alts[-2].weight < alts[-1].weight+weight+1
                    alts[-4].colors[0] == 'r' and alts[-3].colors[0] == 'r'
                    ):
                log_append('RLR %s %s %s %s' % (alts[-4], alts[-3], alts[-2], alts[-1]))
                # LRL/RLR red-reds
                assert alts[-4].colors[1] == 'b'
                assert alts[-3].colors[1] == 'b'
                alts[-4].colors = ('y', 'b')
                alts[-2].colors = ('r', 'b')
                alts[-3].colors = ('b', 'b')

                swapalts(-2, -3)

#                alts.insert(-1, alt)
#                altoffs.insert(-1, off)
#                altskips.insert(-1, skip)
#                altdeltas.insert(-1, delta)
#                
#                alt.off = altoffs[-2]
#                alt.skip = altskips[-2]
#                alt.delta = altdeltas[-2]
#                alt.weight += alts[-2].weight
#                alt.iweight = alts[-2].weight+alts[-1].weight+weight+1
#                alts[-1].colors = (alt.colors[0], alts[-1].colors[1])
#                alt.colors = ('r', 'r') # LR/RL recolor
#                if not alts[-2].lt:
#                    alt.rotates = (2, alt.rotates[1])
#                else:
#                    alt.rotates = (alt.rotates[0], 2)
#                alts[-2] = alt
##                alts[-2], alts[-1] = alts[-1], alts[-2]
##                altoffs[-2], altoffs[-1] = altoffs[-1], altoffs[-2]
##                altskips[-2], altskips[-1] = altskips[-1], altskips[-2]
##                altdeltas[-2], altdeltas[-1] = altdeltas[-1], altdeltas[-2]
##                altweights[-2], altweights[-1] = altweights[-1], altweights[-2]
##                alt.off = altoffs[-1]
##                alt.skip = altskips[-1]
##                alt.delta = altdeltas[-1]
##                alt.weight += alts[-1].weight
##                alts[-1] = alt
##                alts.append(alt)
##                altoffs.append(off)
##                altskips.append(skip)
##                altdeltas.append(delta)
##                altweights.append(weight)
                log_append('RLR -> %s %s %s %s' % (alts[-4], alts[-3], alts[-2], alts[-1]))
            elif (len(alts) >= 4 and
                    # TODO make sure we aren't backtracking?
                    alts[-4].lt != alts[-3].lt and alts[-4].lt != alts[-2].lt and
                    # TODO do we really need to check for removes?
                    value is not None and
                    #False
                    #alts[-1].random < alt.random
                    #alts[-2].weight < alts[-1].weight+weight+1
                    alts[-4].colors[0] == 'r' and alts[-3].colors[0] == 'r'
                    ):
                log_append('RLL %s %s %s %s' % (alts[-4], alts[-3], alts[-2], alts[-1]))
                # LRR/RLL red-reds
                assert alts[-4].colors[1] == 'b'
                assert alts[-3].colors[1] == 'b'
                alts[-3].colors = ('y', 'b')
                alts[-2].colors = ('r', 'b')
                alts[-4].colors = ('b', 'b')

                swapalts(-3, -4)
                swapalts(-2, -3)

#                alts[-1], alts[-2] = alts[-2], alts[-1]
#                altoffs[-1], altoffs[-2] = altoffs[-2], altoffs[-1]
#                altskips[-1], altskips[-2] = altskips[-2], altskips[-1]
#                altdeltas[-1], altdeltas[-2] = altdeltas[-2], altdeltas[-1]
#
#                alts.insert(-1, alt)
#                altoffs.insert(-1, off)
#                altskips.insert(-1, skip)
#                altdeltas.insert(-1, delta)
                


#                alt.off = altoffs[-1]
#                alt.skip = altskips[-1]
#                alt.delta = altdeltas[-1]
#                alt.weight += alts[-1].weight
#                alt.iweight = alts[-2].weight+alts[-1].weight+weight+1
#                alts[-2].colors = (alt.colors[0], alts[-2].colors[1])
#                alt.colors = ('r', 'r') # LR/RL recolor
#                if not alts[-1].lt:
#                    alt.rotates = (3, alt.rotates[1])
#                else:
#                    alt.rotates = (alt.rotates[0], 3)
#                alts[-1] = alts[-2]
#                altoffs[-1], altoffs[-2] = altoffs[-2], altoffs[-1]
#                altskips[-1], altskips[-2] = altskips[-2], altskips[-1]
#                altdeltas[-1], altdeltas[-2] = altdeltas[-2], altdeltas[-1]
#                altweights[-1], altweights[-2] = altweights[-2], altweights[-1]
#                alts[-2] = alt
                log_append('RLL -> %s %s %s %s' % (alts[-4], alts[-3], alts[-2], alts[-1]))

            elif (len(alts) >= 3 and
                    alts[-3].lt == alts[-2].lt and
                    value is not None and
                    #False
                    #alts[-2].random < alts[-1].random
                    alts[-3].colors[0] == 'r' and alts[-2].colors[0] == 'r'
                    ):
                log_append('RR %s %s %s' % (alts[-3], alts[-2], alts[-1]))
                # RR/LL red-reds
                # TODO assert colors[1] is b?
                assert alts[-3].colors[1] == 'b'
                assert alts[-2].colors[1] == 'b'
                alts[-3].colors = ('y', 'b')

#                alts.append(alt)
#                altoffs.append(off)
#                altskips.append(skip)
#                altdeltas.append(delta)

#                alts[-1].off = altoffs[-2]
#                alts[-1].skip = altskips[-2]
#                alts[-1].delta = altdeltas[-2]
#                alts[-1].weight += alts[-2].weight
#                alts[-1].iweight = alts[-2].weight+alts[-1].weight+weight+1
#                alts[-1].colors = ('r', 'r') # LL/RR recolor
#                if not alts[-2].lt:
#                    alts[-1].rotates = (True, alts[-2].rotates[1])
#                else:
#                    alts[-1].rotates = (alts[-2].rotates[0], True)
#                alts.pop(-2)
#                altoffs[-1] = altoffs[-2] ; altoffs.pop(-2)
#                altskips[-1] = altskips[-2] ; altskips.pop(-2)
#                altdeltas[-1] = altdeltas[-2] ; altdeltas.pop(-2)
#                altweights[-1] = altweights[-2] ; altweights.pop(-2)
#                alts.append(alt)
#                altoffs.append(off)
#                altskips.append(skip)
#                altdeltas.append(delta)
#                altweights.append(weight)
                log_append('RR -> %s %s %s' % (alts[-3], alts[-2], alts[-1]))
            elif (len(alts) >= 3 and 
                    # TODO make sure we aren't backtracking?
                    alts[-3].lt != alts[-2].lt and alts[-3].lt == alts[-1].lt and
                    # TODO do we really need to check for removes?
                    value is not None and
                    #False
                    #alts[-1].random < alt.random
                    #alts[-2].weight < alts[-1].weight+weight+1
                    alts[-3].colors[0] == 'r' and alts[-2].colors[0] == 'r'
                    ):
                log_append('RLR %s %s %s' % (alts[-3], alts[-2], alts[-1]))
                # LRL/RLR red-reds
                assert alts[-3].colors[1] == 'b'
                assert alts[-2].colors[1] == 'b'
                alts[-3].colors = ('y', 'b')
                alts[-1].colors = ('r', 'b')
                alts[-2].colors = ('b', 'b')

                swapalts(-1, -2)

#                alts.insert(-1, alt)
#                altoffs.insert(-1, off)
#                altskips.insert(-1, skip)
#                altdeltas.insert(-1, delta)
#                
#                alt.off = altoffs[-2]
#                alt.skip = altskips[-2]
#                alt.delta = altdeltas[-2]
#                alt.weight += alts[-2].weight
#                alt.iweight = alts[-2].weight+alts[-1].weight+weight+1
#                alts[-1].colors = (alt.colors[0], alts[-1].colors[1])
#                alt.colors = ('r', 'r') # LR/RL recolor
#                if not alts[-2].lt:
#                    alt.rotates = (2, alt.rotates[1])
#                else:
#                    alt.rotates = (alt.rotates[0], 2)
#                alts[-2] = alt
##                alts[-2], alts[-1] = alts[-1], alts[-2]
##                altoffs[-2], altoffs[-1] = altoffs[-1], altoffs[-2]
##                altskips[-2], altskips[-1] = altskips[-1], altskips[-2]
##                altdeltas[-2], altdeltas[-1] = altdeltas[-1], altdeltas[-2]
##                altweights[-2], altweights[-1] = altweights[-1], altweights[-2]
##                alt.off = altoffs[-1]
##                alt.skip = altskips[-1]
##                alt.delta = altdeltas[-1]
##                alt.weight += alts[-1].weight
##                alts[-1] = alt
##                alts.append(alt)
##                altoffs.append(off)
##                altskips.append(skip)
##                altdeltas.append(delta)
##                altweights.append(weight)
                log_append('RLR -> %s %s %s' % (alts[-3], alts[-2], alts[-1]))
            elif (len(alts) >= 3 and
                    # TODO make sure we aren't backtracking?
                    alts[-3].lt != alts[-2].lt and alts[-3].lt != alts[-1].lt and
                    # TODO do we really need to check for removes?
                    value is not None and
                    #False
                    #alts[-1].random < alt.random
                    #alts[-2].weight < alts[-1].weight+weight+1
                    alts[-3].colors[0] == 'r' and alts[-2].colors[0] == 'r'
                    ):
                log_append('RLL %s %s %s' % (alts[-3], alts[-2], alts[-1]))
                # LRR/RLL red-reds
                assert alts[-3].colors[1] == 'b'
                assert alts[-2].colors[1] == 'b'
                alts[-2].colors = ('y', 'b')
                alts[-1].colors = ('r', 'b')
                alts[-3].colors = ('b', 'b')

                swapalts(-2, -3)
                swapalts(-1, -2)

#                alts[-1], alts[-2] = alts[-2], alts[-1]
#                altoffs[-1], altoffs[-2] = altoffs[-2], altoffs[-1]
#                altskips[-1], altskips[-2] = altskips[-2], altskips[-1]
#                altdeltas[-1], altdeltas[-2] = altdeltas[-2], altdeltas[-1]
#
#                alts.insert(-1, alt)
#                altoffs.insert(-1, off)
#                altskips.insert(-1, skip)
#                altdeltas.insert(-1, delta)
                


#                alt.off = altoffs[-1]
#                alt.skip = altskips[-1]
#                alt.delta = altdeltas[-1]
#                alt.weight += alts[-1].weight
#                alt.iweight = alts[-2].weight+alts[-1].weight+weight+1
#                alts[-2].colors = (alt.colors[0], alts[-2].colors[1])
#                alt.colors = ('r', 'r') # LR/RL recolor
#                if not alts[-1].lt:
#                    alt.rotates = (3, alt.rotates[1])
#                else:
#                    alt.rotates = (alt.rotates[0], 3)
#                alts[-1] = alts[-2]
#                altoffs[-1], altoffs[-2] = altoffs[-2], altoffs[-1]
#                altskips[-1], altskips[-2] = altskips[-2], altskips[-1]
#                altdeltas[-1], altdeltas[-2] = altdeltas[-2], altdeltas[-1]
#                altweights[-1], altweights[-2] = altweights[-2], altweights[-1]
#                alts[-2] = alt
                log_append('RLL -> %s %s %s' % (alts[-3], alts[-2], alts[-1]))
                
#            elif (len(alts) >= 2 and
#                    alts[-1].lt == alt.lt and
#                    value is not None and
#                    alts[-2].colors[0] == 'r' and alts[-1].colors[0] == 'r'
#                    ):
#                 # half LR/RL rotations
#                 # we can't pull off a full LR/RL rotation here, but we can
#                 # at least make progress for next insertion
#                alt.off = altoffs[-1]
#                alt.skip = altskips[-1]
#                alt.delta = altdeltas[-1]
#                alt.weight += alts[-1].weight
#                alt.iweight = alts[-1].weight+weight+1
#                alt.colors = (alt.colors[0], 'r') # half LR/RL recolor
#                alts[-1] = alt
#            else:
##                else:
##                    # TODO hm, same?
##                    if len(alts) >= 1:
##                        alts[-1].colors = ('r', alts[-1].colors[1])
#                log_append('A %s' % alt)
#                alt.iweight = weight+1
#                alts.append(alt)
#                altoffs.append(off)
#                altskips.append(skip)
#                altdeltas.append(delta)
#                altweights.append(weight)

        # not really an "append", we just recolor based on skips
        def appendskip(skipped_color):
            pass
#            if skipped_color == 'b':
#                if len(alts) >= 1 and alts[-1].colors[0] == 'r':
#                    # maybe this happens normally?
#                    alts[-1].colors = ('b', alts[-1].colors[1])
#                elif len(alts) >= 2 and alts[-1].colors == ('b', 'b') and alts[-2].colors[0] == 'r':
#                    # this happens during rotates
#                    alts[-1].colors = ('b', 'r')
#                    alts[-2].colors = ('b', alts[-2].colors[1])
#                else:
#                    #assert len(alts) < 2 or (alts[-1].colors == ('b','b') and alts[-2].colors[0] == 'b')
#                    #print('oh no!')
#                    pass

        # TODO hm, root?
        delta = 0
        splice = +1 if type == 'create' else 0
        dsplice = -1 if type == 'delete' else 0
        off = len(self.nodes)-1
        skip = 0
        while True:
            if hasattr(self, 'iters'):
                self.iters += 1

            # TODO deduplicate
            # skipped node can end up at both alts[-1] and alts[-2]
            # due to rotates
            if (len(alts) >= 2 and # alts[-1].colors[0] == 'y' and
                    not alts[-2].lt and
                    key >= alts[-2].key):
                # follow post-yellow, flip parent (we only went down
                # this path to collect the necessary nodes)
                log_append('retreat')
                off, alts[-2].off = alts[-2].off, off
                skip, alts[-2].skip = alts[-2].skip, skip
                alts[-2].lt = not alts[-2].lt
            if (len(alts) >= 2 and # alts[-1].colors[0] == 'y' and
                    alts[-2].lt and
                    key < alts[-2].key):
                # follow post-yellow, flip parent (we only went down
                # this path to collect the necessary nodes)
                log_append('retreat')
                off, alts[-2].off = alts[-2].off, off
                skip, alts[-2].skip = alts[-2].skip, skip
                alts[-2].lt = not alts[-2].lt
            elif (len(alts) >= 1 and # alts[-1].colors[0] == 'y' and
                    not alts[-1].lt and
                    key >= alts[-1].key):
                # follow post-yellow, flip parent (we only went down
                # this path to collect the necessary nodes)
                log_append('retreat')
                off, alts[-1].off = alts[-1].off, off
                skip, alts[-1].skip = alts[-1].skip, skip
                alts[-1].lt = not alts[-1].lt
            if (len(alts) >= 1 and # alts[-1].colors[0] == 'y' and
                    alts[-1].lt and
                    key < alts[-1].key):
                # follow post-yellow, flip parent (we only went down
                # this path to collect the necessary nodes)
                log_append('retreat')
                off, alts[-1].off = alts[-1].off, off
                skip, alts[-1].skip = alts[-1].skip, skip
                alts[-1].lt = not alts[-1].lt

            node = self.nodes[off]

            for i, alt in it.islice(enumerate(node.alts), skip, None):
                if hasattr(self, 'iters2'):
                    self.iters2 += 1

#                assert (alt.dont, alt.rotates) in {
#                    (False, (False, False)),
#                    (True,  (False, False)),
#                    (2,     (False, False)),
#                    (3,     (False, False)),
#                    (False, (True,  False)),
#                    (False, (False, True )),
#                    (False, (3,  False   )),
#                    (False, (False, 3    )),
#                    (False, (2,     False)),
#                    (False, (False, 2    ))}, "oh no %s %s %s" % (self, key, (alt.dont, alt.rotates))
                if not alt.lt:
                    # need to trim, otherwise height grows indefinitely
                    if key >= alt.key+delta and not alt.colors[0] == 'y':
                        if True: # alt.key+delta < hi and alt.key+delta > lo:
                            appendalt(
                                LogTree.Alt(
                                    lt=True,
                                    key=alt.key+delta,
                                    weight=weight-alt.weight,
                                    off=off,
                                    skip=i+1,
                                    delta=delta,
                                    random=alt.random,
                                    colors=(alt.colors[1],alt.colors[0] if not rotates[0] else 'b'),
                                    rotates=(False,False),
                                    dont=rotates[0]),
                                off, i, delta, weight)
                            weight = alt.weight
                        else:
                            #assert rotate, "%s %s %s" % (self, key, rotate)
                            appendskip(alt.colors[1])
                            # TODO can red here ever happen?
                            if alt.colors[1] == 'b':
                                skipped += 1
#                            print('S %s' % (alt))
#                            # TODO need yello condition or is this always black?
#                            if len(alts) >= 1 and alts[-1].colors[0] == 'y':
#                                alts[-1].colors = ('b', alts[-1].colors[1])
                        rotate = rotates[1] or alt.dont
                        rotates = (alt.rotates[0] or rotates[1]==2, alt.rotates[1] or rotates[0]==2)
                        #print('gt ', rotates, rotate)
                        # TODO it's interesting we need this min here, but
                        # only with LR/RL rotates
                        lo = max(lo, alt.key+delta)
                        delta += alt.delta
                        off = alt.off
                        skip = alt.skip
                        break
                    else:
                        if True: # alt.key+delta < hi and alt.key+delta > lo:
                            appendalt(
                                LogTree.Alt(
                                    lt=False,
                                    key=alt.key+delta+splice+dsplice,
                                    weight=alt.weight,
                                    off=alt.off,
                                    skip=alt.skip,
                                    delta=delta+alt.delta+splice+dsplice,
                                    random=alt.random,
                                    colors=(alt.colors[0],alt.colors[1] if not rotates[1] else 'b'),
                                    rotates=(alt.rotates[0],alt.rotates[1]),
                                    # can rotates[1] ever happen here?
                                    dont=rotates[1] or alt.dont),
                                off, i, delta+splice+dsplice, weight)
                            weight -= alt.weight
                        else:
                            #assert rotate, "%s %s %s" % (self, key, rotate)
                            appendskip(alt.colors[0])
                            #skipped = True
                            if alt.colors[0] == 'b':
                                skipped += 1
#                            print('S %s' % (alt))
#                            # TODO need yello condition or is this always black?
#                            if len(alts) >= 1 and alts[-1].colors[0] == 'y':
#                                alts[-1].colors = ('b', alts[-1].colors[1])
                        rotate = rotates[0]
                        rotates = (False or rotates[1]==2, False or rotates[0]==2)
                        #print('!gt', rotates, rotate)
                        hi = min(hi, alt.key+delta+splice)
                elif alt.lt:
                    if key < alt.key+delta and not alt.colors[0] == 'y':
                        if True: # alt.key+delta > lo and alt.key+delta < hi:
                            appendalt(LogTree.Alt(
                                    lt=False,
                                    key=alt.key+delta+splice+dsplice,
                                    weight=weight-alt.weight,
                                    off=off,
                                    skip=i+1,
                                    delta=delta+splice+dsplice,
                                    random=alt.random,
                                    colors=(alt.colors[1],alt.colors[0] if not rotates[1] else 'b'),
                                    rotates=(False,False),
                                    dont=rotates[1]),
                                off, i, delta+splice+dsplice, weight)
                            weight = alt.weight
                        else:
                            #assert rotate, "%s %s %s" % (self, key, rotate)
                            appendskip(alt.colors[1])
                            if alt.colors[1] == 'b':
                                skipped += 1
                            #skipped = True
#                            print('S %s' % (alt))
#                            # TODO need yello condition or is this always black?
#                            if len(alts) >= 1 and alts[-1].colors[0] == 'y':
#                                alts[-1].colors = ('b', alts[-1].colors[1])
                        rotate = rotates[0] or alt.dont
                        rotates = (alt.rotates[0] or rotates[1]==2, alt.rotates[1] or rotates[0]==2)
                        #print('lt ', rotates, rotate)
                        hi = min(hi, alt.key+delta+splice)
                        delta += alt.delta
                        off = alt.off
                        skip = alt.skip
                        break
                    else:
                        if True: # alt.key+delta > lo and alt.key+delta < hi:
                            appendalt(
                                LogTree.Alt(
                                    lt=True,
                                    key=alt.key+delta,
                                    weight=alt.weight,
                                    off=alt.off,
                                    skip=alt.skip,
                                    delta=delta+alt.delta,
                                    random=alt.random,
                                    colors=(alt.colors[0],alt.colors[1] if not rotates[0] else 'b'),
                                    rotates=(alt.rotates[0],alt.rotates[1]),
                                    # can rotates[0] ever happen here?
                                    dont=rotates[0] or alt.dont),
                                off, i, delta, weight)
                            weight -= alt.weight
                        else:
                            #assert rotate, "%s %s %s" % (self, key, rotate)
                            appendskip(alt.colors[0])
                            if alt.colors[0] == 'b':
                                skipped += 1
                            #skipped = True
#                            print('S %s' % (alt))
#                            # TODO need yello condition or is this always black?
#                            if len(alts) >= 1 and alts[-1].colors[0] == 'y':
#                                alts[-1].colors = ('b', alts[-1].colors[1])
                        rotate = rotates[1]
                        rotates = (False or rotates[1]==2, False or rotates[0]==2)
                        #print('!lt', rotates, rotate)
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
                            random=random.random(),
                            colors=('r','r')),
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
        for case in ['appends']: #, 'updates', 'removes', 'creates', 'deletes']:
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
