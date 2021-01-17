#!/usr/bin/env python3


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

    def __init__(self, nodes=[]):
        self.nodes = []
        for k, v in nodes:
            self.append(k, v)

    def __str__(self):
        return '[%s]' % ', '.join(str(node) for node in self.nodes)

    def __repr__(self):
        return 'LogTree%s' % self

    def append(self, key, value):
        # build alts
        alts = []
        off = len(self.nodes)-1
        lo, hi = float('-inf'), float('inf')

        while off >= 0:
            node = self.nodes[off]
            # found key?
            # TODO can we base this off lo/hi?
            if node.key == key:
                break

            # build new alt-pointers
            noff = -1
            nkey = self.nodes[off].key
            for altkey, altoff in node.alts:
                if altkey > lo and altkey < hi:
#                    if altkey < node.key and key < node.key:
#                        hi = altkey
#                        nkey = altkey+1 # TODO can do better than +1?
#                        noff = altoff
#                        break
#                    elif altkey > node.key and key > node.key:
#                        lo = altkey
#                        nkey = altkey-1
#                        noff = altoff
#                        break
#                    else:
#                        # TODO hmm, more hi/lo bounding?
#                        alts.append((altkey, altoff))

                    if altkey < node.key and key <= altkey:
                        hi = altkey
                        nkey = altkey+1 # TODO can do better than +1?
                        noff = altoff
                        break
                    elif altkey > node.key and key >= altkey:
                        lo = altkey
                        nkey = altkey-1
                        noff = altoff
                        break
                    else:
                        # TODO hmm, more hi/lo bounding?
                        alts.append((altkey, altoff))

            alts.append((nkey, off))
            off = noff

        # append
        self.nodes.append(LogTree.Node(key, value, alts=alts))

    def lookup(self, key):
        off = len(self.nodes)-1
        lo, hi = float('-inf'), float('inf')

        while off >= 0:
            node = self.nodes[off]
            # found key?
            # TODO can we base this off lo/hi?
            if node.key == key:
                return node.value

            # build new alt-pointers
            for altkey, altoff in node.alts:
                if altkey > lo and altkey < hi:
                    if key == altkey:
                        off = altoff
                        break
                    elif altkey < node.key and key < altkey:
                        hi = altkey
                        off = altoff
                        break
                    elif altkey > node.key and key > altkey:
                        lo = altkey
                        off = altoff
                        break
            else:
                return None

def main():
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


if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
