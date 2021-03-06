#!/usr/bin/env python3

import string
import random
import itertools as it
import sys

from logtree import LogTree

COLORS = {
    'b': '\033[90mb\033[m',
    'r': '\033[31mr\033[m',
    'y': '\033[33my\033[m'
}

def render(tree):
    # build grid
    bounds = (0, 1)
    grid = {}
    for x, node in enumerate(tree.nodes):
        bounds = (max(bounds[0], x+1), bounds[1])
        grid[(x, 0)] = ('%2s' % node.key)[:2]

        for y, alt in enumerate(reversed(node.alts)):
            bounds = (bounds[0], max(bounds[1], y+1+1))
            grid[(x, y+1)] = ''.join(COLORS[c] for c in reversed(alt.colors))
        
    # print
    for y in reversed(range(bounds[1])):
        for x in range(bounds[0]):
            sys.stdout.write(grid.get((x, y), '  '))
        sys.stdout.write('\n')


def main(*xs):
    if xs in [(), ('append',)]:
        # good for appends
        xs = [3,8,6,1,7,4,5,2,0,9]
    elif xs in [('create',)]:
        # good for creates
        xs = [0,1,1,0,3,2,3,1,0,9]

    # create tree
    tree = LogTree()
    action = 'append'
    alphas = it.cycle(string.ascii_lowercase)
    for x in xs:
        if action != 'string':
            try:
                _ = int(x)
            except ValueError:
                action = x
                continue

        if action == 'string':
            x = list(enumerate(x))
            random.shuffle(x)
            for i, c in x:
                tree.append(i, c)
        elif action == 'append':
            tree.append(int(x), next(alphas))
        elif action == 'create':
            tree.create(int(x), next(alphas))
        elif action == 'lookup':
            tree.lookup(int(x))
        elif action == 'traverse':
            tree.traverse()
        else:
            print('unknown action %r' % action)
            sys.exit(1)

    render(tree)

if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
