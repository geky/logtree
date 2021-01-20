#!/usr/bin/env python3

from logtree import LogTree
import csv
import random
import itertools as it
import sys

def in_order_n(n):
    return range(n)

def reversed_n(n):
    return reversed(range(n))

def random_n(n):
    x = list(range(n))
    random.shuffle(x)
    return x

ORDERS = {
    'in_order': in_order_n,
    'reversed': reversed_n,
    'random':   random_n,
}

def main(case, order, path, N=10000, step=10):
    N = int(N)
    step = int(step)
    with open(path, 'a') as f:
        w = csv.writer(f)
        w.writerow(['case', 'n',
            'max_iters', 'avg_iters',
            'max_iters2', 'avg_iters2',
            'height'])

        for n in range(step, N, step):
            print("running %s + %s for n=%r" % (case, order, n))
            if case == 'appends':
                baseline = {}
                iters = []
                iters2 = []
                tree = LogTree()
                for i in ORDERS[order](n):
                    tree.iters = 0
                    tree.iters2 = 0
                    tree.append(i, repr(i))
                    iters.append(tree.iters)
                    iters2.append(tree.iters2)
                    baseline[i] = repr(i)

                max_iters = max(iters)
                avg_iters = sum(iters)/len(iters)
                max_iters2 = max(iters2)
                avg_iters2 = sum(iters2)/len(iters2)
                height = tree.height()

                for i in ORDERS[order](n):
                    if tree.lookup(i) != baseline.get(i):
                        print('failed %s + %s for n=%r, could not find %r' % (
                            case, order, n, i))
                        sys.exit(1)
            elif case == 'updates':
                baseline = {}
                iters = []
                iters2 = []
                tree = LogTree()
                for i in ORDERS[order](n):
                    tree.append(i, 'bad')
                    baseline[i] = 'bad'

                for i in ORDERS[order](n):
                    tree.iters = 0
                    tree.iters2 = 0
                    tree.append(i, repr(i))
                    iters.append(tree.iters)
                    iters2.append(tree.iters2)
                    baseline[i] = repr(i)

                max_iters = max(iters)
                avg_iters = sum(iters)/len(iters)
                max_iters2 = max(iters2)
                avg_iters2 = sum(iters2)/len(iters2)
                height = tree.height()

                for i in ORDERS[order](n):
                    if tree.lookup(i) != baseline.get(i):
                        print('failed %s + %s for n=%r, could not find %r' % (
                            case, order, n, i))
                        sys.exit(1)
            elif case == 'lookups':
                baseline = {}
                iters = []
                iters2 = []
                tree = LogTree()
                for i in ORDERS[order](n):
                    tree.append(i, repr(i))
                    baseline[i] = repr(i)

                for i in ORDERS[order](n):
                    tree.iters = 0
                    tree.iters2 = 0
                    v = tree.lookup(i)
                    iters.append(tree.iters)
                    iters2.append(tree.iters2)
                    if v != baseline.get(i):
                        print('failed %s + %s for n=%r, could not find %r' % (
                            case, order, n, i))
                        sys.exit(1)

                max_iters = max(iters)
                avg_iters = sum(iters)/len(iters)
                max_iters2 = max(iters2)
                avg_iters2 = sum(iters2)/len(iters2)
                height = tree.height()
            elif case == 'traversal':
                baseline = {}
                iters = []
                iters2 = []
                tree = LogTree()
                for i in ORDERS[order](n):
                    tree.append(i, repr(i))
                    baseline[i] = repr(i)

                tree.iters = 0
                tree.iters2 = 0
                traversal = list(tree.traverse())
                iters.append(tree.iters)
                iters2.append(tree.iters2)

                for k, v in traversal:
                    if v != baseline.get(k):
                        print('failed %s + %s for n=%r, could not find %r' % (
                            case, order, n, k))
                        sys.exit(1)

                max_iters = max(iters)
                avg_iters = sum(iters)/len(iters)
                max_iters2 = max(iters2)
                avg_iters2 = sum(iters2)/len(iters2)
                height = tree.height()
            else:
                print("unknown case %r?" % case)
                sys.exit(1)

            w.writerow([case, order, n,
                max_iters, avg_iters,
                max_iters2, avg_iters2,
                height])
            f.flush()

if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
