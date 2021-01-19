#!/usr/bin/env python3

from logtree import LogTree
import csv
import random
import itertools as it
import sys

def in_order_n(n):
    return range(n)

def reverse_order_n(n):
    return reversed(range(n))

def random_n(n):
    x = list(range(n))
    random.shuffle(x)
    return x

INPUTS = {
    'in_order_appends':         in_order_n,
    'reverse_order_appends':    reverse_order_n,
    'random_appends':           random_n,
}

def main(case, path, N=100000, step=100):
    N = int(N)
    step = int(step)
    with open(path, 'a') as f:
        w = csv.writer(f)
        w.writerow(['case', 'n',
            'max_iters', 'avg_iters',
            'max_iters2', 'avg_iters2',
            'height'])

        for n in range(step, N, step):
            print("running %r for n=%r" % (case, n))
            if case in ['in_order_appends',
                    'reverse_order_appends',
                    'random_appends']:
                baseline = {}
                tree = LogTree()
                iters = []
                iters2 = []
                for i in INPUTS[case](n):
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

                for i in INPUTS[case](n):
                    if tree.lookup(i) != baseline.get(i):
                        print('failed %r for n=%r, could not find %r' % (
                            case, n, i))
                        sys.exit(1)

                w.writerow([case, n,
                    max_iters, avg_iters,
                    max_iters2, avg_iters2,
                    height])
                f.flush()
            else:
                print("unknown case %r?" % case)
                sys.exit(1)

if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
