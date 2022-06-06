#!/usr/bin/env python3

# exhaustively search for smallest bugged tree

import itertools as it
import sys
import os
import time
import threading
import queue

from logtree import LogTree

RENDER = os.environ.get('RENDER', None)
if RENDER:
    from trender import tgrid, tprint
    render_thread = None
    render_timeout = float(RENDER)
    render_queue = queue.Queue()
    render_run = True
    last_tree = None

    def render_run():
        render_tallest = 0
        while render_run:
            if last_tree is not None:
                (w, h), grid = tgrid(last_tree)

                if render_tallest:
                    sys.stdout.write('\x1b[%dA' % render_tallest)

                try:
                    while True:
                        msg = render_queue.get(False)
                        print(msg)
                except queue.Empty:
                    pass

                render_tallest = max(h, render_tallest)
                for i in range(render_tallest):
                    sys.stdout.write('\x1b[K\n')

                sys.stdout.write('\x1b[%dA' % h)
                tprint((w, h), grid)

            time.sleep(render_timeout)

    def render_kill():
        global render_run, render_thread
        if render_thread is not None:
            render_run = False
            render_thread.join()
            render_thread = None

def main(N=None):
    if RENDER:
        global render_thread
        render_thread = threading.Thread(target=render_run)
        render_thread.start()

    worst_height = 0
    worst_perm = ()
    try:
        for n in it.islice(it.count(1), N):
            for perm in it.permutations(range(n)):
                try:
                    # build tree
                    tree = LogTree()
                    for x in perm:
                        tree.append(x, 'n%d'%x)

                    if RENDER:
                        global last_tree
                        last_tree = tree

                    # test that lookups work on tree
                    for x in range(n):
                        q = tree.lookup(x)
                        if q != 'n%d'%x:
                            if RENDER:
                                # give time for tree to render
                                time.sleep(render_timeout)
                                render_kill()
                            print('found bad tree')
                            print('lookup(%s) => %s' % (x, q))
                            print('should be => %s' % ('n%d'%x))
                            print('perm %s' % (perm,))
                            sys.exit(1)

                    # worst tree we've seen?
                    if tree.height() > worst_height:
                        worst_height = tree.height()
                        worst_perm = perm
                except AssertionError:
                    if RENDER:
                        # give time for tree to render
                        time.sleep(render_timeout)
                        render_kill()
                    print('assertion failed on perm %s' % (perm,))
                    print()
                    raise

            if not RENDER:
                print('searched all trees of size %s, '
                    'worst height %d' % (n, worst_height))
                print('worst perm %s' % (worst_perm,))
            else:
                render_queue.put('searched all trees of size %s, '
                    'worst height %d' % (n, worst_height))
                render_queue.put('worst perm %s' % (worst_perm,))

        print('done')

    except KeyboardInterrupt:
        if RENDER:
            render_kill()
        print('terminated, worst height %d' % worst_height)
        print('worst perm %s' % (worst_perm,))

if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
