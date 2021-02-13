#!/usr/bin/env python3

import matplotlib
matplotlib.use('SVG')
import matplotlib.pyplot as plt
import networkx as nx
import string
import random

from logtree import LogTree


def render(tree, output):
    # create graph
    G = nx.DiGraph()
    heights = {}
    column_labels = {}
    node_labels = {}
    #edge_labels = {}

    for i, node in enumerate(tree.nodes):
        #G.add_node(i)
        column_labels[i] = '%s%s: %s' % (
            'c' if node.type == 'create' else
            'd' if node.type == 'delete' else
            '',
            node.key, node.value)
        heights[i] = 0

        for j, alt in enumerate(node.alts):
            heights[i] = max(heights.get(i, 0), j+1)
            G.add_node((i, j))
            if j != 0:
                G.add_edge((i, j-1), (i, j))
            G.add_edge((i, j), (alt.off, alt.skip))
            node_labels[(i, j)] = (
                "%s%s%s\nw%s/%s" % (
                    "<" if alt.lt else "≥",
                    alt.key,
                    '%+d' % alt.delta if alt.delta else '',
                    alt.weight, alt.iweight))

        for k, v in heights.items():
            G.add_node((k, v))
            if v != 0:
                G.add_edge((k, v-1), (k, v))
            node_labels[(k, v)] = column_labels[k]

   #pos = {1: (0, 0), 2: (-1, 0.3), 3: (2, 0.17), 4: (4, 0.255), 5: (5, 0.03)}

    #plt.tight_layout(pad=0)
    plt.figure(figsize=(10 + len(tree.nodes), 7))

    # assign positions
    pos = {node: (2*node[0], heights[node[0]]-node[1]) for node in G.nodes}

    options = {
        "font_size": 12, #36,
        "node_size": 3000,
        "node_color": "white",
        "edgecolors": "black",
        "linewidths": 2,
        "width": 2,
        "with_labels": False,
    }
    nx.draw_networkx(G, pos, **options)
    nx.draw_networkx_labels(G, pos, node_labels)
    #nx.draw_networkx_edge_labels(G, pos, edge_labels)

    #ax = fig.add_axes([0, 0, 1, 1])
    ax = plt.gca()
    ax.margins(0, 0.1)
    ax.set_axis_off()
    plt.subplots_adjust(left=0, right=1, top=1, bottom=0)
    plt.savefig(output, bbox_layout='tight', pad_inches=0)


def main(output, action='append', *xs):
    assert action in ['append', 'create', 'string']

    if action == 'string':
        xs = [(i, ord(x)) for i, x in enumerate(xs[0])]
        random.shuffle(xs)
    elif not xs:
        if action == 'append':
            # good for appends
            xs = [3,8,6,1,7,4,5,2,0,9]
        elif action == 'create':
            # good for creates
            xs = [0,1,1,0,3,2,3,1,0,9]
    else:
        xs = [int(x) for x in xs]

    # create tree
    tree = LogTree()
    for i, x in enumerate(xs):
        if action == 'string':
            tree.append(x[0], chr(x[1]))
        elif action == 'append':
            tree.append(x, string.ascii_lowercase[i%26])
        elif action == 'create':
            tree.create(x, string.ascii_lowercase[i%26])

    render(tree, output)

if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
