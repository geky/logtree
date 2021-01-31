#!/usr/bin/env python2
# coding=utf-8

import matplotlib
matplotlib.use('SVG')
import matplotlib.pyplot as plt
import networkx as nx
import string

def main(input, output):
    with open(input) as f:
        # create graph
        G = nx.DiGraph()
        heights = {}
        column_labels = {}
        node_labels = {}
        #edge_labels = {}
        for line in f:
            line = line.split(',')
            if line[0] == 'node':
                #G.add_node(int(line[1]))
                column_labels[int(line[1])] = '%s: %s' % (line[2], line[3])
                heights[int(line[1])] = 0
            elif line[0] == 'alt':
                heights[int(line[2])] = max(
                    heights.get(int(line[2]), 0),
                    int(line[1])+1)
                G.add_node((int(line[2]), int(line[1])))
                if int(line[1]) != 0:
                    G.add_edge((int(line[2]), int(line[1])-1),
                        (int(line[2]), int(line[1])))
                G.add_edge((int(line[2]), int(line[1])), (int(line[7]), int(line[8])))
                node_labels[(int(line[2]), int(line[1]))] = (
                    "%s%sw%s\n%+d" % (
                        "<" if int(line[3]) else ">=",
                        line[4], line[5], #line[6],
                        int(line[9])))
#                if int(line[7]):
#                    edge_labels[
#                        ((int(line[2]), int(line[1])), (int(line[5]), 0))] = (
#                            "%+d" % int(line[7]))
                    

        for k, v in heights.items():
            G.add_node((k, v))
            if v != 0:
                G.add_edge((k, v-1), (k, v))
            node_labels[(k, v)] = column_labels[k]

#        for i, node in enumerate(tree.nodes):
#            G.add_node(i)
#            for altlt, altkey, altoff, altdelta, _ in node.alts:
#                G.add_edge(i, altoff)

#    G.add_edge(1, 2)
#    G.add_edge(1, 3)
#    G.add_edge(1, 5)
#    G.add_edge(2, 3)
#    G.add_edge(3, 4)
#    G.add_edge(4, 5)

    # explicitly set positions
    #pos = {1: (0, 0), 2: (-1, 0.3), 3: (2, 0.17), 4: (4, 0.255), 5: (5, 0.03)}

    #plt.tight_layout(pad=0)
    plt.figure(figsize=(20,7))
    pos = {node: (2*node[0], heights[node[0]]-node[1]) for node in G.nodes}
        

#    pos = nx.spring_layout(G, k=10, seed=3113794652)
#    for i in xrange(len(pos)):
#        pos[i][0] = i*10
   # print(pos)
    

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

    # Set margins for the axes so that nodes aren't clipped
    #ax = fig.add_axes([0, 0, 1, 1])
    ax = plt.gca()
    ax.margins(0, 0.1)
    ax.set_axis_off()
    plt.subplots_adjust(left=0, right=1, top=1, bottom=0)
    plt.savefig(output, bbox_layout='tight', pad_inches=0)

if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
