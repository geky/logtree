#!/usr/bin/env python3

import matplotlib
matplotlib.use('SVG')
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import numpy as np
import csv

CASES = [
    'appends',
    'lookups',
    'traversal',
    'updates',
    'removes',
    'creates',
    'deletes',
]
ORDERS = [
    'random',
    'in_order',
    'reversed',
    'in_order_then_reversed',
    'reversed_then_in_order',
]
FIELDS = [
    'iters',
    'iters2',
    'height',
]

# seaborn color palette
COLORS = ['#4c72b0', '#dd8452', '#55a868', '#c44e52', '#8172b3', '#937860', '#da8bc3', '#8c8c8c', '#ccb974', '#64b5cd']
#COLORS = [COLORS[0], COLORS[1], COLORS[1], COLORS[2], COLORS[2]]

def main(results, output):
    with open(results) as f:
        r = csv.DictReader(f)
        results = list(r)
        matplotlib.rc('font', family='sans-serif', size=11)
        matplotlib.rc('axes', titlesize='medium', labelsize='medium')
        matplotlib.rc('xtick', labelsize='small')
        matplotlib.rc('ytick', labelsize='small')
        matplotlib.rc('axes', prop_cycle=matplotlib.style
            .library['seaborn-deep']['axes.prop_cycle'])

        np.random.seed(sum(map(ord, "hello")))

        gs = gridspec.GridSpec(nrows=len(FIELDS), ncols=len(CASES),
                wspace=0.25, hspace=0.25)

        fig = plt.figure(figsize=(len(CASES)*6, len(FIELDS)*3.5))

        for x, case in enumerate(CASES):
            for y, field in enumerate(FIELDS):
                ax = fig.add_subplot(gs[y, x])
                if y == 0:
                    ax.text(0.5, 1.125, case, ha='center',
                        transform=ax.transAxes)
                ax.set_title(field)

                for i, order in enumerate(ORDERS):
                    ns = [int(r['n']) for r in results
                        if r['case'] == case and r['order'] == order]
                    avgs = [float(r['avg_'+field]) for r in results
                        if r['case'] == case and r['order'] == order]
#                    mins = [float(r['min_'+field]) for r in results
#                        if r['case'] == case and r['order'] == order]
                    maxs = [float(r['max_'+field]) for r in results
                        if r['case'] == case and r['order'] == order]
#                    ax.fill_between(ns, avgs, maxs, step='mid',
#                        color=COLORS[i], alpha=0.1)
                    #ax.plot(ns, mins, color=COLORS[i], alpha=0.75)
                    ax.plot(ns, maxs, color=COLORS[i], alpha=0.75,
                        label=order + ' (max, avg)')
                    ax.plot(ns, avgs, color=COLORS[i], alpha=0.25)

                #ax.legend(loc='lower right', fontsize='x-small')
                if x == len(CASES)-1:
                    ax.legend(loc='upper left',
                        bbox_to_anchor=(1.025, 1.05))
    #            ax.set_xlabel('n')
                ax.set_ylim(0, None)
                ax.set_xlim(0, None)
        #        ax.set_xticklabels([])
        #        ax.set_yticklabels([])
                ax.spines['right'].set_visible(False)
                ax.spines['top'].set_visible(False)

        fig.tight_layout()
        plt.savefig(output, bbox_inches="tight")

if __name__ == "__main__":
    import sys
    main(*sys.argv[1:])
