#!/usr/bin/env python2

import matplotlib
matplotlib.use('SVG')
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import numpy as np
import csv

CASES = [
    'appends',
    'updates',
    'lookups',
    'traversal',
    'removes',
]
ORDERS = [
    'in_order',
    'reversed',
    'random',
#    'in_order_then_reversed',
#    'reversed_then_in_order',
]
FIELDS = [
    'max_iters', 'avg_iters',
    'max_iters2', 'avg_iters2',
    'height',
]

def main(results, output):
    with open(results) as f:
        r = csv.DictReader(f)
        results = list(r)
        matplotlib.rc('font', family='sans-serif', size=11)
        matplotlib.rc('axes', titlesize='medium', labelsize='medium')
        matplotlib.rc('xtick', labelsize='small')
        matplotlib.rc('ytick', labelsize='small')

        np.random.seed(map(ord, "hello"))

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

                for order in ORDERS:
                    ax.plot(
                        [int(r['n']) for r in results
                            if r['case'] == case and r['order'] == order],
                        [float(r[field]) for r in results
                            if r['case'] == case and r['order'] == order],
                        label=order, alpha=0.75)

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
