#!/usr/bin/env python2

import matplotlib
matplotlib.use('SVG')
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import numpy as np
import csv

RESULTS = [
    'max_iters', 'avg_iters',
    'max_iters2', 'avg_iters2',
    'height']

def main(results, output):
    with open(results) as f:
        r = csv.DictReader(f)
        results = list(r)
        matplotlib.rc('font', family='sans-serif', size=11)
        matplotlib.rc('axes', titlesize='medium', labelsize='medium')
        matplotlib.rc('xtick', labelsize='small')
        matplotlib.rc('ytick', labelsize='small')

        np.random.seed(map(ord, "hello"))

        gs = gridspec.GridSpec(nrows=len(RESULTS), ncols=1,
                wspace=0.25, hspace=0.25)

        fig = plt.figure(figsize=(6, len(RESULTS)*3.5))

        for i, field in enumerate(RESULTS):
            ax = fig.add_subplot(gs[i])
            ax.set_title(field)

            ax.plot(
                [int(r['n']) for r in results],
                [float(r[field]) for r in results])

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
