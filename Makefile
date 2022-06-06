
RESULTS ?= results
SHELL := /bin/bash


.PHONY: test
test:
	./logtree.py

.PHONY: bench
bench:
	./bench_all.sh $(RESULTS)

.PHONY: graph
graph:
	./graph_compact.py <(cat $(RESULTS)/*.csv) $(RESULTS)/results_compact.svg
	./graph.py <(cat $(RESULTS)/*.csv) $(RESULTS)/results.svg

