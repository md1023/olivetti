#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys
from graph import Graph


def count_distance(code1, code2):
    """
    >>> None == count_distance(int("1000", 2), int("1111", 2))
    True
    >>> 2 == count_distance(int("100", 2), int("111", 2))
    True
    >>> 1 == count_distance(int("101", 2), int("111", 2))
    True
    >>> 0 == count_distance(int("111", 2), int("111", 2))
    True
    """
    count = 0
    xor = code1 ^ code2
    while xor:
        if count >= 2:
            return
        xor &= xor - 1
        count += 1
    return count


class Hamming(Graph):
    def __init__(self, file_name="hamming.txt"):
        nodes = []
        edges = []
        if len(sys.argv) > 1:
            file_name = sys.argv[1]
        with open(file_name) as f:
            for i, l in enumerate(f.readlines()):
                distance = int(l.replace(" ", ""), 2)
                new_node = distance
                for j, node in enumerate(nodes):
                    cost = count_distance(node, new_node)
                    # 22.495 - best run on 10000
                    if cost is None:
                        continue
                    edge = (j, i, cost)
                    edges.append(edge)
                nodes.append(new_node)
                if i > 10000:
                    print "BREAK"
                    break
        print ">", len(nodes), len(edges)
        del nodes

if __name__ == "__main__":

    graph = Hamming("clustering_big.txt")
    # kruskals_mst = graph.kruskals_mst()
    # print "kruskals:", len(graph.nodes) - len(kruskals_mst)
    exit(0)

    graph = Hamming("hamming.txt")
    kruskals_mst = graph.kruskals_mst()
    assert len(graph.nodes) - len(kruskals_mst) == 1
    # print "kruskals:", kruskals_mst, len(graph.nodes) - len(kruskals_mst)

    # print "-" * 80

    graph = Hamming("hamming2.txt")
    kruskals_mst = graph.kruskals_mst()
    assert len(graph.nodes) - len(kruskals_mst) == 3
    # print "kruskals:", kruskals_mst, len(graph.nodes) - len(kruskals_mst)
