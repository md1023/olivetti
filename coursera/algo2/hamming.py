#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys
from graph import Node, Edge, Graph


class Hamming(Graph):
    def __init__(self, file_name="hamming.txt"):
        self.nodes = []
        self.edges = []
        self.clusters = []
        if len(sys.argv) > 1:
            file_name = sys.argv[1]
        with open(file_name) as f:
            for i, l in enumerate(f.readlines()):
                distance = int(l.replace(" ", ""), 2)
                new_node = Node(i + 1, distance)
                for node in self.nodes:
                    cost = bin(node.distance ^ new_node.distance).count("1")
                    if cost >= 3:
                        continue
                    edge = Edge(node, new_node, cost)
                    # self.clusters.append(edge)
                    self.edges.append(edge)
                self.nodes.append(new_node)
        print ">", len(self.nodes), len(self.edges)

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
