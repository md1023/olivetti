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
                    edge = Edge(node, new_node, cost)
                    self.clusters.append(edge)
                    if cost >= 3:
                        continue
                    self.edges.append(edge)
                self.nodes.append(new_node)
        # k = 3
        # l = len(self.nodes) + 1
        # for i in xrange(len(self.nodes)):
        #     if l - i == k - 1:
        #         break

if __name__ == "__main__":
    graph = Hamming()
    print graph.clusters, graph.edges
    kruskals_mst = graph.kruskals_mst()
    print "kruskals:", len(kruskals_mst)
    # prims_mst = graph.prims_mst()
    # print "prims:", prims_mst
    # print graph.k_clusters(k=5)
