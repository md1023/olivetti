#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys
from graph import Node, Edge


class Hamming(object):
    def __init__(self, file_name="hamming.txt"):
        self.nodes = []
        self.edges = []
        if len(sys.argv) > 1:
            file_name = sys.argv[1]
        with open(file_name) as f:
            for i, l in enumerate(f.readlines()):
                distance = int(l.replace(" ", ""), 2)
                self.nodes.append(Node(i + 1, distance))
        for node in self.nodes:
            for other_node in self.nodes:
                if node == other_node:
                    continue
                cost = bin(node.distance ^ other_node.distance).count("1")
                edge = Edge(node, other_node, cost)
                self.edges.append(edge)

        print self.nodes

Hamming()
