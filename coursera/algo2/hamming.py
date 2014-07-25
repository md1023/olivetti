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
                new_node = Node(i + 1, distance)
                for node in self.nodes:
                    cost = bin(node.distance ^ new_node.distance).count("1")
                    edge = Edge(node, new_node, cost)
                    self.edges.append(edge)
                self.nodes.append(new_node)
        print self.nodes

Hamming()
