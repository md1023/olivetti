#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys
from graph import Node, Edge
NODES_COUNT = 7
EDGES_COUNT = 6


class Hamming(object):
    def __init__(self, file_name="hamming.txt"):
        self.nodes = []
        self.edges = []
        if len(sys.argv) > 1:
            file_name = sys.argv[1]
        with open(file_name) as f:
            for l in f.readlines():
                distance = int(l.replace(" ", ""), 2)
                self.nodes.append(Node(bin(distance)))
        print self.nodes

Hamming()
