# -*- coding: utf-8 -*-
import sys
from operator import attrgetter


class Node(object):
    def __init__(self, name):
        self.edges = []
        self.name = name

    def __repr__(self):
        d = "%s has edges %s" % (self.name, self.edges)
        return d


class Edge(object):
    def __init__(self, inbound, outbound, cost):
        inbound.edges.append(self)
        outbound.edges.append(self)
        self.inbound = inbound
        self.outbound = outbound
        self.cost = cost

    def __repr__(self):
        d = "%s" % (self.cost,)
        return d


class Graph(object):
    """
    For #3
    Answer: 2624
    6 7
    1 2 2474
    2 4 -246
    4 3 640
    4 5 2088
    3 6 4586
    6 5 3966
    5 1 -3824
    """
    def __init__(self, file_name="test_graph.txt"):
        self.nodes = []
        self.edges = []
        if len(sys.argv) > 1:
            file_name = sys.argv[1]
        with open(file_name) as f:
            for l in f.readlines():
                node1, node2, cost = [int(s) for s in l.split()]
                if node1 not in [n.name for n in self.nodes]:
                    node1 = Node(node1)
                    self.nodes.append(node1)
                else:
                    node1 = self.nodes[[n.name for n in self.nodes].index(node1)]
                if node2 not in [n.name for n in self.nodes]:
                    node2 = Node(node2)
                    self.nodes.append(node2)
                else:
                    node2 = self.nodes[[n.name for n in self.nodes].index(node2)]
                self.edges.append(Edge(node1, node2, cost))
        print "G:", "\n".join([str(n) for n in self.nodes])
        self.get_mst()

    def prims_mst(self):
        mst_edges = []
        mst_nodes = []
        edges = sorted(self.edges, key=attrgetter("cost"))
        print "X:", "\n".join([str(n) for n in edges])
        first_edge = edges.pop(0)
        mst_edges.append(first_edge)
        mst_nodes.append(first_edge.inbound)
        mst_nodes.append(first_edge.outbound)

        while edges:
            cheap_edges = []
            for n in mst_nodes:
                for e in n.edges:
                    if e.inbound not in mst_nodes:
                        cheap_edges.append(e)
                    if e.outbound not in mst_nodes:
                        cheap_edges.append(e)
            if not cheap_edges:
                break
            cheapest_edge = sorted(cheap_edges, key=attrgetter("cost")).pop(0)
            edge = edges.pop(edges.index(cheapest_edge))
            mst_edges.append(edge)
            if edge.inbound not in mst_nodes:
                mst_nodes.append(edge.inbound)
            if edge.outbound not in mst_nodes:
                mst_nodes.append(edge.outbound)

        print mst_edges, sum([e.cost for e in mst_edges])

    def kruskals_mst(self):
        pass

    def breadth_first_search(self):
        pass

    def depth_first_search(self):
        pass