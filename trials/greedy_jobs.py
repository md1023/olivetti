# -*- coding: utf-8 -*-
import sys
import unittest
from operator import attrgetter, mul

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
        nodes = []
        edges = []
        if len(sys.argv) > 1:
            file_name = sys.argv[1]
        with open(file_name) as f:
            for l in f.readlines():
                node1, node2, cost = [int(s) for s in l.split()]
                node1 = Node(node1)
                node2 = Node(node2)
                nodes.append(node1)
                nodes.append(node2)
                edges.append(Edge(node1, node2, cost))
        self.nodes = nodes
        self.edges = edges
        # print "G:", "\n".join([str(n) for n in self.nodes])
        self.get_mst()

    def get_mst(self):
        mst_edges = []
        mst_nodes = []
        edges = sorted(self.edges, key=attrgetter("cost"))
        print "X:", "\n".join([str(n) for n in edges])
        first_edge = edges.pop(0)
        mst_edges.append(first_edge)
        mst_nodes.append(first_edge.inbound)
        mst_nodes.append(first_edge.outbound)

        while edges:
            break
            # i = 0
            # new_edge = edges[0]
            # if new_edge.inbound in mst_nodes
        print str(mst_nodes)
            
g = Graph()        

class Job(object):
    def __init__(self, weight, length, deadline=None):
        self.weight = weight
        self.length = length
        if deadline is not None:
            self.deadline = deadline
        self.score1 = float(self.weight) / self.length
        self.score2 = self.weight - self.length

    def __call__(self, start_time):
        completion_time = self.length + start_time
        lateness = completion_time - self.deadline
        if lateness < 0:
            lateness = 0
        d = dict(job = repr(self),
                 start_time = start_time,
                 completion_time = completion_time,
                 lateness = lateness,
                 mulpd = self.length * self.deadline,
                 objective = self.weight * completion_time)
        return d

    def __repr__(self):
        d = "%s %s %s" % (self.weight, self.length, self.deadline)
        # d += "%s %s" (self.score1, self.score2)
        return d

class Executor(object):
    def __init__(self):
        pass

    def __call__(self, *jobs):
        start_time = 0
        infos = []
        objective = 0
        print "\t".join("weight p d w/l start Ct Ct-d, p*d, obj".split())
        for job in jobs:
            info = job(start_time)
            start_time += job.length
            objective += info["objective"]
            infos.append(info)
            print job.weight, "\t", \
                job.length, "\t", \
                job.deadline, "\t", \
                job.score2, "\t", \
                info["start_time"], "\t", \
                info["completion_time"], "\t", \
                info["lateness"], "\t", \
                info["mulpd"], "\t", \
                info["objective"]
        print objective

class TestCases(unittest.TestCase):
    def test_q1andq2(self):
        jobs = []
        file_name = "/tmp/jobs.txt"
        if len(sys.argv) > 1:
            file_name = sys.argv[1]
        with open(file_name) as f:
            for l in f.readlines():
                weight, length = [int(s) for s in l.split()]
                job = Job(weight, length)
                jobs.append(job)
        schedule = sorted(jobs, key=attrgetter("score1", "weight"), reverse=True)
        Executor(*schedule)


def test_p3():
    """
    75 6 5
    50 2 8
    25 5 3
    """
    if True:
        jobs = []
        file_name = "/tmp/jobs.txt"
        if len(sys.argv) > 1:
            file_name = sys.argv[1]
        with open(file_name) as f:
            for l in f.readlines():
                weight, length, deadline = [int(s) for s in l.split()]
                job = Job(weight, length, deadline)
                jobs.append(job)
        e = Executor()
        schedule = sorted(jobs, key=lambda x: mul(*attrgetter("length", "deadline")(x)))
        e(*schedule)
        print "-" * 80
        schedule = sorted(jobs, key=attrgetter("deadline"))
        e(*schedule)
        print "-" * 80
        schedule = sorted(jobs, key=attrgetter("length"))
        e(*schedule)

# test_p3()
