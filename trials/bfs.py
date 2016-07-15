#!/usr/bin/env python
# -*- coding: utf-8 -*-

from collections import OrderedDict

graph = OrderedDict([("ROOT", OrderedDict([
    ("A", OrderedDict([
        ("A1", OrderedDict([("A11", False), ("A12", False)])),
        ("A2", False),
        ("A3", OrderedDict([("A31", False)])),
        ("A4", False)])),
    ("B", False),
    ("C", OrderedDict([("C1", False), ("C2", False)]))
]))])


def bfs(graph, start, visited=None):
    if not visited:
        visited = []
    visited.extend(start)
    for v in start:
        if graph[v]:
            bfs(graph[v], graph[v].keys(), visited)
    return visited

print bfs(graph, ["ROOT"])[::-1]
# print bfs(graph["ROOT"], list("ABC"))[::-1]
