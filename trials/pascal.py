#!/usr/bin/env python

def newRow(prevRow):
    yield 1
    for k in xrange(len(prevRow) - 1):
        yield prevRow[k] + prevRow[k+1]
    yield 1

def triangle(l):
    t = [1]
    yield t
    for _ in xrange(1, l):
        t = list(newRow(t))
        yield t

def pascal(n, k):
    return list(triangle(n))[n-1][k]

print pascal(5, 3)