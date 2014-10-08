#!/usr/bin/env python
# -*- coding: utf-8 -*-

keys = list('abcdef')
values = [1,2,3,4]

def func1(a, b):
    d = dict.fromkeys(a)
    d.update(dict(zip(a,b)))
    return d

def func2(a, b):
    d = dict(zip(a,b))
    for i in xrange(len(a) - len(b)):
        d[keys[len(b) + i]] = None
    return d

def func3(a, b):
    return dict(map(None, a, b))

print func1(keys, values)
print func2(keys, values)
print func3(keys, values)
