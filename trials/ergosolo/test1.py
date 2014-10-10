#!/usr/bin/env python
# -*- coding: utf-8 -*-

keys = list('abcdef')
values = range(len(keys))

def func_fromkeys(a, b):
    # лучший, когда ключей больше
    d = dict.fromkeys(a)
    d.update(dict(zip(a,b)))
    return d

def func_map(a, b):
    # лучший когда объём мал
    return dict(map(None, a, b))

def func_xrange(a, b):
    # лучший, когда ключей не больше
    d = dict(zip(a,b))
    for i in xrange(len(a) - len(b)):
        d[keys[len(b) + i]] = None
    return d

print func_fromkeys(keys, values)
print func_map(keys, values)
print func_xrange(keys, values)

# проверка:
# python -m cProfile -s cumulative test1.py | grep func_
