#!/usr/bin/env python3
import sys
from collections import namedtuple
from itertools import cycle

DIRECTIONS = dict(
    # x y 
    GO_UP=(0, -1),
    GO_RIGHT=(1, 0),
    GO_DOWN=(0, 1),
    GO_LEFT=(-1, 0)
)
DIRECTION_NAMES = ('GO_RIGHT', 'GO_DOWN', 'GO_LEFT', 'GO_UP')

def spiralize(size):

    if not size:
        return []

    # lengths of steps
    l = lambda size: [i for i in range(size, 0, -2)];
    p = lambda size: [size - 1] + [j-1 for i in l(size) for j in [i,i]][:size-1]

    Point = namedtuple('Point', ['x', 'y'])
    points = [Point(0, 0)]

    DIRECTION = cycle(DIRECTION_NAMES)
    heading = next(DIRECTION)

    for index, step in enumerate(p(size)):
        dx, dy = DIRECTIONS.get(heading)    
        for i in range(step):
            x, y = points[-1]
            p = Point(x + dx, y + dy)
            points.append(p)
        heading = next(DIRECTION)

    m = [[0 for i in range(size)] for j in range(size)]
    for p in points:
        m[p.y][p.x] = 1

    return m

assert(spiralize(1) == [[1]])
assert(spiralize(2) == [[1,1],
                                  [0,1]])
assert(spiralize(3) == [[1,1,1],
                                  [0,0,1],
                                  [1,1,1]])
assert(spiralize(5) == [[1,1,1,1,1],
                                  [0,0,0,0,1],
                                  [1,1,1,0,1],
                                  [1,0,0,0,1],
                                  [1,1,1,1,1]])
assert(spiralize(8) == [[1,1,1,1,1,1,1,1],
                                  [0,0,0,0,0,0,0,1],
                                  [1,1,1,1,1,1,0,1],
                                  [1,0,0,0,0,1,0,1],
                                  [1,0,1,0,0,1,0,1],
                                  [1,0,1,1,1,1,0,1],
                                  [1,0,0,0,0,0,0,1],
                                  [1,1,1,1,1,1,1,1]])



