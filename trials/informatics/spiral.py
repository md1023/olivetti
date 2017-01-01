#!/usr/bin/env python3
import sys
from collections import namedtuple
from itertools import cycle


# x, y and neighbour on the right
DIRECTIONS = dict(

    # . >
    #   .
    GO_RIGHT=(1, 0, 0, 1),

    #   .
    # . v
    GO_DOWN=(0, 1, -1, 0),

    # .
    # < .
    GO_LEFT=(-1, 0, 0, -1),

    # ^ .
    # .
    GO_UP=(0, -1, 1, 0)
)
DIRECTION_ORDER = ('GO_RIGHT', 'GO_DOWN', 'GO_LEFT', 'GO_UP')

def spiralize(size):

    if not size:
        return []

    if size == 1:
        return [[1]]

    # lengths of steps
    l = lambda size: [i for i in range(size, 0, -2)];

    # path parts
    p = lambda size: [size - 1] + [j-1 for i in l(size) for j in [i,i]][:size-1]

    Point = namedtuple('Point', ['x', 'y', 'value'])
    points = [Point(0, 0, '1'), Point(0, 1, '0')]

    DIRECTION = cycle(DIRECTION_ORDER)
    heading = next(DIRECTION)

    for index, step in enumerate(p(size)):
        dx, dy, nx, ny = DIRECTIONS.get(heading)
        for i in range(step):
            x, y, value = points[-2]
            p1 = Point(x + dx, y + dy, '1')
            points.append(p1)
            p2 = Point(x + dx + nx, y + dy + ny, '0')
            points.append(p2)
        heading = next(DIRECTION)

    m = [['.' for i in range(size)] for j in range(size)]
    for count, p in enumerate(points):
        m[p.y][p.x] = int(p.value)
    return m
