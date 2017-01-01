#!/usr/bin/env python3
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


def spiralize(size):

    if not size:
        return []

    if size == 1:
        return [[1]]

    m = [[0 for i in range(size)] for j in range(size)]
    m[0][0] = 1
    m[1][0] = 0

    for x, y, value in move(size):
        m[y][x] = value

    return m


def move(size):
    # lengths of straight moves
    l = lambda size: [i for i in range(size, 0, -2)];

    # snake's path in steps
    p = lambda size: [size - 1] + [j-1 for i in l(size) for j in [i,i]][:size-1]

    direction = cycle(('GO_RIGHT', 'GO_DOWN', 'GO_LEFT', 'GO_UP'))

    x, y = 0, 0

    for steps in p(size):
        # turn right (move clockwise)
        dx, dy, nx, ny = DIRECTIONS.get(next(direction))

        # go straight (zero is snake's shadow)
        for _ in range(steps):
            x += dx
            y += dy
            yield x, y, 1
            yield x + nx, y + ny, 0

