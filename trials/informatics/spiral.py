#!/usr/bin/env python3
from itertools import cycle


class Snake:
    def __init__(self, dimension, PathType):
        self.dimension = dimension
        self.path = PathType(dimension.size)
        self.body = 1
        self.shadow = 0

    def move(self):
        direction = cycle(self.path.directions)

        for steps in self.path.traverse():
            self.direction = next(direction)
            for _ in range(steps):
                yield from self.dimension.step(self)


class SpiralPath:
    directions = ('GO_RIGHT', 'GO_DOWN', 'GO_LEFT', 'GO_UP')
    def __init__(self, size):
        # lengths of straight moves
        l = lambda size: [i for i in range(size, 0, -2)];
        # spiral path in steps
        self._path = [size - 1] + [j-1 for i in l(size) for j in [i,i]][:size-1]

    def traverse(self):
        yield from iter(self._path)


class Board(object):
    # x, y and neighbour on the right
    directions = dict(

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

    def __init__(self, size):
        self.size = size

    def step(self, figure):
        new_x, new_y, shadow_x, shadow_y = self.directions.get(figure.direction)
        figure.x += new_x
        figure.y += new_y
        yield figure.x, figure.y, figure.body
        yield figure.x + shadow_x, figure.y + shadow_y, figure.shadow

    def draw(self, Figure, PathType):

        if not self.size:
            return []

        if self.size == 1:
            return [[1]]

        figure = Figure(self, PathType)
        figure.x, figure.y = 0, 0

        m = [[figure.shadow for i in range(self.size)] for j in range(self.size)]
        m[0][0] = figure.body
        m[1][0] = figure.shadow

        for x, y, sign in figure.move():
            m[y][x] = sign

        return m


def spiralize(size):
    return Board(size).draw(Snake, SpiralPath)
