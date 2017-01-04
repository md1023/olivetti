#!/usr/bin/env python3
from itertools import cycle


class Snake:
    def __init__(self, dimension):
        self.size = dimension.size
        self.path = SpiralPath(self.size)
        self.dimension = dimension
        self.direction = self.path.directions[0]

    def step(self, qqq):
        yield from self.path.step(self.dimension, qqq)

    def move(self):
        direction = cycle(self.path.directions)

        for steps in self.path.move():
            qqq = next(direction)
            for _ in range(steps):
                yield from self.step(qqq)


class Path(object):
    def __init__(self):
        self.reset()

    def reset(self):
        self.x = 0
        self.y = 0
        self.path = iter(self._path)

    def move(self):
        for step in self.path:
            yield step


class SpiralPath(Path):
    directions = ('GO_RIGHT', 'GO_DOWN', 'GO_LEFT', 'GO_UP')
    def __init__(self, size):
        # lengths of straight moves
        l = lambda size: [i for i in range(size, 0, -2)];
        # spiral path in steps
        self._path = [size - 1] + [j-1 for i in l(size) for j in [i,i]][:size-1]
        super(SpiralPath, self).__init__()

    def step(self, dimension, qqq):
        self.x, self.y, shadow_x, shadow_y = dimension.directions.get(qqq)
        # body
        yield self.x, self.y, 1
        # shadow
        yield self.x + shadow_x, self.y + shadow_y, 0


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

    def draw(self, figure_name):

        if not self.size:
            return []

        if self.size == 1:
            return [[1]]

        m = [[0 for i in range(self.size)] for j in range(self.size)]
        m[0][0] = 1
        m[1][0] = 0

        figure = figure_name(self)
        for x, y, value in figure.move():
            m[y][x] = value

        return m


def spiralize(size):
    board = Board(size)
    m = board.draw(Snake)
    print(m)


