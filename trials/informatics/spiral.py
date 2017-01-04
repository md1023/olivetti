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


class Snake:
    def __init__(self, size):
        self.size = size
        self.x, self.y = 0, 0
        self.path = SpiralPath(size)

    def step(self):
        body_x, body_y, shadow_x, shadow_y = DIRECTIONS.get(self.direction)
        self.x += body_x
        self.y += body_y
        yield self.x, self.y, 1
        yield self.x + shadow_x, self.y + shadow_y, 0

    def move(self):
        direction = cycle(('GO_RIGHT', 'GO_DOWN', 'GO_LEFT', 'GO_UP'))

        for steps in self.path.move():
            self.direction = next(direction)
            for _ in range(steps):
                yield from self.step()


class Path(object):
    def __init__(self):
        self.reset()

    def reset(self):
        self.path = iter(self._path)

    def move(self):
        for step in self.path:
            yield step


class SpiralPath(Path):
    def __init__(self, size):
        # lengths of straight moves
        l = lambda size: [i for i in range(size, 0, -2)];
        # spiral path in steps
        self._path = [size - 1] + [j-1 for i in l(size) for j in [i,i]][:size-1]
        super(SpiralPath, self).__init__()


def spiralize(size):

    if not size:
        return []

    if size == 1:
        return [[1]]

    m = [[0 for i in range(size)] for j in range(size)]
    m[0][0] = 1
    m[1][0] = 0

    snake = Snake(size)
    for x, y, value in snake.move():
        m[y][x] = value

    return m


