import sys
from collections import namedtuple

heading = dict(
    GO_UP=('y', 1),
    GO_DOWN=('y', -1),
    GO_LEFT=('x', -1),
    GO_RIGHT=('x', 1)
)

# heading = {
#     ('y', 1) = 'GO_UP'
#     ('y', -1) = 'GO_DOWN'
#     ('x', -1) = 'GO_LEFT'
#     ('x', 1) = 'GO_RIGHT'
# }

def heading(index):
    if bool(index % 2) and not int(index / 2):
        return 'GO_UP'
    if bool(index % 2) and int(index / 2):
        return 'GO_DOWN'
    if not bool(index % 2) and int(index / 2):
        return 'GO_LEFT'
    if not bool(index % 2) and not int(index / 2):
        return 'GO_RIGHT'

def spiralize(size):

    if not size:
        return []

    # lengths of steps
    l = lambda size: [i for i in range(size, 0, -2)];

    p = lambda size: [size - 1] + [j-1 for i in l(size) for j in [i,i]][:size-1]
    print(p(size))

    Point = namedtuple('Point', ['x', 'y'])
    points = [Point(0, 0)]

    for index, step in enumerate(p(size)):
        # heading = None
        coordinate = 'y' if bool(index % 2) else 'x'
        direction = -1 if int(index / 2) % 2 else 1

        for i in range(step):
            # newPoint = points[-1]._replace(
            #    **{coordinate: getattr(points[-1], coordinate) + direction})
            last_point = points[-1]
            newPoint = last_point._replace(
                **{coordinate: getattr(last_point, coordinate) + direction})
            points.append(newPoint)

    m = [[0 for i in range(size)] for j in range(size)]
    for p in points:
        m[p.y][p.x] = 1

    return m
