# -*- coding: utf-8 -*-


class Line(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name


class Cell(object):
    def __init__(self, line, column, straight_diagonal, reversed_diagonal):
        self.line = line
        self.column = column
        self.straight_diagonal = straight_diagonal
        self.reversed_diagonal = reversed_diagonal

    def __repr__(self):
        return (self.line, self.column, self.straight_diagonal,
                self.reversed_diagonal)


class Table(object):
    def __init__(self, n, m):
        self.size = n, m
        lines = range(1, n + 1)
        columns = map(chr, range(97, 123))[:m]
        horizontals = [Line(str(i)) for i in lines]
        verticals = [Line(l.upper()) for l in columns]
        straight_diagonal = [Line(str(i)) for i in xrange(n + m - 1)]
        reversed_diagonal = [Line(str(i)) for i in xrange(n + m - 1)]

print Table(3, 5)
