# -*- coding: utf-8 -*-
import sys


class Cell(object):
    def __init__(self, line, column, straight_diagonal, reversed_diagonal):
        self.line = line
        self.column = column
        self.straight_diagonal = straight_diagonal
        self.reversed_diagonal = reversed_diagonal

    def __repr__(self):
        return "".join([self.line, self.column, self.straight_diagonal,
                        self.reversed_diagonal])


class Table(object):
    def __init__(self, n, m):
        self.size = n, m
        lines = range(1, n + 1)
        columns = map(chr, range(97, 123))[:m]
        horizontals = [str(i) for i in lines]
        verticals = [l.upper() for l in columns]
        straight_diagonal = [str(i) for i in xrange(1, n + m)]
        reversed_diagonal = [str(i) for i in xrange(1, n + m)]
        self.cells = []
        for i in xrange(n):
            for j in xrange(m):
                cell = Cell(horizontals[i], verticals[j],
                            straight_diagonal[j + i],
                            reversed_diagonal[n - 1 - i + j])
                sys.stdout.write(repr(cell) + ", ")
                self.cells.append(cell)
            sys.stdout.write("\n")

print Table(3, 5)
