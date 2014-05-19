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

    def has_same_line(self, cell):
        if self.line == cell.line:
            return True
        return False

    def has_same_column(self, cell):
        if self.column == cell.column:
            return True
        return False

    def has_same_straight_diagonal(self, cell):
        if self.straight_diagonal == cell.straight_diagonal:
            return True
        return False

    def has_same_reversed_diagonal(self, cell):
        if self.reversed_diagonal == cell.reversed_diagonal:
            return True
        return False


class Table(object):
    """
    >>> table = Table(3, 5)
    >>> a = Queen(table.cells[0])
    >>> b = Queen(table.cells[1])
    >>> c = Queen(table.cells[8])
    >>> assert(a.conflicts(b) == True)
    >>> assert(a.conflicts(c) == False)
    """
    def __init__(self, n, m):
        self.size = n, m
        lines = range(1, n + 1)
        columns = map(chr, range(ord("A"), ord("Z")))[:m]
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
                # sys.stdout.write(repr(cell) + ", ")
                self.cells.append(cell)
            # sys.stdout.write("\n")

    def draw(self, figures):
        n, m = self.size
        for c in self.cells:
            col = ord(c.column) - ord("A")
            sys.stdout.write(".")
            if col == m - 1:
                sys.stdout.write("\n")


class Queen(object):
    def __init__(self, position):
        self.position = position

    def conflicts(self, piece):
        predicates = ["has_same_line", "has_same_column",
                      "has_same_straight_diagonal", "has_same_reversed_diagonal"]
        return any([getattr(self.position, p)(piece.position)
                    for p in predicates])

    def __repr__(self):
        return "Queen at %s" % (self.position,)


def queens_problem(n, m):
    table = Table(n, m)
    queens = []
    solution = []
    for c in table.cells:
        queen = Queen(c)
        conflicts = [q for q in queens if queen.conflicts(q)]
        if any(conflicts):
            continue
        queens.append(queen)
    print "Queens:", queens
    table.draw(queens)

queens_problem(3, 5)
