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

    def on_line(self, cell,
                predicates=["has_same_line", "has_same_column",
                            "has_same_straight_diagonal",
                            "has_same_reversed_diagonal"]):
        return any([getattr(self, p)(cell) for p in predicates])


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

    def draw(self, figures=[]):
        n, m = self.size
        output = "%s "
        sys.stdout.write("  ")
        for i in xrange(0, m):
            col = chr(i + ord("A"))
            sys.stdout.write(output % col)
        sys.stdout.write("\n1 ")
        for c in self.cells:
            col = ord(c.column) - ord("A")
            fig_pos = [figures[i].position for i in xrange(len(figures))]
            figure = "."
            if c in fig_pos:
                figure = figures[fig_pos.index(c)].character
            sys.stdout.write(output % figure)
            if col == m - 1 and self.cells.index(c) != len(self.cells) - 1:
                sys.stdout.write("\n%s " % (int(c.line) + 1,))
        sys.stdout.write("\n")


class Figure(object):
    character = "."

    def __init__(self, position):
        self.position = position

    def __repr__(self):
        return "Generic figure at %s" % (self.position,)


class Dummy(Figure):
    character = "x"


class Queen(Figure):
    character = "Q"

    def conflicts(self, piece):
        return self.position.on_line(piece.position)


def throw_out_cells(cells, figure):
    """
    >>> t = Table(4, 4)
    >>> q = Queen(t.cells[0])
    >>> t.draw([q] + [Dummy(c) for c in throw_out_cells(t.cells, q)])
      A B C D 
    1 Q . . . 
    2 . . x x 
    3 . x . x 
    4 . x x . 
    >>> q = Queen(t.cells[4])
    >>> t.draw([q] + [Dummy(c) for c in throw_out_cells(t.cells, q)])
      A B C D 
    1 . . x x 
    2 Q . . . 
    3 . . x x 
    4 . x . x 
    """
    return [c for c in cells if not c.on_line(figure.position)]


def build(cells):
    queens = []
    for c in cells:
        queen = Queen(c)
        conflicts = [q for q in queens if queen.conflicts(q)]
        if any(conflicts):
            continue
        queens.append(queen)
    if len(queens) > 7:
        print "Queens:", queens, "total:", len(queens)
    return queens


def queens_problem(n, m):
    table = Table(n, m)
    print len(table.cells)
    for i in xrange(len(table.cells)):
        cells = table.cells[i:] + table.cells[:i]
        queens = build(cells)
        if len(queens) > 7:
            table.draw(queens)

# queens_problem(8, 8)
