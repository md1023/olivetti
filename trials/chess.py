# -*- coding: utf-8 -*-
import sys


class Cell(object):
    def __init__(self, line, column, straight_diagonal, reversed_diagonal):
        self.position = (line, column, straight_diagonal, reversed_diagonal)

    def __repr__(self):
        return "".join(self.position)

    def on_line(self, cell):
        for i in xrange(4):
            if self.position[i] == cell.position[i]:
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

    def draw(self, figures=[]):
        n, m = self.size
        output = "%s "
        sys.stdout.write("  ")
        for i in xrange(0, m):
            col = chr(i + ord("A"))
            sys.stdout.write(output % col)
        sys.stdout.write("\n1 ")
        for c in self.cells:
            col = ord(c.position[1]) - ord("A")
            fig_pos = [figures[i].position for i in xrange(len(figures))]
            figure = "."
            if c in fig_pos:
                figure = figures[fig_pos.index(c)].character
            sys.stdout.write(output % figure)
            if col == m - 1 and self.cells.index(c) != len(self.cells) - 1:
                sys.stdout.write("\n%s " % (int(c.position[0]) + 1,))
        sys.stdout.write("\n")


class Figure(object):
    character = "."
    name = "Generic figure"

    def __init__(self, position):
        self.position = position

    def __repr__(self):
        return "%s at %s" % (self.name, self.position,)


class Dummy(Figure):
    character = "x"


class Rook(Figure):
    # character = "R"
    character = u"\u2656"
    name = "Rook"


class Queen(Figure):
    # character = "Q"
    character = u"\u2655"
    name = "Queen"

    def conflicts(self, piece_or_cell):
        if isinstance(piece_or_cell, Figure):
            piece_or_cell = piece_or_cell.position
        return self.position.on_line(piece_or_cell)


def throw_out_cells(cells, figure, position):
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
    return [c for c in cells if not position.on_line(c)]


solutions = set()
partial_solutions = set()

def backtrack(free_cells, pieces=(), figure=Queen):
    if not free_cells:
        solutions.add(frozenset(pieces))
    for c in free_cells:
        new_pieces = pieces + (c,)
        partial_solution = frozenset(new_pieces)
        if partial_solution in partial_solutions:
            continue
        partial_solutions.add(partial_solution)
        new_free_cells = throw_out_cells(free_cells, figure, c)
        backtrack(new_free_cells, new_pieces, figure)


n = 8
i = 0
table = Table(n, n)
backtrack(table.cells)
for s in solutions:
    if len(s) > n - 1:
        print i, s
        figures = [Queen(c) for c in s]
        table.draw(figures)
        i += 1

print "Total:", i
