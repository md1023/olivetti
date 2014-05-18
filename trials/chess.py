# -*- coding: utf-8 -*-

class Line(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name

class Table(object):
    def __init__(self, n, m):
        self.size = n, m
        lines = range(1, m + 1)
        print lines
        alphabet = map(chr, range(97, 123))[:m]
        horizontals = [Line(str(i)) for i in lines]
        verticals = [Line(l.upper()) for l in alphabet]
        straight_diagonal = [Line("%s%s" % (lines[i], alphabet[i].upper()))
                             for i in xrange(n)] + \
                            [Line("%s%s" % (alphabet[i].upper(), lines[i]))
                             for i in xrange(n)]
        print horizontals, verticals
        print straight_diagonal, len(straight_diagonal)

print Table(8, 8)
