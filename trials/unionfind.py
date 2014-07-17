class DisjointSet(object):
    """
    >>> names = "Thaddeus Max Larraby Siegfried Hans Schwartz".split()
    >>> s = DisjointSet(names)
    >>> print s.leaders
    [0, 1, 2, 3, 4, 5]
    >>> s.union(1, 0)
    >>> print s.leaders
    [0, 0, 2, 3, 4, 5]
    """

    def __init__(self, elements):
        self.__elements = elements
        self.leaders = range(len(self.__elements))

    def find(self, index):
        leader = self.leaders[index]
        if (leader != self.leaders[leader]):
            leader = self.find(leader)
        return leader

    def union(self, index1, index2):
        x = self.find(index1)
        y = self.find(index2)
        self.leaders[x] = y

        return
        if x == y:
            return
        if x.rank < y.rank:
            self.leader[x] = y
        elif x.rank > y.rank:
            self.leader[y] = x
        else:
            self.leader[y] = x
            x.rank += 1

    def __repr__(self):
        persons = ["<%s (%s %s)>" % (self.__elements[i], s, self.__elements[s])
                   for i, s in enumerate(self.leaders)]
        d = "%s" % persons
        return d
