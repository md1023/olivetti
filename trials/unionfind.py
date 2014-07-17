class DisjointSet(object):
    """
    >>> names = "Thaddeus Max Larabee Siegfried Hans Schwartz".split()
    >>> s = DisjointSet(names)
    >>> print s.leaders
    [0, 1, 2, 3, 4, 5]
    >>> s.union("Max", "Thaddeus")
    >>> print s.leaders
    [0, 0, 2, 3, 4, 5]
    """

    def __init__(self, elements):
        self.__elements = elements
        self.leaders = range(len(self.__elements))

    def __getitem__(self, item):
        """
        >>> names = "Thaddeus Max Larabee Siegfried Hans Schwartz".split()
        >>> s = DisjointSet(names)
        >>> s.union("Max", "Thaddeus")
        >>> print s["Max"]
        Thaddeus
        """
        item_index = self.__elements.index(item)
        leader_index = self.leaders[item_index]
        return self.__elements[leader_index]

    def find(self, person):
        index = self.__elements.index(person)

        leader = self.leaders[index]
        if (leader != self.leaders[leader]):
            leader = self.find(leader)
        return leader

    def union(self, person1, person2):
        x = self.find(person1)
        y = self.find(person2)
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
