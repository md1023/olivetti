class DisjointSet(object):
    """
    >>> names = "Thaddeus Max Larabee Siegfried Hans Schwartz".split()
    >>> s = DisjointSet(names)
    >>> s.leaders == [0, 1, 2, 3, 4, 5]
    True
    >>> s.union("Max", "Thaddeus")
    >>> s.leaders == [0, 0, 2, 3, 4, 5]
    True
    """

    def __init__(self, elements):
        self.__elements = elements
        self.leaders = range(len(self.__elements))

    def indexer(self, person):
        index = self.__elements.index(person)
        leader_index = self.leaders[index]
        leader_name = self.__elements[leader_index]
        return (index, leader_index, leader_name)

    def __getitem__(self, person):
        """
        >>> names = "Thaddeus Max Larabee Siegfried Hans Schwartz".split()
        >>> s = DisjointSet(names)
        >>> s.union("Max", "Thaddeus")
        >>> s["Max"] == "Thaddeus"
        True
        >>> s.union("Thaddeus", "Siegfried")
        >>> (s["Max"], s["Thaddeus"]) == ("Siegfried", "Siegfried")
        True
        """
        index, leader_index, leader_name = self.indexer(person)

        if index != leader_index:
            return self[leader_name]
        return leader_name

    def find(self, person):
        index, leader_index, leader_name = self.indexer(person)

        if index != leader_index:
            return self.find(leader_name)
        return leader_index

    def union(self, person1, person2):
        """
        >>> names = "Thaddeus Max Larabee Siegfried Hans Schwartz".split()
        >>> s = DisjointSet(names)
        >>> s["Max"].leader == "Max"
        """
        x = self.find(person1)
        y = self.find(person2)
        self.leaders[x] = y
        # x.leader = y

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
