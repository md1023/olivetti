class Element(object):
    def __init__(self, element_id, leader=None):
        self.id = element_id
        if not leader:
            leader = self
        self.leader = leader
        self.rank = 0

    def __repr__(self):
        d = "%s" % self.id
        return d


class DisjointSet(object):
    """
    >>> names = "Thaddeus Max Larabee Siegfried Hans Schwartz".split()
    >>> s = DisjointSet(names)
    >>> s.union(s["Max"], s["Thaddeus"])
    """

    def __init__(self, elements):
        self.__elements = [Element(e) for e in elements]
        self.leaders = range(len(self.__elements))

    def indexer(self, person):
        index = self.__elements.index(person)
        leader_index = self.leaders[index]
        leader_name = self.__elements[leader_index]
        return (index, leader_index, leader_name)

    def __getitem__(self, element_id):
        for element in self.__elements:
            if element.id == element_id:
                return element
        return None

    def find(self, person):
        leader = person.leader
        if person != leader:
            # find leader in hierarchy
            self.find(person.leader)
        return person

    def union(self, person1, person2):
        """
        >>> names = "Thaddeus Max Larabee Siegfried Hans Schwartz".split()
        >>> s = DisjointSet(names)
        >>> s["Max"].leader == s["Max"]
        True
        >>> s.union(s["Max"], s["Thaddeus"])
        >>> s["Max"].leader == s["Thaddeus"]
        True
        >>> s.union(s["Thaddeus"], s["Siegfried"])
        >>> (s.find("Max"), s.find("Thaddeus")), (s["Siegfried"], s["Siegfried"])
        True
        """
        x = self.find(person1)
        y = self.find(person2)
        x.leader = y

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
