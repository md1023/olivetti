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

    # find element by it's identificator
    def __getitem__(self, element_id):
        for element in self.__elements:
            if element.id == element_id:
                return element
        return None

    # find subset leader
    def find(self, person):
        leader = person.leader
        if person != leader:
            print "> searching", person, person.leader
            # find leader in hierarchy
            self.find(person.leader)
        return person

    # union two subsets
    def union(self, person1, person2):
        """
        >>> names = "Thaddeus Max Larabee Siegfried Hans Schwartz".split()
        >>> s = DisjointSet(names)
        >>> s["Max"].leader == s["Max"]
        True
        >>> s.union(s["Max"], s["Thaddeus"])
        >>> s["Max"].leader == s["Thaddeus"]
        True
        >>> s.find(s["Max"]) == s["Thaddeus"]
        True
        >>> s.union(s["Thaddeus"], s["Siegfried"])
        >>> (s.find(s["Max"]), s.find(s["Thaddeus"])), (s["Siegfried"], s["Siegfried"])
        True
        """
        x = self.find(person1)
        y = self.find(person2)
        if x == y:
            return
        if x.rank < y.rank:
            x.leader = y
        elif x.rank > y.rank:
            y.leader = x
        else:
            x.leader = y
            y.rank += 1

    def __repr__(self):
        persons = ["<%s (%s)>" % (s, s.leader) for s in self.__elements]
        d = "%s" % persons
        return d
