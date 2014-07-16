class DisjointSet(object):
    def __init__(self, x):
        x.parent = x
        x.rank = 0

    def find(self, x):
        if x.parent == x:
            return x
        return self.find(x.parent)

    def union(self, x, y):
        x = self.find(x)
        y = self.find(y)

        if x.rank < y.rank:
            x.parent = y
            return

        if x.rank > y.rank:
            y.parent = x
            return

        y.parent = x
        x.rank += 1
