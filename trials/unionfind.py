names = "Thaddeus Max Larraby Siegfried Hans Schwartz"

persons = names.split()
superiors = range(0, len(persons))


def find(p_index):
    boss = superiors[p_index]
    if (boss != superiors[boss]):
        boss = find(boss)
    return boss


def union(p1_index, p2_index):
    superiors[find(p1_index)] = find(p2_index)


class Company(object):
    def __init__(self, person):
        self.boss = person
        self.rank = 0

    def __repr__(self):
        d = "<%s, %s>" % (self.boss, self.rank)
        return d

print persons

person1 = Company("Thaddeus")
print person1
assert person1.boss == "Thaddeus"

person2 = Company("Max")

assert find(1) == 1

union(0, 1)
print superiors
