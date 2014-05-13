#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys

NULL_NAME = u"нуль"

class Number(object):
    small_names = u"один два три четыре пять шесть семь восемь девять \
    десять одинадцать двенадцать тринадцать четырнадцать \
    пятнадцать шестнадцать семнадцать восемнадцать девятнадцать".split()
    small_names_genitive = u"одна две".split() + small_names[2:]
    small_numbers = [i for i in xrange(1, 20)]

    dozen_names = u"двадцать тридцать сорок пятьдесят шестьдесят \
    семьдесят восемьдесят девяносто".split()
    dozen_numbers = [10*i for i in xrange(2, 10)]

    def __init__(self, number):
        self.number = int(number)
        self.name = self.translate_number()

    def get_name(self):
        l = sorted(self.numbers + [self.number])
        return self.names[l.index(self.number)]

    def translate_number(self):
        u"""
        >>> Number(19).translate_number() == [u"девятнадцать"]
        True
        >>> Number(20).translate_number() == [u"двадцать"]
        True
        >>> Number(21).translate_number() == [u"двадцать", u"один"]
        True
        >>> Number(99).translate_number() == [u"девяносто", u"девять"]
        True
        >>> Number(100).translate_number() == [u"сто"]
        True
        >>> Number(101).translate_number() == [u"сто", u"один"]
        True
        >>> Number(308).translate_number() == [u"тристо", u"восемь"]
        True
        >>> Number(320).translate_number() == [u"тристо", u"двадцать"]
        True
        >>> Number(321).translate_number() == [u"тристо", u"двадцать", u"один"]
        True
        """
        if self.__class__ in (Centicemal, Chiliad, Hundred):
            return [self.get_name()]

        s = combine_tens(self.number)
        name = []
        categories = [Hundred, Centicemal, Centicemal]

        while s:
            value = s.pop(0)
            category = categories.pop(0)
            if not value:
                continue
            number = category(value)
            name.append(number.get_name())

        return name

    def __repr__(self):
        s = " ".join(self.name)
        return s.encode("utf-8")

class Centicemal(Number):
    names = Number.small_names + Number.dozen_names
    genitives = Number.small_names_genitive + Number.dozen_names
    numbers = Number.small_numbers + Number.dozen_numbers

class Hundred(Number):
    names = u"сто двести тристо четыресто пятьсот шестьсот семьсот восемьсот \
    девятьсот".split()
    en_names = [i+" hundred" for i in Number.small_names[:9]]
    numbers = [100*i for i in xrange(1, 10)]

class Chiliad(Number):
    names = u"тысяч миллион биллион триллион квадриллион квинтиллион".split()
    numbers = [10**i-1 for i in xrange(6, 19, 3)]
    genitives = { u"тысяч": (u"а", u"и", u"и", u"и", u""),
                  u"миллион": (u"", u"а", u"а", u"а", u"ов") }
    more_genetives = { u"тысяча": (u"один", u"одна"),
                       u"тысячи": (u"два", u"две") }

    def __init__(self, number, group):
        position = Number(number)

        self.number = int(number)*1000**group
        self.name = position.name

        order = self.check_genitive(number, group)
        if order in self.more_genetives:
            replacement = self.more_genetives[order]
            self.name = [replacement[1] if p == replacement[0] else p
                         for p in self.name]
        self.name += [order]

    def check_genitive(self, number, group):
        order = self.get_name()
        genitive = self.genitives.get(order, self.genitives.get(u"миллион"))
        suffix = genitive[-1]
        last_digit = int(str(number)[-1])
        if last_digit < 5:
            suffix = genitive[last_digit - 1]
        return order + suffix

def break_number(number, point):
    """
    >>> break_number(2318, 3)
    ['002', '318']
    """
    s = str(number)[::-1]
    return [s[i:i+point][::-1].zfill(point) for i in xrange(0, len(s), point)][::-1]

def break_chiliad(number):
    """
    >>> break_chiliad(308)
    [300, 0, 8]
    >>> break_chiliad(310)
    [300, 10, 0]
    >>> break_chiliad(318)
    [300, 10, 8]
    >>> break_chiliad(320)
    [300, 20, 0]
    >>> break_chiliad(321)
    [300, 20, 1]
    """
    l = reversed(list(str(number).zfill(3)))
    s = [int(i)*10**power for power, i in enumerate(l)][::-1]
    assert len(s) == 3
    return s

def combine_tens(number):
    """
    >>> combine_tens(308)
    [300, 8, 0]
    >>> combine_tens(310)
    [300, 10, 0]
    >>> combine_tens(318)
    [300, 18, 0]
    >>> combine_tens(321)
    [300, 20, 1]
    """
    s = break_chiliad(number)
    centicemal = sum(s[1:])
    if centicemal < 20:
        s = [s[0], centicemal, 0]
    assert len(s) == 3
    return s

def translate_chiliad(number):
    if int(number) == 0:
        return NULL_NAME
    complete_name = []
    triplets = break_number(number, 3)
    for group, number in enumerate(reversed(triplets)):
        if group == 0:
            complete_name.append(Number(number))
            continue
        if not int(number):
            continue
        complete_name.append(Chiliad(number, group))
    return " ".join([repr(j) for j in reversed(complete_name)])

if __name__ == "__main__":
    number = raw_input("Enter integer: ")
    if not number:
        sys.stderr.write("No number specified.")
        exit(1)
    try:
        number = int(number)
    except ValueError:
        sys.stderr.write("Wrong number specified.")
        exit(1)
    if number > 10**18 or number < 0:
        sys.stderr.write("Sorry, the number is unavailable for right now.")
        exit(1)
    sys.stdout.write("%s\n%s" % (translate_chiliad(number),
                                 translate_chiliad(oct(number))))
    exit(0)
