#!/usr/bin/env python
# -*- coding: utf-8 -*-

# d  o
# 7  7
# 8  10
# 9  11
#
# 15 17
# 16 20
# 17 21

# number = int(raw_input("Enter natural number: "))
decimal_number = int(131)
octal_number = oct(decimal_number)

small_names = u"один два три четыре пять шесть семь восемь девять \
десять одинадцать двенадцать тринадцать четырнадцать \
пятнадцать шестнадцать семнадцать восемнадцать девятнадцать".split()
small_numbers = [i for i in xrange(1, 20)]
assert len(small_numbers) == len(small_numbers)

dozen_names = u"двадцать тридцать сорок пятьдесят шестьдесят \
семьдесят восемьдесят девяносто".split()
dozen_numbers = [10*i for i in xrange(2, 10)]
assert len(dozen_numbers) == len(dozen_numbers)

centicemal_names = small_names + dozen_names
centicemal_numbers = small_numbers + dozen_numbers

hundred_names = u"сто двести тристо четыресто пятьсот шестьсот семьсот восемьсот \
девятьсот".split()
en_hundred_names = [i+" hundred" for i in small_names[:9]]
hundred_numbers = [100*i for i in xrange(1, 10)]
assert len(hundred_numbers) == len(hundred_numbers)

chiliads_names = u"тысяч миллион биллион триллион квадриллион квинтиллион".split()
chiliads_numbers = [1000**i for i in xrange(1, 7)]
assert len(chiliads_numbers) == len(chiliads_names)

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
    [300, 8]
    >>> combine_tens(310)
    [300, 10]
    >>> combine_tens(318)
    [300, 18]
    >>> combine_tens(321)
    [300, 20, 1]
    """
    s = break_chiliad(number)
    t = []
    i = 0
    while i < len(s):
        value = s[i]
        if value < 20 and i + 1 < len(s):
            value = s[i] + s[i+1]
            i += 1
        t.append(value)
        i += 1
    return t

def get_name(category, number):
    names = globals().get(category+"_names")
    numbers = list(globals().get(category+"_numbers"))
    l = sorted(numbers + [number])
    return names[l.index(number)]

def translate_number(number):
    u"""
    >>> translate_number(308) == u"тристо восемь"
    True
    >>> translate_number(310) == u"тристо десять"
    True
    >>> translate_number(320) == u"тристо двадцать"
    True
    >>> translate_number(321) == u"тристо двадцать один"
    True
    """
    s = combine_tens(number)
    name = []
    categories = ["hundred", "centicemal", "centicemal"]

    while s:
        value = s.pop(0)
        category = categories.pop(0)
        if not value:
            continue
        name.append(get_name(category, value))

    return " ".join(name)

if __name__ == "__main__":
    for i in range(900, 1100):
        print i, translate_number(i)
