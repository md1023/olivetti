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

dozen_names = "двадцать, тридцать, сорок, пятьдесят, шестьдесят, \
семьдесят, восемьдесят, девяносто".split()
dozen_numbers = [10*i for i in xrange(2, 10)]
assert len(dozen_numbers) == len(dozen_numbers)

cent_names = "сто двести тристо четыресто пятьсот шестьсот семьсот восемьсот \
девятьсот".split()
cent_numbers = [100*i for i in xrange(1, 10)]
assert len(cent_numbers) == len(cent_numbers)

chiliads = "тысяч миллион биллион триллион квадриллион квинтиллион".split()

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
    >>> break_chiliad(321)
    [300, 20, 1]
    """
    l = reversed(list(str(number)))
    s = [int(i)*10**power for power, i in enumerate(l)][::-1]
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

if __name__ == "__main__":
    pass
