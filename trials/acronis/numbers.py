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

digits = u"нуль один два три четыре пять шесть семь восемь девять".split()

tens = u"десять одинадцать двенадцать тринадцать четырнадцать \
пятнадцать шестнадцать семнадцать восемнадцать девятнадцать".split()

dozens = "двадцать, тридцать, сорок, пятьдесят, шестьдесят, \
семьдесят, восемьдесят, девяносто".split()

cents = "сто двести тристо четыресто пятьсот шестьсот семьсот восемьсот \
девятьсот".split()

chiliads = "тысяч миллион биллион триллион квадриллион квинтиллион".split()

def break_number(number):
    """
    >>> break_number(2318)
    2000 + 300 + 18
    >>> break_number(2321)
    2000 + 300 + 20 + 1
    """
    s = str(number)[::-1]
    point = 3
    groups = [s[i:i+point][::-1].zfill(3) for i in xrange(0, len(s), point)][::-1]

    return groups

# 100
# 30
# 1

# print decimals[-5], octal_number
