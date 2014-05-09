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

def break_number(number, point):
    """
    >>> break_number(2318, 3)
    ['002', '318']
    """
    s = str(number)[::-1]
    return [s[i:i+point][::-1].zfill(point) for i in xrange(0, len(s), point)][::-1]

def translate_number(number):
    """
    >>> translate_number(2318)
    2318
    >>> translate_number(2321)
    2321
    """
    return number

# 100
# 30
# 1

# print decimals[-5], octal_number
