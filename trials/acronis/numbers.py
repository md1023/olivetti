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

dozens = "десять двадцать, тридцать, сорок, пятьдесят, шестьдесят, \
семьдесят, восемьдесят, девяносто, сто".split()

def break_number(number):
    """
    >>> break_number(2131)
    2000 + 100 + 30 + 1
    """
    s = str(number)[::-1]
    point = 3
    groups = [s[i:i+point] for i in xrange(0, len(s), point)][::-1]

    return groups

# 100
# 30
# 1

# print decimals[-5], octal_number
