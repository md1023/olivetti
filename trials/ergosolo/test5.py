#!/usr/bin/env python
# -*- coding: utf-8 -*-

from urllib import urlopen
from xml.etree.ElementTree import parse

u = urlopen("http://www.cbr.ru/scripts/XML_daily.xml")
doc = parse(u)
valutes = [u"Доллар США", u"Евро"]
for item in doc.iterfind("Valute"):
    if item.findtext("Name") in valutes:
        print item.findtext("CharCode"), item.findtext("Value")
