#!/usr/bin/env python
# -*- coding: utf-8 -*-
import urllib2
import os.path
from bs4 import BeautifulSoup

DUMP_NAME = "./dump.html"
URL = "https://www.softlogic.ru/p/flexbby"


def read_dump(filename):
    if not os.path.isfile(filename):
        return
    page_dump = open(filename, "r")
    page = page_dump.read()
    page_dump.close()
    return page


def read_page(url):
    response = urllib2.urlopen(url)
    page = response.read()
    return page


def dump_page(page, filename):
    dump = open(filename, "w")
    dump.write(page.encode("utf-8"))
    dump.close()


if __name__ == "__main__":
    pass
