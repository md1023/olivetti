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
    dump.write(page)
    dump.close()


def traverse(page_source):
    document = BeautifulSoup(page_source, "html5lib")
    for el in list(document.find_all()):
        if not "style" in str(el):
            continue
        if "style" in el.attrs:
            print "\n", el, "\n", "-"*60, "\n"
        xml = BeautifulSoup(el.text, "xml")
        # from ipdb import set_trace; set_trace()
        for sub_el in xml.find_all():
            if not "style" in sub_el.attrs:
                print "\n", el, "\n", "-"*60, "\n"


if __name__ == "__main__":
    page_source = read_dump(DUMP_NAME)
    if not page_source:
        page_source = read_page(URL)
        dump_page(page_source, DUMP_NAME)
    remove_style(page_source)
