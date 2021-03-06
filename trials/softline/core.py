#!/usr/bin/env python
# -*- coding: utf-8 -*-
import urllib2
import os.path
import sys
from bs4 import BeautifulSoup

DUMP_NAME = "./dump.html"
URL = "https://www.softlogic.ru/p/flexbby"
OUTPUT_NAME = "./replacement.html"


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


def perform_change(attr, el):
    if attr in el.attrs:
        del el.attrs[attr]


def traverse(page_source):
    document = BeautifulSoup(page_source, "html.parser")
    for el in document.find_all()[1:]:
        if not "style" in str(el):
            continue
        perform_change("style", el)
        if not "style" in str(el):
            continue
        # escaped strings parsed as xml
        xml = BeautifulSoup(el.text, "xml")
        if xml.is_empty_element:
            continue
        for sub_el in xml.find_all():
            perform_change("style", sub_el)
        replacement = xml.find_all()[0]
        el.string.replace_with(replacement)
    return document

if __name__ == "__main__":
    if len(sys.argv) > 1:
        DUMP_NAME = sys.argv[1]
    page_source = read_dump(DUMP_NAME)
    if not page_source:
        page_source = read_page(URL)
        dump_page(page_source, DUMP_NAME)
    modified_page = traverse(page_source)
    if len(sys.argv) > 2:
        OUTPUT_NAME = sys.argv[2]
    dump_page(repr(modified_page), OUTPUT_NAME)
    sys.stdout.write("file: \"%s\" - written\n".encode("utf-8") % OUTPUT_NAME)
    sys.exit(0)
