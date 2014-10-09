#!/usr/bin/env python
# -*- coding: utf-8 -*-

import operator

# Поиск в консоли:
# cut -d ' ' -f 1 server.log | sort | uniq -c | sort | tail -5

ips = dict()

with open("server.log", "r") as f:
    for line in f:
        ip = line.partition(" ")[0]
        if ip not in ips:
            ips[ip] = 0
        ips[ip] += 1
    top = sorted(ips.items(), key=operator.itemgetter(1))[-5:]

for ip, count in top:
    print count, ip
