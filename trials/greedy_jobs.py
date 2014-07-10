# -*- coding: utf-8 -*-
import sys
from operator import attrgetter

class Job(object):
    def __init__(self, weight, length):
        self.weight = weight
        self.length = length
        self.score1 = float(self.weight) / self.length
        self.score2 = self.weight - self.length

    def __call__(self, start_time):
        completion_time = self.length + start_time
        d = dict(job = repr(self),
                 start_time = start_time,
                 completion_time = completion_time,
                 objective= self.weight * completion_time)
        return d

    def __repr__(self):
        d = "%s %s" % (self.weight, self.length)
        # d += "%s %s" (self.score1, self.score2)
        return d

class Executor(object):
    def __init__(self, *jobs):
        start_time = 0
        infos = []
        objective = 0
        for job in jobs:
            info = job(start_time)
            start_time += info["completion_time"]
            objective += info["objective"]
            infos.append(info)
            #print job, info["objective"]
        # print str(infos).replace(",", "\n"), objective, "\n"
        zzz = ""
        if objective == 11336:
            zzz = "***************************"
        print zzz + str(objective)

jobs = []

file_name = "/tmp/jobs.txt"
if len(sys.argv) > 1:
    file_name = sys.argv[1]

with open(file_name) as f:
    for l in f.readlines():
        weight, length = [int(s) for s in l.split()]
        job = Job(weight, length)
        jobs.append(job)

# Executor(*jobs)
# print "-" * 20

schedule = sorted(jobs, key=attrgetter("score2"), reverse=True)

import itertools
for job in itertools.permutations(jobs):
    print job
    Executor(*job)

# Executor(*schedule)