# -*- coding: utf-8 -*-
import sys
import unittest
from operator import attrgetter

class Job(object):
    def __init__(self, weight, length, deadline=None):
        self.weight = weight
        self.length = length
        if deadline is not None:
            self.deadline = deadline
        self.score1 = float(self.weight) / self.length
        self.score2 = self.weight - self.length

    def __call__(self, start_time):
        completion_time = self.length + start_time
        lateness = completion_time - self.deadline
        if lateness < 0:
            lateness = 0
        d = dict(job = repr(self),
                 start_time = start_time,
                 completion_time = completion_time,
                 lateness = lateness,
                 objective = self.weight * completion_time)
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
            start_time += job.length
            objective += info["objective"]
            infos.append(info)
            print job.weight, "\t", \
                job.length, "\t", \
                job.score2, "\t", \
                info["start_time"], "\t", \
                info["completion_time"], "\t", \
                info["objective"]
        print objective

class TestCases(unittest.TestCase):
    def test_q1andq2:
        jobs = []
        file_name = "/tmp/jobs.txt"
        if len(sys.argv) > 1:
            file_name = sys.argv[1]
        with open(file_name) as f:
            for l in f.readlines():
                weight, length = [int(s) for s in l.split()]
                job = Job(weight, length)
                jobs.append(job)
        schedule = sorted(jobs, key=attrgetter("score1", "weight"), reverse=True)
        Executor(*schedule)
