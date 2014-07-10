# -*- coding: utf-8 -*-

class Job(object):
    def __init__(self, weight, length):
        self.weight = weight
        self.length = length

    def __call__(self, start_time):
        completion_time = self.length + start_time
        d = dict(start_time = start_time,
                 completion_time = completion_time,
                 objective= self.weight * completion_time,
                 score1 = float(self.weight) / self.length,
                 score2 = self.weight - self.length)
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
        print infos, objective

# job1 = Job(5, 3)
# job2 = Job(2, 1)
# job34 = Job(4, 2)
# job43 = Job(3, 4)

# Executor(job1, job34, job43, job2)
# Executor(job1, job43, job34, job2)

jobs = []
 
with open("/tmp/jobs.txt") as f:
    for l in f.readlines():
        weight, length = [int(s) for s in l.split()]
        job = Job(weight, length)
        jobs.append(job)

Executor(*jobs)



# дело1 время работы 1с
# дело2 время работы 2с

# дело1 завершится на 1-й секунде,
# тогда дело2 начнётся после первой секунды и завершится на 3-й секунде,
# поэтому сумма моментов завершения будет 1 + 3 = 4

# дело2 завершится на 2-й секунде,
# тогда дело1 начнётся после второй секунды и завершится на 3-й секунде,
# поэтому сумма моментов завершения будет 2 + 3 = 5


# дело1 время работы 1с вес3
# дело2 время работы 2с вес2
# дело3 время работы 3с вес1


# 1-3-2 => 3*1 + 1*(1+3) + 2*(3+1+2) = 3 + 4 + 6 = 13

# вес3 > вес1 => дело1++ дело3--
# вес3 > вес2 => дело1++ дело2--
# вес2 > вес1 => дело2++ дело3--

# 1с < 3с => дело1++ дело3--
# 1с < 2с => дело1++ дело2--
# 2с < 3с => дело2++ дело3--

# дела:
# 1: 4
# 2: 0
# 3: -4

# 1-2-3 => 3*1 + 2*(1+2) + 1*(1+2+3) = 3 + 6 + 6 = 15
# 2-1-3 => 2*2 + 3*(2+1) + 1*(2+1+3) = 4 + 9 + 6 = 19
# 2-3-1 => 2*2 + 1*(2+3) + 3*(2+3+1) = 4 + 5 + 18 = 27
# 3-1-2 => 1*3 + 3*(3+1) + 2*(3+1+2) = 3 + 12 + 12 = 27
# 3-2-1 => 1*3 + 2*(3+2) + 3*(3+2+1) = 3 + 10 + 18 = 31



