import sys
import itertools
import time
phases = list("-\\|/")
cycle = itertools.cycle(phases)
reverse_cycle = itertools.cycle(phases[::-1][-1:] + phases[::-1][:-1])
strokes = float(len(phases))

while True:
    line = cycle.next() + " " + reverse_cycle.next()
    sys.stdout.write(line)
    time.sleep(1 / strokes)
    sys.stdout.write("\r" * len(line))
    sys.stdout.flush()
