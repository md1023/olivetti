import random


class Decorator(object):
    def __init__(self, func):
        profiler = random.randint(0, 1000)
        self.func = func
        print "decorator", profiler, id(profiler)
        self.ncalls = 0

    def __call__(self, *args, **kwargs):
        print "wrapper start"
        self.ncalls += 1
        self.func(*args, **kwargs)
        print "wrapper end"


class Foo(object):

    @Decorator
    def bar(self, a, b=1):
        print "bar", a, b

    @Decorator
    def ufo(self, a, b=3):
        print "ufo", a, b

    def frak(self):
        print ">"
        self.bar(1, 2)
        print "-"
        self.bar(5, 6)
        print "-"
        self.ufo(3, 4)
        print "<"

foo = Foo()
foo.frak()
print foo.bar.ncalls, foo.ufo.ncalls
