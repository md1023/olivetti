# -*- coding: utf-8 -*-
def super_func(do_before=None, main_func=None, do_after=None):
    if do_before:
        func, args = do_before[0], tuple(do_before[1:])
        func(*args)
    if main_func:
        func, args = main_func[0], tuple(main_func[1:])
        func(*args)
    if do_after:
        func, args = do_after[0], tuple(do_after[1:])
        func(*args)


def add(a, b):
    return a+b


print "> first:", super_func(None, (add, 1, 2), None)


########

def identity():
    return identity


def unary(arg):
    print "unary:", arg
    return arg


def do_before(before_func):
    def unary_before(*args):
        print "unary_before:", args, before_func
        before_func()
    s = "do_before: %s"
    print s % before_func
    s = "done_before: %s;"
    return unary_before


def do_after(after_func=None):
    def unary_after(arg):
        print "unary_after:", arg, after_func
    s = "do_after: %s"
    print s % after_func
    s = "done_after: %s;"
    return unary_after


@do_before
# @do_after
def middle_func(main_func="zzz"):
    print "pre main_func", main_func
    # if main_func:
    #     func, args = main_func[0], tuple(main_func[1:])
    #     func(*args)
    print "post main_func"
    return lambda arg: arg

print "-" * 60
print middle_func(1,2)
