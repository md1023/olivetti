# -*- coding: utf-8 -*-
def common_func(context, email, injected_func=None):
    func_list = [before, after]
    if injected_func:
        func, args = injected_func[0], injected_func[1:]
        func_list.insert(-1, func)
    # for func in func_list:
    #     print ">", apply(func, context)
    return map(lambda f: apply(f, context), func_list)

def apply(func, *args):
    return func(*args)


def before(a):
    return "a: %s" % a


def after(z):
    return "z: %s" % z


def middle(*m):
    return "m: %s" % list(m)


# print apply(middle, 1, 2, 3) == "m: [1, 2, 3]"
print common_func(1, 2)
