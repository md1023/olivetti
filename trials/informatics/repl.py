#!/usr/bin/env python
import re
from collections import namedtuple

def tokenize(expression):
    if expression == "":
        return []

    regex = re.compile("\s*(=>|[-+*\/\%=\(\)]|[A-Za-z_][A-Za-z0-9_]*|[0-9]*\.?[0-9]+)\s*")
    tokens = regex.findall(expression)
    return [s for s in tokens if not s.isspace()]

token_map = {
    '+': 'ADD', '-': 'ADD',
    '*': 'MUL', '/': 'MUL',
    '(': 'LPAR', ')': 'RPAR'
}
Token = namedtuple('Token', ['name', 'value'])

# LR-parser
rule_map = {
    'add': ['mul ADD add', 'mul'],
    'mul' : ['atom MUL mul', 'atom'],
    'atom': ['NUM', 'LPAR add RPAR', 'neg'],
    'neg' : ['ADD atom'],
}
Rule = namedtuple('Rule', ['name', 'subrule'])

class Interpreter:
    def __init__(self):
        self.vars = {}
        self.functions = {}

    def input(self, expression):
        tokens = [Token(token_map.get(t, 'NUM'), t) for t in tokenize(expression)]
        print tokens
        self.depth = 0
        ast = self.match('add', tokens)
        print ast
        # assert ast == (
        #     Rule(name='add',
        #          subrule=[Rule(name='mul',
        #                        subrule=[Rule(name='atom',
        #                                      subrule=[Token(name='NUM', value='1')])]),
        #                   Token(name='ADD', value='+'),
        #                   Rule(name='add',
        #                        subrule=[Rule(name='mul',
        #                                      subrule=[Rule(name='atom',
        #                                                    subrule=[Token(name='NUM', value='1')])])])]), [])
        
    def match(self, rule, tokens):
        self.depth += 1
        print '\n', "\t" * self.depth, ">>>", rule, rule == tokens[0].name if tokens else ''
        if tokens and rule == tokens[0].name:
            # token, rest = tokens[0], tokens[1:]
            self.depth -= 1
            return Rule(tokens[0], tokens[1:])
        for expansion in rule_map.get(rule, ()):
            print "\t" * self.depth, "%s)" % self.depth, expansion, '(%s)' % rule
            rest = tokens
            subrules = []

            for subrule in expansion.split():
                print "\t" * self.depth, "SUBRULE", subrule
                matches, rest = self.match(subrule, rest)
                if not matches:
                    break
                subrules.append(matches)
            else:
                self.depth -= 1
                return Rule(rule, subrules), rest

        return None, None


interpreter = Interpreter();
    
# Basic arithmetic
assert(interpreter.input("1 + ") == 2)
assert(interpreter.input("2 - 1") == 1)
assert(interpreter.input("2 * 3") == 6)
assert(interpreter.input("8 / 4") == 2)
assert(interpreter.input("7 % 4") == 3)

# Variables
assert(interpreter.input("x = 1") == 1)
assert(interpreter.input("x") == 1)
assert(interpreter.input("x + 3") == 4)
# test.expect_error("input: 'y'", lambda : interpreter.input("y"))
