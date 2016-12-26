#!/usr/bin/env python3
import re
from collections import namedtuple

Token = namedtuple('Tuple', ['kind', 'value'])
terminals = dict(
    ADD = '\+',
    MUL = '\*',
    SUB = '-',
    DIV = '/',
    MOD = '%',
    ASG = '=',
    DOT = '\.',
    LPR = '\(',
    RPR = '\)',
    LTR = '[A-Za-z]',
    DGT = '[0-9]',
    END = '\$'
)

class Lexer:
    def __init__(self, input):
        self.input = iter(input.replace(' ', '') + '$')
        self.repeat_same_token = False

    def _get_next_token(self):
        if self.repeat_same_token:
            self.repeat_same_token = False
            return self.previous_token

        character = next(self.input)
        token = None
        
        for _type, regexp in terminals.items():
            if re.match(regexp, character):
                token = Token(_type, character)
                
        if not token:
            raise ValueError('unknown token %s' % _type)


        self.previous_token = token
        return token

    def get_next_token(self):
        token = self._get_next_token()
        print("got token", token)
        return token

    def repeat(self):
        print('repeating', self.previous_token)
        self.repeat_same_token = True

class ParserError(Exception):
    pass


class Parser:
    def parse(self, input):
        self.lexer = Lexer(input)
        self.stack = [Number]
        token = self.lexer.get_next_token()

        node = self.stack.pop()
        self.stack = getattr(self, 'parse' + node.__name__)(token)[::-1]

        node = self.stack.pop()
        self.stack = getattr(self, 'parse' + node.__name__)(token)[::-1]
        
        print('finish', self.stack, token)
        

    def reverse_list(l):
        return l[::-1]
        
    def parseNumber(self, token):
        print('parsing Number')
        if token.kind == 'DOT':
            return [Dot, Integer]
        if token.kind == 'DGT':
            return [Digit, Number]
        raise ParserError()

    
    def parseInteger(self, token):
        print('parsing Integer')
        if token.kind == 'DOT':
            return [Dot, Integer]
        if token.kind == 'DGT':
            return [Digit, Integer]
        raise ParserError()

    def parseDigit(self, token):
        print('parsing Digit')
        if token.kind == 'DGT':
            print('Digit reached', token)
            self.lexer.get_next_token()
            self.stack.pop()
            return int(token.value)
        raise ParserError()
        
    def visitDigit(self, node):
        """
        Digit(1)
        """
        number = int(node.value)
        return number

    def visitInteger(self, node):
        """
        Integer(Digit(1), Digit(2))
        """
        number = "".join(
            str(self.visit(d)) for d in node.digits
        )
        return int(number)

    def visitNumber(self, node):
        """
        Number(Integer(Digit(1), Digit(2)), Integer(Digit(3)))
        """
        number = "{0}.{1}".format(
            self.visit(node.integral),
            self.visit(node.fraction)
        )
        return float(number)

class Node:
    pass


class Terminal(Node):
    def __init__(self, value):
        self.value = value

        
class Digit(Terminal):
    pass


class Dot(Terminal):
    pass


class NonTerminal(Node):
    pass


class Number(NonTerminal):
    def __init__(self, integral, fraction):
        self.integral = integral
        self.fraction = fraction

class Integer(NonTerminal):
    def __init__(self, digit, integral):
        self.digit = digit
        self.integral = integral


p = Parser()
p.parse('3.5')
