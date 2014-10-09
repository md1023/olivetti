#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re

class State(object):
    def transition_handler(self, input, regexp, state):
        # print self, state, input,regexp
        if re.match(regexp, input, flags=re.IGNORECASE):
            return state

class S0(State):
    def run(self, input):
        return self.transition_handler(input, "[A-Z]", "S1")

class S1(State):
    def run(self, input):
        return self.transition_handler(input, "[A-Z0-9]", "S1") or \
            self.transition_handler(input, "[.-]", "S2")

class S2(State):
    def run(self, input):
        return self.transition_handler(input, "[A-Z0-9]", "S1") or \
            self.transition_handler(input, "[.-]", "S2")

class Machine(object):
    def __init__(self):
        self.states = []
        self.runs = 0
        self.current_state = None

    def reset(self, state):
        self.runs = 0
        self.current_state = state

    def run(self, input):
        self.current_state = self.states[self.current_state].run(input)
        return self.current_state

class LoginChecker(object):
    def __init__(self, login_length = None):
        self.login_length = login_length
        self.machine = Machine()
        self.machine.states = dict(S0=S0(), S1=S1(), S2=S2())

    def run(self, login):
        self.machine.reset("S0")
        for c in login.strip():
            if not self.machine.run(c):
                break
            if self.login_length and self.machine.runs >= self.login_length:
                break
        return self.machine.current_state

if __name__ == "__main__":
    pattern = re.compile(
        "^[A-Z]([A-Z0-9.-]{0,18}?[A-Z0-9])?$",
        flags=re.IGNORECASE)

    s1 = "  a" + "1" * 18 + "z"
    s2 = "a" + "0" * 17 + "z  "
    s3 = "a" + "-" * 18 + "z"
    s4 = "z0"
    s5 = "a1.ba1.b"

    l = LoginChecker()

    for s in [s1, s2, s3, s4, s5]:
        m = pattern.match(s.strip())
        assert m.group(0)
        assert l.run(s) == "S1"
