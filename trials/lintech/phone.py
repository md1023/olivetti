#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from cmd import Cmd


class CloseConnection:
    @staticmethod
    def disconnect(device):
        print("hang up")


class Q1:
    info = "Q1 Ready"

    # CONNECT BLOCK
    @staticmethod
    def S12(device):
        print("connecting", device)


class Q2(CloseConnection):
    info = "Q2 Connecting"

    # ESTABLISH BLOCK
    @staticmethod
    def S23(device):
        pass


class Q3(CloseConnection):
    info = "Q3 Connected"

    # HOLD BLOCK
    @staticmethod
    def S34(device):
        pass


class Q4(CloseConnection):
    info = "Q4 On hold"

    # HOLD BLOCK
    @staticmethod
    def S43(device):
        pass


class State:
    def __init__(self, state=None):
        self._state = state

    def __call__(self, state_cls, handler_name=None):
        self.status()
        handler = getattr(self._state, handler_name)
        if handler:
            handler(self)
            self._state = state_cls
        self.status()


class Device(State):
    def __init__(self):
        super().__init__(Q1)

    def connect(self):
        self(Q2, "S12")

    def disconnect(self):
        self(Q1, "disconnect")

    def hold(self):
        self(Q4, "S34")

    def unhold(self):
        self(Q3, "S43")

    def status(self):
        print(self._state.info)

def msg(message):
    print(message)


class Prompt(Cmd):
    def __init__(self, device):
        super().__init__()
        self.device = device
        self.prompt = "(Phone) "
        self.cmdloop("(Phone) Type help for commands")

    def help_help(self):
        msg("Use the phone")

    def help_quit(self):
        msg("Stop execution")

    def do_quit(self, message):
        return True

    def help_connect(self):
        msg("Call operator")

    def do_connect(self, text):
        self.device.connect()

    def help_disconnect(self):
        msg("Hang up")

    def do_disconnect(self, text):
        self.device.connect()

    def help_hold(self):
        msg("Hold current call")

    def do_hold(self, text):
        self.device.hold()

    def help_unhold(self):
        msg("Return current call")

    def do_unhold(self, text):
        self.device.unhold()


d = Device()
d.connect()
# Prompt(d)
