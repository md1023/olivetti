#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import random
import time
from cmd import Cmd
from threading import Thread


class Observer:
    def __init__(self):
        print("observer init", self)
        self.events = dict()

    def observe(self, target, event, handler, *args):
        if event not in self.events:
            self.events[event] = []
        self.events[event].append((target, handler, args))

    def send_request(self, target, event):
        print("SEND:", self, self.events, ";", target, target.events, event)
        listeners, handlers, args = zip(*target.events[event])
        if self not in listeners:
            return
        for listener, handler, args in target.events[event]:
            handler(self, *args)


class Operator(Observer):
    def __init__(self, device):
        super().__init__()
        self.observe(device, "REQUEST", self.receive_message)

    def receive_message(self, source):
        print("Operator received message")
        self.current_source = source
        self.thread = Thread(target=self.process_message)
        self.thread.start()

    def process_message(self):
        cast = True
        while cast:
            print("Casting connection")
            time.sleep(1.5)
            # 1/4 chance of True
            cast = bool(random.getrandbits(2))
        self.send_message(self.current_source)

    def send_message(self, source):
        self.send_request(self.current_source, "OPERATOR_RESPONSE")


class CloseConnection:
    @staticmethod
    def disconnect(device):
        print("Hanging up")


class Q1:
    info = "Q1 Ready"

    # CONNECT BLOCK
    @staticmethod
    def S12(device):
        print("Connecting to operator...")
        operator = Operator(device)
        device.observe(operator, "OPERATOR_RESPONSE", device, Q2, "S23")
        device.send_request(operator, "REQUEST")
        while device._state is Q1:
            print("Waiting operator response...", device._state)
            time.sleep(1)
        print("Operator finished: %s", operator.thread.is_alive())

class Q2(CloseConnection):
    info = "Q2 Waiting to connect"

    # ESTABLISH BLOCK
    @staticmethod
    def S23(device):
        print("Operator connected")


class Q3(CloseConnection):
    info = "Q3 Connected"

    # HOLD BLOCK
    @staticmethod
    def S34(device):
        print("Setting call on hold")


class Q4(CloseConnection):
    info = "Q4 On hold"

    # HOLD BLOCK
    @staticmethod
    def S43(device):
        print("Retrieving call")


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


class Device(State, Observer):
    def __init__(self):
        Observer.__init__(self)
        State.__init__(self, Q1)

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
        msg("Retrieve current call")

    def do_unhold(self, text):
        self.device.unhold()


phone = Device()
phone.connect()
# Prompt(phone)
