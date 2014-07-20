#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import random
import time
from cmd import Cmd
from threading import Thread


class Observer:
    def __init__(self):
        self.events = dict()
        self._terminate_connection = False

    def observe(self, target, event, handler, *args):
        if event not in self.events:
            self.events[event] = []
        self.events[event].append((target, handler, args))

    def unobserve(self, target, event):
        if event not in self.events:
            return
        deletion_list = []
        for i, t in enumerate(self.events[event]):
            if target in t:
                deletion_list.append(i)
        if len(self.events[event]) == len(deletion_list):
            del self.events[event]
            return
        for i in deletion_list:
            self.events[event].pop(i)

    def new_event(self, target, event):
        if event not in target.events:
            return
        listeners = [l for l,h,a in target.events[event]]
        if self not in listeners:
            return
        for listener, handler, args in target.events[event]:
            handler(listener, *args)


class Operator(Observer):
    def __init__(self, device):
        super().__init__()
        self.observe(device, "REQUEST", self.receive_message)

    def receive_message(self, source):
        self.current_source = source
        self.thread = Thread(target=self.process_message)
        self.thread.start()

    def process_message(self):
        cast = True
        while cast and not self._terminate_connection:
            msg("Casting connection")
            time.sleep(1.5)
            # 1/4 chance of True
            cast = bool(random.getrandbits(1))
        self.send_message()
        print("OPERATOR TERMINATE", self)
        self._terminate_connection = False

    def send_message(self):
        self.new_event(self.current_source, "OPERATOR_RESPONSE")

class CloseConnection:
    @staticmethod
    def disconnect(device):
        msg("Hanging up")


class Q1:
    info = "Q1 Ready"

    # CONNECT BLOCK
    @staticmethod
    def S12(device):
        operator = Operator(device)
        device.operator = operator
        device.observe(operator, "OPERATOR_RESPONSE", Q2.S23, device)
        device.new_event(operator, "REQUEST")

        def wait_response(device, operator):
            predicate = lambda: bool(
                device._state in [Q1, Q2] and
                not device._terminate_connection)
            while predicate():
                time.sleep(1)
                if predicate():
                    msg("Waiting operator response...")
            print("PHONE TERMINATE", device, operator)
            device._terminate_connection = False

        thread = Thread(target=wait_response, args=(device, operator,))
        device.thread = thread
        thread.start()


class Q2(CloseConnection):
    info = "Q2 Waiting to connect"

    @staticmethod
    def disconnect(device):
        operator = device.operator
        device.unobserve(operator, "OPERATOR_RESPONSE")
        device._terminate_connection = True
        operator._terminate_connection = True

    # ESTABLISH BLOCK
    @staticmethod
    def S23(operator, device):
        device._blocked = True
        device._state = Q3
        device.status()
        device._blocked = False

class Q3(CloseConnection):
    info = "Q3 Connected"

    # HOLD BLOCK
    @staticmethod
    def S34(device):
        msg("Setting call on hold")


class Q4(CloseConnection):
    info = "Q4 On hold"

    # HOLD BLOCK
    @staticmethod
    def S43(device):
        msg("Retrieving call")


class State:
    def __init__(self, state=None):
        self._state = state
        self.status()
        self._blocked = False

    def __call__(self, state_cls, handler_name=""):
        if self._blocked:
            msg("Resource blocked")
            return
        self._blocked = True
        handler = getattr(self._state, handler_name, None)
        if not handler:
            print(self._state.info, ". Operation not available")
            return
        handler(self)
        self._state = state_cls
        self.status()
        self._blocked = False


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
        msg(self._state.info)

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
        self.device.disconnect()

    def help_hold(self):
        msg("Hold current call")

    def do_hold(self, text):
        self.device.hold()

    def help_unhold(self):
        msg("Retrieve current call")

    def do_unhold(self, text):
        self.device.unhold()

    def help_status(self):
        msg("Get current status")

    def do_status(self, text):
        self.device.status()

    def do_EOF(self, text):
        return True

    def emptyline(self):
        return


phone = Device()
phone.connect()
time.sleep(3)
phone.disconnect()
phone.connect()
phone.disconnect()
phone.connect()
# Prompt(phone)
