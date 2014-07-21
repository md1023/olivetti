#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import random
import time
from cmd import Cmd
from threading import Thread


class Task(Thread):
    def __init__(self, handler, *args):
        self.thread = Thread(target=handler, args=args)
        self._running = True
        self.thread.start()

    def terminate(self):
        print("Terminate task:", self)


class Observer:
    def __init__(self):
        self.events = dict()
        self.tasks = []

    def stop_all_tasks(self):
        deletion_list = []
        for i, t in enumerate(self.tasks):
            if t._running:
                t.terminate()
            deletion_list.append(i)

        if len(deletion_list) == len(self.tasks):
            self.tasks = []

    def new_task(self, handler, *args):
        task = Task(handler, *args)
        self.tasks.append(task)

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
        listeners = [l for l, h, a in target.events[event]]
        if self not in listeners:
            return
        for listener, handler, args in target.events[event]:
            handler(*args)


class Operator(Observer):
    def __init__(self):
        super().__init__()

    def establish_broadcasting(self, device):
        self.observe(device, "REQUEST", self.receive_message, device)

    def receive_message(self, source):
        self.current_source = source
        self.new_task(self.generate_message)

    def generate_message(self):
        cast = True
        while cast:
            time.sleep(1.5)
            # 1/4 chance of True
            cast = bool(random.getrandbits(1))
            # cast = False
        self.new_event(self.current_source, "OPERATOR_RESPONSE")


class CloseConnection:
    @staticmethod
    def disconnect(device):
        msg("Hanging up")
        device.unobserve(device.operator, "OPERATOR_RESPONSE")
        return (Q1, None)


class Q1:
    info = "Q1 Ready"

    @staticmethod
    def S12(device):
        operator = device.operator
        device.observe(operator, "OPERATOR_RESPONSE",
                       device, "S23")
        return (Q2,
                lambda: device.new_event(operator, "REQUEST"))


class Q2(CloseConnection):
    info = "Q2 Waiting to connect"

    @staticmethod
    def S23(device):
        return (Q3, None)


class Q3(CloseConnection):
    info = "Q3 Connected"

    @staticmethod
    def S34(device):
        return (Q4, None)


class Q4(CloseConnection):
    info = "Q4 On hold"

    @staticmethod
    def S43(device):
        return (Q3, None)


class State:
    def __init__(self, state=None):
        self._state = state
        self.status()
        self._blocked = False

    def __call__(self, handler_name=""):
        self._blocked = True

        handler = getattr(self._state, handler_name, None)
        if not handler:
            print("Operation not available", self._state, handler_name)
            self._blocked = False
            return

        new_state, handle_after = handler(self)
        if new_state:
            self._state = new_state
            self.status()

        self._blocked = False

        if handle_after:
            handle_after()


class Device(State, Observer):
    def __init__(self):
        Observer.__init__(self)
        State.__init__(self, Q1)
        self.operator = Operator()
        self.operator.establish_broadcasting(self)

    def connect(self):
        self("S12")

    def disconnect(self):
        self("disconnect")

    def hold(self):
        self("S34")

    def unhold(self):
        self("S43")

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

if __name__ == "__main__":
    phone = Device()
    Prompt(phone)
