#!/usr/bin/env python3
"""
TSC - Transient Server Client
Messenger based on multiprocessing Listener-Client.
Server awaits message for specified amount of time and shuts down.
Each new message reset timer.
"""
import sys
import errno
from socket import error as socket_error
import signal
from multiprocessing.connection import Listener
from multiprocessing.connection import Client
from contextlib import contextmanager


@contextmanager
def transient_server(address, authkey, seconds):
    class TimeoutException(Exception):
        pass
    def timeout_handler(signum, frame):
        raise TimeoutException('end for listener')
    signal.signal(signal.SIGALRM, timeout_handler)
    server = Listener(address, authkey=authkey)
    try:
        yield server
    except TimeoutException:
        server.close()


def start_server(configuration):
    messages = list()
    configuration.pop('message')
    with transient_server(**configuration) as server:
        while True:
            signal.alarm(configuration.get('seconds'))
            client = server.accept()
            message = client.recv()
            print('received {}'.format(message))
            messages.append(message)
    return messages


def send_message_to_server(configuration):
    address = configuration.get('address')
    authkey = configuration.get('authkey')
    message = configuration.get('message')
    try:
        client = Client(address, authkey=authkey)
        client.send(message)
        client.close()
    except socket_error as serr:
        if serr.errno != errno.ECONNREFUSED:
            raise serr
        print('client couldn\'t connect')


if __name__ == '__main__':
    try:
        mode = dict(
            server=start_server,
            client=send_message_to_server
        ).get(
            sys.argv[1]
        )
    except IndexError:
        print('start server or client')
        exit(1)
    configuration = dict(
        address=('localhost', 25000),
        authkey=b'peekaboo',
        seconds=5,
        message=None
    )
    messages = mode(configuration)
    if messages:
        print(messages)
    print('{} finished'.format(sys.argv[0]))
