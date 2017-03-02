import signal
from multiprocessing.connection import Listener
from contextlib import contextmanager


class TimeoutException(Exception):
    pass

def timeout_handler(signum, frame):
    raise TimeoutException('end for listener')

@contextmanager
def set_timeout(seconds=None):
    signal.signal(signal.SIGALRM, timeout_handler)
    signal.alarm(seconds)
    yield

def message_server(address, authkey):
    server = Listener(address, authkey=authkey)
    messages = list()
    with set_timeout(seconds=5):
        while True:
            try:
                client = server.accept()
                message = client.recv()
                messages.append(message)
            except TimeoutException:
                break
    return messages


messages = message_server(('', 25000), authkey=b'peekaboo')
print messages
