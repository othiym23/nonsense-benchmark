from hashlib import sha256

from concurrence import dispatch, Tasklet
from concurrence.io import BufferedStream, Socket

def verify(input, nonce):
    r = sha256(input + nonce).hexdigest()
    return True if r.endswith('00') else False

def compute(input):
    i = 0
    while True:
        nonce = '%x' % i
        if verify(input, nonce):
            return nonce
        i += 1

def handler(client_socket):
    """writes the familiar greeting to client"""
    stream = BufferedStream(client_socket)
    writer = stream.writer
    writer.write_bytes("ok\r\n")
    reader = stream.reader
    buf = reader.read_bytes()
    print buf
    writer.write_bytes(buf + ':' + compute(buf) + '\n')
    writer.flush()
    stream.close()

def server():
    """accepts connections on a socket, and dispatches
    new tasks for handling the incoming requests"""
    server_socket = Socket.new()
    server_socket.bind(('localhost', 1337))
    server_socket.listen()

    while True:
        client_socket = server_socket.accept()
        Tasklet.new(handler)(client_socket)

if __name__ == '__main__':
    dispatch(server)
