from hashlib import sha256

from twisted.internet.protocol import Protocol, Factory
from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.internet import reactor


def verify(input, nonce):
    r = sha256(input + nonce).hexdigest()
    return True if r.endswith('00') else False

def compute_nonce(input):
    i = 0
    while True:
        nonce = '%02x' % i
        if verify(input, nonce):
            return input + ':' + nonce
        i += 1

class Nonsense(Protocol):

    def connectionMade(self):
        #print 'Connection made'
        self.transport.write('ok\n')

    def dataReceived(self, data):
        #print 'Data Received', data
        nonce = compute_nonce(data)
        #print 'Sending', nonce
        self.transport.write(nonce)
        self.transport.loseConnection()

class NonsenseFactory(Factory):
    protocol = Nonsense

endpoint = TCP4ServerEndpoint(reactor, 1337)
endpoint.listen(NonsenseFactory())
reactor.run()
