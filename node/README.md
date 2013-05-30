## NODE 0.10.x IS REQUIRED

I use streams2 in the client and don't feel like backporting. Thank you for
your attention.

### installation

`cd node/clients ; npm install`

### the servers

There's a simple, single-threaded reference server to use as a baseline in
`reference-server`. There's absolutely nothing fancy about it.

There's also a server that uses Node's built-in multiprocess clustering
module in `multi-worker-server`. It scales to the number of cores in your
system. It's much faster but not a whole lot more complicated.

### implementation details & defensive blamestorming

The whole point of a proof-of-work service is to be CPU-intensive, and at least
in the case of Node, this benchmark tests the speed of context-switching
between C++ and JavaScript as well as the speed of the OpenSSL SHA256
implementation.

The Node code has been pretty extensively profiled by this point, and is
probably about as fast as it's going to get without lots of clever trickery.
