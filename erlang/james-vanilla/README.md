## Erlang

#### Getting it working
First, to install Erlang, I would recommend using homebrew: `brew install erlang`.

Then after installing Erlang, you'll have to compile both modules into a beam file by `erl -compile server hex`. Once this is done you can run it in the `erl` shell or use the command `erl -noshell -s server start`.  This should start the server on port 1337.

Now you can run the node client and you should see it doing it's thing.  Note, that as of right now, there are many packets dropping due to my implementation of the code.
