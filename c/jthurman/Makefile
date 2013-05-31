CC=gcc
CFLAGS=-O2 -I.
DEPS=nonce.h server.h
OBJ=nonce.o
LIBS=-lssl -lcrypto

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

all: client server nonce_test

client: client.o $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)


server: server.o $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)


#
# Exe for manual testing
#
nonce_test: nonce_test.o $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)

clean:
	rm -f *.o

distclean: clean
	rm -f client
	rm -f nonce_test
	rm -f server

