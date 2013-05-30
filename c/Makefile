CC=gcc
CFLAGS=-O2 -I.
DEPS=nonce.h
OBJ=nonce.o
LIBS=-lssl -lcrypto

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)


#
# Exe for manual testing
#
nonce_test: nonce_test.o $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)

clean:
	rm -f *.o

distclean: clean
	rm -f nonce_test

