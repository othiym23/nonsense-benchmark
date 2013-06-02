CC=gcc
CFLAGS=-O2 -I. -pthread
DEPS=server.h
OBJ=
LIBS=-lssl -lcrypto

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

all: server

server: server.o $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)


clean:
	rm -f *.o

distclean: clean
	rm -f server

