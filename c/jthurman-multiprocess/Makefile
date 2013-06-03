CC=gcc
CFLAGS=-O2 -I. -pthread
DEPS=server.h
OBJ=
LIBS=-lssl -lcrypto

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

%.s: %.c $(DEPS)
	$(CC) -S -c -o $@ $< $(CFLAGS)

%.lst: %.c $(DEPS)
	$(CC) -c -g -Wa,-a,-ad $(CFLAGS) $< > $@


all: server

server: server.o $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)


clean:
	rm -f *.lst
	rm -f *.o
	rm -f *.s

distclean: clean
	rm -f server

