CFLAGS=-O3

all: server

server: server.o
	$(CC) $(CFLAGS) $^ -o $@

server.o: server.c
	$(CC) $(CFLAGS) -c $<

clean:
	-rm -rf *.o server
