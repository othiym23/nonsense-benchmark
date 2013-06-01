CFLAGS=-O3

all: server

server: server.o sha2.o
	$(CC) $(CFLAGS) $^ -o $@

sha2.o: sha2.c
	$(CC) $(CFLAGS) -c $<

server.o: server.c
	$(CC) $(CFLAGS) -c $<

clean:
	-rm -rf *.o server