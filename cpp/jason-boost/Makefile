CFLAGS=-O3
LDFLAGS=-L/usr/local/Cellar/boost/1.53.0/lib -lboost_system-mt -lssl -lcrypto
CC=g++

all: simple_server

simple_server: simple_server.o
	$(CC) $(CFLAGS) $^ -o $@  $(LDFLAGS)

simple_server.o: simple_server.cpp
	$(CC) $(CFLAGS) -c $<

clean:
	-rm -rf *.o simple_server

