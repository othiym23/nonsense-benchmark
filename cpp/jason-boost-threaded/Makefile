CFLAGS=-O3
LDFLAGS=-L/usr/local/Cellar/boost/1.53.0/lib -lboost_system-mt -lboost_thread-mt -lssl -lcrypto
CC=g++

all: server

server: server.o
	$(CC) $(CFLAGS) $^ -o $@  $(LDFLAGS)

server.o: server.cpp
	$(CC) $(CFLAGS) -c $<

clean:
	-rm -rf *.o server

