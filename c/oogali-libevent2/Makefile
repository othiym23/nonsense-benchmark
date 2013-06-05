PREFIX=/usr/local
CFLAGS+=-Wall -O3 -I$(PREFIX)/include
LDFLAGS+=-L$(PREFIX)/lib
LIBS+=-levent -levent_pthreads -lssl -lcrypto
LD=$(CC)

SRCS=$(wildcard *.c)
OBJS=$(SRCS:%.c=%.o)
BINARY=server

all: $(BINARY)
$(BINARY): $(OBJS)
	$(LD) -o $@ $^ $(LDFLAGS) $(LIBS)

clean:
	$(RM) $(BINARY) $(OBJS)

.PHONY: clean
