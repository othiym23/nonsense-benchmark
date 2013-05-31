#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/queue.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <event2/event.h>
#include <event2/thread.h>
#include <event2/util.h>
#include <event2/listener.h>
#include <event2/buffer.h>
#include <event2/bufferevent.h>
#include <openssl/ssl.h>
#include <openssl/sha.h>

#define BINDADDR "0.0.0.0"
#define PORT 1337
#define OK "ok\n"

void handle_nonce(struct bufferevent *bev, void *ptr) {
  struct evbuffer *output = NULL;
  unsigned char digest_hash[32];
  char digest_string[SHA256_DIGEST_LENGTH + 1], nonce[1024];
  int i;
  size_t nonce_len;

  output = bufferevent_get_output(bev);
  if (output == NULL) {
    fprintf(stderr, "bufferevent_get_output: could not get handle for output buffer\n");
    return;
  }

  bzero(&nonce, sizeof(nonce));
  nonce_len = bufferevent_read(bev, nonce, sizeof(nonce));
  if (nonce_len == -1) {
    fprintf(stderr, "bufferevent_read: could not read incoming nonce\n");
    return;
  }

  bzero(&digest_hash, sizeof(digest_hash));
  SHA256((unsigned char *)nonce, nonce_len, digest_hash);

  bzero(&digest_string, sizeof(digest_string));
  for (i = 0; i < sizeof(digest_hash); i++) {
    snprintf((digest_string + (i * 2)), 3, "%02x", digest_hash[i]);
  }

  evbuffer_add_printf(output, "%s:%s\n", nonce, digest_string);
}

void conn_event_handler(struct bufferevent *bev, short events, void *ptr) {
  if (events & BEV_EVENT_READING) {
    perror("received error while reading buffered event");
  }

  if (events & BEV_EVENT_WRITING) {
    perror("received error while writing buffered event");
  }

  if (events & BEV_EVENT_ERROR) {
    perror("received unrecoverable error while handling buffered event");
  }

  if (events & (BEV_EVENT_READING | BEV_EVENT_WRITING | BEV_EVENT_EOF | BEV_EVENT_ERROR | BEV_EVENT_TIMEOUT)) {
    bufferevent_free(bev);
  }
}

void incoming_conn(struct evconnlistener *evc, evutil_socket_t sock, struct sockaddr *addr, int socklen, void *ptr) {
  struct event_base *eb = NULL;
  struct bufferevent *bev = NULL;
  struct evbuffer *output = NULL;

  eb = evconnlistener_get_base(evc);
  if (eb == NULL) {
    fprintf(stderr, "evconnlistener_get_base: could not get event base for incoming connection\n");
    return;
  }

  bev = bufferevent_socket_new(eb, sock, BEV_OPT_THREADSAFE | BEV_OPT_CLOSE_ON_FREE);
  if (bev == NULL) {
    fprintf(stderr, "bufferevent_socket_new: could not allocate new event buffer for incoming connection\n");
    return;
  }

  output = bufferevent_get_output(bev);
  if (output == NULL) {
    fprintf(stderr, "bufferevent_get_output: could not get handle for output buffer\n");
    return;
  }

  evbuffer_add(output, OK, strlen(OK));

  bufferevent_setcb(bev, handle_nonce, NULL, conn_event_handler, NULL);
  if (bufferevent_enable(bev, EV_READ) < 0) {
    fprintf(stderr, "bufferevent_enable: could not enable reading of buffered events\n");
    return;
  }
}

void conn_error(struct evconnlistener *evc, void *ptr) {
  struct event_base *eb = NULL;

  event_base_loopexit(eb, NULL);
}

struct evconnlistener *init_server(struct event_base *eb, char *bindaddr, int bindport)
{
  struct evconnlistener *evc = NULL;
  struct sockaddr_in sin;

  bzero(&sin, sizeof(struct sockaddr));
  sin.sin_family = AF_INET;
  sin.sin_addr.s_addr = inet_addr(bindaddr);
  sin.sin_port = htons(bindport);

  /* initialize libevent listener object, and bind */
  evc = evconnlistener_new_bind(eb, incoming_conn, NULL, LEV_OPT_CLOSE_ON_FREE | LEV_OPT_REUSEABLE, -1, (struct sockaddr *)&sin, sizeof(struct sockaddr));
  if (evc == NULL) {
    return NULL;
  }

  evconnlistener_set_cb(evc, incoming_conn, NULL);
  evconnlistener_set_error_cb(evc, conn_error);

  return evc;
}

void usage(void)
{
  fprintf(stderr, "nonsense server benchmark\n");
  fprintf(stderr, "usage: nonsense-server [arguments]\n\n");
  fprintf(stderr, "Arguments:\n");
  fprintf(stderr, "  -b, --bind <ip:port>\t\tIP address and port for nonsense server to bind to\n");
  exit(1);
}

int main(int argc, char **argv)
{
  int o;
  struct event_base *eb = NULL;
  struct evconnlistener *evc = NULL;
  char bindaddr[16];
  char *colon = NULL;
  unsigned int bindport = PORT;
  struct option opts[] = {
    { "bind", required_argument, NULL, 'b' },
    { NULL, 0, NULL, 0 }
  };
  
  /* zero out ip strings */
  bzero(&bindaddr, sizeof(bindaddr));
  
  /* parse arguments */
  while ((o = getopt_long(argc, argv, "b:r:", opts, NULL)) != -1) {
    switch(o) {
      case 'b':
        colon = index(optarg, ':');
        if (colon != NULL) {          
          strncpy(bindaddr, optarg, strlen(optarg) - strlen(colon));
          bindport = atoi(colon + 1);
        }
        break;
      case '?':
      default:
        usage();
    }
  }
  argc -= optind;
  argv += optind;
  
  /* set default bind and redis ips if none given */
  if (*bindaddr == 0) {
    strncpy(bindaddr, BINDADDR, sizeof(bindaddr));
    bindport = PORT;
  }

  /* initialize ssl engine */
  SSL_load_error_strings();
  SSL_library_init();

  /* select pthreads and initialize libevent library */
  if (evthread_use_pthreads() != 0) {
    fprintf(stderr, "evthread_use_pthreads: could not select pthreads for libevent\n");
    return -1;
  }

  eb = event_base_new();
  if (eb == NULL) {
    fprintf(stderr, "event_base_new: could not initialize libevent API\n");
    return -1;
  }

  if (evthread_make_base_notifiable(eb) != 0) {
    fprintf(stderr, "evthread_make_base_notifiable: could not enable thread wake up for libevent\n");
    return -1;
  }

  printf("kernel event notification method: %s\n", event_base_get_method(eb));

  /* initialize server */
  evc = init_server(eb, bindaddr, bindport);

  /* enter main event loop */
  event_base_dispatch(eb);

  if (evc != NULL)
    evconnlistener_free(evc);

  if (eb != NULL)
    event_base_free(eb);

  return 0;
}
