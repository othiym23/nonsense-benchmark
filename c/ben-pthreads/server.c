#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <sched.h>

#ifdef __APPLE__

#include <CommonCrypto/CommonDigest.h>
#define NS_SHA_CTX    CC_SHA256_CTX
#define NS_SHA_INIT   CC_SHA256_Init
#define NS_SHA_UPDATE CC_SHA256_Update
#define NS_SHA_FINAL(CTX, BUF)  CC_SHA256_Final((BUF), (CTX))
#define NS_SHA_LENGTH CC_SHA256_DIGEST_LENGTH

#else /* !__APPLE__ */

#include "sha2.h"
#define NS_SHA_CTX    sha256_ctx
#define NS_SHA_INIT   sha256_init
#define NS_SHA_UPDATE sha256_update
#define NS_SHA_FINAL(CTX, BUF)  sha256_final((CTX), (BUF))
#define NS_SHA_LENGTH SHA256_DIGEST_SIZE

#endif

#define QUERY_MAX 1024
#define NONCE_MAX 1024
#define RSP_MAX (QUERY_MAX + NONCE_MAX + 1)

#define HANDSHAKE "ok\n"

#define NTHREADS 64

/*
 * Stupid thread-safe queue implementation for passing work off to worker
 * threads from the main thread. This is LIFO, not FIFO (so 'queue' is a bit of
 * a misnomer), because I'm lazy.
 */

typedef struct _queue {
  size_t size;
  size_t capacity;
  void **buffer;
  pthread_mutex_t lock;
  pthread_cond_t empty;
  pthread_cond_t full;
} queue_t;

void queue_init(queue_t *queue, size_t capacity) {
  queue->size = 0;
  queue->capacity = capacity;
  queue->buffer = malloc(capacity * sizeof(void *));
  pthread_mutex_init(&queue->lock, NULL);
  pthread_cond_init(&queue->empty, NULL);
  pthread_cond_init(&queue->full, NULL);
}

void queue_push(queue_t *q, void *item) {
  pthread_mutex_lock(&q->lock);
  while (q->size >= q->capacity) {
    pthread_cond_wait(&q->full, &q->lock);
  }
  q->buffer[q->size] = item;
  q->size++;
  pthread_cond_signal(&q->empty);
  pthread_mutex_unlock(&q->lock);
}

void * queue_pop(queue_t *q) {
  void *item;
  pthread_mutex_lock(&q->lock);
  while (q->size == 0) {
    pthread_cond_wait(&q->empty, &q->lock);
  }
  item = q->buffer[q->size-1];
  q->size--;
  pthread_cond_signal(&q->full);
  pthread_mutex_unlock(&q->lock);
  return item;
}

size_t generate_nonce(int n, char *buf, size_t buf_size) {
  size_t i;
  static char *crib = "0123456789abcdef";
  for (i = 0; i < buf_size && n; i++) {
    buf[i] = crib[(n & 0xf)];
    n = n >> 4;
  }
  return i;
}

size_t compute_nonce(void *msg, size_t msg_len, void *nonce_buf, size_t nonce_buf_size) {
  unsigned char buf[NS_SHA_LENGTH];
  size_t nonce_len = 0;

  NS_SHA_CTX basis, ctx;
  NS_SHA_INIT(&basis);
  NS_SHA_UPDATE(&basis, msg, msg_len);

  for (int i = 0; ; i++) {
    nonce_len = generate_nonce(i, nonce_buf, nonce_buf_size);

    memcpy(&ctx, &basis, sizeof(NS_SHA_CTX));
    NS_SHA_UPDATE(&ctx, nonce_buf, nonce_len);
    NS_SHA_FINAL(&ctx, buf);

    if (buf[NS_SHA_LENGTH-1] == '\0') {
      return nonce_len;
    }
  }
}

void handle_connection(int conn) {
  const char *handshake = "ok\n";
  char rsp_buf[RSP_MAX];
  char query[QUERY_MAX];
  char nonce[NONCE_MAX];
  ssize_t query_len = 0;
  ssize_t nonce_len = 0;
  ssize_t write_len = 0;

  write_len = write(conn, HANDSHAKE, 3);
  if (write_len == -1) {
    fprintf(stderr, "Failed to write handshake to client\n");
    exit(-1);
  }

  query_len = read(conn, query, QUERY_MAX);
  if (query_len == -1) {
    fprintf(stderr, "Failed to read query from client\n");
    exit(-1);
  }

  nonce_len = compute_nonce(query, query_len, &nonce, NONCE_MAX);

  memcpy(rsp_buf, query, query_len);
  rsp_buf[query_len] = ':';
  memcpy(&rsp_buf[query_len+1], nonce, nonce_len);

  write_len = write(conn, rsp_buf, (query_len + 1 + nonce_len));
  if (write_len == -1) {
    fprintf(stderr, "Failed to write nonce to client\n");
    exit(-1);
  }

  close(conn);
}

void * work_loop(void *ctx) {
  queue_t *q = (queue_t *)ctx;
  while (1) {
    int conn = (int)queue_pop(q);
    handle_connection(conn);
  }
}

int main(int argc, char **argv) {
  int port = 1337;
  int sock = -1;
  int ret = -1;

  pthread_t threads[NTHREADS];
  queue_t q;
  struct sockaddr_in addr;
  int len_inet;

  queue_init(&q, 256);

  sock = socket(PF_INET, SOCK_STREAM, 0);
  if (sock == -1) {
    fprintf(stderr, "Failed to create listening socket\n");
    exit(-1);
  }

  memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  inet_aton("0.0.0.0", &addr.sin_addr);
  len_inet = sizeof(addr);

  ret = bind(sock, (struct sockaddr *)&addr, len_inet);
  if (ret == -1) {
    fprintf(stderr, "Failed to bind() to port %u\n", port);
    exit(-1);
  }

  ret = listen(sock, 1024);
  if (ret == -1) {
    fprintf(stderr, "listen() failed\n");
    exit(-1);
  }

  for (int i = 0; i < NTHREADS; i++) {
    pthread_t *thread = &threads[i];
    ret = pthread_create(thread, NULL, work_loop, &q);
    if (ret != 0) {
      fprintf(stderr, "Failed to spawn thread %d\n", i);
      exit(-1);
    }
  }

  int num = 0;
  while (1) {
    int conn = accept(sock, NULL, NULL);
    if (conn == -1) {
      fprintf(stderr, "Failed to accept()\n");
      exit(-1);
    }
    queue_push(&q, (void *)conn);
  }

  return 0;
}
