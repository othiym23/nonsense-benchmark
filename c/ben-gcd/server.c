#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>

#include <dispatch/dispatch.h>
#include <CommonCrypto/CommonDigest.h>

#define QUERY_MAX 1024
#define NONCE_MAX 1024
#define RSP_MAX (QUERY_MAX + NONCE_MAX + 1)

#define HANDSHAKE "ok\n"

size_t compute_nonce(void *msg, size_t msg_len, void *nonce_buf, size_t nonce_buf_size) {
  unsigned char buf[CC_SHA256_DIGEST_LENGTH];
  size_t nonce_len = 0;
  for (int i = 0; ; i++) {
    nonce_len = snprintf(nonce_buf, nonce_buf_size, "%x", i);

    CC_SHA256_CTX ctx;
    CC_SHA256_Init(&ctx);
    CC_SHA256_Update(&ctx, msg, msg_len);
    CC_SHA256_Update(&ctx, nonce_buf, nonce_len);
    CC_SHA256_Final(buf, &ctx);

    if (buf[CC_SHA256_DIGEST_LENGTH-1] == '\0') {
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

int main(int argc, char **argv) {
  int port = 1337;
  int sock = -1;
  int ret = -1;

  struct sockaddr_in addr;
  int len_inet;

  dispatch_queue_t queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);

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

  ret = listen(sock, 2048);
  if (ret == -1) {
    fprintf(stderr, "listen() failed\n");
    exit(-1);
  }

  while (1) {
    int conn = accept(sock, NULL, NULL);
    if (conn == -1) {
      fprintf(stderr, "Failed to accept()\n");
      exit(-1);
    }

    dispatch_async(queue, ^{
      handle_connection(conn);
    });
  }

  return 0;
}
