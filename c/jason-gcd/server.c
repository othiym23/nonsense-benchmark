#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>

#include <dispatch/dispatch.h>
#include <CommonCrypto/CommonDigest.h>

#define QUERY_MAX        1024
#define HANDSHAKE        "ok\n"
#define HANDSHAKE_SIZE   3
#define HASH_STR_SIZE    64
#define HEX_CRIB         "fedcba9876543210123456789abcdef"

int itoa_16(unsigned int value, char* result) {
  char* ptr = result, tmp_char;
  unsigned int tmp_value, len = 0;

  do {
    tmp_value = value;
    value /= 16;
    *ptr++ = HEX_CRIB[15 + (tmp_value - value * 16)];
    len++;
  } while ( value );

  return len;
}

void handle_connection(int conn) {
  ssize_t ret = 0;
  char buffer[QUERY_MAX];
  char *nonce_str;
  unsigned char result[HASH_STR_SIZE/2];
  unsigned int nonce = 0, nonce_str_len = 0;
  CC_SHA256_CTX context;

  ret = write(conn, HANDSHAKE, HANDSHAKE_SIZE);
  if (ret == -1) {
    fprintf(stderr, "Failed to write handshake to client\n");
    exit(-1);
  }

  ret = read(conn, buffer, HASH_STR_SIZE);
  if (ret == -1) {
    fprintf(stderr, "Failed to read query from client\n");
    exit(-1);
  }

  buffer[HASH_STR_SIZE] = ':';
  nonce_str = (char*)buffer + HASH_STR_SIZE + 1;

  while(true) {
    nonce_str_len = itoa_16(nonce, nonce_str);

    CC_SHA256_Init(&context);
    CC_SHA256_Update(&context, buffer, HASH_STR_SIZE);
    CC_SHA256_Update(&context, nonce_str, nonce_str_len);
    CC_SHA256_Final(result, &context);

    if(result[31] == 0)
      break;

    nonce++;
  }

  ret = write(conn, buffer, HASH_STR_SIZE + 1 + nonce_str_len);
  if (ret == -1) {
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
