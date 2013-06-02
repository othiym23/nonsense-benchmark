#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "server.h"


int main(int argc, char *argv[]) {
  int                 sock_fd;
  int                 i;
  int                 yes = 1;
  struct sockaddr_in  local_addr;

  if ((sock_fd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    printf("Unable to create socket\n");
    exit(1);
  }

  if (setsockopt(sock_fd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) == -1) {
    printf("Unable to set socket options\n");
    exit(1);
  }

  local_addr.sin_family      = AF_INET;
  local_addr.sin_port        = htons(SERVER_PORT);
  local_addr.sin_addr.s_addr = INADDR_ANY;
  memset(&(local_addr.sin_zero), '\0', 8);

  if ((bind(sock_fd, (struct sockaddr *)&local_addr, sizeof(struct sockaddr))) == -1) {
    printf("Unable to bind to port %d\n", SERVER_PORT);
    exit(1);
  }

  // Prefork some workers
  for (i = 0; i < SERVER_WORKERS; i++) {
    if (!fork()) { // The child
      int           new_fd;
      int           bytes_in = 0;
      char          result[RESULT_SIZE + NONCE_SIZE + 1];
      char         *nonce_buf;
      char         *ptr, tmp_char;
      unsigned int  tmp_value, value;
 
      NS_SHA_CTX     base_sha;
      NS_SHA_CTX     test_sha;
      unsigned char  md_value[32];
      unsigned int   nonce_num = 1;
      int            nonce_len = 0;

      if (listen(sock_fd, SERVER_BACKLOG) == -1) {
        printf("Unable to listen\n");
        exit(1);
      }

      // Start taking connections
      while (1) {
        if ((new_fd = accept(sock_fd, NULL, NULL)) == -1 ) {
          printf("Unable to accept connection!\n");
          continue;
        }

        if (send(new_fd, SERVER_HELLO, SERVER_HELLO_LEN, 0) == -1) {
          printf("Failed to send handshake\n");
        }

        if ((bytes_in = recv(new_fd, result, CLIENT_REQ_SIZE, 0)) < 1) {
          printf("Invalid data, goodbye!\n");
          close(new_fd);
          continue;
        }
        result[bytes_in] = ':';

        NS_SHA_INIT(&base_sha);
        NS_SHA_UPDATE(&base_sha, result, bytes_in);

        nonce_buf = (char *)result + bytes_in + 1;

        do {
          // Convert nonce_num to a hex string
          value     = nonce_num;
          nonce_len = 0;
          do {
            nonce_buf[nonce_len] = HEX_CRIB[(value & 0xf)];
            value = value >> 4;
            nonce_len++;
          } while ( value );


          // Copy the base_sha to test_sha so we don't rehash the base string
          memcpy(&test_sha, &base_sha, sizeof(NS_SHA_CTX));

          // Update the sha, and grab the result
          NS_SHA_UPDATE(&test_sha, nonce_buf, nonce_len);
          NS_SHA_FINAL(&test_sha, md_value);
        } while ((md_value[NS_SHA_LENGTH - 1] != 0) && (nonce_num++));

        if (send(new_fd, result, nonce_len + bytes_in + 1, 0) == -1) {
          printf("Failed to return completed string: %s\n", result);
        }

        // Cleanup
        close(new_fd);
        nonce_num = 1;
      }
    }
  }

  wait(NULL);
  return 0;
}

