#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <signal.h>
#include <openssl/evp.h>

#include "server.h"


int main(int argc, char *argv[]) {
  int                 sock_fd;
  int                 i;
  int                 yes = 1;
  struct sockaddr_in  local_addr;
  const EVP_MD       *md;

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

  md = EVP_sha256();       // Using sha256 for these

  for (i = 0; i < SERVER_WORKERS; i++) {
    if (!fork()) { // The child
      int   new_fd;
      int   bytes_in = 0;
      int   res_len  = 0;
      char  result[RESULT_SIZE + NONCE_SIZE + 1];
      char *nonce_buf;
      char *ptr, tmp_char;
      unsigned int tmp_value, value;
 
      EVP_MD_CTX    *base_sha = NULL;
      EVP_MD_CTX    *test_sha = NULL;
      unsigned char  md_value[EVP_MAX_MD_SIZE];
      unsigned int   nonce_num = 1;
      int            nonce_len = 0;
      int            md_len = 0;

      base_sha = EVP_MD_CTX_create();
      test_sha = EVP_MD_CTX_create();

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

        nonce_buf = (char *)result + bytes_in + 1;

        EVP_DigestInit_ex(base_sha, md, NULL);
        EVP_DigestUpdate(base_sha, result, bytes_in);

        do {
          ptr   = nonce_buf;
          value = nonce_num;
          nonce_len = 0;
          do {
            tmp_value = value;
            value /= 16;
            *ptr++ = HEX_CRIB[15 + (tmp_value - value * 16)];
            nonce_len++;
          } while ( value );


          EVP_MD_CTX_copy_ex(test_sha, base_sha);
          EVP_DigestUpdate(test_sha, nonce_buf, nonce_len);

          EVP_DigestFinal_ex(test_sha, md_value, &md_len);
          EVP_MD_CTX_cleanup(test_sha);
        } while ((md_value[31] != 0) && (nonce_num++));

        if (send(new_fd, result, nonce_len + bytes_in + 1, 0) == -1) {
          printf("Failed to return completed string: %s\n", result);
        }

        // Cleanup
        close(new_fd);
        EVP_MD_CTX_cleanup(base_sha);
        nonce_num = 1;
      }
    }
  }

  wait(NULL);
  return 0;
}

