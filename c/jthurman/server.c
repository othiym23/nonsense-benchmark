#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <signal.h>

#include "server.h"
#include "nonce.h"


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

  for (i = 0; i < SERVER_WORKERS; i++) {
    if (!fork()) { // The child
      int   new_fd;
      int   bytes_in = 0;
      int   res_len  = 0;
      char  result[RESULT_SIZE];
      char *nonce;

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

        nonce = (char *)result + bytes_in + 1;
        res_len = calc_nonce(result, bytes_in, nonce, RESULT_SIZE - bytes_in - 1) + bytes_in + 1;
        result[res_len] = '\0';

        if (send(new_fd, result, res_len, 0) == -1) {
          printf("Failed to return completed string: %s\n", result);
        }

        close(new_fd);
      }
    }
  }

  wait(NULL);
  return 0;
}

