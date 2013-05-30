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


int main() {
  int                 sock_fd;
  int                 new_fd;
  int                 sin_size;
  int                 yes = 1;
  unsigned long       time_in_micros;
  struct timeval      tv_start, tv_end;
  struct sockaddr_in  local_addr;
  struct sockaddr_in  remote_addr;
  struct sigaction    sa;
  const char          handshake[] = "OK\r\n";

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

  if (listen(sock_fd, MAX_BACKLOG) == -1) {
    printf("Unable to listen\n");
    exit(1);
  }
  printf("Listening on port %d\n", SERVER_PORT);

  sa.sa_handler = sigchild_handler;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_RESTART;

  if (sigaction(SIGCHLD, &sa, NULL) == -1) {
    printf("Problem with sigaction\n");
    exit(1);
  }

  printf("\n\nClient           | Milliseconds | Result\n");
  printf("-----------------+--------------+-------------------------------------------\n");
  // Start taking connections
  while (1) {
    sin_size = sizeof(struct sockaddr_in);

    if ((new_fd = accept(sock_fd, (struct sockaddr *)&remote_addr, &sin_size)) == -1 ) {
      printf("Unable to accept connection!\n");
      continue;
    }

    gettimeofday(&tv_start, NULL);


    if (!fork()) { // The child
      close(sock_fd);  // doesn't need to listen
      char           buf[MAX_BUF_SIZE];
      char           result[(MAX_BUF_SIZE * 2) + 1];
      unsigned char *nonce;
      int            bytes_in = 0;

      if (send(new_fd, handshake, sizeof(handshake), 0) == -1) {
        printf("Failed to send handshake\n");
      }

      if ((bytes_in = recv(new_fd, buf, MAX_BUF_SIZE - 1, 0)) == -1) {
        printf("Failed to recv data\n");
      } else {
        buf[bytes_in] = '\0';
        if (bytes_in > 1) buf[bytes_in - 2] = '\0';     // Strip the \r\n

        nonce = find_nonce(buf);

        sprintf(result, "%s:%s\r\n", buf, nonce);

        if (send(new_fd, result, strlen(result), 0) == -1) {
          printf("Failed to return completed string: %s\n", result);
        }
      }
      close(new_fd);
      gettimeofday(&tv_end, NULL);
      time_in_micros = ((1000000 * tv_end.tv_sec) + tv_end.tv_usec) - ((1000000 * tv_start.tv_sec) + tv_start.tv_usec);

      result[strlen(result) - 2] = '\0';  // strip off the \r\n now that we are done
      printf("%16s | %12d | %s\n", inet_ntoa(remote_addr.sin_addr), (time_in_micros / 1000), result);
      exit(0);
    }
    close(new_fd);
  }

  return 0;
}



void sigchild_handler(int s) {
  while (wait(NULL) > 0);
}

