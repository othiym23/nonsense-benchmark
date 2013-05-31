#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <netdb.h>
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
  int                 bytes_in = 0;
  int                 sock_fd;
  int                 sin_size;
  char                out[MAX_BUF_SIZE];
  char                buf[(MAX_BUF_SIZE * 2) + 1];
  char                res[(MAX_BUF_SIZE * 2) + 1];
  unsigned char      *nonce;
  struct hostent     *server_host;
  struct sockaddr_in  server_addr;


  if (argc != 3) {
    printf("usage: client hostname string\n");
    exit(1);
  }

  if (strlen(argv[2]) > (MAX_BUF_SIZE - 3)) {  // Need a \r\n\0
    printf("string is too long. Max: %d\n", MAX_BUF_SIZE);
    exit(1);
  }
  sprintf(out, "%s\r\n", argv[2]);

  if ((server_host = gethostbyname(argv[1])) == NULL) {
    printf("Unable to identify host: %s\n", argv[1]);
    exit(1);
  }

  if ((sock_fd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    printf("Unable to create socket\n");
    exit(1);
  }

  server_addr.sin_family = AF_INET;
  server_addr.sin_port   = htons(SERVER_PORT);
  server_addr.sin_addr   = *((struct in_addr *)server_host->h_addr);
  memset(&(server_addr.sin_zero), '\0', 8);

  if (connect(sock_fd, (struct sockaddr *)&server_addr, sizeof(struct sockaddr)) == -1) {
    printf("Failed to connect to: %s:%d\n", argv[1], SERVER_PORT);
    exit(1);
  }

  if ((bytes_in = recv(sock_fd, buf, MAX_BUF_SIZE - 1, 0)) == -1) {
    printf("No response from host.\n");
    exit(1);
  }

  buf[bytes_in] = '\0';

  if (strncmp(buf, SERVER_HELLO, bytes_in) != 0) {
    printf("Invalid hello from server.\n");
    exit(1);
  }

  if (send(sock_fd, out, strlen(out), 0) == -1) {
    printf("Failed to send string: %s\n", argv[2]);
    exit(1);
  }

  // Now get the result
  if ((bytes_in = recv(sock_fd, buf, (MAX_BUF_SIZE * 2), 0)) == -1) {
    printf("No result from host.\n");
    exit(1);
  }

  close(sock_fd);


  buf[bytes_in] = '\0';
  if (bytes_in > 1) buf[bytes_in - 2] = '\0';  // strip off \r\n

  // Calculate it ourselves to make sure
  nonce = find_nonce(argv[2]);

  sprintf(res, "%s:%s", argv[2], nonce);

  if (strcmp(res, buf) != 0) {
    printf("Result is wrong!\n");
  } else {
    printf("Result is correct!\n");
  }

  printf("   Ours:  %s\n", res);
  printf(" Theirs:  %s\n", buf);

  return 0;
}

