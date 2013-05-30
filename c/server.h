#ifndef SERVER_H
#define SERVER_H



#define SERVER_PORT   1337
#define MAX_BUF_SIZE  256
#define MAX_BACKLOG   10
#define SERVER_HELLO  "OK\r\n"

void sigchild_handler(int s);


#endif
