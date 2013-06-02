#ifndef SERVER_H
#define SERVER_H

#define SERVER_PORT      1337
#define SERVER_WORKERS   64      // We know how many users will be attacking us
#define SERVER_BACKLOG   1024    // Probably don't need this since we know the attack size above
#define SERVER_HELLO     "ok\n"  // Should be \r\n
#define SERVER_HELLO_LEN 3

#define CLIENT_REQ_SIZE  256     // Size of random string from the client
#define RESULT_SIZE      CLIENT_REQ_SIZE + NONCE_SIZE + 1
#define NONCE_SIZE       32

#define HEX_CRIB         "fedcba9876543210123456789abcdef"


#endif
