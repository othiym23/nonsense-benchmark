#ifndef SERVER_H
#define SERVER_H


#ifdef __APPLE__

#include <CommonCrypto/CommonDigest.h>
#define NS_SHA_CTX    CC_SHA256_CTX
#define NS_SHA_INIT   CC_SHA256_Init
#define NS_SHA_UPDATE CC_SHA256_Update
#define NS_SHA_FINAL(CTX, BUF)  CC_SHA256_Final((BUF), (CTX))
#define NS_SHA_LENGTH CC_SHA256_DIGEST_LENGTH

#else /* !__APPLE__ */

#include <openssl/sha.h>
#define NS_SHA_CTX              SHA256_CTX
#define NS_SHA_INIT             SHA256_Init
#define NS_SHA_UPDATE           SHA256_Update
#define NS_SHA_FINAL(CTX, BUF)  SHA256_Final((BUF), (CTX))
#define NS_SHA_LENGTH           32

#endif




#define SERVER_PORT      1337
#define SERVER_WORKERS   70      // We know how many users will be attacking us
#define SERVER_BACKLOG   1024    // Probably don't need this since we know the attack size above
#define SERVER_HELLO     "ok\n"  // Should be \r\n
#define SERVER_HELLO_LEN 3

#define CLIENT_REQ_SIZE  256     // Size of random string from the client
#define RESULT_SIZE      CLIENT_REQ_SIZE + NONCE_SIZE + 1
#define NONCE_SIZE       32

#define HEX_CRIB         "0123456789abcdef"


#endif
