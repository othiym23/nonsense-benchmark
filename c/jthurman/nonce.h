#ifndef NONCE_H
#define NONCE_H

#define NONCE_SIZE       32

/* Prototypes */
int calc_nonce(void *str, int len, void *nonce_buf, int nonce_buf_len);

#endif
