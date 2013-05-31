#include <stdio.h>
#include <string.h>
#include <openssl/evp.h>
#include "nonce.h"

int calc_nonce(void *str, int len, void *nonce_buf, int nonce_buf_len) {
  EVP_MD_CTX    *base_sha = NULL;
  EVP_MD_CTX    *test_sha = NULL;
  const EVP_MD  *md;

  unsigned char  md_value[EVP_MAX_MD_SIZE];
  unsigned long  nonce_num = 0;
  int            nonce_len = 0;
  int            md_len = 0;

  if(!str) return -1;

  //OpenSSL_add_all_digests();

  md = EVP_sha256();       // Using sha256 for these

  base_sha = EVP_MD_CTX_create();
  test_sha = EVP_MD_CTX_create();

  EVP_DigestInit_ex(base_sha, md, NULL);
  EVP_DigestUpdate(base_sha, str, len);

  do {
    nonce_num++;
    nonce_len = snprintf(nonce_buf, nonce_buf_len, "%lx", nonce_num);

    EVP_MD_CTX_copy_ex(test_sha, base_sha);
    EVP_DigestUpdate(test_sha, nonce_buf, nonce_len);

    EVP_DigestFinal_ex(test_sha, md_value, &md_len);
    EVP_MD_CTX_cleanup(test_sha);
  } while (md_value[md_len - 1] != 0);

  EVP_MD_CTX_destroy(base_sha);
  EVP_MD_CTX_destroy(test_sha);

  return nonce_len;
}
