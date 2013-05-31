#include <stdio.h>
#include <string.h>
#include <openssl/evp.h>
#include "nonce.h"

unsigned char *find_nonce(char *str);

unsigned char *find_nonce(char *str) {
  EVP_MD_CTX    *base_sha = NULL;
  EVP_MD_CTX    *test_sha = NULL;
  const EVP_MD  *md;

  unsigned char  md_value[EVP_MAX_MD_SIZE];
  unsigned char  *buf;
  unsigned long  nonce_num = 0;
  int            md_len, i = 0;

  if(!str) return NULL;

  OpenSSL_add_all_digests();

  md = EVP_sha256();       // Using sha256 for these

  base_sha = EVP_MD_CTX_create();
  test_sha = EVP_MD_CTX_create();

  EVP_DigestInit_ex(base_sha, md, NULL);
  EVP_DigestUpdate(base_sha, str, strlen(str));

  if (!(buf = (char *)malloc(256)))
    return NULL;

  buf[0] = 0;

  do {
    nonce_num++;
    sprintf(buf, "%lx", nonce_num);

    EVP_MD_CTX_copy_ex(test_sha, base_sha);
    EVP_DigestUpdate(test_sha, buf, strlen(buf));

    EVP_DigestFinal_ex(test_sha, md_value, &md_len);
    EVP_MD_CTX_cleanup(test_sha);
  } while (md_value[md_len - 1] != 0);

  EVP_MD_CTX_destroy(base_sha);
  EVP_MD_CTX_destroy(test_sha);

#ifdef DEBUG
  // Debug
  for(i = 0; i < md_len; i++) printf("%02x", md_value[i]);
  printf("\n");
#endif

  return buf;
}
