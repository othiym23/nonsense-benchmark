#include <stdio.h>
#include <string.h>
#include <openssl/evp.h>
#include <sys/time.h>
#include "nonce.h"

// #define DEBUG   1


main(int argc, char *argv[]) {
  unsigned char  *nonce;
  struct timeval  tv_start, tv_end;
  unsigned long   time_in_micros;

  if(!argv[1]) {
    printf("Usage: mdtest <string>\n");
    exit(1);
  }

  gettimeofday(&tv_start, NULL);
  nonce = find_nonce(argv[1]);
  gettimeofday(&tv_end, NULL);

  // calculate the time
  time_in_micros = ((1000000 * tv_end.tv_sec) + tv_end.tv_usec) - ((1000000 * tv_start.tv_sec) + tv_start.tv_usec);

  printf("%s:%s\n", argv[1], nonce);
  printf("Time in micros: %d\n", time_in_micros);
}

