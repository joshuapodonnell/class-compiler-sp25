#include <inttypes.h>
#include <stdio.h>

extern int64_t entry();

int main(int argc, char **argv) {
  printf("%lld", entry());
  return 0;
}
