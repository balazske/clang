
#define M *X = (int *)0

void F1(int **X) {
  M;
}

#undef M
#define M *Y = (int *)0

void F2(int **Y) {
  M;
}
