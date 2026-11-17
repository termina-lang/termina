/* The object of the fixture objects.o: gcc -c -O0 -fno-common -fno-asynchronous-unwind-tables objects.c */

unsigned char termina__cov_stmt__m[4];

unsigned char termina__cov_cond__m[3][2] = { { 1, 0 } };

extern unsigned char termina__cov_stmt__other[2];

static int counter;

int step(void) {
    counter = counter + termina__cov_stmt__other[0];
    return counter;
}
