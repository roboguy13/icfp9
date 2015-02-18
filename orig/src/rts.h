#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

int32_t** arrays;
int32_t* arraySizes;
int arrayCount = 0;
int maxArrayCount = 32;

int32_t* freeList;
int freeListSize = 0;
int maxFreeListSize = 32;

int32_t registers[8];

int executionFinger = 0;

void condMove(int regANum, int regBNum, int regCNum) {
  if (registers[regCNum]) {
    registers[regANum] = registers[regBNum];
  }
}

void arrayIndex(int regANum, int regBNum, int regCNum) {
  registers[regANum] = arrays[regBNum][regCNum];
}

void addition(int regANum, int regBNum, int regCNum) {
  registers[regANum] = registers[regBNum] + registers[regCNum];
}

void multiplication(int regANum, int regBNum, int regCNum) {
  registers[regANum] = registers[regBNum] * registers[regCNum];
}

void division(int regANum, int regBNum, int regCNum) {
  registers[regANum] = registers[regBNum] / registers[regCNum];
}

void notAnd(int regANum, int regBNum, int regCNum) {
  registers[regANum] = ~(registers[regBNum] & registers[regCNum]);
}


void halt() {
  exit(0);
}

void allocation(int regBNum, int regCNum) {
  int newIx;

  if (freeListSize > 0) {
    newIx = freeList[freeListSize-1];

  } else {
    if (arrayCount >= maxArrayCount) {
      maxArrayCount *= 2;
      arrays = realloc(arrays, sizeof(int32_t*) * maxArrayCount);
      arraySizes = realloc(arraySizes, sizeof(int32_t*) * maxArrayCount);
    }

    newIx = arrayCount;
  }

  arraySizes[newIx] = registers[regCNum];

  arrays[newIx] = malloc(sizeof(int32_t) * registers[regCNum]);
  registers[regBNum] = newIx;
}

void abandonment(int regCNum) {
  int arrayIx = registers[regCNum];

  free(arrays[arrayIx]);

  if (freeListSize >= maxFreeListSize) {
    maxFreeListSize *= 2;
    freeList = realloc(freeList, sizeof(int32_t) * maxFreeListSize);
  }

  freeList[freeListSize] = arrayIx;
}

void output(int regCNum) {
  putc(registers[regCNum], stdout);
}

void input(int regCNum) {
  int c = getc(stdin);

  if (c == EOF) {
    registers[regCNum] = 255;
  } else {
    registers[regCNum] = c;
  }
}

void loadProgram(int regBNum, int regCNum) {
  memcpy(arrays[0], arrays[registers[regBNum]], arraySizes[registers[regBNum]]);

  executionFinger = registers[regCNum];
}

extern void run();

int main() {
  return 0;
}

