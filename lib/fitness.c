
#include <stdlib.h>

#define BYTE_DIFFERENCE(bytes1, bytes2, i) \
    (unsigned long)abs((int)bytes1[i] - (int)bytes2[i])

unsigned long calculate_fitness(unsigned char* bytes1, unsigned char* bytes2,
                                int length) {
    int i;
    unsigned long acc = 0;
    for(i=0; i<length; i+=4) {
        acc += BYTE_DIFFERENCE(bytes1, bytes2, i);
        acc += BYTE_DIFFERENCE(bytes1, bytes2, i+1);
        acc += BYTE_DIFFERENCE(bytes1, bytes2, i+2);
    }

    return acc;
}

unsigned long sum_fitness(unsigned char* bytes,
                          int length) {
    int i;
    unsigned long acc = 0;
    for(i=0; i<length; i+=4) {
        acc += bytes[i];
        acc += bytes[i+1];
        acc += bytes[i+2];
    }

    return acc;
}
