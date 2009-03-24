
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct vec2 {
    float x;
    float y;
};
typedef struct vec2 vec2;

struct vec2_vector {
    vec2* data;
    int length;
};
typedef struct vec2_vector vec2_vector;
