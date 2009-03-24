
#include "../lib/util/triangulate.c"

void test_triangulate() {
    printf("Testing triangulate... ");
    vec2 vecs[15];
    vecs[0].x = 0;
    vecs[0].y = 0;
    
    vecs[1].x = 0;
    vecs[1].y = 5;
    
    vecs[2].x = 5;
    vecs[2].y = 9;
    
    vecs[3].x = -45;
    vecs[3].y = 0;

    vecs[4].x = 25;
    vecs[4].y = -5;

    vecs[5].x = 2;
    vecs[5].y = 33;

    vecs[6].x = -5;
    vecs[6].y = 30;

    int i;
    vec2_vector *data;
    if(!(data = triangulate(vecs, 7))) {
        for(i=0; i<data->length; i++) {
            printf("(%f, %f)\n", data->data[i].x, data->data[i].y);
        }
    }
    else {
        printf("failed\n");
    }
}

int main(int argc, char** argv) {
    test_triangulate();
    return 0;
}
