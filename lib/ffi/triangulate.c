// "Efficient Polygon Triangulation"
// 
// I stole this code from a Flipcode article:
// http://www.flipcode.com/archives/Efficient_Polygon_Triangulation.shtml
// It was originally written in C++ and I ported it to C.
// - James

#include "triangulate.h"

static const float EPSILON=0.0000000001f;

float area(vec2 *contour, int len) {
    int n = len;
    int p, q;
    float A=0.0f;

    for(p=n-1,q=0; q<n; p=q++) {
        A+= contour[p].x*contour[q].y - contour[q].x*contour[p].y;
    }
    return A*0.5f;
}

/*
  InsideTriangle decides if a point P is Inside of the triangle
  defined by A, B, C.
*/
int inside_triangle(float Ax, float Ay,
                    float Bx, float By,
                    float Cx, float Cy,
                    float Px, float Py) {
  float ax, ay, bx, by, cx, cy, apx, apy, bpx, bpy, cpx, cpy;
  float cCROSSap, bCROSScp, aCROSSbp;

  ax = Cx - Bx;  ay = Cy - By;
  bx = Ax - Cx;  by = Ay - Cy;
  cx = Bx - Ax;  cy = By - Ay;
  apx= Px - Ax;  apy= Py - Ay;
  bpx= Px - Bx;  bpy= Py - By;
  cpx= Px - Cx;  cpy= Py - Cy;

  aCROSSbp = ax*bpy - ay*bpx;
  cCROSSap = cx*apy - cy*apx;
  bCROSScp = bx*cpy - by*cpx;

  return ((aCROSSbp >= 0.0f) && (bCROSScp >= 0.0f) && (cCROSSap >= 0.0f));
}

int snip(vec2 *contour, int u, int v, int w, int n, int *V) {
  int p;
  float Ax, Ay, Bx, By, Cx, Cy, Px, Py;

  Ax = contour[V[u]].x;
  Ay = contour[V[u]].y;

  Bx = contour[V[v]].x;
  By = contour[V[v]].y;

  Cx = contour[V[w]].x;
  Cy = contour[V[w]].y;

  if ( EPSILON > (((Bx-Ax)*(Cy-Ay)) - ((By-Ay)*(Cx-Ax))) ) return 0;

  for (p=0;p<n;p++) {
      if( (p == u) || (p == v) || (p == w) ) continue;
      Px = contour[V[p]].x;
      Py = contour[V[p]].y;
      if (inside_triangle(Ax,Ay,Bx,By,Cx,Cy,Px,Py)) return 0;
  }

  return 1;
}

// This is an awful, horrible hack which paints a sludgy black mark
// on my history of programming.  We know how we will be calling this
// library, so at least for now, for optimizations, hold the results
// of the triangulation in these global variables which are passed
// around in scheme BUT NEVER KEPT.
/* vec2_vector out_vector; */
/* vec2 out_result[500]; */

vec2_vector triangulate(vec2 *contour, int len) {
    int n = len;
    int v, m;

    vec2_vector out_vector;
    out_vector.data = NULL;
    out_vector.length = 0;
    
    if(n < 3) return out_vector;
    
    int V[n];
    int idx = 0;
    vec2 *result = calloc(500, sizeof(vec2));
    
    /* we want a counter-clockwise polygon in V */

    if ( 0.0f < area(contour, len) )
        for(v=0; v<n; v++) V[v] = v;
    else
        for(v=0; v<n; v++) V[v] = (n-1)-v;

    int nv = n;

    /*  remove nv-2 Vertices, creating 1 triangle every time */
    int count = 2*nv;   /* error detection */

    for(m=0, v=nv-1; nv>2; ) {        
        if (0 >= (count--)) {
            return out_vector;
        }

        /* three consecutive vertices in current polygon, <u,v,w> */
        int u = v  ; if (nv <= u) u = 0;     /* previous */
        v = u+1; if (nv <= v) v = 0;         /* new v    */
        int w = v+1; if (nv <= w) w = 0;     /* next     */

        if (snip(contour, u, v, w, nv, V)) {
            int a,b,c,s,t;

            /* true names of the vertices */
            a = V[u]; b = V[v]; c = V[w];

            /* output Triangle */
            if(idx >= 500) {
                return out_vector;
            }
            result[idx++] = contour[a];
            result[idx++] = contour[b];
            result[idx++] = contour[c];

            m++;

            /* remove v from remaining polygon */
            for(s=v,t=v+1;t<nv;s++,t++) V[s] = V[t]; nv--;

            /* resest error detection counter */
            count = 2*nv;
        }
    }

    out_vector.data = result;
    out_vector.length = idx;
    return out_vector;
}
