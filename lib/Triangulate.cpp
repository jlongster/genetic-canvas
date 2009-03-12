//
// Triangulate.cpp
//
// Two different methods for triangulating polygons.
//
// Copyright (c) 2006-2008 Virtual Terrain Project
// Free for all uses, see license.txt for details.
//

#include "Triangulate.h"

static const float EPSILON=0.0000000001f;

float Triangulate_f::Area(const FLine2 &contour)
{
	int n = contour.GetSize();

	float A=0.0f;

	for (int p=n-1,q=0; q<n; p=q++)
	{
		A+= contour[p].x*contour[q].y - contour[q].x*contour[p].y;
	}
	return A*0.5f;
}

float Triangulate_f::Area(const FLine3 &contour)
{
	int n = contour.GetSize();

	float A=0.0f;

	for (int p=n-1,q=0; q<n; p=q++)
	{
		A+= contour[p].x*contour[q].z - contour[q].x*contour[p].z;
	}
	return A*0.5f;
}

/*
 InsideTriangle decides if a point P is Inside of the triangle
 defined by A, B, C.
*/
bool Triangulate_f::InsideTriangle(float Ax, float Ay,
						float Bx, float By,
						float Cx, float Cy,
						float Px, float Py)
{
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
};

bool Triangulate_f::Snip(const FLine2 &contour,int u,int v,int w,int n,int *V)
{
	int p;
	float Ax, Ay, Bx, By, Cx, Cy, Px, Py;

	Ax = contour[V[u]].x;
	Ay = contour[V[u]].y;

	Bx = contour[V[v]].x;
	By = contour[V[v]].y;

	Cx = contour[V[w]].x;
	Cy = contour[V[w]].y;

	if ( EPSILON > (((Bx-Ax)*(Cy-Ay)) - ((By-Ay)*(Cx-Ax))) ) return false;

	for (p=0;p<n;p++)
	{
		if( (p == u) || (p == v) || (p == w) ) continue;
		Px = contour[V[p]].x;
		Py = contour[V[p]].y;
		if (InsideTriangle(Ax,Ay,Bx,By,Cx,Cy,Px,Py)) return false;
	}

	return true;
}

bool Triangulate_f::Snip(const FLine3 &contour,int u,int v,int w,int n,int *V)
{
	int p;
	float Ax, Ay, Bx, By, Cx, Cy, Px, Py;

	Ax = contour[V[u]].x;
	Ay = contour[V[u]].z;

	Bx = contour[V[v]].x;
	By = contour[V[v]].z;

	Cx = contour[V[w]].x;
	Cy = contour[V[w]].z;

	if ( EPSILON > (((Bx-Ax)*(Cy-Ay)) - ((By-Ay)*(Cx-Ax))) ) return false;

	for (p=0;p<n;p++)
	{
		if( (p == u) || (p == v) || (p == w) ) continue;
		Px = contour[V[p]].x;
		Py = contour[V[p]].z;
		if (InsideTriangle(Ax,Ay,Bx,By,Cx,Cy,Px,Py)) return false;
	}

	return true;
}

bool Triangulate_f::Process(const FLine2 &contour,FLine2 &result)
{
	/* allocate and initialize list of Vertices in polygon */

	int n = contour.GetSize();
	if ( n < 3 ) return false;

	int *V = new int[n];

	/* we want a counter-clockwise polygon in V */

	if ( 0.0f < Area(contour) )
		for (int v=0; v<n; v++) V[v] = v;
	else
		for (int v=0; v<n; v++) V[v] = (n-1)-v;

	int nv = n;

	/*  remove nv-2 Vertices, creating 1 triangle every time */
	int count = 2*nv;   /* error detection */

	for (int m=0, v=nv-1; nv>2; )
	{
		/* if we loop, it is probably a non-simple polygon */
		if (0 >= (count--))
		{
			//** Triangulate: ERROR - probable bad polygon!
			return false;
		}

		/* three consecutive vertices in current polygon, <u,v,w> */
		int u = v  ; if (nv <= u) u = 0;	/* previous */
		v = u+1; if (nv <= v) v = 0;		/* new v */
		int w = v+1; if (nv <= w) w = 0;	/* next */

		if ( Snip(contour,u,v,w,nv,V) )
		{
			int a,b,c,s,t;

			/* true names of the vertices */
			a = V[u]; b = V[v]; c = V[w];

			/* output Triangle */
			result.Append( contour[a] );
			result.Append( contour[b] );
			result.Append( contour[c] );

			m++;

			/* remove v from remaining polygon */
			for (s=v,t=v+1;t<nv;s++,t++) V[s] = V[t]; nv--;

			/* resest error detection counter */
			count = 2*nv;
		}
	}
	delete V;
	return true;
}

bool Triangulate_f::Process(const FLine3 &contour,FLine3 &result)
{
	/* allocate and initialize list of Vertices in polygon */

	int n = contour.GetSize();
	if ( n < 3 ) return false;

	int *V = new int[n];

	/* we want a counter-clockwise polygon in V */

	if ( 0.0f < Area(contour) )
		for (int v=0; v<n; v++) V[v] = v;
	else
		for (int v=0; v<n; v++) V[v] = (n-1)-v;

	int nv = n;

	/*  remove nv-2 Vertices, creating 1 triangle every time */
	int count = 2*nv;   /* error detection */

	for (int m=0, v=nv-1; nv>2; )
	{
		/* if we loop, it is probably a non-simple polygon */
		if (0 >= (count--))
		{
			//** Triangulate: ERROR - probable bad polygon!
			return false;
		}

		/* three consecutive vertices in current polygon, <u,v,w> */
		int u = v  ; if (nv <= u) u = 0;	/* previous */
		v = u+1; if (nv <= v) v = 0;		/* new v */
		int w = v+1; if (nv <= w) w = 0;	/* next */

		if ( Snip(contour,u,v,w,nv,V) )
		{
			int a,b,c,s,t;

			/* true names of the vertices */
			a = V[u]; b = V[v]; c = V[w];

			/* output Triangle */
			result.Append( contour[a] );
			result.Append( contour[b] );
			result.Append( contour[c] );

			m++;

			/* remove v from remaining polygon */
			for (s=v,t=v+1;t<nv;s++,t++) V[s] = V[t]; nv--;

			/* resest error detection counter */
			count = 2*nv;
		}
	}
	delete V;
	return true;
}

bool Triangulate_f::Process(const FLine3 &contour, vtArray<int> &result)
{
	/* allocate and initialize list of Vertices in polygon */

	int n = contour.GetSize();
	if ( n < 3 ) return false;

	int *V = new int[n];

	/* we want a counter-clockwise polygon in V */

	if ( 0.0f < Area(contour) )
		for (int v=0; v<n; v++) V[v] = v;
	else
		for (int v=0; v<n; v++) V[v] = (n-1)-v;

	int nv = n;

	/*  remove nv-2 Vertices, creating 1 triangle every time */
	int count = 2*nv;   /* error detection */

	for (int m=0, v=nv-1; nv>2; )
	{
		/* if we loop, it is probably a non-simple polygon */
		if (0 >= (count--))
		{
			//** Triangulate: ERROR - probable bad polygon!
			return false;
		}

		/* three consecutive vertices in current polygon, <u,v,w> */
		int u = v  ; if (nv <= u) u = 0;	/* previous */
		v = u+1; if (nv <= v) v = 0;		/* new v */
		int w = v+1; if (nv <= w) w = 0;	/* next */

		if ( Snip(contour,u,v,w,nv,V) )
		{
			int a,b,c,s,t;

			/* true names of the vertices */
			a = V[u]; b = V[v]; c = V[w];

			/* output Triangle */
			result.Append( a );
			result.Append( b );
			result.Append( c );

			m++;

			/* remove v from remaining polygon */
			for (s=v,t=v+1;t<nv;s++,t++) V[s] = V[t]; nv--;

			/* resest error detection counter */
			count = 2*nv;
		}
	}
	delete V;
	return true;
}


//////////////////////////////////////////////////////////////////////////
// Double-precision version
//

double Triangulate_d::Area(const DLine2 &contour)
{
	int n = contour.GetSize();

	double A=0.0f;

	for (int p=n-1,q=0; q<n; p=q++)
	{
		A+= contour[p].x*contour[q].y - contour[q].x*contour[p].y;
	}
	return A*0.5f;
}

/*
 InsideTriangle decides if a point P is Inside of the triangle
 defined by A, B, C.
*/
bool Triangulate_d::InsideTriangle(double Ax, double Ay,
						double Bx, double By,
						double Cx, double Cy,
						double Px, double Py)
{
	double ax, ay, bx, by, cx, cy, apx, apy, bpx, bpy, cpx, cpy;
	double cCROSSap, bCROSScp, aCROSSbp;

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
};

bool Triangulate_d::Snip(const DLine2 &contour,int u,int v,int w,int n,int *V)
{
	int p;
	double Ax, Ay, Bx, By, Cx, Cy, Px, Py;

	Ax = contour[V[u]].x;
	Ay = contour[V[u]].y;

	Bx = contour[V[v]].x;
	By = contour[V[v]].y;

	Cx = contour[V[w]].x;
	Cy = contour[V[w]].y;

	if ( EPSILON > (((Bx-Ax)*(Cy-Ay)) - ((By-Ay)*(Cx-Ax))) ) return false;

	for (p=0;p<n;p++)
	{
		if( (p == u) || (p == v) || (p == w) ) continue;
		Px = contour[V[p]].x;
		Py = contour[V[p]].y;
		if (InsideTriangle(Ax,Ay,Bx,By,Cx,Cy,Px,Py)) return false;
	}

	return true;
}

bool Triangulate_d::Process(const DLine2 &contour,DLine2 &result)
{
	/* allocate and initialize list of Vertices in polygon */

	int n = contour.GetSize();
	if ( n < 3 ) return false;

	int *V = new int[n];

	/* we want a counter-clockwise polygon in V */

	if ( 0.0f < Area(contour) )
		for (int v=0; v<n; v++) V[v] = v;
	else
		for (int v=0; v<n; v++) V[v] = (n-1)-v;

	int nv = n;

	/*  remove nv-2 Vertices, creating 1 triangle every time */
	int count = 2*nv;   /* error detection */

	for (int m=0, v=nv-1; nv>2; )
	{
		/* if we loop, it is probably a non-simple polygon */
		if (0 >= (count--))
		{
			//** Triangulate: ERROR - probable bad polygon!
			return false;
		}

		/* three consecutive vertices in current polygon, <u,v,w> */
		int u = v  ; if (nv <= u) u = 0;	/* previous */
		v = u+1; if (nv <= v) v = 0;		/* new v */
		int w = v+1; if (nv <= w) w = 0;	/* next */

		if ( Snip(contour,u,v,w,nv,V) )
		{
			int a,b,c,s,t;

			/* true names of the vertices */
			a = V[u]; b = V[v]; c = V[w];

			/* output Triangle */
			result.Append( contour[a] );
			result.Append( contour[b] );
			result.Append( contour[c] );

			m++;

			/* remove v from remaining polygon */
			for (s=v,t=v+1;t<nv;s++,t++) V[s] = V[t]; nv--;

			/* resest error detection counter */
			count = 2*nv;
		}
	}
	delete V;
	return true;
}

#if 0

/************************************************************************/
/*** END OF CODE SECTION TRIANGULATE.CPP BEGINNING OF TEST.CPP A SMALL **/
/*** TEST APPLICATION TO DEMONSTRATE THE USAGE OF THE TRIANGULATOR	   **/
/************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


#include "triangulate.h"

void main(int argc,char **argv)
{
	// Small test application demonstrating the usage of the triangulate
	// class.

	// Create a pretty complicated little contour.
	DLine2 a;
	a.Append( DPoint2(0,6));
	a.Append( DPoint2(0,0));
	a.Append( DPoint2(3,0));
	a.Append( DPoint2(4,1));
	a.Append( DPoint2(6,1));
	a.Append( DPoint2(8,0));
	a.Append( DPoint2(12,0));
	a.Append( DPoint2(13,2));
	a.Append( DPoint2(8,2));
	a.Append( DPoint2(8,4));
	a.Append( DPoint2(11,4));
	a.Append( DPoint2(11,6));
	a.Append( DPoint2(6,6));
	a.Append( DPoint2(4,3));
	a.Append( DPoint2(2,6));

	// allocate a polyline to hold the answer.
	DLine2 result;

	//  Invoke the triangulator to triangulate this polygon.
	Triangulate_d::Process(a,result);

	// print out the results.
	int tcount = result.GetSize()/3;

	for (int i=0; i<tcount; i++)
	{
		const DPoint2 &p1 = result[i*3+0];
		const DPoint2 &p2 = result[i*3+1];
		const DPoint2 &p3 = result[i*3+2];
		printf("Triangle %d => (%0.0f,%0.0f) (%0.0f,%0.0f) (%0.0f,%0.0f)\n",
			i+1,
			p1.x,p1.y,p2.x,p2.y,p3.x,p3.y);
	}
}

#endif	// Test code


///////////////////////////////////////////////////////////////////////////
// Interface code for the Triangle library.

#define ANSI_DECLARATORS
#define REAL double
extern "C" {
#include "triangle/triangle.h"
}

/**
 * Another triangulation algorithm, far more powerful, is the Triangle library.
 * Provide a convenient way to call it.
 */
void CallTriangle(const DLine2 &contour, DLine2 &result)
{
	struct triangulateio in, out;
	int counter;
	int i;

	// point list
	in.numberofpoints = contour.GetSize();
	in.pointlist = (REAL *) malloc(in.numberofpoints * 2 * sizeof(REAL));
	for ( i = 0; i < in.numberofpoints; ++i )
	{
		in.pointlist[2*i] = contour[i].x;
		in.pointlist[2*i + 1] = contour[i].y;
	}
	in.numberofpointattributes = 0;
	in.pointattributelist = (REAL *) NULL;
	in.pointmarkerlist = (int *) NULL;
	in.numberoftriangles = 0;

	// segment list
	in.numberofsegments = contour.GetSize() - 1;
	in.segmentlist = (int *) malloc(in.numberofsegments * 2 * sizeof(int));
	in.segmentmarkerlist = (int *) NULL;
	counter = 0;
	for (i = 0; i < in.numberofsegments; i++)
	{
		in.segmentlist[counter++] = i;
		in.segmentlist[counter++] = (i+1)%in.numberofsegments;
	}
	// no holes or regions
	in.numberofholes = 0;
	in.holelist = (REAL *) NULL;
	in.numberofregions = 0;
	in.regionlist = (REAL *) NULL;

	// prep the output structures
	out.pointlist = (REAL *) NULL;        // Not needed if -N switch used.
	out.pointattributelist = (REAL *) NULL;
	out.pointmarkerlist = (int *) NULL;   // Not needed if -N or -B switch used.
	out.trianglelist = (int *) NULL;      // Not needed if -E switch used.
	out.triangleattributelist = (REAL *) NULL;
	out.neighborlist = (int *) NULL;      // Needed only if -n switch used.
	out.segmentlist = (int *) NULL;
	out.segmentmarkerlist = (int *) NULL;
	out.edgelist = (int *) NULL;          // Needed only if -e switch used.
	out.edgemarkerlist = (int *) NULL;    // Needed if -e used and -B not used.

	// Triangulate the points.  Switches are chosen:
	// to read and write a PSLG (p)
	// number everythingfrom zero (z),
	triangulate("pz", &in, &out, NULL);

	// now copy the triangle results back into vtdata structures
	for ( i = 0; i < out.numberoftriangles; ++i )
	{
		int n1 = out.trianglelist[i * 3];
		int n2 = out.trianglelist[i * 3 + 1];
		int n3 = out.trianglelist[i * 3 + 2];
		result.Append(DPoint2( out.pointlist[2*n1], out.pointlist[2*n1 + 1] ));
		result.Append(DPoint2( out.pointlist[2*n2], out.pointlist[2*n2 + 1] ));
		result.Append(DPoint2( out.pointlist[2*n3], out.pointlist[2*n3 + 1] ));
	}
	// free mem allocated to the "Triangle" structures
	free(in.pointlist);
	free(in.segmentlist);

	free(out.pointlist);
	free(out.pointattributelist);
	free(out.pointmarkerlist);
	free(out.trianglelist);
	free(out.triangleattributelist);
	free(out.neighborlist);
	free(out.segmentlist);
	free(out.segmentmarkerlist);
	free(out.edgelist);
	free(out.edgemarkerlist);
}

/**
 * Another triangulation algorithm, far more powerful, is the Triangle library.
 * Provide a convenient way to call it.
 */
void CallTriangle(const DPolygon2 &contour, DLine2 &result)
{
	struct triangulateio in, out;

	// point list
	in.numberofpoints = contour.NumTotalVertices();
	in.pointlist = (REAL *) malloc(in.numberofpoints * 2 * sizeof(REAL));
	int counter = 0;
	for (unsigned int ring = 0; ring < contour.size(); ring++)
	{
		const DLine2 &polyline = contour[ring];
		for (unsigned i = 0; i < polyline.GetSize(); i++)
		{
			in.pointlist[counter++] = polyline[i].x;
			in.pointlist[counter++] = polyline[i].y;
		}
	}
	in.numberofpointattributes = 0;
	in.pointattributelist = (REAL *) NULL;
	in.pointmarkerlist = (int *) NULL;
	in.numberoftriangles = 0;

	// segment list: each ring of N points has N edges, each of those
	//  edges is a "segment" passed to Triangle
	in.numberofsegments = 0;
	for (unsigned int ring = 0; ring < contour.size(); ring++)
		in.numberofsegments += contour[ring].GetSize();

	in.segmentlist = (int *) malloc(in.numberofsegments * 2 * sizeof(int));
	in.segmentmarkerlist = (int *) NULL;

	counter = 0;
	int start = 0;
	for (unsigned int ring = 0; ring < contour.size(); ring++)
	{
		int num_points = contour[ring].GetSize();
		for (int i = 0; i < num_points; i++)
		{
			in.segmentlist[counter++] = start+i;
			in.segmentlist[counter++] = start+((i+1)%num_points);
		}
		start += num_points;
	}

	// hole list
	in.numberofholes = contour.size() - 1;
	in.holelist = (REAL *) malloc(in.numberofholes * 2 * sizeof(REAL));

	counter = 0;
	for (unsigned int ring = 1; ring < contour.size(); ring++)
	{
		// We must provide a 2D point inside each hole.  However, the hole itself
		//  might have concavities, so we cannot simply use the hole's centroid.
		// Instead, call Triangle for each potentially complex hole!
		DPoint2 p;
		const DLine2 &hole = contour[ring];
		if (hole.GetSize() < 5)
			p = hole.Centroid();
		else
		{
			DLine2 result2;
			CallTriangle(hole, result2);
			p = (result2[0] + result2[1] + result2[2])/3;
		}
		in.holelist[counter++] = p.x;
		in.holelist[counter++] = p.y;
	}
	in.numberofregions = 0;
	in.regionlist = (REAL *) NULL;

	// prep the output structures
	out.pointlist = (REAL *) NULL;        // Not needed if -N switch used.
	out.pointattributelist = (REAL *) NULL;
	out.pointmarkerlist = (int *) NULL;   // Not needed if -N or -B switch used.
	out.trianglelist = (int *) NULL;      // Not needed if -E switch used.
	out.triangleattributelist = (REAL *) NULL;
	out.neighborlist = (int *) NULL;      // Needed only if -n switch used.
	out.segmentlist = (int *) NULL;
	out.segmentmarkerlist = (int *) NULL;
	out.edgelist = (int *) NULL;          // Needed only if -e switch used.
	out.edgemarkerlist = (int *) NULL;    // Needed if -e used and -B not used.

	// Triangulate the points.  Switches are chosen:
	// to read and write a PSLG (p)
	// number everythingfrom zero (z),
	triangulate("pz", &in, &out, NULL);

	// now copy the triangle results back into vtdata structures
	for (int i = 0; i < out.numberoftriangles; i++)
	{
		int n1 = out.trianglelist[i * 3];
		int n2 = out.trianglelist[i * 3 + 1];
		int n3 = out.trianglelist[i * 3 + 2];
		result.Append(DPoint2( out.pointlist[2*n1], out.pointlist[2*n1 + 1] ));
		result.Append(DPoint2( out.pointlist[2*n2], out.pointlist[2*n2 + 1] ));
		result.Append(DPoint2( out.pointlist[2*n3], out.pointlist[2*n3 + 1] ));
	}

	// free mem allocated to the "Triangle" structures
	free(in.pointlist);
	free(in.segmentlist);
	free(in.holelist);

	free(out.pointlist);
	free(out.pointattributelist);
	free(out.pointmarkerlist);
	free(out.trianglelist);
	free(out.triangleattributelist);
	free(out.neighborlist);
	free(out.segmentlist);
	free(out.segmentmarkerlist);
	free(out.edgelist);
	free(out.edgemarkerlist);
}

