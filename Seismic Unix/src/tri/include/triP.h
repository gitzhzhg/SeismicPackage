/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* private include file for a 2-dimensional model of triangles */

#ifndef TRIP_H
#define TRIP_H

#include "tri.h"

#define TRI_INFINITY FLT_MAX

void makeEdgeVertex (Vertex *v1, float x, float y, Face *f, 
	Edge **enew, Vertex **vnew);
void makeEdgeFace (Vertex *v1, Vertex *v2, Edge **enew, Face **fnew);
void killEdge (Edge *e, Face **fs);
void makeBoundaryEdgeTri (Vertex *v, Edge **enew, Tri **tnew);
void killBoundaryEdge (Edge *e);
void circum (float x1, float y1, float x2, float y2, float x3, float y3,
	float *xc, float *yc, float *rs);
void circumTri (Tri *t);
int inCircum (float x, float y, float xc, float yc, float rs);
int inCircumTri (float x, float y, Tri *t);
int in3Vertices (float x, float y, Vertex *v1, Vertex *v2, Vertex *v3);
int inTri (float x, float y, Tri *t);
int edgesColinear (Edge *e1, Edge *e2);
int vertexBetweenVertices (Vertex *v, Vertex *v1, Vertex *v2);
void checkModel (Model *m);

/* types and functions used by readModel and writeModel */
#define MODELTYPE 0
#define FACETYPE 1
#define EDGEUSETYPE 2
#define EDGETYPE 3
#define VERTEXUSETYPE 4
#define VERTEXTYPE 5
typedef void* address_t;
typedef struct AStruct {
        address_t oaddress;     /* old address of node                  */
        address_t naddress;     /* new address--only used in readModel  */
        struct AStruct *aLeft;  /* left child                           */
        struct AStruct *aRight; /* right child                          */
} Address;
Address *updateAddressTree(Address *head, address_t oa, int *found);

#endif /* TRIP_H */
