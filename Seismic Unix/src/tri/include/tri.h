/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* include file for a 2-dimensional model of triangles */

#ifndef TRI_H
#define TRI_H

#include "par.h"

/*******************/
/* DATA STRUCTURES */
/*******************/

/* vertex */
typedef struct VStruct {
	struct VUStruct *vu;		/* list of uses of this vertex */
	int fixed;			/* non-zero if vertex is fixed */
	float x,y;			/* coordinates of vertex */
	void *va;			/* vertex attributes */
} Vertex;
typedef struct VUStruct {
	struct EUStruct *eu;		/* owning edge-use */
	struct VUStruct *vuPrev;	/* previous vertex-use in list */
	struct VUStruct *vuNext;	/* next vertex-use in list */
	struct VStruct *v;		/* vertex being used */
	void *vua;			/* vertex-use attributes */
} VertexUse;

/* edge */
typedef struct EStruct {
	struct EUStruct *eu;		/* list of edge-uses */
	int fixed;			/* non-zero if edge is fixed */
	void *ea;			/* edge attributes */
} Edge;
typedef struct EUStruct {
	struct FStruct *f;		/* owning face */
	struct VUStruct *vu;		/* starting vu of this eu */
	struct EUStruct *euMate;	/* opposite side of edge */
	struct EUStruct *euCW;		/* clockwise eu in face's eu list */
	struct EUStruct *euCCW;		/* counter-cw eu in face's eu list */
	struct EStruct *e;		/* edge being used */
	void *eua;			/* edge-use attributes */
} EdgeUse;

/* face */
typedef struct FStruct {
	struct MStruct *m;		/* owning model */
	struct FStruct *fPrev;		/* previous face in list */
	struct FStruct *fNext;		/* next face in list */
	struct EUStruct *eu;		/* list of edge-uses in face */
	float xc,yc,rs;			/* circumcircle for triangular faces */
	void *fa;			/* face attributes */
} Face;

/* triangle (a 3-sided face) */
typedef Face Tri;

/* model */
typedef struct MStruct {
	struct FStruct *f;			/* list of faces in model */
	float xmin,ymin,xmax,ymax;		/* model bounding rectangle */
	float eps;				/* distance < eps is zero */
	size_t sma,sfa,seua,sea,svua,sva;	/* attribute sizes */
	void (*vAdd)(struct MStruct *m, Vertex *v);	/* callbacks for */
	void (*vDel)(struct MStruct *m, Vertex *v);	/* vertex and */
	void (*tAdd)(struct MStruct *m, Tri *e);	/* triangle add */
	void (*tDel)(struct MStruct *m, Tri *e);	/* and delete */
	void *ma;				/* model attributes */
} Model;


/***********************/
/* FUNCTION PROTOTYPES */
/***********************/

Model *makeModel (float xmin, float ymin, float xmax, float ymax);
void killModel (Model *m);
Vertex* addVertexToModel (Model *m, float x, float y);
void deleteVertexFromModel (Model *m, Vertex *v);
Vertex* nearestVertexInModel (Model *m, Vertex *start, float x, float y);
Edge* nearestEdgeInModel (Model *m, Edge *start, float x, float y);
Tri* insideTriInModel (Model *m, Tri *start, float x, float y);
float distanceToEdge (Edge *e, float x, float y);
void projectToEdge (Edge *e, float *x, float *y);
int fixEdgeBetweenVertices (Vertex *v1, Vertex *v2);
int unfixEdge (Edge *e);
void writeModel (Model *m, FILE *fp);
Model *readModel (FILE *fp);

#endif /* TRI_H */
