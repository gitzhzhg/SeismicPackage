/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* included materials for Tetrahedral model building and ray tracing */

#ifndef CWP_TETRA_H
#define CWP_TETRA_H

#include "par.h"

#define UseKiloMeter
#define InfDistance 1000
#define InfTime 10000 /* ms */

struct TTAB {
      unsigned short t;  /* real t=t/SHRT_MAX*InfTime */
      unsigned char cs;  /* real cs=cs/UCHAR_MAX */
      unsigned char r;   /* real r=r/UCHAR_MAX*InfDistance */
};

struct FACET {
      int ip[3];      /* indices for 3 control points of this facet */
      float cn[3];    /* centered normal determined by (x2-x1)x(x1-x0) */
      int itetra[2];  /* two tetra indices on both sides */
      float area;     /* the area of this facet */
      float ct[10];   /* curved tile coefficients */
                      /* ct[0] x^2 + ct[1] xy + ct[2] xz + ct[3] yz +
                         ct[4] y^2 + ct[5] z^2 + cn[0] x + cn[1] y +
                         cn[2] z + ct[6] */ 
};

struct TETRA {
      int ip[4];       /* 4 control points */
      int ifacet[4];   /* 4 facets */
      int ireg;        /* region of this tetra */
      float gs[3];     /* gradient of sloth */
      float v;         /* volume */
};

struct POINT {
      float x[3];
      float s;
      float n[3];
};

/* Tetrahedra related subroutines */
float tetra_volume(float x0[3], float x1[3], float x2[3], float x3[3]);
float area3d( float x0[3], float x1[3], float x2[3]); 

#endif /* end TETRA_H */
