/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* include file for sloth (slowness-squared) models */

#ifndef SLOTH_H
#define SLOTH_H
/* vertex attributes */
typedef struct VAStruct {
	float s;		/* sloth at vertex */
} VertexAttributes;

/* edge attributes */
typedef struct EAStruct {
	int k;			/* index of edge */ 
} EdgeAttributes;

/* edge-use attributes */
typedef struct EUAStruct {
	float tx;		/* x componenent of unit tangent vector */ 
	float tz;		/* z componenent of unit tangent vector */
	float c;		/* curvature */
} EdgeUseAttributes;

/* face attributes */
typedef struct FAStruct {
	float s00;		/* s(x,z) = s00+x*dsdx+z*dsdz */
	float dsdx;		/* gradient ds/dx */
	float dsdz;		/* gradient ds/dz */
        float dens;             /* density */
        float qfac;             /* Q-factor */
} FaceAttributes;
typedef FaceAttributes TriAttributes;

/* parameters at ray ends */
typedef struct REStruct {
	float sigma;		/* integral of velocity^2 w.r.t. time */
	float x;		/* horizontal (lateral) coordinate  */
	float z;		/* vertical (depth) coordinate */
	float px;		/* slowness in x direction - dt/dx */
	float pz;		/* slowness in z direction - dt/dz */
	float t;		/* time */
	float q1,p1,q2,p2;	/* dynamic ray parameters ala Cerveny */
	int kmah;		/* kmah index (counts zeros of q2) */
	int nref;		/* number of reflections */
	float sb;		/* sloth at beginning of ray (at source) */
	float se;		/* sloth at end of ray */
	float dsdxe;		/* horizontal sloth derivative at end of ray */
	float dsdze;		/* vertical sloth derivative at end of ray */
	float ab;		/* takeoff angle in radians */
	int kend;		/* index of edge at which ray ends */
        float ampli;            /* effect of refl/transm on amplitude */
        float ampliphase;       /* effect of refl/transm on phase */
        float atten;            /* effect of attenuation on amplitude */  
        float dangle;           /* angle increment */        
} RayEnd;

#endif /* SLOTH_H */
