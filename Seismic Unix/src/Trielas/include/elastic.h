/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

/* include file for elastic 2D anisotropic models */


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
	float a1111;
	float a3333;		
	float a1133;		
        float a1313;            
        float a1113;             
        float a3313; 
        float a1212;            
        float a2323;             
        float a1223;
        float rho;
        int mindex;
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
	float vgx;		/* incidence ray velocity */
	float vgz;		/* incidence ray velocity */
	float ab;		/* takeoff angle in radians */
	int kend;		/* index of edge at which ray ends */
        float ampli;            /* effect of refl/transm on amplitude */
        float ampliphase;       /* effect of refl/transm on phase */
        float dangle;           /* angle increment */
	int mode;		/* ray mode */
	float g11,g13,g33;      /* polarisations */
	int nameref;            /* name of reflector */
	int num;		/* number of ray */
	float rhob;		/* density at ray begin */
	float rhoe;		/* density at ray end */

} RayEnd;

typedef struct RCStruct {
        int k;
	int nhits;
	int *hitseq;
} RefCheck;

/* prototypes for functions defined */

/* lib Snell */
int solveForSlowqP(float s, float c, float pl, float a1111, float a3333,
	float a1133, float a1313, float a1113, float a3313, float *pxnew, float 
	*pznew, float rt);	
int solveForSlowqSV(float s, float c,  float pl, float a1111, float
	a3333, float a1133, float a1313, float a1113, float a3313, float 
	*pxnew, float *pznew, float rt);	
int findPiso (float a3333, float pl, float gx, float gz, float *px, float *pz,
	float *vgx, float *vgz, float *g11, float *g13, float *g33, int r);
int findSiso (float a1313, float pl, float gx, float gz, float *px, float *pz,
	float *vgx, float *vgz, float *g11, float *g13, float *g33, int r);
int findqSH(float si, float co, float pl, float a1212, float a1223,
	float a2323, float *pxnew, float *pznew, float *vgx, float *vgz,
        int r);

/* lib Coeff */
int rt_iso_real(float vp1, float vp2, float vs1, float vs2, float rho1,
	float rho2, float pl, int modei, int modet, int rort, float
	*coeff);
int rt_iso_cmplx(float vp1, float vp2, float vs1, float vs2, float rho1,
	float rho2, float pl, int modei, int modet, int rort, float
	*coeff,float *phase);
int rt_SHa_real(float c1212i, float c2323i, float c1223i, float c1212t, 
	float c2323t, float c1223t, float pxi, float pzi, float pxt, 
	float pzt, float pxr, float pzr, int rort, float *coeff, float gz, 	
	float gx);
int rt_ani_real(float gz, float gx, float a1111i, float a3333i, float 	
	a1133i, float a1313i, float a1113i, float a3313i, float rhoi, 
	float a1111t, float a3333t, float a1133t, float a1313t, float a1113t, 
	float a3313t, float rhot, float pxi, float pzi, float g11i, float g13i,
	 float g33i, float pxg,float pzg, float g11g, float g13g, float g33g, 	
	int modei, int modeg, int rort, float *coeff,FILE *ifp);

/* lib Rot */
void rotvector (float *x, float *z, float si, float co );
void rottensh (float *a1212, float *a2323, float *a1223, float si,float co);
void rottensor (float *a1111, float *a3333, float *a1133, float *a1313,
	float *a1113, float *a3313, float si, float co );

/* elaray.c*/
void polar(float px, float pz, float g11, float g13,
	float g33, float *polx, float *polz, int mode );
int findnewmode(int mode , int* newmode, int conv, int mindex);
int findqPqSV(float s, float c, float pl, float a1111, float a3333,
	float a1133,float a1313, float a1113, float a3313, int mode, float
	*pxnew, float *pznew, float *vgx, float *vgz, float *g11, float *g13,  
	float *g33, float rt, FILE *ifp);
