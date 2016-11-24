/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* global variables for reflectivity code */
int nL;                 /* number of layers */
int nSamples;           /* number of samples */
int hanningFlag;        /* if active convolve with hanning window */
int nF;                 /* number of frequencies */
float wCR, wCP;               /* module and phase of complex frequency */
float rho1rho2;         /* rho1 * rho2 */
float aux, auxm1, auxm2;
float auxm3, auxm4;
float angle; 		/* auxiliary variables */
float percW;            /* amount of frequency windowing */ 
float dF;               /* frequency increment */
float f1, f2;           /* frequency limits */
float dt;               /* time sampling interval */
float zs;		/* source depth */
float wR; 		/* reference frequency */
float wCRwR; 		/* complex frequency (module) / reference frequency */
float *thick, *alpha, *beta, 
      *rho, *qP, *qS;   /* elastic/inelastic constants and thickness */
float *operator;        /* shaping filter */
float *window;          /* hanning window */
float J00, J11;         /* Bessel functions order 0 and 1 */
complex *PSlowness;     /* P-wave slowness squared */
complex *SSlowness;     /* S-wave slowness squared */
complex *S2Velocity;    /* S-wave velocity squared */
complex dd;             /* auxiliary variable */
complex uC, uuC;	/* complex slowness, uuC = uC * uC */
complex uC2;		/* uC * 2 */
complex uuC2;		/* uuC * 2 */
complex zeroC;          /* cmplx(0,0) */
complex wC;		/* complex frequency */
complex coeffD[8];	/* reflection and transmition coefficients */ 
			/* for downgoing waves */
complex coeffU[8];	/* reflection and transmition coefficients */ 
			/* for upgoing waves */
complex rm[2][2];       /* R- reflectivity matrix */
complex rp[2][2];       /* R+ reflectivity matrix */
SeisSlave logInfo;      /* report structure */
