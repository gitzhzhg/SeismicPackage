/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* global variables for reflectivity code */
int nL;                 /* number of layers */
int nSamples;           /* number of samples */
int numberPar;          /* number of independent parameters */
int limRange;           /* #layers in target zone for inversion */
int lim[2];             /* defines target zone for inversion */  
int vpF;                /* auxiliary flag */
int vsF;                /* auxiliary flag */
int rhoF;               /* auxiliary flag */        
int vpFrechet;          /* p-wave Frechet derivatiave flag */
int vsFrechet;          /* s-wave Frechet derivatiave flag */ 
int rhoFrechet;         /* density Frechet derivatiave flag */ 
int hanningFlag;        /* if active convolve with hanning window */
int nF;                 /* number of frequencies */
int nR;                 /* number of receivers */       
int VERTICAL, RADIAL;   /* specifies geophone orientation */  
float *taper;           /* taper for slowness domain */       
float *recArray;        /* receiver array */      
float wCR, wCP;         /* module and phase of complex frequency */
float rho1rho2;         /* rho1 * rho2 */
float aux, auxm1, auxm2;
float auxm3, auxm4;
float angle; 		/* auxiliary variables */
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
complex **PSlowness;    /* P-wave slowness squared */
complex **SSlowness;    /* S-wave slowness squared */
complex **S2Velocity;   /* S-wave velocity squared */
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
complex aux1, aux2, aux3;
                        /* auxiliar quantities */
complex h[2][2];        /* free-surface compensation */
complex ***DmB;         /* Frechet derivative matrix */
complex **aux11, **aux12, **aux21, **aux22;
complex **aux11Old, **aux12Old, **aux21Old, **aux22Old;
                        /* auxiliary variable */  
complex coeffDFr[3][8]; /* Frechet derivatives */
complex coeffUFr[3][8]; /* Frechet derivatives */    
complex **v1, **v2;     /* auxiliar variables */       
complex **derFactor;    /* auxiliar quantity for Frechet derivative */
SeisSlave logInfo;      /* report structure */
