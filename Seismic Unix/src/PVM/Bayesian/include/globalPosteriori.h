/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

int numberPar;                /* number of parameter sets */
int IMPEDANCE;                /* IMPEDANCE flag */        
int nDM;                      /* number of samples in misfit computation */
int vpFrechet = 0;            /* flag for Frechet derivatives p-wave */
int vsFrechet = 0;            /* flag for Frechet derivatives s-wave */
int rhoFrechet = 0;           /* flag for Frechet derivatives density */
int ipFrechet;                /* flag for Frechet derivatives p-wave imp. */
int isFrechet;                /* flag for Frechet derivatives s-wave imp. */
int vpF;                      /* auxiliary flag */
int vsF;                      /* auxiliary flag */
int rhoF;                     /* auxiliary flag */
int PRIOR;                    /* use of PRIOR indormation */
int nL;                       /* number of layers */
int nSamples;                 /* number of samples / trace */
int RADIAL, VERTICAL;         /* type of displacements */
int nF;                       /* number of frequencies */
int gradCount, modCount;      /* counting gradient and objective */
                              /* function evaluations */
int lim[2];                   /* specify the target for inversion */
int limRange;                 /* lim[1] - lim[0] */
int nU;                       /* number of slownesses */
int nR;                       /* number of receivers */
int hanningFlag;              /* if active smooth spectrum with */
                              /* hanning window */
int directWave;               /* direct wave flag */   
float J00, J11;               /* Bessel functions */
float dt;                     /* time sampling interval */
float tMax;                   /* maximum comparing time */
float **obsData  ;            /* input data */
float *recArray;              /* receiver array */  
float **modelData;            /* model data */  
float **F;                    /* Frechet derivative matrix */
float *thick, *alpha, *beta, 
      *rho, *qP, *qS;         /* elastic constants and thickness */
float *alphaMean, *betaMean, *rhoMean;
      			      /* mean models */
float *alpha0, *beta0, *rho0;
      			      /* used in the model update */
float zs;                     /* source depth */  
float wR; 	   	      /* reference frequency */
float wCRwR; 		      /* complex frequency / reference frequency */
float wCR, wCP;               /* module and phase of complex frequency */
float dF;                     /* frequency increment */
float f1, f2;                 /* frequency limits */
float t1, t2;                 /* time misfit limits */
float aux, auxm1, auxm2;
float auxm3, auxm4;
float angle;   	  	      /* auxiliary variables */
float tau;                    /* magnitude of wrap-around attenuation */
float r1, dR;                 /* defines receiver array */
float u1, u2;                 /* slowness window */
float alpham, betam, rhom;    /* vp, vs and rho for source layer */
float epslon1, epslon2;       /* auxiliary quantities */ 
float dU;                     /* slowness interval */
float wRef;                   /* used in complex slowness */
float oFNorm;                 /* normalization for objective  */
                              /* function */
float *window;                /* hanning window */ 
float *taper;                 /* taper for slowness domain */
float *CD;                    /* data covariance matrix */
float *CMP;                   /* p-wave model covariance matrix */
float *CMS;                   /* s-wave model covariance matrix */
float *CMrho;                 /* density model covariance matrix */
float **dataObs;	      /* observed data (frequency domain) */
complex dUC;                  /* complex slowness interval */
complex muC;                  /* uC * -1 */
complex irr[2][2];            /* = (I - R-R+) */
complex irrI[2][2];           /* = (I - R-R+)^-1 */
complex **v1, **v2;           /* potential vectors */
complex h[2][2];              /* free-surface matrix */
complex **PSlowness;          /* P-wave slowness squared */
complex **SSlowness;          /* S-wave slowness squared */
complex **S2Velocity;         /* S-wave velocity squared */
complex **derFactor;          /* Auxiliar quantity used in gradient */
                              /* computation */
complex **resCD;              /* current residual dotted into covariance */
complex ***DmB;               /* FRECHET derivatives of the reflectivity */
                              /* matrices */
complex dd;                   /* auxiliary variable */
complex uC, uuC;	      /* complex slowness, uuC = uC * uC */
complex uC2;		      /* uC * 2 */
complex uuC2;		      /* uuC * 2 */
complex zeroC;                /* cmplx(0,0) */
complex wC;		      /* complex frequency */
complex coeffD[8];	      /* reflection and transmition coefficients */ 
			      /* for downgoing waves */
complex coeffU[8];	      /* reflection and transmition coefficients */ 
			      /* for upgoing waves */
complex coeffDFr[3][8];	      /* Frechet derivatives */ 
complex coeffUFr[3][8];	      /* Frechet derivatives */ 
complex rm[2][2];             /* R- reflectivity matrix */
complex rp[2][2];             /* R+ reflectivity matrix */
complex aux1, aux2, aux3;
complex **aux11, **aux12, **aux21, **aux22;
complex **aux11Old, **aux12Old, **aux21Old, **aux22Old;
                              /* auxiliary variable */
