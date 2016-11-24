/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

extern int numberPar;                 /* number of parameter sets */
extern int IMPEDANCE;                 /* IMPEDANCE flag */
extern int nDM;                       /* number of samples in misfit */
                                      /* computation */
extern int vpFrechet;                 /* flag for Frechet derivatives */
                                      /* p-wave */
extern int vsFrechet;                 /* flag for Frechet derivatives */
                                      /* s-wave */
extern int rhoFrechet;                /* flag for Frechet derivatives */
				      /* density */
extern int ipFrechet;                 /* flag for Frechet derivatives */
                                      /* p-wave imp. */
extern int isFrechet;                 /* flag for Frechet derivatives */
                                      /* s-wave imp. */  
extern int vpF;                       /* auxiliary flag */
extern int vsF;                       /* auxiliary flag */
extern int rhoF;                      /* auxiliary flag */      
extern int PRIOR;                     /* use of PRIOR indormation */  
extern int nL;                        /* number of layers */
extern int nSamples;                  /* number of samples / trace */
extern int RADIAL, VERTICAL;          /* type of displacements */
extern int nF;                        /* number of frequencies */
extern int gradCount, modCount;       /* counting gradient and objective */
                                      /* function evaluations */
extern int lim[2];                    /* specify the target for inversion */
extern int limRange;                  /* lim[1] - lim[0] */
extern int nU;                        /* number of slownesses */
extern int nR;                        /* number of receivers */
extern int hanningFlag;               /* if active smooth spectrum with */
                                      /* hanning window */
extern int directWave;                /* direct wave flag */ 
extern float J00, J11;                /* Bessel functions */
extern float dt;                      /* time sampling interval */
extern float tMax;                    /* maximum comparing time */
extern float **obsData;               /* input data */
extern float *recArray;               /* receiver array */    
extern float **modelData;             /* model data */
extern float **F;                     /* Frechet derivative matrix */ 
extern float *thick, *alpha, *beta, 
      *rho, *qP, *qS;                 /* elastic constants and thickness */
extern float *alphaMean, *betaMean, *rhoMean;
      			              /* mean models */ 
extern float *alpha0, *beta0, *rho0;
      			              /* used in the model update */
extern float zs;                      /* source depth */  
extern float wR; 	   	      /* reference frequency */
extern float wCRwR; 		      /* complex frequency / reference */
                                      /* frequency */
extern float wCR, wCP;                /* module and phase of complex */
                                      /* frequency */
extern float dF;                      /* frequency increment */
extern float f1, f2;                  /* frequency limits */
extern float t1, t2;                  /* time misfit limits */
extern float aux, auxm1, auxm2;
extern float auxm3, auxm4;
extern float angle;   	  	      /* auxiliary variables */
extern float tau;                     /* magnitude of wrap-around */
                                      /* attenuation */
extern float r1, dR;                  /* defines receiver array */
extern float u1, u2;                  /* slowness window */
extern float alpham, betam, rhom;     /* vp, vs and rho for source layer */
extern float epslon1, epslon2;        /* auxiliary quantities */ 
extern float dU;                      /* slowness interval */
extern float oFNorm;                  /* normalization for objective  */
                                      /* function */
extern float wRef;                    /* used in complex slowness */
extern float *window;                 /* hanning window */ 
extern float *taper;                  /* taper for slowness domain */
extern float *CD;                     /* data covariance matrix */
extern float *CMP;                    /* p-wave model covariance matrix */
extern float *CMS;                    /* s-wave model covariance matrix */
extern float *CMrho;                  /* density model covariance matrix */
extern float **dataObs;	              /* observed data (frequency domain) */
extern complex dUC;                   /* extern complex slowness interval */
extern complex muC;                   /* uC * -1 */
extern complex irr[2][2];             /* = (I - R-R+) */
extern complex irrI[2][2];            /* = (I - R-R+)^-1 */
extern complex **v1, **v2;            /* potential vectors */
extern complex h[2][2];               /* free-surface matrix */
extern complex **PSlowness;           /* P-wave slowness squared */
extern complex **SSlowness;           /* S-wave slowness squared */
extern complex **S2Velocity;          /* S-wave velocity squared */
extern complex **derFactor;           /* Auxiliar quantity used in gradient */
                                      /* computation */
extern complex **resCD;               /* current residual dotted into */
                                      /* covariance */
extern complex ***DmB;                /* FRECHET derivatives  */
                                      /* matrices */
extern complex dd;                    /* auxiliary variable */
extern complex uC, uuC;	              /* extern complex slowness */
                                      /* uuC = uC * uC */
extern complex uC2;		      /* uC * 2 */
extern complex uuC2;		      /* uuC * 2 */
extern complex zeroC;                 /* cmplx(0,0) */
extern complex wC;		      /* extern complex frequency */
extern complex coeffD[8];	      /* reflection and transmition */ 
			              /* coefficients for downgoing waves */
extern complex coeffU[8];	      /* reflection and transmition */
                                      /* coefficients  for upgoing waves */
extern complex coeffDFr[3][8];	      /* Frechet derivatives */ 
extern complex coeffUFr[3][8];	      /* Frechet derivatives */ 
extern complex rm[2][2];              /* R- reflectivity matrix */
extern complex rp[2][2];              /* R+ reflectivity matrix */
extern complex aux1, aux2, aux3;
extern complex **aux11, **aux12, **aux21, **aux22;
extern complex **aux11Old, **aux12Old, **aux21Old, **aux22Old;
                                      /* auxiliary variable */
