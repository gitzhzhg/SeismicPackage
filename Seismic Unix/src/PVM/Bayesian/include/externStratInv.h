/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

extern int IMPEDANCE;                 /* inversion done with impedance */
extern int numberPar;                 /* number of parameter sets */
extern int nDM;                       /* number of samples in misfit */
                                      /* computation */
extern int nProc;                     /* number of processors */       
extern int nTotalSamples;             /* total number of samples in misfit */
extern int dataIsFit;                 /* flag for data fitting */
extern int DIRECTWAVE;                /* flag for direct wave */
extern int MUTE;                      /* flag for muting data */
extern int vpFrechet;                 /* flag for Frechet derivatives p-wave */
extern int vsFrechet;                 /* flag for Frechet derivatives s-wave */
extern int rhoFrechet;                /* flag for Frechet derivatives dens   */
extern int vpF;                       /* auxiliary flag */
extern int vsF;                       /* auxiliary flag */
extern int rhoF;                      /* auxiliary flag */ 
extern int PRIOR;                     /* use of PRIOR indormation */  
extern int DATACOV;                   /* use of of data covariances */  
extern int RADIAL, VERTICAL;          /* type of displacements */
extern int gradCount, modCount;       /* counting gradient and objective */
                                      /* function evaluations */
extern int initF, lastF;              /* integer frequency delimiters */    
extern int nFreqPart;                 /* number of frequency partitions */   
extern int lim[2];                    /* specify the target for inversion */
extern int *processes;                /* array with process ids */
extern int **procInfo;                /* frequency limits for each */
                                      /* processor */ 
extern int **statusFreq;              /* monitors processed frequencies */    
extern int limRange;                  /* lim[1] - lim[0] */
extern float scaleData;               /* used in data scaling */ 
extern float noiseVar;                /* standard deviation of noise */
extern float J00, J11;                /* Bessel functions */
extern float dt;                      /* time sampling interval */
extern float tMax;                    /* maximum comparing time */
extern float *thick, *alpha, *beta, 
      *rho, *qP, *qS;                 /* elastic constants and thickness */
extern float *alphaMean, *betaMean, *rhoMean;
      			              /* mean models */ 
extern float *alpha0, *beta0, *rho0;
      			              /* used in the model update */
extern float wCRwR; 		      /* complex frequency / reference */
                                      /* frequency */
extern float wCR, wCP;                /* module and phase of complex */
                                      /* frequency */
extern float t1, t2;                  /* time misfit limits */
extern float *t1Mute;                 /* muting information */
extern float aux, auxm1, auxm2;
extern float auxm3, auxm4;
extern float angle;   	  	      /* auxiliary variables */
extern float alpham, betam, rhom;     /* vp, vs and rho for source layer */
extern float epslon1, epslon2;        /* auxiliary quantities */ 
extern float oFNorm;                  /* normalization for objective  */
                                      /* function */
extern float wRef;                    /* used in complex slowness */
extern float *window;                 /* hanning window */ 
extern float *taper;                  /* taper for slowness domain */
extern float *CD;                     /* data covariance matrix */
extern float *CMvP;                   /* vP model covariance matrix */
extern float *CMvS;                   /* vS model covariance matrix */
extern float *CMrho;                  /* rho model covariance matrix */
extern float **dataObs;	              /* observed data (frequency domain) */
extern float *recArray;               /* receiver array */    
extern complex **resCD;               /* current residual dotted into */
                                      /* covariance */
extern complex zeroC;                 /* cmplx(0,0) */
extern complex aux1, aux2, aux3;
extern INFO info[1];                 /* basic information for modeling */ 
