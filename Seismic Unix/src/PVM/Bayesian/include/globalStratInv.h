/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

int IMPEDANCE;                /* inversion done with impedance */
int numberPar;                /* number of parameter sets */
int nDM;                      /* number of samples in misfit computation */
int nProc;                    /* number of processors */
int nTotalSamples;            /* total number of samples in misfit */
int dataIsFit;                /* flag for data fitting */      
int DIRECTWAVE;               /* flag for direct wave */  
int MUTE;                     /* flag for muting synthetic data */  
int vpFrechet;                /* flag for Frechet derivatives p-wave */
int vsFrechet;                /* flag for Frechet derivatives s-wave */
int rhoFrechet;               /* flag for Frechet derivatives density */
int vpF;                      /* auxiliary flag */
int vsF;                      /* auxiliary flag */
int rhoF;                     /* auxiliary flag */  
int PRIOR;                    /* use of PRIOR indormation */
int DATACOV;                  /* use of of data covariances */
int RADIAL, VERTICAL;         /* type of displacements */
int gradCount, modCount;      /* counting gradient and objective */
                              /* function evaluations */
int initF, lastF;             /* integer frequency delimiters */  
int nFreqPart;                /* number of frequency partitions */    
int lim[2];                   /* specify the target for inversion */
int limRange;                 /* lim[1] - lim[0] */
int *processes;               /* array with process ids */      
int **procInfo;               /* frequency limits for each processor */ 
int **statusFreq;             /* monitors processed frequencies */
float scaleData;              /* used in data scaling */   
float noiseVar;               /* standard deviation of noise */
float J00, J11;               /* Bessel functions */
float dt;                     /* time sampling interval */
float tMax;                   /* maximum comparing time */
float *thick, *alpha, *beta, 
      *rho, *qP, *qS;         /* elastic constants and thickness */
float *alphaMean, *betaMean, *rhoMean;
      			      /* mean models */
float *alpha0, *beta0, *rho0;
      			      /* used in the model update */
float wCRwR; 		      /* complex frequency / reference frequency */
float wCR, wCP;               /* module and phase of complex frequency */
float t1, t2;                 /* time misfit limits */
float *t1Mute;                /* muting information */
float aux, auxm1, auxm2;
float auxm3, auxm4;
float angle;   	  	      /* auxiliary variables */
float alpham, betam, rhom;    /* vp, vs and rho for source layer */
float epslon1, epslon2;       /* auxiliary quantities */ 
float wRef;                   /* used in complex slowness */
float oFNorm;                 /* normalization for objective  */
                              /* function */
float *window;                /* hanning window */ 
float *taper;                 /* taper for slowness domain */
float *CD;                    /* data covariance matrix */
float *CMvP;                  /* vP model covariance matrix */
float *CMvS;                  /* vS model covariance matrix */
float *CMrho;                 /* rho model covariance matrix */
float **dataObs;	      /* observed data (frequency domain) */
float *recArray;              /* receiver array */
complex **resCD;              /* current residual dotted into covariance */
complex zeroC;                /* cmplx(0,0) */
complex aux1, aux2, aux3;
INFO info[1];                 /* basic information for modeling */  
