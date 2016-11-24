/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* couple of definitions */
typedef struct _generalInfo {
   char recFile[100];
   char id[100];
   int nR;
   int directWave;
   int nL;
   int nF;
   int nU;
   int hanningFlag;
   int numberPar;
   int limRange;
   int nSamples;
   int nFreqProc;
   int vertical;
   int radial;
   int lim[2];
   int vpFrechet;
   int vsFrechet;
   int rhoFrechet;
   int verbose;
   float r1;
   float dR;
   float zs;
   float u1;
   float u2;
   float dU;
   float f1;
   float f2;
   float dF;
   float F1, F2, F3;
   float percU, percW;
   float wR;
   float tau;
} INFO;
   
#define I       	cmplx(0,1)
#define IC      	cmplx(0,-1)
#define TAUMAX      	0.489
#define FACTOR      	1.0001
#define BOUND      	0.8	
#define INITIAL_LAMBDA  1.0 
#define LAMBDA_MIN  	0.4 
#define ALPHA      	0.25
#define MAX_ITER_LS    	20	
#define RELATIVE_CHANGE	0.00001	
#define MIN_GRAD    	0.01	
#define MIN_CHANGE    	0.01	
#define PERC_WINDOW    	0.05	
#define SGN0(x)         ((x) <= 0 ? -1.0 : 1.0)

/* function declaration */

void RTd(int iL1, int iL2);

void RTu(int iL1, int iL2);

void frechetRTd(int iL1, int iL2, int iF);

void frechetRTu(int iL1, int iL2, int iF);

void horSlowness();

void horSlownessFrechet();

void Bessels(float arg);

void Rm();

void RmFrechet();

void Rp();

void filter(float percW);

void frechetRm(complex E[2][2], complex tD[2][2], complex tUinv[2][2], 
	       complex mTtD[2][2], complex mT[2][2], complex rU[2][2], 
	       complex inv[2][2], complex wThick, complex am, 
	       complex bm, int iL);

void matMult(complex A[2][2], complex B[2][2], complex C[2][2]);

void buildFreeSurfaceCompensation(complex am, complex bm);

void freeSurface(complex **v1, complex **v2);

void freeSurfaceFrechet(complex **v1, complex **v2);

void displacements(int iU);

void displacementsFrechet(int iU);

float modeling();

void gradient(float *gradient);

void inputData(char* dataFile);

void inputCovar(char* corrDataFile, char *corrModelFile[3]);  

float dot(float *a, float *b, int n);

float lineSearch(float *search, float of0, float slope);

int update(float step, float *search);

void restore(float step, float *search);

float newSearch(float *grad0, float *grad1, float *search);

int stop(float *grad0, float *grad1, float oF0, float oF1, int maxIter);

float walltime();

void normalize(float *vector, int n);
