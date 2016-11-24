/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* couple of definitions */
#define I       	cmplx(0,1)
#define IC      	cmplx(0,-1)
#define TAUMAX      	0.489
#define FACTOR      	1.0001
#define BOUND      	0.8	
#define INITIAL_LAMBDA  1.0 
#define LAMBDA_MIN  	0.05 
#define ALPHA      	0.20
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

void RmFrechet();

void filter(float percW);

void frechetRm(complex E[2][2], complex tD[2][2], complex tUinv[2][2], 
	       complex mTtD[2][2], complex mT[2][2], complex rU[2][2], 
	       complex inv[2][2], complex wThick, complex am, 
	       complex bm, int iL);

void matMult(complex A[2][2], complex B[2][2], complex C[2][2]);

void buildFreeSurfaceCompensation(complex am, complex bm);

void freeSurface(complex **v1, complex **v2);

void freeSurfaceFrechet(complex **v1, complex **v2);

void displacementsFrechet(int iU);

void gradient();

void modeling(char* info);

void inputCovar(char* corrDataFile, char *corrModelFile[3]);  
