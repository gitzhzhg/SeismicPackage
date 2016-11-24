/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* include file for libAnisotropy */

#ifndef CWP_ANISOTROPY_H

#define CWP_ANISOTROPY_H

#include "par.h"

/* stiffness parameter */
typedef struct StiffStruct {
	double a1111;
	double a3333;
	double a1133;
	double a1313;
	double a1113;
	double a3313;
	double a1212;
	double a2323;
	double a1223;
} Stiff2D;

typedef struct Stiff3DStruct {
	double a1111;
	double a1112;
	double a1113;
	double a1122;
	double a1123;
	double a1133;
	double a1212;
	double a1213;
	double a1222;
	double a1223;
	double a1233;
	double a1313;
	double a1322;
	double a1323;
	double a1333;
	double a2222;
	double a2223;
	double a2233;
	double a2323;
	double a2333;
	double a3333;
} Stiff3D;

typedef struct MatrixStruct {
	double a11;
	double a12;
	double a13;
	double a21;
	double a22;
	double a23;
	double a31;
	double a32;
	double a33;
} Matrix;

typedef struct Vector3DStruct {
	double x;
	double y;
	double z;
} Vector3D;

/* prototypes for functions defined */

int graebner2D(Stiff2D *spar1, double rho1, Stiff2D *spar2, double rho2,
	 double pl, int modei, int modet, int rort, double *coeff);

void rottens2D (Stiff2D *spar, double phi);

int thom2stiffVTI (double vp, double vs, double eps, double delta, double 
	gamma, Stiff2D *spar, int sign);

int thom2stiffTI (double vp, double vs, double eps, double delta, double gamma,
	double phi, Stiff2D *spar, int sign);

int stiff2thomVTI (double a1111, double a3333, double a1133, double a1313, 	
	double a1212, double *vp, double *vs, double *eps,
        double *delta, double *gamma);

int gvelpolTI (double a1111, double a3333, double a1133, double a1313,
	double a1113, double a3313, double px, double pz, double *vgx,
	double *vgz, double *g11n, double *g13n, double *g33n);

void rotvector (double *x, double *z, double phi );


int p_hor2DTI (Stiff2D *spar, double s, int mode, double *p);


int p_vert2DTI(Stiff2D *spar1, double pl, int modei, double *p_vert);


int v_phase2DTI(Stiff2D *spar1, double sangle, int mode, double *v_phase);

int stiff2tv(Stiff2D *spar,double *alpha,double *beta,double *ev,
	     double *dv,double *gv);

int thom2tv(double vp,double vs,double eps,double delta,double gamma,
	    double *alpha,double *beta,double *ev,double *dv,double *gv);

#endif
