/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#ifndef TAUP_H
#define TAUP_H

/******************************************************************************
 header file with prototypes for functions used internally to compute
	forward and inverse F-K or t-x global slant stacks (taup transforms).
	Also subroutines to compute discrete forward and inverse radon 
	transforms. Radon transforms can be computed with linear, parabolic,
	hyperbolic and pseudo hyperbolic algorithms. 
	Radon subroutine were extracted from SU program suradon.c writen by 
	John Anderson 
******************************************************************************/
#include "par.h"
#include "VND.h"

/* function prototypes */
void fwd_FK_sstack (float dt, int nt, int nx, float xmin, float dx, int np,
	float pmin, float dp, float fmin, float **traces, float **out_traces);

void fwd_tx_sstack (float dt, int nt, int nx, float xmin, float dx, int np,
	float pmin, float dp, float **traces, float **out_traces);

void forward_p_transform(int nx, int nt, float dt, float pmax, float pmin,
        float dp, float depthref, float f1, float f2, float freq1, float freq2,
        int lagc, int lent, int lenx, int xopt, int ninterp, int nwin,
        float prewhite, float interoff, float offref, int igopt, float dx,
        float fx, float pmula, float pmulb, float **in_traces,
        float **out_traces);

void inverse_p_transform(int nx, int nt, float dt, float pmax, float pmin,
        float dp, float depthref, float f1, float f2, float interoff,
        float offref, int igopt, float dx, float fx, float **in_traces,
        float **out_traces);

void inv_FK_sstack (float dt, int nt, int nx, float xmin, float dx, int np,
	float pmin, float dp, float fmin, float **traces, float **out_traces); 

void inv_tx_sstack (float dt, int nt, int nx, int npoints, float xmin, 
	float dx, int np, float pmin, float dp, float **traces,
	float **out_traces); 

void rho_filter (int npoints, int nt, float dt, float *rho);

float gofx(int igopt, float offset, float intercept_off, float refdepth);

float freqweight(int j, float df, float f1, float f2);

void compute_r( float w, int nx, float *g, int np, float dp, complex *r);

void compute_rhs( float w, int nx, float *g, complex *data, int np, 
		float pmin, float dp, complex *rhs);

int ctoep( int n, complex *r, complex *a, complex *b, complex *f, complex *g );

int ctoephcg( int niter, int n, complex *a, complex *x, complex *y, 
	complex *s, complex *ss, complex *g, complex *rr);

void htmul(int n, complex *a, complex *x, complex *y);

void ga_xinterpolate(float **in_traces, float **out_traces, int ninterp, 
		int nt, int nx, float freq1, float freq2, int lagc, 
		int lent, int lenx, int xopt, float dt, int iopt);

float rcdot(int n, complex *a, complex *b);

void runav(int n,int len,float *a,float *b);

void taupmute(int ip,int ipa,int ipb,int nt, int ltap, float *rt);

#endif /* TAUP_H */
