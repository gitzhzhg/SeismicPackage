/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SMOOTH3D: $Revision: 1.10 $ ; $Date: 2011/11/16 16:42:16 $		*/

#include <par.h>

/*********************** self documentation **********************/
char *sdoc[] ={ 
"									",
" SMOOTH3D - 3D grid velocity SMOOTHing by the damped least squares	",
"									",
"	smooth3d <infile >outfile [parameters]				",
"									",
" Required Parameters:							",
" n1=		number of samples along 1st dimension			",
" n2=		number of samples along 2nd dimension			",
" n3=		number of samples along 3rd dimension			",
"									",
" Optional Parameters:							",
"									",
" Smoothing parameters (0 = no smoothing)				",
" r1=0.0	operator length in 1st dimension			",
" r2=0.0	operator length in 2nd dimension			",
" r3=0.0	operator length in 3rd dimension			",
"									",
" Sample intervals:							",
" d1=1.0	1st dimension						",
" d2=1.0	2nd dimension						",
" d3=1.0	3rd dimension						",
" iter=2	number of iteration used				",
" time=0	which dimension the time axis is (0 = no time axis)	",
" depth=1	which dimension the depth axis is (ignored when time>0)	",
" mu=1		the relative weight at maximum depth (or time)		",
" verbose=0	=1 for printing minimum wavelengths			",
" slowness=0	=1 smoothing on slowness; =0 smoothing on velocity	",
" vminc=0	velocity values below it are cliped before smoothing	",
" vmaxc=99999	velocity values above it are cliped before smoothing	",
"									",
" Notes:								",
" 1. The larger the smoothing parameters, the smoother the output velocity.",
"    These parameters are lengths of smoothing operators in each dimension.",
" 2. iter controls the orders of derivatives to be smoothed in the output",
"    velocity. e.g., iter=2 means derivatives until 2nd order smoothed.	",
" 3. mu is the multipler of smoothing parameters at the bottom compared to",
"    those at the surface.						",
" 4. Minimum wavelengths of each dimension and the total may be printed	",
"   for the resulting output velocity is. To compute these parameters for",
"   the input velocity, use r1=r2=r3=0.					",
" 5. Smoothing directly on slowness works better to preserve traveltime.",
"   So the program optionally converts the input velocity into slowness	", 
"   and smooths the slowness, then converts back into velocity.		",
"									",
NULL};

/*
 * Author:  CWP: Zhenyue Liu  March 1995
 *
 * Reference:
 * Liu, Z., 1994,"A velocity smoothing technique based on damped least squares",
 *		in Project Reveiew, May 10, 1994, Consortium Project on
 *		Seismic Inverse Methods for Complex Stuctures.
 */
/**************** end self doc ********************************/

/* Protypes for functions used internally */
void vsm3d(float ***vel,int n3,int n2,int n1,int iter,int depth,
  	float r3,float r2,float r1,float mu,int sl,float vminc,float vmaxc); 
void wavel(int n1, int n2, int n3, float d1, float d2, float d3, 
	int time, float *wl, float ***v);

int
main(int argc,char **argv)
{
 
	int  n1, n2, n3, depth, time, verbose, iter, slowness;
	float r1, r2, r3, d1, d2, d3, *wl, ***vel, mu, vminc, vmaxc;
 
	FILE *invp=stdin, *outvp=stdout;


	/* initialization */
	initargs(argc,argv) ;
	requestdoc(0);


	/*-----------get required parameters-----------*/
	if( !getparint("n1",&n1) ) n1 = 0 ;
	if( n1 <= 0 ) err("sample number of 1st dimension invalid" ) ;
	if( !getparint("n2",&n2) ) n2 = 0 ;
	if( n2 <= 0 ) err("sample number of 2nd dimension invalid" ) ;
	if( !getparint("n3",&n3) ) n3 = 0 ;
	if( n3 <= 0 ) err("sample number of 3rd dimension invalid" ) ;


	/*-----------get optional parameters-----------*/

	/* sample intervals */
	if( !getparfloat("d1",&d1) ) d1 = 1.0 ;
	if( !getparfloat("d2",&d2) ) d2 = 1.0 ;
	if( !getparfloat("d3",&d3) ) d3 = 1.0 ;

	/*   smoothing parameters    */
	if( !getparfloat("r1",&r1) || n1<4) r1 = 0. ;
	if( !getparfloat("r2",&r2) || n2<4) r2 = 0. ;
	if( !getparfloat("r3",&r3) || n3<4) r3 = 0. ;

/*	 scale smoothing parameters	*/
	r1 = (d1>0)?r1/d1:0;
	r2 = (d2>0)?r2/d2:0;
	r3 = (d3>0)?r3/d3:0;
	r1 = 0.5*r1*r1 ;
	r2 = 0.5*r2*r2 ;
	r3 = 0.5*r3*r3 ;
  
	/*   get iteration number for smoothing operator */
	if(!getparint("iter", &iter)) iter = 2;
	if(iter<=0 || iter>3) err("\t iter must be between 1 and 3!\n");

	/*   description for vertical dimension    */
	if(!getparint("time",&time )) time = 0;
	if(!getparint("depth",&depth) ) depth = 1;
	if(time) depth = time;

 
	/*   relative weight at bottom     */
	if(!getparfloat("mu",&mu) ) mu = 1.0;
	if(mu<1) err("mu must not be less than 1 \n");

	/*   smoothing on velocity or slowness     */
	if(!getparint("slowness",&slowness) ) slowness = 0;

	/*   clips of velocity before smoothing     */
	if(!getparfloat("vminc",&vminc) ) vminc = 0;
	if(!getparfloat("vmaxc",&vmaxc) ) vmaxc = 99999;

     /*   allocate input file    */
	vel  = alloc3float(n1,n2,n3) ;
	wl = alloc1float(4);

	/*   read input velocity file     */
	efread((char *)vel[0][0],sizeof(float),n1*n2*n3,invp);
 
	/*   perform smoothing operation    */
	vsm3d(vel,n3,n2,n1,iter,depth,r3,r2,r1,mu,slowness,vminc,vmaxc); 
 
	/*   write output velocity file     */
	efwrite((char *)vel[0][0],sizeof(float),n1*n2*n3,outvp);  


	if(!getparint("verbose",&verbose)) verbose = 0;
	if(verbose) {
		wavel(n1,n2,n3,d1,d2,d3,time,wl,vel);
 		fprintf(stderr,"minimum wavelengths of smoothed velocity:\n");
		fprintf(stderr,"\tlambda1 = %g,\n", wl[1]);
		fprintf(stderr,"\tlambda2 = %g,\n", wl[2]);
		fprintf(stderr,"\tlambda3 = %g,\n", wl[3]);
		fprintf(stderr,"\tlambda = %g,\n", wl[0]);
  	}
        checkpars();

	/*   close input and output files    */
	efclose(invp) ;
	efclose(outvp) ; 

	return(CWP_Exit());
}
 
void wavel(int n1, int n2, int n3, float d1, float d2, float d3,
	int time, float *wl, float ***v)
/*************************************************************************
Compute minimum wavelengths for individual dimension and total
*************************************************************************/
{
	int  i1,i2, i3;
	float  k=0, k1=0, k2=0, k3=0, dv1, dv2, dv3, dv, vel, ovel;
	float ddv1, ddv2, ddv3, ddv, scal=0.0;
	float od1=1.0/d1, od2=1.0/d2, od3=1.0/d3;
	float o2d1=od1*od1, o2d2=od2*od2, o2d3=od3*od3;
	float d2v1, d2v2, d2v3;
 
 
   	for(i3=0; i3<n3; ++i3) 
 	    for(i2=0; i2<n2; ++i2)
		for(i1=0; i1<n1; ++i1){
			vel = v[i3][i2][i1];
			ovel = 1.0/vel;
			if(i1==0||i1==n1-1) d2v1 = ddv1 = 0.;
			else {
				dv1 = (vel-v[i3][i2][i1-1])*od1*ovel;
				d2v1 = dv1*dv1;
				ddv1 = (v[i3][i2][i1-1]-2.0*vel+
					  v[i3][i2][i1+1])*o2d1*ovel;
				if(ddv1<0) ddv1 = -ddv1;
			}
			if(i2==0||i2==n2-1)  d2v2 = ddv2 = 0.;
			else {
				dv2 = (vel-v[i3][i2-1][i1])*od2*ovel;
				d2v2 = dv2*dv2;
				ddv2 = (v[i3][i2-1][i1]-2.0*vel+
					  v[i3][i2+1][i1])*o2d2*ovel;
				if(ddv2<0) ddv2 = -ddv2;
			}
			if(i3==0||i3==n3-1) d2v3 = ddv3 = 0.;
			else {
				dv3 = (vel-v[i3-1][i2][i1])*od3*ovel;
				d2v3 = dv3*dv3;
				ddv3 = (v[i3-1][i2][i1]-2.0*vel+
					  v[i3+1][i2][i1])*o2d3*ovel;
				if(ddv3<0) ddv3 = -ddv3;
			}
 			if(time) {
				scal = 2000.*ovel;
				scal = scal*scal;
			}
			if(time==1) {
				d2v1 *= scal;
				ddv1 *= scal;
			}
			else if(time==2){
				d2v2 *= scal;
				ddv2 *= scal;
			}
 			else if(time==3){
				d2v3 *= scal;
				ddv3 *= scal;
			}



  			if(k1<d2v1) k1 = d2v1;
			if(k1<ddv1) k1 = ddv1;

  			if(k2<d2v2) k2 = d2v2;
			if(k2<ddv2) k2 = ddv2;

  			if(k3<d2v3) k3 = d2v3;
			if(k3<ddv3) k3 = ddv3;

  	 		dv = d2v1+d2v2+d2v3;
			ddv = ddv1+ddv2+ddv3;
 			if(k<dv)  k = dv; 
			if(k<ddv) k = ddv; 
 		}
	 
	wl[0] = (k)? 2.0*PI/sqrt(k): -1;
	wl[1] = (k1)? 2.0*PI/sqrt(k1): -1;
	wl[2] = (k2)? 2.0*PI/sqrt(k2): -1;
	wl[3] = (k3)? 2.0*PI/sqrt(k3): -1;
}

/* Prototype for function used internally */
void tripd2(float **d, float **e, float **b, int n, int m);
 
void vsm3d(float ***v,int n3,int n2,int n1,int iter,int depth,
	 float r3,float r2,float r1,float mu,int sl,float vmin,float vmax)
/***************************************************************************
Smooth 3d-velocity.  
*************************************************************************/

{
	int  i2, i1, i3, i;		
	float **d=NULL, **e=NULL, **f=NULL, *w, ww=1.0;
 
 /*	compute the weight function */
	w = alloc1float(n1+n2+n3-2);
	if(depth==1){
		mu = (mu*mu-1.0)/(n1*n1);
		for(i1=0; i1<n1; ++i1) w[i1] = 1.0/(1+i1*i1*mu);
	}
	if(depth==2){
 		mu = (mu*mu-1.0)/(n2*n2);
		for(i2=0; i2<n2; ++i2) w[i2] = 1.0/(1+i2*i2*mu);
	}
	if(depth==3){
 		mu = (mu*mu-1.0)/(n3*n3);
		for(i3=0; i3<n3; ++i3) w[i3] = 1.0/(1+i3*i3*mu);
	}

/*	scale  smoothing parameters according to the iteration number	*/
	if(iter==1) {
		r1 /= 3.39*3.39;
		r2 /= 3.39*3.39;
		r3 /= 3.39*3.39;
	} else if(iter==2){
		r1 /= 5.19*5.19;
		r2 /= 5.19*5.19;
		r3 /= 5.19*5.19;
	} else {
		r1 /= 6.60*6.60;
		r2 /= 6.60*6.60;
		r3 /= 6.60*6.60;
	}


	/*  clip velocity  */
	for(i3=0; i3<n3; ++i3) 
	    for(i2=0; i2<n2; ++i2)
		for(i1=0; i1<n1; ++i1){
			if(v[i3][i2][i1] >vmax) v[i3][i2][i1] = vmax;
			if(v[i3][i2][i1] <vmin) v[i3][i2][i1] = vmin;
		}

	if(sl) {
	/*  smoothing on slowness  */
		for(i3=0; i3<n3; ++i3) 
			for(i2=0; i2<n2; ++i2)
				for(i1=0; i1<n1; ++i1)
					v[i3][i2][i1] = 1.0/v[i3][i2][i1];
	}
	

	if(r2>0.) {
 
/*	smoothing velocity in the second direction */

	/* allocate space */
 	d = alloc2float(n1,n2);
	e = alloc2float(n1,n2);
	f = alloc2float(n1,n2);
 
 
	for(i3=0; i3<n3; ++i3){
		if(depth==3) ww = w[i3];
	 	for(i2=0; i2<n2-1; ++i2){
			if(depth==2) ww = w[i2+1];
			for(i1=0; i1<n1; ++i1){
				if(depth==1) ww = w[i1];
				d[i2][i1] = ww+r2*2.0;
				e[i2][i1] = -r2;
 				f[i2][i1] = ww*v[i3][i2+1][i1];
			}
		}
			for(i1=0; i1<n1; ++i1){
	  		d[n2-2][i1] -= r2;
			f[0][i1] += r2*v[i3][0][i1];
  		}
	 	tripd2(d,e,f,n2-1,n1);

	    for(i=1; i<iter; ++i) {
	 	for(i2=0; i2<n2-1; ++i2){
			if(depth==2) ww = w[i2+1];
			for(i1=0; i1<n1; ++i1){
				if(depth==1) ww = w[i1];
				d[i2][i1] = ww+r2*2.0;
				e[i2][i1] = -r2;
 				f[i2][i1] *= ww;
			}
		}
			for(i1=0; i1<n1; ++i1){
	  		d[n2-2][i1] -= r2;
			f[0][i1] += r2*v[i3][0][i1];
  		}
	 	tripd2(d,e,f,n2-1,n1);
	    }

	 	for(i2=0; i2<n2-1; ++i2)
			for(i1=0; i1<n1; ++i1)
				v[i3][i2+1][i1] = f[i2][i1];
	}
	}
 
	if(r3>0.) {
/*	smooth velocity in  the third  direction */

	/* allocate space */
 	d = alloc2float(n1,n3);
	e = alloc2float(n1,n3);
	f = alloc2float(n1,n3); 
 
	for(i2=0; i2<n2; ++i2){
		if(depth==2) ww = w[i2];
	 	for(i3=0; i3<n3-1; ++i3){
			if(depth==3) ww = w[i3+1];
			for(i1=0; i1<n1; ++i1){
				if(depth==1) ww = w[i1];
				d[i3][i1] = ww+2.*r3;
				e[i3][i1] = -r3;
 				f[i3][i1] = ww*v[i3+1][i2][i1];
			}
 		}
			for(i1=0; i1<n1; ++i1){
	  		d[n3-2][i1] -= r3;
			f[0][i1] += r3*v[0][i2][i1];
  		}
	 	tripd2(d,e,f,n3-1,n1);

	    for(i=1; i<iter; ++i){
	 	for(i3=0; i3<n3-1; ++i3){
			if(depth==3) ww = w[i3+1];
			for(i1=0; i1<n1; ++i1){
				if(depth==1) ww = w[i1];
				d[i3][i1] = ww+2.*r3;
				e[i3][i1] = -r3;
 				f[i3][i1] *= ww;
			}
 		}
			for(i1=0; i1<n1; ++i1){
	  		d[n3-2][i1] -= r3;
			f[0][i1] += r3*v[0][i2][i1];
  		}
	 	tripd2(d,e,f,n3-1,n1);
	    }

	 	for(i3=0; i3<n3-1; ++i3)
			for(i1=0; i1<n1; ++i1)
				v[i3+1][i2][i1] = f[i3][i1];
	}
	}
	
	if(r1>0.) {
/*	smooth velocity in  the first direction */

	/* allocate space */
 	d = alloc2float(1,n1);
	e = alloc2float(1,n1);
	f = alloc2float(1,n1);
 
	for(i3=0; i3<n3; ++i3){
		if(depth==3) ww = w[i3];
	 	for(i2=0; i2<n2; ++i2){
			if(depth==2) ww = w[i2];
			for(i1=0; i1<n1-1; ++i1){
				if(depth==1) ww = w[i1+1];
				d[i1][0] = ww+r1*2.0;
				e[i1][0] = -r1;
 				f[i1][0] = ww*v[i3][i2][i1+1];
			}
	  		d[n1-2][0] -= r1;
			f[0][0] += r1*v[i3][i2][0];
	   		tripd2(d,e,f,n1-1,1);

		    for(i=1; i<iter; ++i) {
			for(i1=0; i1<n1-1; ++i1){
				if(depth==1) ww = w[i1+1];
				d[i1][0] = ww+r1*2.0;
				e[i1][0] = -r1;
 				f[i1][0] *= ww;
			}
	  		d[n1-2][0] -= r1;
			f[0][0] += r1*v[i3][i2][0];
	 		tripd2(d,e,f,n1-1,1);
		    }

 			for(i1=0; i1<n1-1; ++i1)
				v[i3][i2][i1+1] = f[i1][0];
		}
	}
	}

	if(sl) {
		for(i3=0; i3<n3; ++i3) 
			for(i2=0; i2<n2; ++i2)
				for(i1=0; i1<n1; ++i1)
					v[i3][i2][i1] = 1.0/v[i3][i2][i1];
	}

	free1float(w);
	if(r1>0. || r2>0. || r3>0.) {
		free2float(d);
		free2float(e);
		free2float(f);
 	}
}
	
	
void tripd2(float **d, float **e, float **b, int n, int m)
/*****************************************************************************
Given m n-by-n symmetric, tridiagonal, positive definite matri2 A's and m
n-vector b's, the following algorithm overwrites b with the solution to Ax = b.
The first dimension of arrays is independent of the algorithm. 

  d() the diagonal of A 
  e() the superdiagonal of A
*****************************************************************************/
{
	int k, i; 
	float temp;
	
	/* decomposition */
	for(k=1; k<n; ++k) 
		for(i=0; i<m; ++i){ 
	  		 temp = e[k-1][i];
	   		e[k-1][i] = temp/d[k-1][i];
	   d[k][i] -= temp*e[k-1][i];
	}

	/* substitution	*/
	for(k=1; k<n; ++k) 
		 for(i=0; i<m; ++i) 
 			b[k][i] -= e[k-1][i]*b[k-1][i];
	
	for(i=0; i<m; ++i) 
		b[n-1][i] /=d[n-1][i];

	for(k=n-1; k>0; --k) 
		 for(i=0; i<m; ++i) 
			 b[k-1][i] = b[k-1][i]/d[k-1][i]-e[k-1][i]*b[k][i]; 
	
}
