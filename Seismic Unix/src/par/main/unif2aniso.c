/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.			*/

/* UNIF2ANISO: $Revision: 1.16 $ ; $Date: 2011/11/22 16:49:35 $	*/

#include "par.h"
#include "anisotropy.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									 ",
" UNIF2ANISO - generate a 2-D UNIFormly sampled profile of elastic	",
"	constants from a layered model.					",
" 									",
"  unif2aniso < infile [Parameters]					",
" 									",
" Required Parameters:							",
" none 									",
" 									",
" Optional Parameters:							",
" ninf=5	number of interfaces					",
" nx=100	number of x samples (2nd dimension)			",
" nz=100	number of z samples (1st dimension)			",
" dx=10		x sampling interval					",
" dz=10		z sampling interval					",
" 									",
" npmax=201	maximum number of points on interfaces			",
" 									",
" fx=0.0	first x sample						",
" fz=0.0	first z sample						",
" 									",
" 									",
" x0=0.0,0.0,..., 	distance x at which vp00 and vs00 are specified	",
" z0=0.0,0.0,..., 	depth z at which vp00 and vs00 are specified	",
" 									",
" vp00=1500,2000,...,	P-velocity at each x0,z0 (m/sec)		",
" vs00=866,1155...,	S-velocity at each x0,z0 (m/sec)		",
" rho00=1000,1100,...,	density at each x0,z0 (kg/m^3)			",
" q00=110,120,130,..,		attenuation Q, at each x0,z0 (kg/m^3)	",
" 									",
" eps00=0,0,0...,	Thomsen or Sayers epsilon			",
" delta00=0,0,0...,	Thomsen or Sayers delta				",
" gamma00=0,0,0...,	Thomsen or Sayers gamma				",
" 									",
" dqdx=0.0,0.0,...,	x-derivative of Q (d q/dx)			",
" dqdz=0.0,0.0,...,	z-derivative of Q (d q/dz)			",
" 									",
" drdx=0.0,0.0,...,	x-derivative of density (d rho/dx)		",
" drdz=0.0,0.0,...,	z-derivative of density (d rho/dz)		",
" 									",
" dvpdx=0.0,0.0,...,	x-derivative of P-velocity (dvp/dx)		",
" dvpdz=0.0,0.0,...,	z-derivative of P-velocity (dvs/dz)		",
" 									",
" dvsdx=0.0,0.0,...,	x-derivative of S-velocity (dvs/dx)		",
" dvsdz=0.0,0.0,...,	z-derivative of S-velocity (dvs/dz)		",
" 									",
" dedx=0.0,0.0,...,	x-derivative of epsilon (de/dx)			",
" dedz=0.0,0.0,...,	z-derivative of epsilon with depth z (de/dz)	",
" 									",
" dddx=0.0,0.0,...,	x-derivative of delta (dd/dx)			",
" dddz=0.0,0.0,...,	z-derivative of delta (dd/dz)			",
" 									",
" dgdz=0.0,0.0,...,	x-derivative of gamma (dg/dz)			",
" dgdx=0.0,0.0,...,	z-derivative of gamma (dg/dx)			",
" 									",
" phi00=0,0,..., 	rotation angle(s) in each layer			",
" 									",
" ...output filenames 							",
" c11_file=c11_file	output filename for c11 values	 		",
" c13_file=c13_file	output filename for c13 values	 		",
" c15_file=c15_file	output filename for c15 values	 		",
" c33_file=c33_file	output filename for c33 values	 		",
" c35_file=c35_file	output filename for c35 values	 		",
" c44_file=c44_file	output filename for c44 values	 		",
" c55_file=c55_file	output filename for c55 values	 		",
" c66_file=c66_file	output filename for c66 values	 		",
" 									",
" rho_file=rho_file	output filename for density values 		",
" q_file=q_file		output filename for Q values	 		",
" 									",
" paramtype=1   =1 Thomsen parameters, =0 Sayers parameters(see below)	",
" method=linear		for linear interpolation of interface		",
" 			=mono for monotonic cubic interpolation of interface",
"			=akima for Akima's cubic interpolation of interface",
"			=spline for cubic spline interpolation of interface",
" 									",
" tfile=		=testfilename  if set, a sample input dataset is",
" 			 output to \"testfilename\".			",
" 			 						",
" Notes:								",
" The input file is an ASCII file containing x z values representing a	",
" piecewise continuous velocity model with a flat surface on top.	",
" 			 						",
" The surface and each successive boundary between media is represented ",
" by a list of selected x z pairs written column form. The first and	",
" last x values must be the same for all boundaries. Use the entry	",
" 1.0  -99999  to separate the entries for successive boundaries. No	",
" boundary may cross another. Note that the choice of the method of	",
" interpolation may cause boundaries to cross that do not appear to	",
" cross in the input data file.						",
" 			 						",
" The number of interfaces is specified by the parameter \"ninf\". This ",
" number does not include the top surface of the model. The input data	",
" format is the same as a CSHOT model file with all comments removed.	",
" 			 						",
" The algorithm works by transforming the P-wavespeed , S-wavespeed,	",
" density and the Thomsen or Sayers parameters epsilon, delta, and gamma",
" into elastic stiffness coefficients. Furthermore, the	user can specify",
" rotations, phi, to the elasticity tensor in each layer.		",
"  									",
" Common ranges of Thomsen parameters are				",
"  epsilon:  0.0 -> 0.5							",
"  delta:   -0.2 -> 0.4							",
"  gamma:	0.0 -> 0.4							",
" 									",
" If only P-wave, S-wave velocities and density is given as input,	",
" the model is, by definition,  isotropic.				",
" 									",
" If files containing Thomsen/Sayers parameters are given, the model	",
" will be assumed to have VTI symmetry.		 			",
"									",
" Example using test input file generating feature:			",
" unif2aniso tfile=testfilename  produces a 5 interface demonstration model",
" unif2aniso < testfilename 						",
" ximage < c11_file n1=100 n2=100					",
" ximage < c13_file n1=100 n2=100					",
" ximage < c15_file n1=100 n2=100					",
" ximage < c33_file n1=100 n2=100					",
" ximage < c35_file n1=100 n2=100					",
" ximage < c44_file n1=100 n2=100					",
" ximage < c55_file n1=100 n2=100					",
" ximage < c66_file n1=100 n2=100					",
" ximage < rho_file n1=100 n2=100					",
" ximage < q_file   n1=100 n2=100					",
"									",
NULL};

/*
 * Credits:
 *	CWP: John Stockwell, April 2005. 
 * 	CWP: based on program unif2 by Zhenyue Liu, 1994 
 */

/**************** end self doc *******************************************/

/* sample input data set */
char *sample[] = {
"   0	 0",
"1000	 0",
"   1	-99999",
"   0	200",
"1000	200",
"   1	-99999",
"   0	400",
" 100.	400",
" 400	560",
" 550.	560",
"1000.	360",
"   1	-99999",
"   0	800",
" 100.	800",
" 400.	750.",
" 500.	780",
" 550.	780",
"1000.	500",
"   1.   -99999",
"   0	950",
" 100	950",
" 400	900.",
" 500	800.",
" 550	800.",
" 750	920",
"1000	930",
"   1	-99999",
"   0	1000",
"1000	1000",
"   1	-99999",
NULL};

/* pointer to that test dataset */
char **sampleptr=sample;

Stiff2D spar;		/* stiffness tensor			*/

int
main(int argc, char **argv)
{
	int nx;		/* size of 2nd (slow) dimension of data */
	int nz;		/* size of 1st (fast) dimension of data */
	int i,j,ix,iz;	/* counters				*/
	int ninf;	/* number of interfaces			*/
	int npmax;	/* max number of points on an interface	*/
	int npt;	/* number of points on an interface	*/

	int paramtype=0;/* =1 Thomsen, =0 Sayers 		*/
	int verbose=0;	/* =0 silent, =1 chatty 		*/

	float z;	/* fast dimension coordinate		*/
	float fx;	/* first value in x			*/
	float fz;	/* first value in z			*/
	float dx;	/* x sampling interval			*/
	float dz;	/* z sampling interval				*/


	float *vp=NULL;		/* P-velocity function calculated	*/
	float *vs=NULL;		/* S-velocity function calculated	*/
	float *rho=NULL;	/* density function calculated		*/
	float *q=NULL;		/* Q function calculated		*/
	float *epsilon=NULL;	/* epsilon function  calculated		*/
	float *delta=NULL;	/* delta function calculated		*/
	float *gamma=NULL;	/* delta function calculated		*/
	double *phi=NULL;	/* angle to rotate elasticity tensor */


	float *x0=NULL;		/* initial x position values		*/
	float *z0=NULL;		/* initial z position values		*/
	float *vp00=NULL;	/* P-wavespeeds at x0,z0 positions	*/
	float *vs00=NULL;	/* S-wavespeeds at x0,z0 positions	*/
	float *rho00=NULL;	/* densities at x0,z0 positions		*/
	float *q00=NULL;	/* Q values at x0,z0 positions		*/
	float *eps00=NULL;	/* epsilon at x0,z0 positions		*/
	float *delta00=NULL;	/* delta at x0,z0 positions		*/
	float *gamma00=NULL;	/* gamma at x0,z0 positions		*/
	double *phi00=NULL;	/* angle to rotate elasticity tensor */

	float *dvpdx=NULL;	/* x rates of change of V-P by layer	*/ 
	float *dvpdz=NULL;	/* z rates of change of V-P by layer	*/
	float *dvsdx=NULL;	/* x rates of change of V-S by layer	*/ 
	float *dvsdz=NULL;	/* z rates of change of V-S by layer	*/
	float *drdx=NULL;	/* x rates of change of density by layer*/ 
	float *drdz=NULL;	/* z rates of change of density by layer*/
	float *dqdx=NULL;	/* x rates of change of Q by layer	*/ 
	float *dqdz=NULL;	/* z rates of change of Q by layer	*/
	float *dedx=NULL;	/* x rates of change of epsilon by layer*/ 
	float *dedz=NULL;	/* z rates of change of epsilon by layer*/
	float *dddx=NULL;	/* x rates of change of delta by layer*/ 
	float *dddz=NULL;	/* z rates of change of delta by layer*/
	float *dgdx=NULL;	/* x rates of change of gamma by layer*/ 
	float *dgdz=NULL;	/* z rates of change of gamma by layer	*/


	float **zs=NULL;	/* array of z values for interpolator	*/
	float *xs=NULL;		/* array of x values for interpolator	*/
	float *xint=NULL;	/* x values defining an interface	*/
	float *zint=NULL;	/* z values defining an interface	*/

	float (*zind)[4]=NULL;	/* array of interpolation coefficients	*/ 

   	char *method="linear";	/* interpolation method		*/
	char *tfile="";		/* test filename		*/
	FILE *tfilefp;		/* ...its file pointer		*/	

	/* .. stiffness values */
	float **c11=NULL;	/* stiffness c11		*/
	float **c13=NULL;	/* stiffness c13		*/
	float **c33=NULL;	/* stiffness c33		*/
	float **c15=NULL;	/* stiffness c15		*/
	float **c35=NULL;	/* stiffness c15		*/
	float **c55=NULL;	/* stiffness c15		*/
	float **c44=NULL;	/* stiffness c44		*/
	float **c66=NULL;	/* stiffness c66		*/
	float **rhooutput=NULL;	/* density values		*/
	float **qoutput=NULL;	/* density values		*/

	/* ... output files */
	char *c11_file;		/* c11 filename		*/
	FILE *c11fp=NULL;	/* ... its file pointer */
	char *c13_file;		/* c13 filename		*/
	FILE *c13fp=NULL;	/* ... its file pointer */
	char *c33_file;		/* c33 filename		*/
	FILE *c33fp=NULL;	/* ... its file pointer */
	char *c15_file;		/* c15 filename		*/
	FILE *c15fp=NULL;	/* ... its file pointer */
	char *c35_file;		/* c35 filename		*/
	FILE *c35fp=NULL;	/* ... its file pointer */
	char *c55_file;		/* c55 filename		*/
	FILE *c55fp=NULL;	/* ... its file pointer */
	char *c44_file;		/* c44 filename		*/
	FILE *c44fp=NULL;	/* ... its file pointer */
	char *c66_file;		/* c66 filename		*/
	FILE *c66fp=NULL;	/* ... its file pointer */
	char *rho_file;		/* rho filename		*/
	FILE *rhofp=NULL;	/* ... its file pointer */
	char *q_file;		/* rho filename		*/
	FILE *qfp=NULL;		/* ... its file pointer */

	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);
	
	/* If the user specifies the name of a test datafile */
	/* output test dataset to this file */
	getparstring("tfile", &tfile);
	if (*tfile!='\0') {
		if((tfilefp=efopen(tfile,"w"))==NULL)
			err("cannot open tfile=%s\n",tfile);

		while(*sampleptr) fprintf(tfilefp,"%s\n", *sampleptr++);
		efclose(tfilefp);
		exit(EXIT_SUCCESS);
	}

	/* Get parameters */
	if (!getparint("ninf",&ninf))	ninf=5;
	if (!getparint("nx",&nx))	nx=100;
	if (!getparint("nz",&nz))	nz=100;
	if (!getparfloat("dx",&dx))	dx=10.0;
	if (!getparfloat("dz",&dz))	dz=10.0;
	if (!getparfloat("fx",&fx)) fx = 0.;
	if (!getparfloat("fz",&fz)) fz = 0.;
	if (!getparint("npmax",&npmax)) npmax=201;
		getparstring("method",&method);
	if (!getparint("paramtype",&paramtype))	paramtype=1;
	if (!getparint("verbose",&paramtype))	verbose=0;

	/* Open output files */
	if( !getparstring("c11_file",&c11_file)) c11_file = "c11_file";
	c11fp = efopen(c11_file,"w");
	if( !getparstring("c13_file",&c13_file)) c13_file = "c13_file";
	c13fp = efopen(c13_file,"w");
	if( !getparstring("c33_file",&c33_file)) c33_file = "c33_file";
	c33fp = efopen(c33_file,"w");
	if( !getparstring("c15_file",&c15_file)) c15_file = "c15_file";
	c15fp = efopen(c15_file,"w");
	if( !getparstring("c35_file",&c35_file)) c35_file = "c35_file";
	c35fp = efopen(c35_file,"w");
	if( !getparstring("c55_file",&c55_file)) c55_file = "c55_file";
	c55fp = efopen(c55_file,"w");
	if( !getparstring("c44_file",&c44_file)) c44_file = "c44_file";
	c44fp = efopen(c44_file,"w");
	if( !getparstring("c66_file",&c66_file)) c66_file = "c66_file";
	c66fp = efopen(c66_file,"w");
	if( !getparstring("rho_file",&rho_file)) rho_file = "rho_file";
	rhofp = efopen(rho_file,"w");
	if( !getparstring("q_file",&q_file)) q_file = "q_file";
	qfp = efopen(q_file,"w");
	
	/* Allocate space */
	vp = ealloc1float(nz);
	vs = ealloc1float(nz);
	rho = ealloc1float(nz);
	q = ealloc1float(nz);
	epsilon = ealloc1float(nz);
	delta = ealloc1float(nz);
	gamma = ealloc1float(nz);
	phi = ealloc1double(nz);
	x0 = ealloc1float(ninf+1);
	z0 = ealloc1float(ninf+1);
	vp00 = ealloc1float(ninf+1);
	vs00 = ealloc1float(ninf+1);
	rho00 = ealloc1float(ninf+1);
	q00 = ealloc1float(ninf+1);
	eps00 = ealloc1float(ninf+1);
	delta00 = ealloc1float(ninf+1);
	gamma00 = ealloc1float(ninf+1);
	phi00 = ealloc1double(ninf+1);

	dvpdx = ealloc1float(ninf+1);
	dvpdz = ealloc1float(ninf+1);
	dvsdx = ealloc1float(ninf+1);
	dvsdz = ealloc1float(ninf+1);
	drdx = ealloc1float(ninf+1);
	drdz = ealloc1float(ninf+1);

	dedx = ealloc1float(ninf+1);
	dedz = ealloc1float(ninf+1);
	dddx = ealloc1float(ninf+1);
	dddz = ealloc1float(ninf+1);
	dgdx = ealloc1float(ninf+1);
	dgdz = ealloc1float(ninf+1);
	dqdx = ealloc1float(ninf+1);
	dqdz = ealloc1float(ninf+1);


	xint =  ealloc1float(npmax);
	zint =  ealloc1float(npmax);
	xs =  ealloc1float(nx);
	zs =  ealloc2float(nx,ninf+1);

	c11 = ealloc2float(nz,nx);
	c13 = ealloc2float(nz,nx);
	c33 = ealloc2float(nz,nx);
	c44 = ealloc2float(nz,nx);
	c66 = ealloc2float(nz,nx);
	c15 = ealloc2float(nz,nx);
	c35 = ealloc2float(nz,nx);
	c55 = ealloc2float(nz,nx);
	rhooutput = ealloc2float(nz,nx);
	qoutput = ealloc2float(nz,nx);

	/* Zero output arrays */
	memset((void *) c11[0],0,nz*nx*FSIZE);
	memset((void *) c13[0],0,nz*nx*FSIZE);
	memset((void *) c33[0],0,nz*nx*FSIZE);
	memset((void *) c44[0],0,nz*nx*FSIZE);
	memset((void *) c66[0],0,nz*nx*FSIZE);
	memset((void *) c35[0],0,nz*nx*FSIZE);
	memset((void *) c55[0],0,nz*nx*FSIZE);
	memset((void *) rhooutput[0],0,nz*nx*FSIZE);
	memset((void *) qoutput[0],0,nz*nx*FSIZE);

	/* Compute uniformly sampled xs */
	for(ix=0; ix<nx; ++ix)
		xs[ix] = fx+ix*dx; 
	
	/* Input picked interfaces and make interpolation velocity values  */
	for(i=0; i<=ninf; ++i) {
		j= -1;
		do{
			j +=1;
			if(j>=npmax) 
		err("The point number on the %ith interface exceeds %i!",
				i,npmax);
			scanf("%f%f\n", &xint[j], &zint[j]);
		} while( zint[j]>-9999);
			npt = j;

		/* if linear interpolation or only one input sample */
		if (method[0]=='l' || nx==1) {
				intlin(npt,xint,zint,zint[0],zint[npt-1],
				nx,xs,zs[i]);
		/* else, if monotonic interpolation */
   		} else if (method[0]=='m') {
				zind = (float (*)[4])ealloc1float(npt*4);
				cmonot(npt,xint,zint,zind);
				intcub(0,npt,xint,zind,nx,xs,zs[i]);

			/* else, if Akima interpolation */
   		} else if (method[0]=='a') {
				zind = (float (*)[4])ealloc1float(npt*4);
				cakima(npt,xint,zint,zind);
				intcub(0,npt,xint,zind,nx,xs,zs[i]);

			/* else, if cubic spline interpolation */
			} else if (method[0]=='s') {
			   	zind = (float (*)[4])ealloc1float(npt*4);
			  	csplin(npt,xint,zint,zind);
			   	intcub(0,npt,xint,zind,nx,xs,zs[i]);

 		/* else, if unknown method specified */
 		} else {
  			err("%s is an unknown interpolation method!\n",method);
 		}

		for(ix=0; ix<nx; ++ix){
 			if(i>0 && zs[i][ix]<zs[i-1][ix])
			 err("the %ith interface is above the %ith one at position \n\t\t(%g,%g)! \n", i,i-1,xs[ix],zs[i][ix]);
		}

	}
	 	
	/* Input layer velocities, densities, anisotropy parameters and */
	/* their derivatives */
	if (!getparfloat("x0",x0))
 		for(i=0;i<=ninf;++i) x0[i] = 0.;
	if (!getparfloat("z0",z0))
 		for(i=0;i<=ninf;++i) z0[i] = 0.;
	if (!getparfloat("vp00",vp00))
 		for(i=0;i<=ninf;++i) vp00[i] = 1500.+ 500*i;
	if (!getparfloat("vs00",vs00))
 		for(i=0;i<=ninf;++i) vs00[i] = sqrt(3)*(1500.+ 500*i);
	if (!getparfloat("rho00",rho00))
 		for(i=0;i<=ninf;++i) rho00[i] = 1000.0 + 10*i;
	if (!getparfloat("q00",q00))
 		for(i=0;i<=ninf;++i) q00[i] = 100.0 + 10*i;

	/* added by Zhu Zhaolin, Tongji University */
	if (!getparfloat("eps00",eps00))
 		for(i=0;i<=ninf;++i) eps00[i] = 0.0;
	if (!getparfloat("delta00",delta00))
 		for(i=0;i<=ninf;++i) delta00[i] = 0.0;
	if (!getparfloat("gamma00",gamma00))
 		for(i=0;i<=ninf;++i) gamma00[i] = 0.0;
	
	if (!getparfloat("dvpdx",dvpdx)) 
		for(i=0;i<=ninf;++i) dvpdx[i] = 0.;
	if (!getparfloat("dvpdz",dvpdz)) 
		for(i=0;i<=ninf;++i) dvpdz[i] = 0.;
	
	if (!getparfloat("dvsdx",dvsdx)) 
		for(i=0;i<=ninf;++i) dvsdx[i] = 0.;
	if (!getparfloat("dvsdz",dvsdz)) 
		for(i=0;i<=ninf;++i) dvsdz[i] = 0.;
	
	/* added by Cristiano Calonaci, CINECA SuperComputing Center, Italy */
	if (!getparfloat("dqdx",dqdx))
		for(i=0;i<=ninf;++i) dqdx[i] = 0.;
	if (!getparfloat("dqdz",dqdz))
		for(i=0;i<=ninf;++i) dqdz[i] = 0.;

	if (!getparfloat("drdx",drdx)) 
		for(i=0;i<=ninf;++i) drdx[i] = 0.;
	if (!getparfloat("drdz",drdz)) 
		for(i=0;i<=ninf;++i) drdz[i] = 0.;
	
	if (!getparfloat("dedx",dedx)) 
		for(i=0;i<=ninf;++i) dedx[i] = 0.;
	if (!getparfloat("dedz",dedz)) 
		for(i=0;i<=ninf;++i) dedz[i] = 0.;
	
	/* added by Cristiano Calonaci, CINECA SuperComputing Center, Italy */
	if (!getparfloat("dddx",dddx))
		for(i=0;i<=ninf;++i) dddx[i] = 0.;
	if (!getparfloat("dddz",dddz))
		for(i=0;i<=ninf;++i) dddz[i] = 0.;

	if (!getparfloat("dgdx",dgdx)) 
		for(i=0;i<=ninf;++i) dgdx[i] = 0.;
	if (!getparfloat("dgdz",dgdz)) 
		for(i=0;i<=ninf;++i) dgdz[i] = 0.;
	
	if (!getpardouble("phi00",phi00)) 
		for(i=0;i<=ninf;++i) phi00[i] = 0.;

	/* compute linear parameters */
	for(ix=0; ix<nx; ++ix){ 
		j = 1;
		for(iz=0,z=fz; iz<nz; ++iz,z+=dz){
			if(z>=zs[ninf][ix]) {
				vp[iz] = vp00[ninf]
					  +(xs[ix]-x0[ninf])*dvpdx[ninf]
					  +(z-z0[ninf])*dvpdz[ninf];
				vs[iz] = vs00[ninf]
					  +(xs[ix]-x0[ninf])*dvsdx[ninf]
					  +(z-z0[ninf])*dvsdz[ninf];
				rho[iz] = rho00[ninf]
					  +(xs[ix]-x0[ninf])*drdx[ninf]
					  +(z-z0[ninf])*drdz[ninf];
				q[iz] = q00[ninf]
					  +(xs[ix]-x0[ninf])*dqdx[ninf]
					  +(z-z0[ninf])*dqdz[ninf];
				epsilon[iz] = eps00[ninf]
					  +(xs[ix]-x0[ninf])*dedx[ninf]
					  +(z-z0[ninf])*dedz[ninf];
				delta[iz] = delta00[ninf]
					  +(xs[ix]-x0[ninf])*dddx[ninf]
					  +(z-z0[ninf])*dddz[ninf];
				gamma[iz] = gamma00[ninf]
					  +(xs[ix]-x0[ninf])*dgdx[ninf]
					  +(z-z0[ninf])*dgdz[ninf];
				phi[iz]	= phi00[ninf];
			} else if(z<=zs[1][ix]) {
				vp[iz] = vp00[0]
					  +(xs[ix]-x0[0])*dvpdx[0]
					  +(z-z0[0])*dvpdz[0];
				vs[iz] = vs00[0]
					  +(xs[ix]-x0[0])*dvsdx[0]
					  +(z-z0[0])*dvsdz[0];
				rho[iz] = rho00[0]
					  +(xs[ix]-x0[0])*drdx[0]
					  +(z-z0[0])*drdz[0];
				q[iz] = q00[0]
					  +(xs[ix]-x0[0])*dqdx[0]
					  +(z-z0[0])*dqdz[0];
				epsilon[iz] = eps00[0]
					  +(xs[ix]-x0[0])*dedx[0]
					  +(z-z0[0])*dedz[0];
				delta[iz] = delta00[0]
					  +(xs[ix]-x0[0])*dddx[0]
					  +(z-z0[0])*dddz[0];
				gamma[iz] = gamma00[0]
					  +(xs[ix]-x0[0])*dgdx[0]
					  +(z-z0[0])*dgdz[0];
				phi[iz]	= phi00[0];
			} else {
					for(j=j; z>zs[j][ix]; ++j);
				j -= 1;
				vp[iz] = vp00[j]
					 +(xs[ix]-x0[j])*dvpdx[j]
					 +(z-z0[j])*dvpdz[j];
				vs[iz] = vs00[j]
					 +(xs[ix]-x0[j])*dvsdx[j]
					 +(z-z0[j])*dvsdz[j];
				rho[iz] = rho00[j]
					 +(xs[ix]-x0[j])*drdx[j]
					 +(z-z0[j])*drdz[j];
				q[iz] = q00[j]
					 +(xs[ix]-x0[j])*dqdx[j]
					 +(z-z0[j])*dqdz[j];
				epsilon[iz] = eps00[j]
					 +(xs[ix]-x0[j])*dedx[j]
					 +(z-z0[j])*dedz[j];
				delta[iz] = delta00[j]
					 +(xs[ix]-x0[j])*dddx[j]
					 +(z-z0[j])*dddz[j];
				gamma[iz] = gamma00[j]
					 +(xs[ix]-x0[j])*dgdx[j]
					 +(z-z0[j])*dgdz[j];
				phi[iz]	= phi00[j];
			}
		}
		/* loop over gridpoints and do calculations */
		if (paramtype==1) { /* Thomsen anisotropy parameters */
			float b=0.0,c=0.0;	/* temporary variables */ 

			for(iz=0; iz<nz; ++iz){
				/* ... calculate stiffnesses */
				c33[ix][iz] = vp[iz]*vp[iz]*rho[iz];
				c44[ix][iz] = vs[iz]*vs[iz]*rho[iz];
				c11[ix][iz] = c33[ix][iz]*(1+2*epsilon[iz]);
				c66[ix][iz] = c44[ix][iz]*(1+2*gamma[iz]);
				c = c33[ix][iz]*(2*c44[ix][iz]*(1+delta[iz])
					- c33[ix][iz]*(1+2*delta[iz]));
				b = 2*c44[ix][iz];
				c13[ix][iz] = (-b+sqrt(b*b-4*c))/2;
				c15[ix][iz] = 0.0;
				c35[ix][iz] = 0.0;
				c55[ix][iz] = c44[ix][iz];

				rhooutput[ix][iz] = rho[iz];
				qoutput[ix][iz] = q[iz];

			}
		} else {  /* Sayers anisotropy parameters */
			for(iz=0; iz<nz; ++iz){

				/* ... calculate stiffnesses */
				c33[ix][iz] = vp[iz]*vp[iz]*rho[iz];
				c44[ix][iz] = vs[iz]*vs[iz]*rho[iz];
				c11[ix][iz] = c33[ix][iz]*(1+2*epsilon[iz]);
				c66[ix][iz] = c44[ix][iz]*(1+2*gamma[iz]);
				c13[ix][iz] = (delta[iz]+1)*c33[ix][iz]
							-2*c44[ix][iz];
				c15[ix][iz] = 0.0;
				c35[ix][iz] = 0.0;
				c55[ix][iz] = c44[ix][iz];
				rhooutput[ix][iz] = rho[iz];
				qoutput[ix][iz] = q[iz];
			}
		}

		for(iz=0; iz<nz; ++iz) { /* compute rotation */

				spar.a1111 = (double) c11[ix][iz];
				spar.a1133 = (double) c13[ix][iz];
				spar.a3333 = (double) c33[ix][iz];
				spar.a2323 = (double) c44[ix][iz];
				spar.a1212 = (double) c66[ix][iz];
				spar.a1113 = (double) c15[ix][iz];
				spar.a3313 = (double) c35[ix][iz];
				spar.a1313 = (double) c55[ix][iz];
				/* added by Cristiano Calonaci, CINECA SuperComputing Center, Italy */
				spar.a1223 = (double) 0.;

				rottens2D(&spar,phi[iz]);

				/*rotating ISO media, a1113 ... is not equal */
				/* to zero becuase of precision */
				/* So following sentences was added by */
				/* Zhu Zhaolin, But only for myself */
				/* If don't care mini-values, you can */
				/* delete them*/
				if(fabs(spar.a1111)<1.0e-3) spar.a1111=0;
				if(fabs(spar.a1133)<1.0e-3) spar.a1133=0;
				if(fabs(spar.a3333)<1.0e-3) spar.a3333=0;
				if(fabs(spar.a1313)<1.0e-3) spar.a1313=0;
				if(fabs(spar.a1113)<1.0e-3) spar.a1113=0;
				if(fabs(spar.a3313)<1.0e-3) spar.a3313=0;
				if(fabs(spar.a1212)<1.0e-3) spar.a1212=0;
				if(fabs(spar.a2323)<1.0e-3) spar.a2323=0;

				c11[ix][iz] = (float) spar.a1111;
				c13[ix][iz] = (float) spar.a1133;
				c33[ix][iz] = (float) spar.a3333;
				c44[ix][iz] = (float) spar.a2323;
				c66[ix][iz] = (float) spar.a1212;
				c15[ix][iz] = (float) spar.a1113;
				c35[ix][iz] = (float) spar.a3313;
				c55[ix][iz] = (float) spar.a1313;
		}
	
		
	}

	
	/* Write the output files to disk */
	efwrite(*c11,sizeof(float),nz*nx,c11fp);
	efwrite(*c13,sizeof(float),nz*nx,c13fp);
	efwrite(*c33,sizeof(float),nz*nx,c33fp);
	efwrite(*c44,sizeof(float),nz*nx,c44fp);
	efwrite(*c66,sizeof(float),nz*nx,c66fp);
	efwrite(*c15,sizeof(float),nz*nx,c15fp);
	efwrite(*c35,sizeof(float),nz*nx,c35fp);
	efwrite(*c55,sizeof(float),nz*nx,c55fp);
	efwrite(*rhooutput,sizeof(float),nz*nx,rhofp);
	efwrite(*qoutput,sizeof(float),nz*nx,qfp);

	if(verbose){
		  warn("Output file for c11 parameter plane : %s ",c11_file);
	  	  warn("Output file for c13 parameter plane : %s ",c13_file);
		  warn("Output file for c33 parameter plane : %s ",c33_file);
		  warn("Output file for c44 parameter plane : %s ",c44_file);
		  warn("Output file for c66 parameter plane : %s ",c66_file);
		  warn("Output file for c15 parameter plane : %s ",c15_file);
		  warn("Output file for c35 parameter plane : %s ",c35_file);
		  warn("Output file for c55 parameter plane : %s ",c55_file);
		  warn("Output file for rho parameter plane : %s ",rho_file);
		  warn("Output file for q parameter plane : %s ",q_file);
	}


	/* Free workspace */
	free1float(vp);
	free1float(vs);
	free1float(rho);
	free1float(q);
	free1float(epsilon);
	free1float(delta);
	free1float(gamma);
	free1float(dvpdx);
	free1float(dvpdz);
	free1float(dvsdx);
	free1float(dvsdz);
	free1float(dedx);
	free1float(dedz);
	free1float(dgdx);
	free1float(dgdz);
	free1float(dddx);
	free1float(dddz);
	free1float(drdx);
	free1float(drdz);
	free2float(c11);
	free2float(c13);
	free2float(c33);
	free2float(c44);
	free2float(c66);
	free2float(c15);
	free2float(c35);
	free2float(c55);
	
	/* added by Cristiano Calonaci, CINECA SuperComputing Center, Italy */
	free1float(x0);
	free1float(z0);
	free1float(vp00);
	free1float(vs00);
	free1float(rho00);
	free1float(q00);
	free1float(eps00);
	free1float(delta00);
	free1float(gamma00);
	free1double(phi00);
	free1float(dqdx);
	free1float(dqdz);
	free1float(xs);
	free2float(zs);
	free1double(phi);
	free1float(xint);
	free1float(zint);
	free2float(rhooutput);
	free2float(qoutput);
	if(c11fp != NULL) efclose(c11fp);
	if(c13fp != NULL) efclose(c13fp);
	if(c33fp != NULL) efclose(c33fp);
	if(c15fp != NULL) efclose(c15fp);
	if(c35fp != NULL) efclose(c35fp);
	if(c55fp != NULL) efclose(c55fp);
	if(c44fp != NULL) efclose(c44fp);
	if(c66fp != NULL) efclose(c66fp);
	if(rhofp != NULL) efclose(rhofp);
	if(qfp != NULL) efclose(qfp);
					
	return(CWP_Exit());
		
}
