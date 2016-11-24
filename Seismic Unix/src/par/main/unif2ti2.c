/* Copyright (c) Colorado School of Mines, 2010.*/
/* All rights reserved.                       */

#include "par.h"
#include "anisotropy.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" UNIF2TI2 - generate a 2-D UNIFormly sampled profile of stiffness 	",
" 	coefficients of a layered medium (only for P waves). 		",
" 									",
"  unif2ti2 < infile [Parameters]					",
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
" ns=0          number of samples in vertical direction for smoothing   ",
"		parameters across boundary				",
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
" eps00=0,0,0...,	Thomsen parameter epsilon at each x0,z0		",
" delta00=0,0,0...,	Thomsen	parameter delta at each x0,z0		",
" rho00=1000,1100,...,	density at each x0,z0 (kg/m^3)			",
" 									",
" dvpdx=0.0,0.0,...,	x-derivative of P-velocity (dvp/dx)		",
" dvpdz=0.0,0.0,...,	z-derivative of P-velocity (dvs/dz)		",
" 									",
" dedx=0.0,0.0,...,	x-derivative of epsilon (de/dx)			",
" dedz=0.0,0.0,...,	z-derivative of epsilon with depth z (de/dz)	",
" 									",
" dddx=0.0,0.0,...,	x-derivative of delta (dd/dx)			",
" dddz=0.0,0.0,...,	z-derivative of delta (dd/dz)			",
" 									",
" drdx=0.0,0.0,...,	x-derivative of density (d rho/dx)		",
" drdz=0.0,0.0,...,	z-derivative of density (d rho/dz)		",
" 									",
" nufile= 		binary file containning tilt value at each grid point",
" 									",
" 									",
" ...output filenames 							",
" c11_file=c11_file	output filename for c11 values	 		",
" c13_file=c13_file	output filename for c13 values	 		",
" c15_file=c15_file	output filename for c15 values	 		",
" c33_file=c33_file	output filename for c33 values	 		",
" c35_file=c35_file	output filename for c35 values	 		",
" c55_file=c55_file	output filename for c55 values	 		",
" 									",
" 									",
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
" The algorithm works by transforming the P-wavespeed, Thomsen parameters ",
" epsilon and delta, and the tilt of the symmetry axis into density-normalized", 
" stiffness coefficients. 						",
" 									",
" At this stage, the tilt-field file can be prepared using the 		",
" Matlab M-file nu_mod.m based on 2D interpolation between interfaces.  ", 
" The binary file contains nu values at each grid point.		",
" The interfaces are obtained by interpolation on the picked ones 	",
" stored in the infile, and the symmetry axis at each point of interface",
" is assumed to be parallel to the normal direction.			",
"  									",
" Common ranges of Thomsen parameters are				",
"  epsilon:  0.0 -> 0.5							",
"  delta:   -0.2 -> 0.4							",
" 									",
" 									",
" If the tilt-field file is not given, the model will be assumed to 	",
" have VTI symmetry. 				 			",
"									",
" Example using test input file generating feature:			",
" unif2aniso tfile=testfilename  produces a 5 interface demonstration model",
" unif2aniso < testfilename 						",
" ximage < c11_file n1=100 n2=100					",
" ximage < c13_file n1=100 n2=100					",
" ximage < c15_file n1=100 n2=100					",
" ximage < c33_file n1=100 n2=100					",
" ximage < c35_file n1=100 n2=100					",
" ximage < c55_file n1=100 n2=100					",
" ximage < rho_file n1=100 n2=100					",
"									",
NULL};

/*
 * Credits:
 *      Modified by Pengfei Cai (CWP), Dec 2011
 *      Modified by Xiaoxiang Wang (CWP), Aug 2010
 * 	Based on program unif2aniso by John Stockwell, 2005 
 */

/**************** end self doc *******************************************/

/* sample input data set */
char *sample[] = {
"   0         0",
"1000         0",
"   1    -99999",
"   0       200",
"1000       200",
"   1    -99999",
"   0       400",
" 100.      400",
" 400       560",
" 550.      560",
"1000.      360",
"   1    -99999",
"   0       800",
" 100.      800",
" 400.      750.",
" 500.      780",
" 550.      780",
"1000.      500",
"   1.   -99999",
"   0       950",
" 100       950",
" 400       900.",
" 500       800.",
" 550       800.",
" 750       920",
"1000       930",
"   1    -99999",
"   0      1000",
"1000      1000",
"   1    -99999",
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
	int ns;		/* number of samples in vertical direction for smoothing parameters across boundary */
     	
     	int nl;
     	int nh;	
     	
     	int vti_flag;    /* VTI flag				*/

	float z;	/* fast dimension coordinate		*/
	float fx;	/* first value in x			*/
	float fz;	/* first value in z			*/
	float dx;	/* x sampling interval			*/
	float dz;	/* z sampling interval				*/


	float **vp=NULL;	/* P-velocity function calculated	*/
        float **vs=NULL;
	float **epsilon=NULL;	/* epsilon function  calculated		*/
	float **delta=NULL;	/* delta function calculated		*/
	float **nu=NULL;	/* tilt angle in rad			*/
	float **rho=NULL;	/* density function calculated		*/

	float *x0=NULL;		/* initial x position values		*/
	float *z0=NULL;		/* initial z position values		*/
	float *vp00=NULL;	/* P-wavespeeds at x0,z0 positions	*/
        float *vs00=NULL;       /* S-wavespeeds at x0,z0 positions      */
	float *eps00=NULL;	/* epsilon at x0,z0 positions		*/
	float *delta00=NULL;	/* delta at x0,z0 positions		*/
	float *rho00=NULL;	/* densities at x0,z0 positions		*/
	
	float *dvpdx=NULL;	/* x rates of change of V-P by layer	*/ 
	float *dvpdz=NULL;	/* z rates of change of V-P by layer	*/
        float *dvsdx=NULL;      /* x rates of change of V-S by layer    */
        float *dvsdz=NULL;      /* z rates of change of V-S by layer    */
	float *dedx=NULL;	/* x rates of change of epsilon by layer*/ 
	float *dedz=NULL;	/* z rates of change of epsilon by layer*/
	float *dddx=NULL;	/* x rates of change of delta by layer	*/ 
	float *dddz=NULL;	/* z rates of change of delta by layer	*/

	float *drdx=NULL;	/* x rates of change of density by layer*/ 
	float *drdz=NULL;	/* z rates of change of density by layer*/

	float **zs=NULL;	/* array of z values for interpolator	*/
	float *xs=NULL;		/* array of x values for interpolator	*/
	float *xint=NULL;	/* x values defining an interface	*/
	float *zint=NULL;	/* z values defining an interface	*/

	float (*zind)[4]=NULL;	/* array of interpolation coefficients	*/ 

   	char *method="spline";	/* interpolation method		*/
	char *tfile="";		/* test filename		*/
	FILE *tfilefp;		/* ...its file pointer		*/
	
	char *nufile="";	/* tilt-field filename		*/	
        FILE *nufp;		/* ... its file pointer */

	/* .. stiffness values */
	float **c11=NULL;	/* stiffness c11		*/
	float **c13=NULL;	/* stiffness c13		*/
	float **c33=NULL;	/* stiffness c33		*/
	float **c15=NULL;	/* stiffness c15		*/
	float **c35=NULL;	/* stiffness c15		*/
	float **c55=NULL;	/* stiffness c15		*/

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
	char *rho_file;		/* rho filename		*/
	FILE *rhofp=NULL;	/* ... its file pointer */
	
	char *vp_file;		/* vp filename		*/
	FILE *vpfp=NULL;	/* ... its file pointer */
        char *vs_file;          /* vs filename          */
        FILE *vsfp=NULL;        /* ... its file pointer */
	char *epsilon_file;	/* epsilon filename	*/
	FILE *epsilonfp=NULL;	/* ... its file pointer */
	char *delta_file;	/* delta filename	*/
	FILE *deltafp=NULL;	/* ... its file pointer */
	


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
	
	if (!getparint("ns",&ns))	ns=0;
	
	if (!getparstring("nufile",&nufile)) { 
		vti_flag=1;
		printf("Since user does not give tilt-field file, the medium is assumed to be VTI\n"); }
	else {
		vti_flag=0; }
		
		
	
	/*if (!getparint("vti_flag",&vti_flag)) */
	/*printf("Since user does not define VTI_flag, the medium is assumed to be VTI\n");*/
	/*vti_flag=1;*/
    	
    	getparstring("method",&method);
	
	/*if (vti_flag==0)*/
	/*getparstring("nufile",&nufile);*/
	

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
	
	if( !getparstring("vp_file",&vp_file)) vp_file = "vp_file";
	vpfp = efopen(vp_file,"w");
        if( !getparstring("vs_file",&vs_file)) vs_file = "vs_file";
        vsfp = efopen(vs_file,"w");
	if( !getparstring("epsilon_file",&epsilon_file)) epsilon_file = "epsilon_file";
	epsilonfp = efopen(epsilon_file,"w");
	if( !getparstring("delta_file",&delta_file)) delta_file = "delta_file";
	deltafp = efopen(delta_file,"w");
	
	if( !getparstring("rho_file",&rho_file)) rho_file = "rho_file";
	rhofp = efopen(rho_file,"w");
	
	/* Allocate space */
	vp = ealloc2float(nz,nx);
        vs = ealloc2float(nz,nx);
	epsilon = ealloc2float(nz,nx);
	delta = ealloc2float(nz,nx);
	nu = ealloc2float(nz,nx);
	rho = ealloc2float(nz,nx);
	x0 = ealloc1float(ninf+1);
	z0 = ealloc1float(ninf+1);
	vp00 = ealloc1float(ninf+1);
        vs00 = ealloc1float(ninf+1);
	eps00 = ealloc1float(ninf+1);
	delta00 = ealloc1float(ninf+1);
	rho00 = ealloc1float(ninf+1);

	dvpdx = ealloc1float(ninf+1);
	dvpdz = ealloc1float(ninf+1);
        dvsdx = ealloc1float(ninf+1);
        dvsdz = ealloc1float(ninf+1);
	dedx = ealloc1float(ninf+1);
	dedz = ealloc1float(ninf+1);
	dddx = ealloc1float(ninf+1);
	dddz = ealloc1float(ninf+1);
	drdx = ealloc1float(ninf+1);
	drdz = ealloc1float(ninf+1);


	xint =  ealloc1float(npmax);
	zint =  ealloc1float(npmax);
	xs =  ealloc1float(nx);
	zs =  ealloc2float(nx,ninf+1);

	c11 = ealloc2float(nz,nx);
	c13 = ealloc2float(nz,nx);
	c15 = ealloc2float(nz,nx);
	c33 = ealloc2float(nz,nx);
	c35 = ealloc2float(nz,nx);
	c55 = ealloc2float(nz,nx);


	/* Zero output arrays */
	memset((void *) c11[0],0,nz*nx*FSIZE);
	memset((void *) c13[0],0,nz*nx*FSIZE);
	memset((void *) c15[0],0,nz*nx*FSIZE);	
	memset((void *) c33[0],0,nz*nx*FSIZE);
	memset((void *) c35[0],0,nz*nx*FSIZE);
	memset((void *) c55[0],0,nz*nx*FSIZE);
	
	memset((void *) vp[0],0,nz*nx*FSIZE);
        memset((void *) vs[0],0,nz*nx*FSIZE);
	memset((void *) epsilon[0],0,nz*nx*FSIZE);
	memset((void *) delta[0],0,nz*nx*FSIZE);
	memset((void *) rho[0],0,nz*nx*FSIZE);
	
	
	/* Compute uniformly sampled xs */
	for(ix=0; ix<nx; ++ix)
		xs[ix] = fx+ix*dx; 
	
	/* Input picked interfaces and make interpolation velocity values  */
	for(i=0; i<=ninf; ++i) 
	{
		j= -1;
		do
		{
			j +=1;
			if(j>=npmax) 
		err("The point number on the %ith interface exceeds %i!",
				i,npmax);
			scanf("%f%f\n", &xint[j], &zint[j]);
		} while( zint[j]>-9999);
			npt = j;

		/* if linear interpolation or only one input sample */
		if (method[0]=='l' || nx==1) 
		{
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
 		} else 
 		{
  			err("%s is an unknown interpolation method!\n",method);
 		}

		for(ix=0; ix<nx; ++ix)
		{
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
                for(i=0;i<=ninf;++i) vs00[i] = 1500.+ 500*i;
	if (!getparfloat("eps00",eps00))
 		for(i=0;i<=ninf;++i) eps00[i] = 0.0;
	if (!getparfloat("delta00",delta00))
 		for(i=0;i<=ninf;++i) delta00[i] = 0.0;
 		
 	if (!getparfloat("rho00",rho00))
 		for(i=0;i<=ninf;++i) rho00[i] = 1000.0 + 10*i;
 		
 	if (!getparfloat("drdx",drdx)) 
		for(i=0;i<=ninf;++i) drdx[i] = 0.;
	if (!getparfloat("drdz",drdz)) 
		for(i=0;i<=ninf;++i) drdz[i] = 0.;

	if (!getparfloat("dvpdx",dvpdx)) 
		for(i=0;i<=ninf;++i) dvpdx[i] = 0.;
	if (!getparfloat("dvpdz",dvpdz)) 
		for(i=0;i<=ninf;++i) dvpdz[i] = 0.;
        if (!getparfloat("dvsdx",dvsdx))
                for(i=0;i<=ninf;++i) dvsdx[i] = 0.;
        if (!getparfloat("dvsdz",dvsdz))
                for(i=0;i<=ninf;++i) dvsdz[i] = 0.;
	
	if (!getparfloat("dedx",dedx)) 
		for(i=0;i<=ninf;++i) dedx[i] = 0.;
	if (!getparfloat("dedz",dedz)) 
		for(i=0;i<=ninf;++i) dedz[i] = 0.;

	if (!getparfloat("dddx",dddx)) 
		for(i=0;i<=ninf;++i) dddx[i] = 0.;
	if (!getparfloat("dddz",dddz)) 
		for(i=0;i<=ninf;++i) dddz[i] = 0.;
		

	if (vti_flag==0)		
	{
		if ((nufp=fopen(nufile,"r"))==NULL)
			err("error opening nufile=%s!\n",nufile);
		if (fread(nu[0],sizeof(float),nz*nx,nufp)!=nz*nx)
			err("error reading nufile=%s!\n",nufile);
	}


	/* compute linear parameters */
	for(ix=0; ix<nx; ++ix)
	{	
		for(iz=0,z=fz; iz<nz; ++iz,z+=dz)
		{
			if(z>=zs[ninf][ix]) 
			{
				vp[ix][iz] = vp00[ninf]
					  +(xs[ix]-x0[ninf])*dvpdx[ninf]
					  +(z-z0[ninf])*dvpdz[ninf];
				vs[ix][iz] = vs00[ninf]
                                          +(xs[ix]-x0[ninf])*dvsdx[ninf]
                                          +(z-z0[ninf])*dvsdz[ninf];
				epsilon[ix][iz] = eps00[ninf]
					  +(xs[ix]-x0[ninf])*dedx[ninf]
					  +(z-z0[ninf])*dedz[ninf];
				delta[ix][iz] = delta00[ninf]
					  +(xs[ix]-x0[ninf])*dddx[ninf]
					  +(z-z0[ninf])*dddz[ninf];
				rho[ix][iz] = rho00[ninf]
					  +(xs[ix]-x0[ninf])*drdx[ninf]
					  +(z-z0[ninf])*drdz[ninf];
			 }
			 else 
			 {
				for(j=0; j<ninf; j++)
					if(z>=zs[j][ix] && z<zs[j+1][ix])
					{
					vp[ix][iz] = vp00[j]
						+(xs[ix]-x0[j])*dvpdx[j]
					 	+(z-z0[j])*dvpdz[j];
					vs[ix][iz] = vs00[j]
                                                +(xs[ix]-x0[j])*dvsdx[j]
                                                +(z-z0[j])*dvsdz[j];
					epsilon[ix][iz] = eps00[j]
					 	+(xs[ix]-x0[j])*dedx[j]
					 	+(z-z0[j])*dedz[j];
					delta[ix][iz] = delta00[j]
					 	+(xs[ix]-x0[j])*dddx[j]
					 	+(z-z0[j])*dddz[j];
					rho[ix][iz] = rho00[j]
					 	+(xs[ix]-x0[j])*drdx[j]
					  	+(z-z0[j])*drdz[j];
					}
			}
			
		}
	}
	
	
	if(ns!=0){		
	if(ns%2==0){		
	for(ix=0; ix<nx; ++ix)
	{	
		for(j=1; j<=ninf; j++)
		{
			nl = (zs[j][ix]-fz)/dz-ns/2+1;
			nh = (zs[j][ix]-fz)/dz+ns/2+1;		
			for(iz=nl+1; iz<nh; ++iz)
			{
				vp[ix][iz] = vp[ix][nl] + (iz-nl)*(vp[ix][nh]-vp[ix][nl])/ns;
				vs[ix][iz] = vs[ix][nl] + (iz-nl)*(vs[ix][nh]-vs[ix][nl])/ns;
				epsilon[ix][iz] = epsilon[ix][nl] + (iz-nl)*(epsilon[ix][nh]-epsilon[ix][nl])/ns;
				delta[ix][iz] = delta[ix][nl] + (iz-nl)*(delta[ix][nh]-delta[ix][nl])/ns;
				rho[ix][iz] = rho[ix][nl] + (iz-nl)*(rho[ix][nh]-rho[ix][nl])/ns;
			}
		}
	}
	} else {
	for(ix=0; ix<nx; ++ix)
	{	
		for(j=1; j<=ninf; j++)
		{  
			nl = (zs[j][ix]-fz)/dz-ns/2;
			nh = (zs[j][ix]-fz)/dz+ns/2+1;		
			for(iz=nl+1; iz<nh; ++iz)
			{
				vp[ix][iz] = vp[ix][nl] + (iz-nl)*(vp[ix][nh]-vp[ix][nl])/ns;
				vs[ix][iz] = vs[ix][nl] + (iz-nl)*(vs[ix][nh]-vs[ix][nl])/ns;
				epsilon[ix][iz] = epsilon[ix][nl] + (iz-nl)*(epsilon[ix][nh]-epsilon[ix][nl])/ns;
				delta[ix][iz] = delta[ix][nl] + (iz-nl)*(delta[ix][nh]-delta[ix][nl])/ns;
				rho[ix][iz] = rho[ix][nl] + (iz-nl)*(rho[ix][nh]-rho[ix][nl])/ns;
			}
		}
	
	}
	}
	}
	
	if (vti_flag==1)
	{	
	for(ix=0; ix<nx; ++ix)
	{	
		for(iz=0; iz<nz; ++iz)
		{
			/* ... calculate stiffnesses */
			
			float b=0.0,c=0.0;
			c33[ix][iz] = vp[ix][iz]*vp[ix][iz]*rho[ix][iz];;
			c55[ix][iz] = 0.25*c33[ix][iz];
			c11[ix][iz] = c33[ix][iz]*(1+2*epsilon[ix][iz]);
				
			c = c33[ix][iz]*(2*c55[ix][iz]*(1+delta[ix][iz])
				- c33[ix][iz]*(1+2*delta[ix][iz]));
			b = 2*c55[ix][iz];
				
			c13[ix][iz] = (-b+sqrt(b*b-4*c))/2;
			c15[ix][iz] = 0.0;
			c35[ix][iz] = 0.0;	
		}
	} 
	}
	else 
	{
	for(ix=0; ix<nx; ++ix)
	{	
		for(iz=0; iz<nz; ++iz)
		{	
				
			thom2stiffTI(vp[ix][iz],vs[ix][iz],epsilon[ix][iz],delta[ix][iz],epsilon[ix][iz],nu[ix][iz],&spar,1);
			
			c11[ix][iz] = ((float) spar.a1111)*rho[ix][iz];
        		c13[ix][iz] = ((float) spar.a1133)*rho[ix][iz];
			c15[ix][iz] = ((float) spar.a1113)*rho[ix][iz];
			c33[ix][iz] = ((float) spar.a3333)*rho[ix][iz];
			c35[ix][iz] = ((float) spar.a3313)*rho[ix][iz];
			c55[ix][iz] = ((float) spar.a1313)*rho[ix][iz]; 
		}
			
	}
	}
	
	
	/* Write the output files to disk */
	efwrite(*c11,sizeof(float),nz*nx,c11fp);
	efwrite(*c13,sizeof(float),nz*nx,c13fp);
	efwrite(*c15,sizeof(float),nz*nx,c15fp);
	efwrite(*c33,sizeof(float),nz*nx,c33fp);
	efwrite(*c35,sizeof(float),nz*nx,c35fp);
	efwrite(*c55,sizeof(float),nz*nx,c55fp);
	
	efwrite(*vp,sizeof(float),nz*nx,vpfp);
	efwrite(*vs,sizeof(float),nz*nx,vsfp);
	efwrite(*epsilon,sizeof(float),nz*nx,epsilonfp);
	efwrite(*delta,sizeof(float),nz*nx,deltafp);
	efwrite(*rho,sizeof(float),nz*nx,rhofp);


	/* Free workspace */
	free2float(vp);
        free2float(vs);
	free2float(epsilon);
	free2float(delta);
	free2float(rho);
	free1float(dvpdx);
	free1float(dvpdz);
	free1float(dedx);
	free1float(dedz);
	free1float(dddx);
	free1float(dddz);
	free1float(drdx);
	free1float(drdz);
	
	free2float(c11);
	free2float(c13);
	free2float(c33);
	free2float(c15);
	free2float(c35);
	free2float(c55);
	
	free2float(nu);

					
	return(CWP_Exit());
		
}
		
	
				
			
		
		
		
		
		
		
