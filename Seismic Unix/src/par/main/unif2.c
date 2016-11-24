/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* UNIF2: $Revision: 1.12 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									 ",
" UNIF2 - generate a 2-D UNIFormly sampled velocity profile from a layered",
"  	 model. In each layer, velocity is a linear function of position.",
" 									",
"  unif2 < infile > outfile [parameters]				",
" 									",
" Required parameters:							",
" none									",
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
" x0=0.0,0.0,..., 	distance x at which v00 is specified		",
" z0=0.0,0.0,..., 	depth z at which v00 is specified		",
" v00=1500,2000,2500...,	velocity at each x0,z0 (m/sec)		",
" dvdx=0.0,0.0,...,	derivative of velocity with distance x (dv/dx)	",
" dvdz=0.0,0.0,...,	derivative of velocity with depth z (dv/dz)	",
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
" piecewise continuous velocity model with a flat surface on top. The surface",
" and each successive boundary between media are represented by a list of",
" selected x z pairs written column form. The first and last x values must",
" be the same for all boundaries. Use the entry   1.0  -99999  to separate",
" entries for successive boundaries. No boundary may cross another. Note",
" that the choice of the method of interpolation may cause boundaries 	",
" to cross that do not appear to cross in the input data file.		",
" The number of interfaces is specified by the parameter \"ninf\". This ",
" number does not include the top surface of the model. The input data	",
" format is the same as a CSHOT model file with all comments removed.	",
"									",
" Example using test input file generating feature:			",
" unif2 tfile=testfilename    produces a 5 interface demonstration model",
" unif2 < testfilename | psimage n1=100 n2=100 d1=10 d2=10 | ...	",
"									",
NULL};

/*
 * Credits:
 * 	CWP: Zhenyue Liu, 1994 
 *      CWP: John Stockwell, 1994, added demonstration model stuff. 
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

int
main(int argc, char **argv)
{
	int nx;		/* size of 2nd (slow) dimension of data */
	int nz;		/* size of 1st (fast) dimension of data */
	int i,j,ix,iz;	/* counters				*/
	int ninf;	/* number of interfaces			*/
	int npmax;	/* max number of points on an interface	*/
	int npt;	/* number of points on an interface	*/

	float z;	/* fast dimension coordinate		*/
	float fx;	/* first value in x			*/
	float fz;	/* first value in z			*/
	float dx;	/* x sampling interval			*/
	float dz;	/* z sampling interval				*/

	float *v=NULL;		/* velocity function calculated		*/

	float *x0=NULL;		/* initial x position values		*/
	float *z0=NULL;		/* initial z position values		*/

	float *v00=NULL;	/* velocities at x0,z0 positions	*/
	float *dvdx=NULL;	/* x rates of change of v in each layer */ 
	float *dvdz=NULL;	/* z rates of change of v in each layer */

	float **zs=NULL;	/* array of z values for interpolator	*/
	float *xs=NULL;		/* array of x values for interpolator	*/
	float *xint=NULL;	/* x values defining an interface	*/
	float *zint=NULL;	/* z values defining an interface	*/

	float (*zind)[4]=NULL;	/* array of interpolation coefficients	*/ 

   	char *method="linear";	/* interpolation method		*/
	FILE *out_file=stdout;	/* output file			*/
	char *tfile="";		/* test filename		*/
	FILE *tfilefp;		/* ...its file pointer		*/	

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

	
	/* Allocate space */
	v = alloc1float(nz);
	x0 = alloc1float(ninf+1);
	z0 = alloc1float(ninf+1);
	v00 = alloc1float(ninf+1);
	dvdx = alloc1float(ninf+1);
	dvdz = alloc1float(ninf+1);
	xint =  alloc1float(npmax);
	zint =  alloc1float(npmax);
	xs =  alloc1float(nx);
	zs =  alloc2float(nx,ninf+1);

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
	 	
	/* Input  layer velocities and velocity derivatives */
	if (!getparfloat("x0",x0))
 		for(i=0;i<=ninf;++i) x0[i] = 0.;
	if (!getparfloat("z0",z0))
 		for(i=0;i<=ninf;++i) z0[i] = 0.;
	if (!getparfloat("v00",v00))
 		for(i=0;i<=ninf;++i) v00[i] = 1500.+ 500*i;
	if (!getparfloat("dvdx",dvdx)) 
		for(i=0;i<=ninf;++i) dvdx[i] = 0.;
	if (!getparfloat("dvdz",dvdz)) 
		for(i=0;i<=ninf;++i) dvdz[i] = 0.;

        checkpars();

	/* compute linear velocity */
	for(ix=0; ix<nx; ++ix){ 
		j = 1;
		for(iz=0,z=fz; iz<nz; ++iz,z+=dz){
			if(z>=zs[ninf][ix]) {
				v[iz] = v00[ninf]+(xs[ix]-x0[ninf])*dvdx[ninf]
					+(z-z0[ninf])*dvdz[ninf];
			} else if(z<=zs[1][ix]) {
				v[iz]=v00[0]+(xs[ix]-x0[0])*dvdx[0]
					+(z-z0[0])*dvdz[0];
			} else {
					for(j=j; z>zs[j][ix]; ++j);
				j -= 1;
				v[iz] =	v00[j]+(xs[ix]-x0[j])*dvdx[j]
					+(z-z0[j])*dvdz[j];
			}
		}
		efwrite(v,sizeof(float),nz,out_file);
	}
					
	return(CWP_Exit());
		
}
