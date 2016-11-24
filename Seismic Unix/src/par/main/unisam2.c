/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* UNISAM2: $Revision: 1.12 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" UNISAM2 - UNIformly SAMple a 2-D function f(x1,x2)			",
" 									",
" unisam2 [optional parameters] <inputfile >outputfile			",
" 									",
" Required Parameters:							",
" none									",
" Optional Parameters:							",
" x1=             array of x1 values at which input f(x1,x2) is sampled	",
" ... Or specify a unform linear set of values for x1 via:		",
" nx1=1           number of input samples in 1st dimension		",
" dx1=1           input sampling interval in 1st dimension		",
" fx1=0           first input sample in 1st dimension			",
" ...									",
" n1=1            number of output samples in 1st dimension		",
" d1=             output sampling interval in 1st dimension		",
" f1=             first output sample in 1st dimension			",
" x2=             array of x2 values at which input f(x1,x2) is sampled	",
" ... Or specify a unform linear set of values for x2 via:		",
" nx2=1           number of input samples in 2nd dimension		",
" dx2=1           input sampling interval in 2nd dimension		",
" fx2=0           first input sample in 2nd dimension			",
" ...									",
" n2=1            number of output samples in 2nd dimension		",
" d2=             output sampling interval in 2nd dimension		",
" f2=             first output sample in 2nd dimension			",
" ... 									",
" method1=linear  =linear for linear interpolation			",
"                 =mono for monotonic bicubic interpolation		",
"                 =akima for Akima bicubic interpolation		",
"                 =spline for bicubic spline interpolation		",
" method2=linear  =linear for linear interpolation			",
"                 =mono for monotonic bicubic interpolation		",
"                 =akima for Akima bicubic interpolation		",
"                 =spline for bicubic spline interpolation		",
" 									",
" NOTES:								",
" The number of input samples is the number of x1 values times the	",
" number of x2 values.  The number of output samples is n1 times n2.	",
" The output sampling intervals (d1 and d2) and first samples (f1 and f2)",
" default to span the range of input x1 and x2 values.  In other words,	",
" d1=(x1max-x1min)/(n1-1) and f1=x1min; likewise for d2 and f2.		",
" 									",
" Interpolation is first performed along the 2nd dimension for each	",
" value of x1 specified.  Interpolation is then performed along the	",
" 1st dimension.							",
" 									",
NULL};

/*
 * AUTHOR:  Dave Hale, Colorado School of Mines, 01/12/91\n"
 */
/**************** end self doc ********************************/

int
main(int argc, char **argv)
{
	int nx1,nx2,n1,n2,ix1,ix2,i1,i2;
	float d1,d2,f1,f2,dx1,dx2,fx1,fx2,x1min,x1max,x2min,x2max,*x1,*x2,
		*u1,*u2,(*g2d)[4],(*h1d)[4],*g2,*h2,**g,**h;
	char *method1="linear";
	char *method2="linear";

	/* hook up getpar */
	initargs(argc,argv);
	requestdoc(0);

	/* get parameters */
	if ((nx1=countparval("x1"))!=0) {
		x1 = ealloc1float(nx1);
		getparfloat("x1",x1);
	} else {
		if (!getparint("nx1",&nx1)) nx1 = 1;
		if (!getparfloat("dx1",&dx1)) dx1 = 1.0;
		if (!getparfloat("fx1",&fx1)) fx1 = 0.0;
		x1 = ealloc1float(nx1);
		for (ix1=0; ix1<nx1; ++ix1)
			x1[ix1] = fx1+ix1*dx1;
	}
	for (ix1=1,x1min=x1max=x1[0]; ix1<nx1; ++ix1) {
		if (x1[ix1]<x1min) x1min = x1[ix1];
		if (x1[ix1]>x1max) x1max = x1[ix1];
	}
	if (!getparint("n1",&n1)) n1 = 1;
	if (!getparfloat("d1",&d1)) d1 = (n1==1?0.0:(x1max-x1min)/(n1-1));
	if (!getparfloat("f1",&f1)) f1 = x1min;
	if ((nx2=countparval("x2"))!=0) {
		x2 = ealloc1float(nx2);
		getparfloat("x2",x2);
	} else {
		if (!getparint("nx2",&nx2)) nx2 = 1;
		if (!getparfloat("dx2",&dx2)) dx2 = 1.0;
		if (!getparfloat("fx2",&fx2)) fx2 = 0.0;
		x2 = ealloc1float(nx2);
		for (ix2=0; ix2<nx2; ++ix2)
			x2[ix2] = fx2+ix2*dx2;
	}
	for (ix2=1,x2min=x2max=x2[0]; ix2<nx2; ++ix2) {
		if (x2[ix2]<x2min) x2min = x2[ix2];
		if (x2[ix2]>x2max) x2max = x2[ix2];
	}
	if (!getparint("n2",&n2)) n2 = 1;
	if (!getparfloat("d2",&d2)) d2 = (n2==1?0.0:(x2max-x2min)/(n2-1));
	if (!getparfloat("f2",&f2)) f2 = x2min;
	getparstring("method1",&method1);
	getparstring("method2",&method2);
        checkpars();

	if (method1[0]!='l' && method1[0]!='m' &&
		method1[0]!='a' && method1[0]!='s')
		err("%s is an unknown interpolation method!\n",method1);
	if (method2[0]!='l' && method2[0]!='m' &&
		method2[0]!='a' && method2[0]!='s')
		err("%s is an unknown interpolation method!\n",method2);
		
	/* compute uniformly sampled values at which to evaluate function */
	u1 = ealloc1float(n1);
	for (i1=0; i1<n1; ++i1)
		u1[i1] = f1+i1*d1;
	u2 = ealloc1float(n2);
	for (i2=0; i2<n2; ++i2)
		u2[i2] = f2+i2*d2;
	
	/* allocate space for input function values */
	g = ealloc2float(nx1,nx2);
	
	/* allocate space for intermediate interpolated output */
	h = ealloc2float(nx1,n2);
	
	/* read input function */
	if (fread(g[0],sizeof(float),nx1*nx2,stdin)!=nx1*nx2)
		err("error reading input file!");
		
	/* allocate workspace for interpolation */
	g2 = ealloc1float(nx2);
	h2 = ealloc1float(n2);
	
	/* loop over 1st dimension, interpolating along 2nd dimension */
	for (ix1=0; ix1<nx1; ++ix1) {
	
		/* get input function values along 2nd dimension */
		for (ix2=0; ix2<nx2; ++ix2)
			g2[ix2] = g[ix2][ix1];
		
		/* if linear interpolation or only one input sample */
		if (method1[0]=='l' || nx2==1) {
			intlin(nx2,x2,g2,g2[0],g2[nx2-1],n2,u2,h2);

		/* else, if monotonic interpolation */
		} else if (method1[0]=='m') {
			g2d = (float(*)[4])ealloc1float(nx2*4);
			cmonot(nx2,x2,g2,g2d);
			intcub(0,nx2,x2,g2d,n2,u2,h2);

		/* else, if Akima interpolation */
		} else if (method1[0]=='a') {
			g2d = (float(*)[4])ealloc1float(nx2*4);
			cakima(nx2,x2,g2,g2d);
			intcub(0,nx2,x2,g2d,n2,u2,h2);

		/* else, if cubic spline interpolation */
		} else if (method1[0]=='s') {
			g2d = (float(*)[4])ealloc1float(nx2*4);
			csplin(nx2,x2,g2,g2d);
			intcub(0,nx2,x2,g2d,n2,u2,h2);

		}
		
		/* save function values interpolated along 2nd dimension */
		for (i2=0; i2<n2; ++i2)
			h[i2][ix1] = h2[i2];
	}
	
	/* free workspace */
	free1float(g2);
	free1float(h2);
	
	/* free space for input and allocate space for output */
	free2float(g);
	g = ealloc2float(n1,n2);
 	
	/* loop over 2nd dimension, interpolating along 1st dimension */
	for (i2=0; i2<n2; ++i2) {
		
		/* if linear interpolation or only one input sample */
		if (method2[0]=='l' || nx1==1) {
			intlin(nx1,x1,h[i2],h[i2][0],h[i2][nx1-1],n1,u1,g[i2]);

		/* else, if monotonic interpolation */
		} else if (method2[0]=='m') {
			h1d = (float(*)[4])ealloc1float(nx1*4);
			cmonot(nx1,x1,h[i2],h1d);
			intcub(0,nx1,x1,h1d,n1,u1,g[i2]);

		/* else, if Akima interpolation */
		} else if (method2[0]=='a') {
			h1d = (float(*)[4])ealloc1float(nx1*4);
			cakima(nx1,x1,h[i2],h1d);
			intcub(0,nx1,x1,h1d,n1,u1,g[i2]);

		/* else, if cubic spline interpolation */
		} else if (method2[0]=='s') {
			h1d = (float(*)[4])ealloc1float(nx1*4);
			csplin(nx1,x1,h[i2],h1d);
			intcub(0,nx1,x1,h1d,n1,u1,g[i2]);

		}
	}
		
	/* write output function */
	if (fwrite(g[0],sizeof(float),n1*n2,stdout)!=n1*n2)
		err("error writing output file!");

	return(CWP_Exit());
}
