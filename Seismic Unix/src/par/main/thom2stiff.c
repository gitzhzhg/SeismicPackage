/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* THOM2STIFF: $Revision: 1.4 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include <par.h>
#include "anisotropy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" THOM2STIFF - convert Thomsen's parameters into (density normalized)	",
"                 stiffness components for transversely isotropic 	",
"	 	   material with in-plane-tilted axis of symmetry	",
"									",
" thom2stiff [optional parameter] 		                        ",
"									",
" vp=2         symm.axis p-wave velocity                        	",
" vs=1         symm.axis s-wave velocity                        	",
" eps=0        Thomsen's (generic) epsilon 	         		",
" delta=0      Thomsen's (generic) delta         			",
" gamma=0      Thomsen's (generic) gamma                                ",
" rho=1        density                					",
" phi=0        angle(DEG) vertical --> symmetry axes (clockwise)        ",
" sign=1       sign of c11+c44 (generally sign=1)                       ",
" outpar=/dev/tty	output parameter file				",
" 									",
" Output:								",
" aijkl,cijkl	(density normalized) stiffness components               ",
NULL};

/* 
 * Author: CWP: Andreas Rueger  1995
 */

/**************** end self doc ********************************/

int main (int argc, char **argv)
{

	double vp,vs,eps,delta,gamma,phi,rho;
	double temp;
        /* v33,v44; */
	char *outpar=NULL;	/* name of file holding output parfile  */
	FILE *outparfp=NULL;	/* ... its file pointer			*/

	int sign;
	
	Stiff2D *spar1;

	spar1=(Stiff2D*)emalloc(sizeof(Stiff2D));

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	if (!getpardouble("vp",&vp)) vp = 2.0;
	if (!getpardouble("vs",&vs)) vs = 1.0;
	if (!getpardouble("rho",&rho)) rho = 1;
	if (!getpardouble("eps",&eps)) eps = 0.;
	if (!getpardouble("delta",&delta)) delta = 0.;
	if (!getpardouble("gamma",&gamma)) gamma = 0.;
	if (!getpardouble("phi",&phi)) phi = 0.;
	if (!getparint("sign",&sign)) sign = 1;

	if (!getparstring("outpar", &outpar))  outpar = "/dev/tty" ;
	outparfp = efopen(outpar, "w");
        checkpars();


	if (!thom2stiffTI(vp,vs,eps,delta,gamma,phi*PI/180.,spar1,sign) ){
		fprintf(stderr," problems in thom2stiffTI ");
		return (-1);
	}
	
	temp=spar1->a1111;
	fprintf(outparfp," a1111 = %f \t c1111 = %f \n",temp,temp*rho);
	temp=spar1->a3333;
	fprintf(outparfp," a3333 = %f \t c3333 = %f \n",temp,temp*rho);
        temp=spar1->a1133;
	fprintf(outparfp," a1133 = %f \t c1133 = %f \n",temp,temp*rho);
	temp=spar1->a1313;
	fprintf(outparfp," a1313 = %f \t c1313 = %f \n",temp,temp*rho);
	temp=spar1->a1212;
	fprintf(outparfp," a1212 = %f \t c1212 = %f \n",temp,temp*rho);
	temp=spar1->a2323;
	fprintf(outparfp," a2323 = %f \t c2323 = %f \n",temp,temp*rho);

	/*
	fprintf(outparfp," INPUT TO ANISYN: \n\n");
	fprintf(outparfp," V11 = %f\n",  sqrt(spar1->a1111)*1000);
	fprintf(outparfp," V22 = %f\n",  sqrt(spar1->a3333)*1000);
	fprintf(outparfp," V33 = %f\n",  sqrt(spar1->a3333)*1000);
	fprintf(outparfp," V44 = %f\n",  sqrt(spar1->a2323)*1000);
	fprintf(outparfp," V55 = %f\n",  sqrt(spar1->a1313)*1000);
	fprintf(outparfp," V66 = %f\n",  sqrt(spar1->a1212)*1000);
	fprintf(outparfp," V12 = %f\n",  sqrt(spar1->a1133)*1000);
	fprintf(outparfp," V13 = %f\n",  sqrt(spar1->a1133)*1000);
	v33=sqrt(spar1->a3333)*1000;
        v44=sqrt(spar1->a2323)*1000;

	fprintf(outparfp," V23 = %f\n \n\n",  sqrt(v33*v33-2*v44*v44)); */
	return (1);
}

