/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* TRI2UNI: $Revision: 1.5 $ ; $Date: 2011/11/21 16:56:25 $	*/

#include "par.h"
#include "Triangles/tri.h"
#include "Triangles/sloth.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" TRI2UNI - convert a TRIangulated model to UNIformly sampled model	",
"									",
" tri2uni <triangfile >uniformfile n2= n1= [optional parameters]	",
"									",
" Required Parameters:							",
" n1=                     number of samples in the first (fast) dimension",
" n2=                     number of samples in the second dimension	",
"									",
" Optional Parameters:							",
" d1=1.0                 sampling interval in first (fast) dimension	",
" d2=1.0                 sampling interval in second dimension		",
" f1=0.0                 first value in dimension 1 sampled		",
" f2=0.0                 first value in dimension 2 sampled		",
"									",
" Note:									",
" The triangulated/uniformly-sampled quantity is assumed to be sloth=1/v^2",
"									",
NULL};
/*
 *
 * AUTHOR:  Dave Hale, Colorado School of Mines, 04/23/91
 *
 */
/**************** end self doc ***********************************/

/* the main program */
int main (int argc, char **argv)
{
	int n2,n1,ix,iz;
	float d2,f2,d1,f1,x,z,xmin,xmax,zmin,zmax,**s;
	Tri *t;
	TriAttributes *ta;
	Model *m;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	/* get required parameters */
	if (!getparint("n1",&n1)) err("must specify n1!");
	if (!getparint("n2",&n2)) err("must specify n2!");

	/* get optional parameters */
	if (!getparfloat("d1",&d1)) d1 = 1.0;
	if (!getparfloat("d2",&d2)) d2 = 1.0;
	if (!getparfloat("f1",&f1)) f1 = 0.0;
	if (!getparfloat("f2",&f2)) f2 = 0.0;
        checkpars();


	/* read input triangulated sloth model */
	m = readModel(stdin);

	/* determine min and max x and z coordinates */
	xmin = m->ymin;
	xmax = m->ymax;
	zmin = m->xmin;
	zmax = m->xmax;

	/* allocate space for uniformly sampled sloth */
	s = ealloc2float(n1,n2);

	/* loop over all samples */
	for (ix=0,x=f2,t=NULL; ix<n2; ++ix,x+=d2) {
		if (ABS(x-xmin)<0.01*d2) x = xmin;
		if (ABS(x-xmax)<0.01*d2) x = xmax;
		if (x<xmin || x>xmax)
			err("x=%g must be between xmin=%g and xmax=%g",
				x,xmin,xmax);
		for (iz=0,z=f1; iz<n1; ++iz,z+=d1) {
			if (ABS(z-zmin)<0.01*d1) z = zmin;
			if (ABS(z-zmax)<0.01*d1) z = zmax;
			if (z<zmin || z>zmax)
				err("z=%g must be between zmin=%g and zmax=%g",
					z,zmin,zmax);
			t = insideTriInModel(m,t,z,x);
			ta = (TriAttributes*)t->fa;
			s[ix][iz] = ta->s00+x*ta->dsdx+z*ta->dsdz;
		}
	}

	/* write uniformly sampled sloth */
	fwrite(s[0],sizeof(float),n1*n2,stdout);

	return EXIT_SUCCESS;
}
