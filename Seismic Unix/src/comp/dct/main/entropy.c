/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* ENTROPY: $Revision: 1.5 $ ; $Date: 2011/11/17 00:17:48 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"								",
" ENTROPY - compute the ENTROPY of a signal			",
"								",
"  entropy < stdin n= > stdout					",
"								",
" Required Parameter:						",
"  n		number of values in data set			",
"								",
" Optional Parameters:						",
"  none								",
"  								",
NULL};

/*
 * Author: CWP: Tong Chen, 1995.
 * 
 */
/**************** end self doc ********************************/

int
main(int argc, char **argv)
{
	int i, j, n;
	float *f,  max, step, rstep;
	float fhist[1024], dev, rdev, ent, error; 
	int hist[1024];
	 
	initargs(argc, argv);
	requestdoc(1);

	MUSTGETPARINT("n",&n);

	f = alloc1float(n);

	fread(f,sizeof(float),n,stdin);
	
	for(i=0;i<1024;i++) hist[i] = 0;

	for(i=0,rdev=0.;i<n;i++)
	   rdev += f[i]*f[i]; 
	rdev = rdev/n;
	rdev = sqrt(rdev);

	if(!getparfloat("dev",&dev)) dev = rdev;

        checkpars();
	fprintf(stderr,"dev=%f\n", dev);

	step = dev*3.464*.01;

	rstep = 1./step;

	error = 0.;
	for(i=0;i<n;i++){
	    max = f[i]*rstep;
	    error += (NINT(max)*step - f[i])*(NINT(max)*step - f[i]);
	    hist[NINT(max)+512] ++;
	}

	error = error/n;
	error = sqrt(error);
	error = error/rdev;

	ent = 0.;
	for(j=0;j<1024;j++){
		fhist[j] = ((float) hist[j])/((float) n);
		if(hist[j])
		    ent += fhist[j]*log(fhist[j])/log(2.);
	}
	ent = -ent;

fprintf(stderr,"entropy of the signal is=%f, average error=%f\n",ent, error);

	fwrite(fhist,sizeof(float),1024,stdout);

	return EXIT_SUCCESS;
}
