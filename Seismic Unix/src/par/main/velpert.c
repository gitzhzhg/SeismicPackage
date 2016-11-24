/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* VELPERT: $Revision: 1.6 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" VELPERT - estimate velocity parameter perturbation from covariance 	",
"	 of imaged depths in common image gathers (CIGs)		",
"									",
" verpert <dfile dzfile=dzfile >outfile [parameters]			",
"									",
" Required Parameters:							",
" dfile			input of imaged depths in CIGs			",
" dzfile=dzfile		input of dz/dv at the imaged depths in CIGs	",
" outfile		output of the estimated parameter 		",
" noff			number of offsets 				",
" ncip			number of common image gathers 			",
"									",
" Optional Parameters:							",
" moff=noff	number of first offsets used in velocity estimation  	",
"									",
" Notes:								",
" 1. This program is part of Zhenyue Liu's velocity analysis technique.	",
"    The input dzdv values are computed using the program dzdv.		",
" 2. For given depths, using moff smaller than noff may avoid poor 	",
"    values of dz/dv at far offsets. However, a too small moff used	",
"    will the sensitivity of velocity error to the imaged depth.	",
" 3. Outfile contains three parts:					",
"    dlambda	correction of the velocity paramter. dlambda plus	",
"    		the initial parameter (used in migration) will	be	",
"		the updated one.					",
"    deviation	to measure how close imaged depths are to each other	",
"    		in CIGs. Old deviation corresponds to the initial	",
"		parameter; new deviation corresponds to the updated one.",
"    sensitivity  to predict how sensitive the error in the estimated	",
"		parameter is to an error in the measurement of imaged	",
"		depths.							",
"									",
"       error of parameter <= sensitivity * error of depth.		",
"									",
NULL};

/* 
 * Author:  Zhenyue Liu, 12/29/93,  Colorado School of Mines
 *
 * Reference: 
 * Liu, Z. 1995, "Migration Velocity Analysis", Ph.D. Thesis, Colorado
 *      School of Mines, CWP report #168.
 */
/**************** end self doc ***********************************/

/* prototypes for functions used interally */
void devia(float *a, int n);
float dotp(float *a, float *b, int n);

int
main (int argc, char **argv)
{
	int icip, ncip, noff, ioff, moff;
	float dev_old, dev_new, sens, den, num, dlambda;
	float **g, **z;
	FILE *infp=stdin, *outfp=stdout, *dzfp;
	char *dzfile="";

	/* hook up getpar to handle the parameters */
	initargs(argc, argv);
	requestdoc(1);
	
	/* get required parameters */
	if (!getparint("ncip",&ncip)) err("must specify ncip!\n");
	if (!getparint("noff",&noff)) err("must specify noff!\n");

	/* get optional parameters */
 	if (!getparstring("dzfile",&dzfile)) dzfile = "dzfile";
	if (!getparint("moff",&moff)) moff = noff;
	if (moff>noff) moff = noff;
	if(moff<=1) err("number of offsets must be greater than 1!\n");
	
        checkpars();

	/* allocate space */
	g = ealloc2float(noff,ncip);
	z = alloc2float(noff,ncip);
	
	/* read imaged depths */
	if(fread(z[0],sizeof(float),noff*ncip,infp)!=noff*ncip)
		err("cannot read %d values from file %s\n",ncip*noff,infp);

	/* read dz/dv */
	dzfp = fopen(dzfile,"r");
	if(fread(g[0],sizeof(float),noff*ncip,dzfp)!=noff*ncip) 
		err("cannot read %d values from file %s\n",ncip*noff,dzfp);	
	

	/* compute the covariance using old velocity	*/
	dev_old = num = den = 0.0;
	for(icip=0; icip<ncip; ++icip){	
		devia(z[icip],moff);
		devia(g[icip],moff);

		num += dotp(g[icip],z[icip],moff);
		den += dotp(g[icip],g[icip],moff);

		dev_old += dotp(z[icip],z[icip],moff);
	}	 	

	dev_old = sqrt(dev_old/(moff*ncip));

	if(den==0.0)
		err("dz/dv cannot be identical to each other!\n");
	dlambda = -num/den;

	warn("perturbation of parameter, dlambda = %g" ,dlambda);	
	fprintf(outfp,"\tperturbation of parameter, dlambda = %g\n"
			,dlambda);

	/* compute the covariance using new velocity	*/
	dev_new = 0;
	for(icip=0; icip<ncip; ++icip){	
		for(ioff=0; ioff<moff; ++ioff)
		   	z[icip][ioff] += g[icip][ioff]*dlambda;

		devia(z[icip],moff);
		dev_new += dotp(z[icip],z[icip],moff);
	}	 	

	dev_new = sqrt(dev_new/(moff*ncip));

	warn("old deviation = %g, new deviation = %g,\n", dev_old,dev_new);
	fprintf(outfp,"\t old deviation = %g, new deviation = %g,\n",
		dev_old,dev_new);
	
	/* compute the sensitivity of this velocity estimate	*/
	sens = sqrt(moff*ncip/den);
	sens = sqrt(moff*ncip/den);
	warn("sensitivity = %g, \n", sens);
	fprintf(outfp,"\t sensitivity = %g, \n", sens);

	free2float(g);
	free2float(z);

	return(CWP_Exit());
}


void devia(float *a, int n)
/* compute deviation of an array
input:
  a	array with demension n
output:
  a	difference between the original a and its average
 */
{
	float avb;
	int i;

	/* compute average of a	*/
	avb = 0;
	for(i=0; i<n; ++i)
		avb += a[i];
	avb /= n;

	for(i=0; i<n; ++i)
		a[i] -=  avb;
}
 
float dotp(float *a, float *b, int n)
/* dot product of two arrays	*/
{
	int i;
	float c;

	c = 0.;
	for(i=0; i<n; ++i)
		c +=b[i]*a[i];

	return c;
}
