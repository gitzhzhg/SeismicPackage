/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*CELLAUTO: $Revision: 1.4 $ ; $Date: 2011/11/22 16:45:57 $		*/

#include "par.h"
#include <time.h>

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" CELLAUTO - Two-dimensional CELLular AUTOmata			  	",
"									",
"   cellauto > stdout [optional params]					",
"									",
" Optional Parameters:							",
" n1=500	output dimensions of image (n1 x n1 pixels)	 	",
" rule=30	CA rule (Wolfram classification)			",
" 		Others: 54,60,62,90,94,102,110,122,126			",
"                       150,158,182,188,190,220,222,225,226,250		",
" fill=0	Don't fill image (=1 fill image)			",
" f0=330	fill zero values with f0				",
" f1=3000	fill non-zero values with f1				",
" ic=1		initial condition for centered unit value at t=0	",
"               = 2 for multiple random units				",
" nc=20		number of random units (if ic=2)			",
" tc=1		random initial units at t=0 (if ic=2)			",
"               = 2 for initial units at random (t,x)			",
" verbose=0	silent operation					",
"               = 1 echos 'porosity' of the CA in bottom half of image	",
" seed=from_clock    	random number seed (integer)            	",
"									",
" Notes:								",
" This program generates a select set of Wolframs fundamental cellular	",
" automata. This may be useful for constructing rough, vuggy wavespeed	",
" profiles. The numbering scheme follows Stephen Wolfram's.		",
"									",
" Example: 								",
"  cellauto rule=110 ic=2 nc=100 fill=1 f1=3000 | ximage n1=500 nx=500 &",
"									",
" Here we simulate a complex near surface with air-filled 		",
" vugs in hard country rock, with smoothing applied via smooth2 	",
"									",
"  cellauto rule=110 ic=2 nc=100 fill=1 f1=3000 n1=500 |		",
"  smooth2 n1=500 n2=500 r1=5 r2=5 > vfile.bin				",
"									",
NULL};

/* Credits:
 *	UHouston: Chris Liner 	
 *
 * Trace header fields accessed:  ns
 * Trace header fields modified:  ns and delrt
 */
/**************** end self doc *******************************************/

/* function prototypes */
static float ruleCA (int rule, int code);

int
main(int argc, char **argv)
{
	int n1=0,n2=0,nz=0,nx=0,ix,it;
	float **ca=NULL;	/* output image matrix */

	int rule;		/* cellular automata rule */
	int a, b, c;		/* binary numbers (0,1)  */
	int code=0;		
	int ic, nc, tc;		/* initial conditions	*/
	int i;			/* counter 		*/
	float val;

	unsigned int seed;      /* random number seed */
	float wht, blk;		/* flags for white and black */
	int fill;		/* flag for filling with velocities */
	float f0, f1;		/* fill values		*/
	float *data=NULL;	/* output data array */
	int count=0;

	/* hook up getpar */
	initargs(argc, argv);
	requestdoc(0);

	/* get parameters */
	if (!getparint("n1",&n1)) 	n1=500;
		if(ISODD(n1)) {
			nz = n1;
		} else {  
			nz= n1+1;
		}
	/* set output slow dimension */
	n2=n1;	
	nx=2*nz + 1;

	if (!getparint("rule",&rule)) 	rule=30;
	if (!getparint("ic",&ic)) 	ic=1;
	if (!getparint("nc",&nc)) 	nc=20;
	if (!getparint("tc",&tc)) 	tc=1;
	if (!getparint("fill",&fill)) 	fill=0;
	if (!getparfloat("f0",&f0)) 	f0=330;
	if (!getparfloat("f1",&f1)) 	f1=3000;
	
	/* allocate workspace */
	data = ealloc1float(nz);
	ca = ealloc2float(nz,nx);

	/* Set seed */
	if (!getparuint("seed", &seed)) { /* if not supplied, use clock */
		if (-1 == (seed = (unsigned int) time((time_t *) NULL))) {
			err("time() failed to set seed");
		}
	}
	sranuni(seed);
	
	/* set CA seed */
	if (ic==1) ca[(nx-1)/2][0] = 1.0;
	if (ic==2) {
		for (i=0; i<nc; ++i) {
			ix = nx/4 + franuni()*nx/2;
			if (tc==2) { 
				it = franuni()*nz/2;
			} else {
				it = 0;
			}
			ca[ix][it] = 1.0;
			/* warn("ix=%i",ix); */
		}
	}

	/* build ca matrix */
	for (it = 1; it < nz; ++it) {
		
		for (ix = 1; ix < nx-1; ++ix) {

			/* get 3 vals centered above this location */
			a = (int) ca[ix-1][it-1];
			b = (int) ca[ix][it-1];
			c = (int) ca[ix+1][it-1];
			
			/* map the pattern into an integer code
			   111,110,101,100,011,010,001,000  <-- input pattern
			   111,110,101,100, 11, 10,  1,  0  <-- code
			*/
			if (a!=0 && b!=0 && c!=0) code = 111;
			if (a!=0 && b!=0 && c==0) code = 110;
			if (a!=0 && b==0 && c!=0) code = 101;
			if (a!=0 && b==0 && c==0) code = 100;
			if (a==0 && b!=0 && c!=0) code =  11;
			if (a==0 && b!=0 && c==0) code =  10;
			if (a==0 && b==0 && c!=0) code =   1;
			if (a==0 && b==0 && c==0) code =   0;

			/* apply ca rule to get value at this location */
			val = ruleCA(rule,code);
			
			/* update ca matrix */
			if (val == 1) ca[ix][it] += val;
			
			/* 
				warn("ix,it,a,b,c,code,val=%i,%i,%i,%i,%i,%i,%f",
			        	ix,it,a,b,c,code,val);
			*/

		}

	}
	
	/* pull off traces and output */
	wht = 0;
	blk = 0;

	count=0;
	for (ix=nx/4 ; ix < 3*nx/4 ; ++ix){


		for (it = 0 ; it < nz ; ++it){
		
			if ( (it > nz/2) && (ca[ix][it] == 0) ) wht += 1.0;
			if ( (it > nz/2) && (ca[ix][it] != 0) ) blk += 1.0;			
			if ( fill == 0 ) {
				data[it] = ca[ix][it];
			} else {
				if ( ca[ix][it] == 0.0 ) data[it] = f0;
				if ( ca[ix][it] != 0.0 ) data[it] = f1;
			}
			
		}		

		if (count < n2) fwrite(data,n1,sizeof(float),stdout);

		++count;
	}
	
	warn("bottom half porosity = %f",100*wht/(wht+blk));

	return(CWP_Exit());
}


static float ruleCA (int rule, int code)
/*****************************************************************************
value for cellular automata   
******************************************************************************
Input:
code	pattern code
val	output value
	111,110,101,100, 11, 10,  1,  0  <-- code
******************************************************************************
Notes: 
******************************************************************************
Author:  Chris Liner, 10/21/2006
******************************************************************************/
{

	if (rule==30) {
		switch (code)	{	
			case 111:
				return 0.0;
			break;
			case 110:
				return 0.0;
			break;
			case 101:
				return 0.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 1.0;
			break;
			case 10:
				return 1.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==54) {
		switch (code)	{	
			case 111:
				return 0.0;
			break;
			case 110:
				return 0.0;
			break;
			case 101:
				return 1.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 0.0;
			break;
			case 10:
				return 1.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==60) {
		switch (code)	{	
			case 111:
				return 0.0;
			break;
			case 110:
				return 0.0;
			break;
			case 101:
				return 1.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 1.0;
			break;
			case 10:
				return 1.0;
			break;
			case 1:
				return 0.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==62) {
		switch (code)	{	
			case 111:
				return 0.0;
			break;
			case 110:
				return 0.0;
			break;
			case 101:
				return 1.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 1.0;
			break;
			case 10:
				return 1.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==90) {
		switch (code)	{	
			case 111:
				return 0.0;
			break;
			case 110:
				return 1.0;
			break;
			case 101:
				return 0.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 1.0;
			break;
			case 10:
				return 0.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==94) {
		switch (code)	{	
			case 111:
				return 0.0;
			break;
			case 110:
				return 1.0;
			break;
			case 101:
				return 0.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 1.0;
			break;
			case 10:
				return 1.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==102) {
		switch (code)	{	
			case 111:
				return 0.0;
			break;
			case 110:
				return 1.0;
			break;
			case 101:
				return 1.0;
			break;
			case 100:
				return 0.0;
			break;
			case 11:
				return 0.0;
			break;
			case 10:
				return 1.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==110) {
		switch (code)	{	
			case 111:
				return 0.0;
			break;
			case 110:
				return 1.0;
			break;
			case 101:
				return 1.0;
			break;
			case 100:
				return 0.0;
			break;
			case 11:
				return 1.0;
			break;
			case 10:
				return 1.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==122) {
		switch (code)	{	
			case 111:
				return 0.0;
			break;
			case 110:
				return 1.0;
			break;
			case 101:
				return 1.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 1.0;
			break;
			case 10:
				return 0.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==126) {
		switch (code)	{	
			case 111:
				return 0.0;
			break;
			case 110:
				return 1.0;
			break;
			case 101:
				return 1.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 1.0;
			break;
			case 10:
				return 1.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==150) {
		switch (code)	{	
			case 111:
				return 1.0;
			break;
			case 110:
				return 0.0;
			break;
			case 101:
				return 0.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 0.0;
			break;
			case 10:
				return 1.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==158) {
		switch (code)	{	
			case 111:
				return 1.0;
			break;
			case 110:
				return 0.0;
			break;
			case 101:
				return 0.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 1.0;
			break;
			case 10:
				return 1.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==182) {
		switch (code)	{	
			case 111:
				return 1.0;
			break;
			case 110:
				return 0.0;
			break;
			case 101:
				return 1.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 0.0;
			break;
			case 10:
				return 1.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==188) {
		switch (code)	{	
			case 111:
				return 1.0;
			break;
			case 110:
				return 0.0;
			break;
			case 101:
				return 1.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 1.0;
			break;
			case 10:
				return 1.0;
			break;
			case 1:
				return 0.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==190) {
		switch (code)	{	
			case 111:
				return 1.0;
			break;
			case 110:
				return 0.0;
			break;
			case 101:
				return 1.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 1.0;
			break;
			case 10:
				return 1.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==220) {
		switch (code)	{	
			case 111:
				return 1.0;
			break;
			case 110:
				return 1.0;
			break;
			case 101:
				return 0.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 1.0;
			break;
			case 10:
				return 1.0;
			break;
			case 1:
				return 0.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==222) {
		switch (code)	{	
			case 111:
				return 1.0;
			break;
			case 110:
				return 1.0;
			break;
			case 101:
				return 0.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 1.0;
			break;
			case 10:
				return 1.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==225) {
		switch (code)	{	
			case 111:
				return 1.0;
			break;
			case 110:
				return 1.0;
			break;
			case 101:
				return 1.0;
			break;
			case 100:
				return 0.0;
			break;
			case 11:
				return 0.0;
			break;
			case 10:
				return 0.0;
			break;
			case 1:
				return 0.0;
			break;
			case 0:
				return 1.0;
			break;	
		}
	}
	if (rule==226) {
		switch (code)	{	
			case 111:
				return 1.0;
			break;
			case 110:
				return 1.0;
			break;
			case 101:
				return 1.0;
			break;
			case 100:
				return 0.0;
			break;
			case 11:
				return 0.0;
			break;
			case 10:
				return 0.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==250) {
		switch (code)	{	
			case 111:
				return 1.0;
			break;
			case 110:
				return 1.0;
			break;
			case 101:
				return 1.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 1.0;
			break;
			case 10:
				return 0.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	if (rule==260) {
		/* x(it+1,ix) = x(it,ix-1) - x(it,ix) + x(it,ix+1) */
		switch (code)	{	
			case 111:
				return 1.0;
			break;
			case 110:
				return 0.0;
			break;
			case 101:
				return 2.0;
			break;
			case 100:
				return 1.0;
			break;
			case 11:
				return 0.0;
			break;
			case 10:
				return -1.0;
			break;
			case 1:
				return 1.0;
			break;
			case 0:
				return 0.0;
			break;	
		}
	}
	err("Bad rule number");
	return EXIT_FAILURE;
}
