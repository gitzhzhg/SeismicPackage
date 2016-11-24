/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUNAN: $Revision: 1.8 $ ; $Date: 2011/12/21 23:19:56 $        */

#include <math.h>
#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[]={
"								",
" SUNAN - remove NaNs & Infs from the input stream		",
"								",
"    sunan < in.su >out.su					",
"								",
" Optional parameters:						",
" verbose=1	echo locations of NaNs or Infs to stderr	",
"	        =0 silent					",
" ...user defined ... 						",
"								",
" value=0.0	NaNs and Inf replacement value			",
" ... and/or....						",
" interp=0	=1 replace NaNs and Infs by interpolating	",
"                   neighboring finite values			",
"								",
" Notes:							",
" A simple program to remove NaNs and Infs from an input stream.",
" The program sets NaNs and Infs to \"value\" if interp=0. When	",
" interp=1 NaNs are replaced with the average of neighboring values",
" provided that the neighboring values are finite, otherwise	",
" NaNs and Infs are replaced by \"value\".			",
NULL};

/*
 * Author: Reginald H. Beardsley  2003   rhb@acm.org
 *
 *  A simple program to remove NaNs & Infs from an input stream. They
 *  shouldn't be there, but it can be hard to find the cause and fix
 *  the problem if you can't look at the data.
 *
 *  Interpolation idea comes from a version of sunan modified by
 *  Balasz Nemeth while at Potash Corporation in Saskatchewan.
 *
 */

/**************** end self doc ********************************/

segy tr;

int
main(int argc, char **argv)
{

	int i;			/* counter			*/
	int itr=0;		/* trace counter		*/
	int verbose;		/* =0 silent,  =1 chatty	*/
	int interp;		/* =1 interpolate to get NaN	*/
				/* and Inf replacement values	*/
			
	float value;		/* value to set NaN and Infs to */

	/* Initialize */
   	initargs(argc,argv);
   	requestdoc(1);

	/* Get info from first trace */
	if(!gettr(&tr) ) err("Can't get first trace \n");

	/* Get parameters */
	if(!getparint("verbose",&verbose))	verbose = 1;
	if(!getparint("interp",&interp))	interp = 0;
	if(!getparfloat("value",&value))	value = 0.0;
        checkpars();

	/* Loop over traces */
	do{
		++itr;
      		for(i=0; i<tr.ns; ++i){
		    if(!isfinite(tr.data[i])) {
		       if (verbose)
	                warn("found NaN trace = %d  sample = %d", itr, i);

			if (interp) { /* interpolate nearest neighbors */
				      /* for NaN replacement value     */
				if (i==0 && isfinite(tr.data[i+1])) { 
					tr.data[i]=tr.data[i+1];
				} else if(i==tr.ns-1 && isfinite(tr.data[i-2])) {
					tr.data[i]= tr.data[i-2];
				} else if( isfinite(tr.data[i-1]) &&
						isfinite(tr.data[i+1]) ) {
					tr.data[i]=(tr.data[i-1]+tr.data[i+1])/2.0;
				}
			}
				
			/* use user defined NaNs replacement value */
            	       	tr.data[i] = value;
			}
		    }

      		puttr(&tr);
	} while(gettr(&tr));

	return(CWP_Exit());
}
