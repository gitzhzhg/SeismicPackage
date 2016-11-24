/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUTRCOUNT: $Revision: 1.5 $ ; $Date: 2015/08/11 21:31:50 $	*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "su.h"
#include "segy.h"

/*********************** self documentation *****************************/
char *sdoc[] = {
" SUTRCOUNT - SU program to count the TRaces in infile		",
"       							",
"   sutrcount < infile					     	",
" Required parameters:						",
"       none							",
" Optional parameter:						",
"    outpar=stdout						",
" Notes:       							",
" Once you have the value of ntr, you may set the ntr header field",
" via:      							",
"       sushw key=ntr a=NTR < datain.su  > dataout.su 		",
" Where NTR is the value of the count obtained with sutrcount 	",
NULL};

/*
 * Credits:  B.Nemeth, Potash Corporation, Saskatchewan 
 * 		given to CWP in 2008 with permission of Potash Corporation
 */

/**************** end self doc ********************************/
   
/* Segy data constants */
segy tr;				/* SEGY trace */

int 
main(int argc, char **argv)
{
	/* Segy data constans */
	int ntr=0;		/* number of traces			*/
	char *outpar=NULL;	/* name of file holding output		*/
	FILE *outparfp=stdout;	/* ... its file pointer			*/

	initargs(argc, argv);
   	requestdoc(1);
	
	/* Get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	if (!getparstring("outpar", &outpar))	outpar = "/dev/stdout" ;
	
	outparfp = efopen(outpar, "w");

        checkpars();
	/* Loop over traces getting a count */
	do {
		++ntr;
	} while(gettr(&tr));

	fprintf(outparfp, "%d", ntr);

	return(CWP_Exit());

}
