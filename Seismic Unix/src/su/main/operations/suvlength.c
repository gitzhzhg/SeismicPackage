/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUVLENGTH: $Revision: 1.8 $ ; $Date: 2011/11/16 23:09:52 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUVLENGTH - Adjust variable length traces to common length   	",
" 								",
" suvlength <vdata >stdout					",
" 								",
" Required parameters:						",
" 	none							",
" 								",
" Optional parameters:						",
" 	ns=	output number of samples (default: 1st trace ns)",
NULL};
/**************** end self doc ***********************************/

/* Credits:
 *	CWP: Jack Cohen, John Stockwell
 *
 * Trace header fields accessed:  ns
 * Trace header fields modified:  ns
 */

segy tr;

int
main(int argc, char **argv)
{
	int ns;		/* samples on output traces	*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


        /* Get info from first trace */ 
        if (!fvgettr(stdin, &tr))  err("can't get first trace"); 
        if (!getparint("ns", &ns)) ns = tr.ns;
        checkpars();


	/* Loop over the traces */
	do {
		int nt = tr.ns;
				
		if (nt < ns) /* pad with zeros */
                	memset((void *)(tr.data + nt), 0, (ns-nt)*FSIZE);
		tr.ns = ns;
		puttr(&tr);
	} while (fvgettr(stdin, &tr));
	
	return(CWP_Exit());
}

