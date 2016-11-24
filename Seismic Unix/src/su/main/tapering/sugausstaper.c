/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUTAPER: $Revision: 1.5 $ ; $Date: 2011/11/16 23:33:10 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUGAUSSTAPER - Multiply traces with gaussian taper		",
" 								",
" sugausstaper < stdin > stdout [optional parameters]		",
" 								",
" Required Parameters:					   	",
"   <none>							",
"								",
" Optional parameters:					   	",
" key=offset    keyword of header field to weight traces by 	",
" x0=300        key value defining the center of gaussian window", 
" xw=50         width of gaussian window in units of key value 	",
"								",
" Notes:							",
" Traces are multiplied with a symmetrical gaussian taper 	",
"  	w(t)=exp(-((key-x0)/xw)**2)				",
" unlike \"sutaper\" the value of x0 defines center of the taper",
" rather than the edges of the data.				",
NULL};

/* Credits:
 *
 *	Thomas Bohlen, formerly of TU Bergakademie, Freiberg GDR
 *      most recently of U Karlsruhe
 *          04.01.2002
 *
 * Trace header fields accessed: ns
 */
/**************** end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	char *key;	/* header key word from segy.h		*/
	char *type;     /* ... its type				*/	
	int index;	/* ... its index			*/
	Value val;	/* ... its value			*/
	float fval;     /* ... its value cast to float		*/

	int ns;		/* number of sample points on traces	*/
	float x0, xw;	/* centre and width of gauss taper	*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get optional parameters */
	if (!getparstring("key", &key))	key = "offset";
	if (!getparfloat("x0",&x0))	x0 = 300.0;
	if (!getparfloat("xw",&xw))	xw = 50.0;

        checkpars();
	/* Get key type and index */
	type = hdtype(key);
	index = getindex(key);

	/* Get info from first trace */
	if (!gettr(&tr))  err ("can't get first trace");
	ns = tr.ns;
 
	/* Loop through traces */
	do {
		register int i = 0;	     /* counter */

		/* Get value of key and convert to float */
		gethval(&tr, index, &val);
		fval = vtof(type,val);

		/* Loop over samples in trace and apply weighting */
		for (i=0; i < ns; ++i)
			tr.data[i] *= exp(-pow((fval-x0)/xw,2));

		/* Put out weighted traces */
		puttr(&tr);

	} while (gettr(&tr));





	return(CWP_Exit());
}
