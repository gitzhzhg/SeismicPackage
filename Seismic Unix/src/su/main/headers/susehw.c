/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSETEHW: $Revision: 1.3 $ ; $Date: 2012/03/28 16:44:34 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include "bheader.h"
#include "tapebhdr.h"


/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUSEHW - Set the value the Header Word denoting trace number within	",
"	     an Ensemble defined by the value of another header word	",
"									",
"     susehw <stdin >stdout [options]					",
"									",
" Required Parameters:							",
"	none								",
"									",
" Optional Parameters:							",
" key1=cdp	Key header word defining the ensemble			",
" key2=cdpt	Key header word defining the count within the ensemble	",
" a=1		starting value of the count in the ensemble		",
" b=1		increment or decrement within the ensemble		",
"									",
" Notes:								",
" This code was written because suresstat requires cdpt to be set.	",
" The computation is 							",
" 	val(key2) = a + b*i						",
"									",
" The input data must first be sorted into constant key1 gathers.	",
" Example: setting the cdpt field					", 
"        susetehw < cdpgathers.su a=1 b=1 key1=cdp key2=cdpt > new.su	",
"									",
NULL};

/* Credits:
 *  CWP: John Stockwell (Feb 2008) in answer to a question by Warren Franz
 *        based on various codes, including susplit, susshw, suchw
 */
/**************** end self doc *******************************************/

/* Prototypes of functions used internally */
void setval(cwp_String type, Value *valp, double a, double b, double i);

segy tr;

int
main(int argc, char **argv)
{
	cwp_String key1;	/* key word labeling the ensemble	*/
	Value val1;		/* value of key1			*/
	int ival1;		/* key1 value as integer 		*/
	int ival1old;		/* last value of ival			*/ 
	cwp_String type1;	/* key1's type				*/
	int index1;		/* index of key1			*/

	int i=0;		/* counter 				*/

	cwp_String key2;	/* key word labelin ensemble members	*/
	Value val2;		/* value of key2			*/
	cwp_String type2;	/* key2's type				*/
	int index2;		/* index of key2			*/

	double a;		/* initial header value within ensemble */
	double b;		/* increment (decrement) within ensemble*/

	int verbose;		/* =1 to echo filenames, etc.		*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Default parameters;  User-defined overrides */
	if (!getparstring("key1", &key1))	key1="cdp";
	if (!getparstring("key2", &key2))	key2="cdpt";
	if (!getpardouble("a", &a))	 	a=1;
	if (!getpardouble("b", &b))	 	b=1;
	if (!getparint("verbose",&verbose)) 	verbose=0;

        checkpars();

	/* Evaluate time bounds from getpars and first header */
	if (!gettr(&tr)) err("can't get first trace");

	/* Set up key values */
	type1 = hdtype(key1);
	index1 = getindex(key1);
	type2 = hdtype(key2);
	index2 = getindex(key2);

	/* Output debugging information */
	if (verbose) warn("key1 = %s",key1);
	if (verbose) warn("key2 = %s",key2);

	/* Get value of key1 from the first trace */
	gethval(&tr, index1, &val1);
	ival1 = vtoi(type1, val1);
	ival1old = ival1;

	/* Main loop over traces */
	i = 0;
	do {
		/* get value of key1 header field */
		gethval(&tr, index1, &val1);
		ival1 = vtoi(type1, val1);

		/* test to see if we are in a new ensemble */
		if (ival1!=ival1old) { 
			ival1old = ival1;
			i = 0; /* beginning of a new ensemble */
		}

		/* set header value in trace */
		setval(type2, &val2, a,  b,  i);
		puthval(&tr,index2,&val2);

		/* increment trace counter */
		++i;

		/* output trace with modified header */
		puttr(&tr);

	} while (gettr(&tr));

	return(CWP_Exit());
}

void setval( cwp_String type, Value *valp, double a, double b, double i)
/**********************************************************************
sethval - set header word value on a trace
***********************************************************************
cwp_String type     type (int, short, long, double, float)
Value *valp         pointer to value
double a            initial header value
double b            increment (decrement) of header values
double i	    trace counter
***********************************************************************
Notes:
The SU header fields have a variety of types. Getting this right requires
a bit of doing.  
***********************************************************************
Author:  CWP: John Stockwell,  Feb 2008, hacked from the one in sushw.
**********************************************************************/
{
	switch (*type) {
	case 's':
		err("can't set char header word");
	break;
	case 'h':
		valp->h = (a + b * i);
	break;
	case 'u':
		valp->u = (a + b * i);
	break;
	case 'l':
		valp->l = (long) (a + b * i );
	break;
	case 'v':
		valp->v = (unsigned long) (a + b * i);
	break;
	case 'i':
		valp->i = (a + b * i );
	break;
	case 'p':
		valp->p = (a + b * i );
	break;
	case 'f':
		valp->f = (a + b * i );
	break;
	case 'd' :
		valp->d = (a + b * i );
	default:
		err("unknown type %s", type);
	break;
	}
	return;
}

