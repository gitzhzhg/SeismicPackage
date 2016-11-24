/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUCLIPHEAD - Clip header values					",
" 									",
" sucliphead <stdin >stdout [optional parameters]			",
"									",
" Required parameters:							",
"	none								",
"									",
" Optional parameters:							",
"	key=cdp,...			header key word(s) to clip	",
"	min=0,...			minimum value to clip		",
"	max=ULONG_MAX,ULONG_MAX,...	maximum value to clip		",
"									",
"									",
NULL}; 

/* Credits:
 *	Geocon: Garry Perratt
 *
 */
/**************** end self doc ********************************/

void changeval(cwp_String type, Value *valp, float fval);

segy tr;

int
main(int argc, char **argv)
{
	cwp_String key[SU_NKEYS];	/* array of keywords		*/
	cwp_String type[SU_NKEYS];	/* array of types for key	*/
	int index[SU_NKEYS];		/* array of indexes for key	*/
	int ikey;		/* key counter				*/
	int nkeys;		/* number of header fields set		*/
	int n;			/* number of min,max values   		*/
	Value val;		/* value of key field			*/
	double fval;		/* value of key field			*/
	float *min=NULL;	/* array of "min" values		*/
	float *max=NULL;	/* array of "max" values		*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get "key" values */
	if ((nkeys=countparval("key"))!=0) {
		getparstringarray("key",key);

	} else {
		key[0]="cdp";
	}

	/* get types and indexes corresponding to the keys */
	for (ikey=0; ikey<nkeys; ++ikey) {
		type[ikey]=hdtype(key[ikey]);
		index[ikey]=getindex(key[ikey]);
	}

	/* get "min" values */
	if ((n=countparval("min"))!=0) { 
		if (n!=nkeys)
		err("number of a values not equal to number of keys");
		min=ealloc1float(n);
		getparfloat("min",min);
	} else {
		min=ealloc1float(nkeys);
		for (ikey=0; ikey<nkeys; ++ikey) min[ikey]=0.;
	}

	/* get "max" values */
	if ((n=countparval("max"))!=0) { 
		if (n!=nkeys)
		err("number of a values not equal to number of keys");
		max=ealloc1float(n);
		getparfloat("max",max);
	} else {
		max=ealloc1float(nkeys);
		for (ikey=0; ikey<nkeys; ++ikey) max[ikey]=ULONG_MAX;
	}

        checkpars();
	/* get types and index values */
	for (ikey=0; ikey<nkeys; ++ikey) {
		type[ikey] = hdtype(key[ikey]);
		index[ikey] = getindex(key[ikey]);
	}

	while (gettr(&tr)) {
		for (ikey=0; ikey<nkeys; ++ikey) {
			gethval(&tr, index[ikey], &val);
			fval = vtof(type[ikey], val);
			if (fval < min[ikey]) {
				changeval(type[ikey], &val, min[ikey]);
				puthval(&tr, index[ikey], &val);
			} else if (fval > max[ikey]) {
				changeval(type[ikey], &val, max[ikey]);
				puthval(&tr, index[ikey], &val);
			}
		}
		puttr(&tr);
	}

	return(CWP_Exit());
}

void changeval(cwp_String type, Value *valp, float fval)
{
	switch (*type) {
	case 's':
		err("cannot change char header word");
	break;
	case 'h':
		valp->h = (short) fval;
	break;
	case 'u':
		valp->u = (unsigned short) fval;
	break;
	case 'l':
		valp->l = (long) fval;
	break;
	case 'v':
		valp->v = (unsigned long) fval;
	break;
	case 'i':
		valp->i = (int) fval;
	break;
	case 'p':
		valp->p = (unsigned int) fval;
	break;
	case 'f':
		valp->f = (float) fval;
	break;
	case 'd':
		valp->d = (double) fval;
	break;
	default:
		err("unknown type %s", type);
	break;
	}
}
