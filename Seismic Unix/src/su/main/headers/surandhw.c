/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SURANDHW: $Revision: 1.3 $ ; $Date: 2011/11/16 22:10:29 $        */

#include "su.h"
#include "segy.h"
#include "header.h"
#include <time.h>
#include <signal.h>

/*********************** self documentation ******************************/
char *sdoc[] = {
" 								",
" surandhw - set header word to random variable 		",
" 								",
" surandhw <stdin >stdout key=tstat a=0 min=0 max=1		",
" 								",
" Required parameters:						",
" 	none (no op)						",
" 								",
" Optional parameters:						",
" 	key=tstat	header key word to set			",
" 	a=0		=1 flag to add original value to final key",
" 	noise=gauss	noise probability distribution		",
" 			=flat for uniform; default Gaussian	",
" 	seed=from_clock	random number seed (integer)		",
" 	min=0		minimum random number			",
" 	max=1		maximum radnom number		 	",
" 								",
" NOTES:							",
" The value of header word key is computed using the formula:	",
" 	val(key) = a * val(key) + rand				",
" 								",
" Example:							",
"  	surandhw <indata key=tstat a=0 min=0 max=10  > outdata	",
" 								",
NULL}; 
/**************** end self doc ****************************************/

/* Credits:
 *  Eric Verschuur Delft University of Technology
 *  eric@delphi.tn.tudelft.nl
 *
 *  based on sushw written by:
 *	SEP: Einar Kjartannson
 *	CWP: Jack K. Cohen
 *
 * Caveat:
 *	All constants are cast to doubles.
 */


segy tr;

/* Prototypes */
void setrandval(cwp_String type, Value *valp,
	double a, double noise);

/* Noise probability distributions */
#define	GAUSS	0
#define	FLAT	1


int 
main(int argc, char **argv)
{
	cwp_String key;		/* header key field			*/
	cwp_String type;	/* ... its type				*/
	int index=0;		/* ... its index			*/ 
	Value val;		/* ... its value			*/

	double a;	/* flag =1 add noise to original value		*/
	double min; 	/* minimum value of noise			*/
	double max;	/* maximum value of noise			*/
	double noise;	/* random value for header or to add to header	*/
	cwp_String stype;	/* name of type of distribution		*/
	int itype=0;		/* integer index for distribution type	*/

	unsigned int seed;	/* random number seed			*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get parameters */
	if (!getparstring("key", &key))	 key = "tstat";
	if (!getpardouble("a"  , &a))	 a = 1;
	if (!getpardouble("min"  , &min)) min = 0;
	if (!getpardouble("max"  , &max)) max = 1;

	/* Get noise type */
	if (!getparstring("noise", &stype))	stype = "gauss";

	if      (STREQ(stype, "gauss")) itype = GAUSS;
	else if (STREQ(stype, "flat"))  itype = FLAT;
	else     err("noise=\"%s\", must be gauss or flat", stype);

	/* Set seed */
	if (!getparuint("seed", &seed)) { /* if not supplied, use clock */
		if (-1 == (seed = (unsigned int) time((time_t *) NULL))) {
			err("time() failed to set seed");
		}
	}
        checkpars();

	if (itype == GAUSS) srannor(seed); 
			sranuni(seed);

	type = hdtype(key);
	index = getindex(key);

	while (gettr(&tr)) {

		/* get new noise value */
		switch (itype) {
		case GAUSS: /* frannor gives elements in N(0,1)--ie. pos & negs */
			noise = min + ((max - min) * (1 + frannor())/2);
		break;
		case FLAT: /* franuni gives elements in [0, 1] */
			noise = (max - min) * franuni() + min;
		break;
		default:	/* defensive programming */
			err("%d: mysterious itype = %d", __LINE__, itype);
		}

		/* get desired key from header and add noise */
		gethval(&tr, index, &val);
		setrandval(type, &val, a, noise);
		puthval(&tr, index, &val);

		/* write updated trace */
		puttr(&tr);
	}

        return(CWP_Exit());
}


void 
setrandval( cwp_String type, Value *valp, double a, double noise)
/*********************************************************************
setrandval - set a random value
type	the type of the header value
valp	pointer to the value
a	flag =0 set header value to value of noise 
        =1 add noise to original header value
noise	random value
*********************************************************************/
{
	switch (*type) {
	case 's':
		err("can't set char header word");
	break;
	case 'h':
		valp->h = a * valp->h + noise;
	break;
	case 'u':
		valp->u = a * valp->u + noise;
	break;
	case 'l':
		valp->l = a * valp->l + noise;
	break;
	case 'v':
		valp->v = a * valp->v + noise;
	break;
	case 'i':
		valp->i = a * valp->i + noise;
	break;
	case 'p':
		valp->p = a * valp->p + noise;
	break;
	case 'f':
		valp->f = a * valp->f + noise;
	break;
	case 'd':
		valp->d = a * valp->d + noise;
	default:
		err("unknown type %s", type);
	break;
	}
	return;
}
