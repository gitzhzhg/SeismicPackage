/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2010.*/
/* All rights reserved.                       */

/* SUAHW: $Revision: 1.3 $ ; $Date: 2011/11/16 22:10:29 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUAHW - Assign Header Word using another header word			",
"									",
"  suahw <stdin >stdout [optional parameters]				",
"									",
" Required parameters:							",
"  key1=ep		output key 					",
"  key2=fldr		input key 					",
"  a=			array of key1 output values			",
"  b=			array of key2 input values			",
"  mode=extrapolate	how to assign a key1-value, when the key2-value	",
"			is not found in b:				",
"			=interpolate	interpolate			",
"			=extrapolate	interpolate and extrapolate	",
"			=zero		zero key1-values		",
"			=preserve	preserve key1-values		",
"			=transfer	transfer key2-values to key1	",
"									",
" Optional parameters:							",
"  key3=tracf		input key 					",
"  c=			array of key3 input values			",
"									",
" The key1-value is assigned based on the key2-value and the arrays a,b.",
" If the header value of key2= equals the n'th element in b=, then the	",
" header value key1= is set to the n'th element in a=.			",
" The arrays a= and b= must have the same size, and the elements of b=	",
" must be in ascending order.						",
"									",
" The mode-switch decides what to do when a trace header has a key2-value",
" that is not an element of the b-array:				",
"    zero - the key1-value will be set to zero				",
"    preserve - the key1-value will not be modified			",
"    transfer - the key2-value will be assigned to key1			",
"    interpolate - if the key2-value is greater than the n'th element	",
"	and less than the (n+1)'th element of b=, then the key1-value	",
"	will be	interpolated accordingly from the n'th and (n+1)'th	",
"	element of a=. Otherwise, key1 will not be changed.		",
"    extrapolate - same as interpolate, plus, if the key2-value is	",
"	smaller/greater than the first/last element of b=, then the	",
"	key1-value will be set to the first/last element of a=		",
"									",
" The array c= can be used to prevent the modification of trace headers	",
" with certain key3-values. The number of elements in c= is independent	",
" of the other arrays.							",
" The key1-value will not be modified, if the mode-switch is set to	",
"    zero, preserve, transfer - and the key3-value is an element of c=	",
"    interpolate, extrapolate - and the key3-value is outside of c=	",
"				(smaller than the first or greater than	",
"				the last element of c=)			",
"									",
" Examples:								",
"  Assign shot numbers 1-3 to field file ID 1009,1011,1015 and 0 to the	",
"  remaining FFID (fldr):						",
"    suahw <data a=1,2,3 b=1009,1011,1015 mode=zero			",
"									",
"  Use channel numbers (tracf) to assign stations numbers (tracr) for a	",
"  split spread with a gap:						",
"    suahw <data key1=tracr a=151,128,124,101 key2=tracf b=1,24,25,48	",
"									",
"  Assign shot-statics:							",
"    suahw <data key1=sstat key2=ep a=-32,13,-4 b=1,2,3			",
"									",
"  Set trid to 0 for channel 1-24, but only for the record 1016:	",
"    suahw <data key1=trid key2=tracf key3=fldr a=0,0 b=1,24 c=1016	",
NULL};

/* Credits:
 *	Florian Bleibinhaus, U Salzburg, Austria
 *	cloned from suchw of Einar Kajartansson, SEP
 */
/**************** end self doc ***********************************/

#define ZERO		0
#define INTERPOLATE	1
#define EXTRAPOLATE	2
#define PRESERVE	3
#define TRANSFER	4

segy tr;

/* Prototypes */

void assgnval(cwp_String type1, Value *valp1, double dval1);

int
main(int argc, char **argv)
{
	cwp_String key1;		/* output key			*/
	cwp_String key2;		/* input key			*/
	cwp_String key3;		/* input key			*/
	cwp_String type1;		/* type for output key		*/
	cwp_String type2;		/* type for input key		*/
	cwp_String type3;		/* type for input key		*/
	cwp_String mode;		/* interpolate, zero, ...	*/
	int imode=EXTRAPOLATE;
	int i;				/* counter of array a,b		*/
	int ival;			/* loop counter for arrays a,b	*/
	int nvals;			/* number of values in a,b	*/
	int nvalsc;			/* number of values in c	*/
	int index1;			/* array of indexes for key1 	*/
	int index2;			/*      ....        for key2	*/
	int index3;			/*      ....        for key3	*/
	int modval1;			/* modify key1-value		*/

	Value val1;			/* value of key1		*/
	Value val2;			/* value of key2		*/
	Value val3;			/* value of key3		*/
	double dval1;			/* value of key1		*/
	double dval2;			/* value of key2		*/
	double dval3;			/* value of key3		*/

	double *a=NULL;			/* array of "a" values		*/
	double *b=NULL;			/* array of "b" values		*/
	double *c=NULL;			/* array of "c" values		*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get parameters */
	if (!getparstring("mode", &mode)) mode = "extrapolate";
	if 	(STREQ(mode, "zero")) imode = ZERO;
	else if (STREQ(mode, "interpolate")) imode = INTERPOLATE;
	else if (STREQ(mode, "preserve")) imode = PRESERVE;
	else if (STREQ(mode, "transfer")) imode = TRANSFER;
	else if (!STREQ(mode,"extrapolate"))
		err("unknown mode=\"%s\", see self-doc", mode);
	if (!getparstring("key1", &key1)) key1 = "ep";
	if (!getparstring("key2", &key2)) key2 = "fldr";
	if (!getparstring("key3", &key3)) key3 = "tracf";

	/* get a's */
	nvals = countparval("a");
	if ( nvals==0 ) err("need array of a-values!");
	a = ealloc1double(nvals);
	getpardouble("a",a);

	/* get b's */
	if ( (i=countparval("b"))!=nvals )
			err("number of b's and a's must be equal!");
	b = ealloc1double(nvals);
	getpardouble("b",b);

	/* are b's in ascending order? */
	for ( i=1; i<nvals; ++i ) {
		if ( b[i-1] >= b[i] ) err("b's must be in ascending order!");
	}

	/* get c's */
	if ( (nvalsc=countparval("c")) > 0 ) {
		c = ealloc1double(nvalsc);
		getpardouble("c",c);
	}

        checkpars();

	/* get types and index values */
	type1  = hdtype(key1);
	type2  = hdtype(key2);
	type3  = hdtype(key3);
	index1 = getindex(key1);
	index2 = getindex(key2);
	index3 = getindex(key3);

	/* loop over traces */
	while (gettr(&tr)) {

		/* get header values */
		gethval(&tr, index1, &val1);
		gethval(&tr, index2, &val2);
		gethval(&tr, index3, &val3);
		dval1 = vtod( type1, val1);
		dval2 = vtod( type2, val2);
		dval3 = vtod( type3, val3);

		/* check what to do about key3 */
		modval1 = 1;
		if ( nvalsc > 0 ) {
			switch (imode) {
			case ZERO:
			case PRESERVE:
			case TRANSFER:
				modval1 = 0;
				for ( i=0; i<nvalsc; ++i ) {
					if ( dval3 == c[i] ) modval1 = 1;
				}
			break;
			case INTERPOLATE:
			case EXTRAPOLATE:
				if ( dval3 < c[0] ) modval1 = 0;
				if ( dval3 > c[nvalsc-1] ) modval1 = 0;
				break;

			}
		}

		/* find position of val2 in b-array */
		ival = 0;
		for ( i=0; i<nvals; ++i ) {
			if ( dval2 >= b[i] ) ival = i;
		}

		if ( dval2 == b[ival] ) {	/* val2 found in b-array */
			dval1 = a[ival];
		} else {			/* val2 not found in b-array */
			switch (imode) {
			case ZERO:
				dval1 = 0.;
			break;
			case INTERPOLATE:
				if ( dval2 >= b[0] && dval2 <= b[nvals-1] ) {
					dval1 = a[ival] + (a[ival+1]-a[ival])*
					    (dval2-b[ival])/(b[ival+1]-b[ival]);
				}
			break;
			case EXTRAPOLATE:
				if ( dval2 < b[0] ) {
					dval1 = a[0];
				} else if ( dval2 > b[nvals-1] ) {
					dval1 = a[nvals-1];
				} else {
					dval1 = a[ival] + (a[ival+1]-a[ival])*
					    (dval2-b[ival])/(b[ival+1]-b[ival]);
				}
			break;
			case TRANSFER:
				dval1 = dval2;
			break;
			case PRESERVE:
			break;
			}
		}

		if ( modval1 == 1 ) {
			assgnval(type1,&val1,dval1);
			puthval(&tr, index1, &val1);
		}
		puttr(&tr);
	}

	return(CWP_Exit());
}


void assgnval(cwp_String type1, Value *valp1, double dval1)
{
		switch (*type1) {
		case 's':
			err("can't change char header word");
		break;
		case 'h':
			valp1->h = (short) dval1;
		break;
		case 'u':
			valp1->u = (unsigned short) dval1;
		break;
		case 'l':
			valp1->l = (long) dval1;
		break;
		case 'v':
			valp1->v = (unsigned long) dval1;
		break;
		case 'i':
			valp1->i = (int) dval1;
		break;
		case 'p':
			valp1->p = (unsigned int) dval1;
		break;
		case 'f':
			valp1->f = (float) dval1;
		break;
		case 'd':
			valp1->d = (double) dval1;
		break;
		default:
			err("unknown type %s", type1);
		break;
		}
}
