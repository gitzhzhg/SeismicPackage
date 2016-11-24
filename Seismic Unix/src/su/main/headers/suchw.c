/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUCHW: $Revision: 1.18 $ ; $Date: 2011/11/16 22:10:29 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUCHW - Change Header Word using one or two header word fields	",
"									",
"  suchw <stdin >stdout [optional parameters]				",
"									",
" Required parameters:							",
" none									",
"									",
" Optional parameters:							",
" key1=cdp,...	output key(s) 						",
" key2=cdp,...	input key(s) 						",
" key3=cdp,...	input key(s)  						",
" a=0,...		overall shift(s)				",
" b=1,...		scale(s) on first input key(s) 			",
" c=0,...		scale on second input key(s) 			",
" d=1,...		overall scale(s)				",
" e=1,...		exponent on first input key(s)",
" f=1,...		exponent on second input key(s)",
"									",
" The value of header word key1 is computed from the values of		",
" key2 and key3 by:							",
"									",
"	val(key1) = (a + b * val(key2)^e + c * val(key3)^f) / d		",
"									",
" Examples:								",
" Shift cdp numbers by -1:						",
"	suchw <data >outdata a=-1					",
"									",
" Add 1000 to tracr value:						",
" 	suchw key1=tracr key2=tracr a=1000 <infile >outfile		",
"									",
" We set the receiver point (gx) field by summing the offset		",
" and shot point (sx) fields and then we set the cdp field by		",
" averaging the sx and gx fields (we choose to use the actual		",
" locations for the cdp fields instead of the conventional		",
" 1, 2, 3, ... enumeration):						",
"									",
"   suchw <indata key1=gx key2=offset key3=sx b=1 c=1 |			",
"   suchw key1=cdp key2=gx key3=sx b=1 c=1 d=2 >outdata			",
"									",
" Do both operations in one call:					",
"									",
" suchw<indata key1=gx,cdp key2=offset,gx key3=sx,sx b=1,1 c=1,1 d=1,2 >outdata",
"									",
NULL};

/* Credits:
 *	SEP: Einar Kjartansson
 *	CWP: Jack K. Cohen
 *      CWP: John Stockwell, 7 July 1995, added array of keys feature
 *      Delphi: Alexander Koek, 6 November 1995, changed calculation so
 *              headers of different types can be expressed in each other
 */
/**************** end self doc ***********************************/

/* prototype for function used internally */
void changeval(cwp_String type1, Value *valp1, cwp_String type2,
	       Value *valp2, cwp_String type3, Value *valp3,
		double a, double b, double c, double d, double e, double f);

segy tr;

int
main(int argc, char **argv)
{
	cwp_String key1[SU_NKEYS];	/* output key(s)		*/
	cwp_String key2[SU_NKEYS];	/* first input key(s)		*/
	cwp_String key3[SU_NKEYS];	/* second input key(s)		*/
	cwp_String type1[SU_NKEYS];	/* array of types for key1	*/
	cwp_String type2[SU_NKEYS];	/* array of types for key2	*/
	cwp_String type3[SU_NKEYS];	/* array of types for key3	*/
	int nkeys;			/* number of keys to be computed*/
	int n;				/* counter of keys getparred	*/
	int ikey;			/* loop counter of keys 	*/
	int index1[SU_NKEYS];		/* array of indexes for key1 	*/
	int index2[SU_NKEYS];		/*      ....        for key2	*/
	int index3[SU_NKEYS];		/*      ....        for key3	*/

	Value val1;			/* value of key1		*/
	Value val2;			/* value of key2		*/
	Value val3;			/* value of key3		*/

	double *a;			/* array of "a" values		*/
	double *b;			/* array of "b" values		*/
	double *c;			/* array of "c" values		*/
	double *d;			/* array of "d" values		*/
	double *e;			/* array of "e" values		*/
	double *f;			/* array of "f" values		*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get parameters */
	/* get key1's */
	if ((n=countparval("key1"))!=0){
		nkeys=n;
		getparstringarray("key1",key1);
	} else { /* set default */
		nkeys=1;
		key1[0]="cdp";	
	}

	/* get key2's */
	if ((n=countparval("key2"))!=0){
		if (n!=nkeys)
			err("number of key2's and key1's must be equal!");

		getparstringarray("key2",key2);
	} else { /* set default */
		if (nkeys!=1)
			err("number of key2's and key1's must be equal!");

		key2[0]="cdp";	
	}

	/* get key3's */
	if ((n=countparval("key3"))!=0){
		if (n!=nkeys)
			err("number of key3's and key1's must be equal!");

		getparstringarray("key3",key3);
	} else { /* set default */
		if (nkeys!=1)
			err("number of key3's and key1's must be equal!");
		key3[0]="cdp";	
	}

	/* get a's */
	if ((n=countparval("a"))!=0){
		if (n!=nkeys)
			err("number of a's and key1's must be equal!");

		a=ealloc1double(nkeys);
		getpardouble("a",a);
	} else { /* set default */
		a=ealloc1double(nkeys);
		for (ikey=0; ikey<nkeys; ++ikey)
			a[ikey]=0.;
	}

	/* get b's */
	if ((n=countparval("b"))!=0){
		if (n!=nkeys)
			err("number of b's and key1's must be equal!");

		b=ealloc1double(nkeys);
		getpardouble("b",b);
	} else { /* set default */
		b=ealloc1double(nkeys);
		for (ikey=0; ikey<nkeys; ++ikey)
			b[ikey]=1.;
	}

	/* get c's */
	if ((n=countparval("c"))!=0){
		if (n!=nkeys)
			err("number of c's and key1's must be equal!");

		c=ealloc1double(nkeys);
		getpardouble("c",c);
	} else { /* set default */
		c=ealloc1double(nkeys);
		for (ikey=0; ikey<nkeys; ++ikey)
			c[ikey]=0.;
	}

	/* get d's */
	if ((n=countparval("d"))!=0){
		if (n!=nkeys)
			err("number of d's and key1's must be equal!");

		d=ealloc1double(nkeys);
		getpardouble("d",d);
	} else { /* set default */
		d=ealloc1double(nkeys);
		for (ikey=0; ikey<nkeys; ++ikey)
			d[ikey]=1.;
	}

	/* get e's */
	if ((n=countparval("e"))!=0){
		if (n!=nkeys)
			err("number of e's and key1's must be equal!");

		e=ealloc1double(nkeys);
		getpardouble("e",e);
	} else { /* set default */
		e=ealloc1double(nkeys);
		for (ikey=0; ikey<nkeys; ++ikey)
			e[ikey]=1.;
	}

	/* get f's */
	if ((n=countparval("f"))!=0){
		if (n!=nkeys)
			err("number of f's and key1's must be equal!");

		f=ealloc1double(nkeys);
		getpardouble("f",f);
	} else { /* set default */
		f=ealloc1double(nkeys);
		for (ikey=0; ikey<nkeys; ++ikey)
			f[ikey]=1.;
	}

        checkpars();
	for (ikey=0; ikey<nkeys; ++ikey) {
			
		/* get types and index values */
		type1[ikey]  = hdtype(key1[ikey]);
		type2[ikey]  = hdtype(key2[ikey]);
		type3[ikey]  = hdtype(key3[ikey]);
		index1[ikey] = getindex(key1[ikey]);
		index2[ikey] = getindex(key2[ikey]);
		index3[ikey] = getindex(key3[ikey]);
		}

	/* loop over traces */
	while (gettr(&tr)) {

		/* loop over key fields */
		for (ikey=0; ikey<nkeys; ++ikey) {
			
			/* get header values */
			gethval(&tr, index2[ikey], &val2);
			gethval(&tr, index3[ikey], &val3);

			changeval(type1[ikey], &val1, type2[ikey], &val2,
				type3[ikey], &val3, a[ikey], b[ikey], c[ikey],
				d[ikey], e[ikey], f[ikey]);
			puthval(&tr, index1[ikey], &val1);
		}
		puttr(&tr);
	}

	return(CWP_Exit());
}


void changeval(cwp_String type1, Value *valp1, cwp_String type2,
	       Value *valp2, cwp_String type3, Value *valp3,
		double a, double b, double c, double d, double e, double f)
{
	double dval2=vtod( type2, *valp2);
	double dval3=vtod( type3, *valp3);
	double dval1=(a+b*pow(dval2,e)+c*pow(dval3,f))/d;

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
