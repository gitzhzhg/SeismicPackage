/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUHTMATH: $Revision: 1.4 $ ; $Date: 2011/11/16 22:10:29 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUHTMATH - do unary arithmetic operation on segy traces with 	",
"	     headers values					",
" 								",
" suhtmath <stdin >stdout				",
" 								",
" Required parameters:						",
"	none							",
"								",
" Optional parameter:						",
"	key=tracl	header word to use			",
"	op=nop		operation flag				",
"			nop   : no operation			",
"			add   : add header to trace		",
"			mult  : multiply trace with header	",
"			div   : divide trace by header		",
"								",
"	scale=1.0	scalar multiplier for header value	",
"	const=0.0	additive constant for header value	",
"								",
" Operation order:						",      
"								",
" op=add:	out(t) = in(t) + (scale * key + const)		",
" op=mult:	out(t) = in(t) * (scale * key + const)		",
" op=div:	out(t) = in(t) / (scale * key + const)		",
NULL};

/* Credits:
 *	Matthias Imhof, Virginia Tech, Fri Dec 27 09:17:29 EST 2002
 */
/**************** end self doc ***********************************/


#define	NOP	0
#define	ADD	1
#define	MULT	2
#define	DIV	3

segy tr;

float getval(   cwp_String type, Value *valp)
{
	switch (*type) {
	case 's':
		err("can't get char header word");
	break;
	case 'h':
		return(valp->h);
	break;
	case 'u':
		return(valp->u);
	break;
	case 'l':
		return(valp->l);
	break;
	case 'v':
		return(valp->v);
	break;
	case 'i':
		return(valp->i);
	break;
	case 'p':
		return(valp->p);
	break;
	case 'f':
		return(valp->f);
	break;
	case 'd':
		return(valp->d);
	default:
		err("unknown type %s", type);
	break;
	}
	return(0.0);
}

int	main(int argc, char **argv)
{
	cwp_String key;		/* header key word from segy.h		*/
	cwp_String type;	/* ... its type				*/
	int indx;		/* ... its index			*/
	Value val;		/* value of key in current gather	*/

	float c, s;

	cwp_String op="nop";	/* operation: nop, add, ..., 		*/
	int iop=NOP;		/* integer abbrev. for op in switch	*/
	int nt;			/* number of samples on input trace	*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	if (!getparfloat   ("const", &c))	 	c = 0.0;
	if (!getparfloat   ("scale", &s))	 	s = 1.0;

	if (!getparstring("key", &key))		key = "tracl";
	type = hdtype(key);
	indx = getindex(key);

	/* Get operation, recall iop initialized to the default NOP */
	getparstring("op", &op);
        checkpars();

	if      (STREQ(op, "add"))	iop = ADD;
	else if (STREQ(op, "mult"))	iop = MULT;
	else if (STREQ(op, "div"))	iop = DIV;
	else	err("unknown operation=\"%s\", see self-doc", op);


	/* Get information from first trace */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;

	/* Main loop over traces */
	do {	float	f;
		
		gethval(&tr, indx, &val);
		f = s * getval(type, &val) + c;
			
		switch(iop) { register int i;
		case ADD:
			for (i = 0; i < nt; ++i)
				tr.data[i] += f;
		break;
		case MULT:
			for (i = 0; i < nt; ++i) {
				tr.data[i] *= f;
			}
		break;
		case DIV:
			for (i = 0; i < nt; ++i) {
				tr.data[i] /= f;
			}
		break;
		case NOP:
		break;
		default:  /* defensive programming */
			err("mysterious operation=\"%s\"", op);
		} /* end scope of i */
		
		puttr(&tr);

	} while (gettr(&tr));


	return(CWP_Exit());
}
