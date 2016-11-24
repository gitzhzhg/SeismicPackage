/* Copyright (c) Colorado School of Mines, 2005.*/
/* All rights reserved.                       */

/* SUMATH: $Revision: 1.1 $ ; $Date: 2013/06/03 18:15:40 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUMATH - do math operation on su data 		",
" 								",
" suop <stdin >stdout op=mult					",
" 								",
" Required parameters:						",
"	none							",
"								",
" Optional parameter:						",
"	op=mult		operation flag				",
"			--------- operations -----------------	",
"			add   : o = i + a    (o=out; i=in)	",
"			sub   : o = i - a			",
"			mult  : o = i * a  			",
"			div   : o = i / a  			",
"			pow   : o = i ^ a			",
"			spow  : o = sgn(i) * abs(i) ^ a  	",
"			--------- operation parameter --------	",
"	a=1							",
"	copy=1		n>1 copy each trace n times		",
"								",
" Note:								",
" There is overlap between this program and \"sugain\" and	",
" \"suop\"								",
NULL};

/* Credits:
 *
 *	U Arkansas: Chris Liner Jun 2013
 *
 * Notes:
 */
/**************** end self doc ***********************************/


#define	ADD	1
#define	SUB	2
#define	MULT	3
#define	DIV	4
#define POW	5
#define SPOW	6

segy tr;

int
main(int argc, char **argv) {
	cwp_String op="mult";	/* operation: add, sub, ..., 		*/
	int iop=MULT;		/* integer abbrev. for op in switch	*/
	int nt;			/* number of samples on input trace	*/
	float a;
	int copy, j, tracl;


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get information from first trace */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	tracl = tr.tracl;

	/* Get operation, recall iop initialized to the default FABS */
	getparstring("op", &op);
	if 	(STREQ(op, "add"))	iop = ADD;
	else if (STREQ(op, "sub"))	iop = SUB;
	else if (STREQ(op, "div"))	iop = DIV;
	else if (STREQ(op, "pow"))	iop = POW;
	else if (STREQ(op, "spow"))	iop = SPOW;
	else if (!STREQ(op, "mult"))	
		err("unknown operation=\"%s\", see self-doc", op);
	if (!getparfloat("a", &a)) a = 1;
	if (!getparint("copy", &copy)) copy = 1;
	if (copy > 1) tracl = 1;

	/* Main loop over traces */
	do {

		switch(iop) { register int i;
		case ADD:
			for (i = 0; i < nt; ++i)
				tr.data[i] += a;
		break;
		case SUB:
			for (i = 0; i < nt; ++i)
				tr.data[i] -= a;
		break;
		case MULT:
			for (i = 0; i < nt; ++i)
				tr.data[i] *= a;
		break;
		case DIV:
			for (i = 0; i < nt; ++i)
				tr.data[i] /= a;
		break;
		case POW:
			for (i = 0; i < nt; ++i)
				tr.data[i] = pow(tr.data[i],a);
		break;
		case SPOW:
			for (i = 0; i < nt; ++i)
				tr.data[i] = SGN(tr.data[i])*pow(ABS(tr.data[i]),a);
		break;
		default:  /* defensive programming */
			err("mysterious operation=\"%s\"", op);
		} /* end scope of i */
		
		if (copy == 1) {
			puttr(&tr);
		} else {
			for (j = 1; j <= copy; ++j) {
				tr.tracl = tracl;
				puttr(&tr);
				++tracl;
			}
		}

	} while (gettr(&tr));


	return(CWP_Exit());
}
