/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUABSHW: $Revision: 1.13 $ ; $Date: 2011/11/16 22:10:29 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUABSHW - replace header key word by its absolute value	",
" 								",
" suabshw <stdin >stdout key=offset				",
" 								",
" Required parameters:						",
" 	none							",
" 								",
" Optional parameter:						",
" 	key=offset		header key word			",
" 								",
NULL};

/* Credits:
 *	CWP: Jack K. Cohen
 */
/**************** end self doc ***********************************/


#define KEY	"offset"	/* Default key word to take abs() of */

/* function prototype of function used internally */
void absval(cwp_String type, Value *valp);

segy tr;

int
main(int argc, char **argv)
{
	cwp_String key;	/* header key word from segy.h		*/
	cwp_String type;/* ... its type				*/
	int index;	/* ... its index in hdr.h		*/
	Value val;	/* ... its value			*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get key parameter */
	if (!getparstring("key", &key))	key = KEY;

        checkpars();

	type = hdtype(key);
	index = getindex(key);

	while (gettr(&tr)) {
		gethval(&tr, index, &val);
		absval(type, &val);
		puthval(&tr, index, &val);
		puttr(&tr);
	}


	return(CWP_Exit());
}


void absval(cwp_String type, Value *valp)
{
	switch (*type) {
	case 's': err("can't absval char header word"); break;
	case 'u':	/* do nothing if unsigned type */
	case 'v':
	case 'p':                                      break;
	case 'h': if (valp->h < 0) valp->h = -valp->h; break;
	case 'l': if (valp->l < 0) valp->l = -valp->l; break;
	case 'i': if (valp->i < 0) valp->i = -valp->i; break;
	case 'f': if (valp->f < 0) valp->f = -valp->f; break;
	case 'd': if (valp->d < 0) valp->d = -valp->d; break;
	default: err("unknown type %s", type);         break;
	}
}
