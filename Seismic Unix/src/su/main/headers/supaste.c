/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUPASTE: $Revision: 1.17 $ ; $Date: 2011/11/16 22:10:29 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUPASTE - paste existing SU headers on existing binary data	",
" 								",
" supaste <bare_data >segys  ns= head=headers ftn=0		",
"								",
" Required parameter:						",
"	ns=the number of samples per trace			",
" 								",
" Optional parameters:						",
" 	head=headers	file with segy headers			",
"	ftn=0		Fortran flag				",
"			0 = unformatted data from C		",
"			1 = ... from Fortran			",
"	verbose=0	1= echo number of traces pasted		",
" Caution:							",
"	An incorrect ns field will munge subsequent processing.	",
"								",
" Notes:							",
" This program is used when the option head=headers is used in	",
" sustrip. See:   sudoc sustrip    for more details. 		",
"								",
" Related programs:  sustrip, suaddhead				",
NULL};

/* Credits:
 *	CWP:  Jack K. Cohen, November 1990
 */
/**************** end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	cwp_String head;	/* name of file holding headers		*/
	FILE *headfp;		/* ... its file pointer			*/
	int ns;			/* number of data samples on the segys	*/
	int ftn;		/* fortran flag				*/
	int verbose;		/* flag				*/
	char junk[ISIZE];	/* to discard ftn junk  		*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get parameters */
	if (!getparint   ("ns"  , &ns))	 err("must specify ns=");
	if (!getparstring("head", &head))  head = "headers";
	if (!getparint   ("ftn"  , &ftn))  ftn  = 0;
	if (!getparint   ("verbose"  , &verbose))  verbose  = 0;
	if (ftn != 0 && ftn != 1)  err("ftn=%d must be 0 or 1", ftn);

        checkpars();


	/* Open file with saved headers */
	headfp = efopen(head, "r");


	/* Reconstruct the segys--if the ftn option is	*/
	/* selected, omit the int before and after each	*/
	/* trace giving the length of the trace in bytes*/
	/* as per the Fortran unformatted record format.*/
	while (!(feof(headfp) || ferror(headfp) || feof(stdin) || ferror(stdin))) {
		static int ntr=0; /* for user info only */

		/* Do read of header for the segy */
		if (0 >= efread(&tr, HDRBYTES, 1, headfp)) {
			if (verbose) warn("ntr=%d", ntr);
			break;	/* may or may not be successful return */
		}

		/* set output number of traces */
		tr.ns = ns;

		/* If Fortran data, read past the record size bytes */
		if (ftn) efread(junk, ISIZE, 1, stdin);


		/* Do read of data for the segy */
		switch (efread(tr.data, FSIZE, ns, stdin)) {
		case 0: /* oops, no data for this header */
			warn("header without data for trace #%d", ntr+1);
			return EXIT_FAILURE;
		default:
			puttr(&tr);
			++ntr;
		}

		/* If Fortran data, read past the record size bytes */
		if (ftn) efread(junk, ISIZE, 1, stdin);
	}


	return(CWP_Exit());
}
