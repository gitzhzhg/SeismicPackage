/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSTRIP: $Revision: 1.20 $ ; $Date: 2011/11/16 22:10:29 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUSTRIP - remove the SEGY headers from the traces		",
" 								",
" sustrip <stdin >stdout head=/dev/null outpar=/dev/tty ftn=0	",
" 								",
" Required parameters:						",
" 	none							",
" 								",
" Optional parameters:						",
" 	head=/dev/null		file to save headers in		",
" 								",
" 	outpar=/dev/tty		output parameter file, contains:",
" 				number of samples (n1=)		",
" 				number of traces (n2=)		",
" 				sample rate in seconds (d1=)	",
" 								",
" 	ftn=0			Fortran flag			",
" 				0 = write unformatted for C	",
" 				1 = ... for Fortran		",
" 								",
" Notes:							",
" Invoking head=filename will write the trace headers into filename.",
" You may paste the headers back onto the traces with supaste	",
" See:  sudoc  supaste 	 for more information 			",
" Related programs: supaste, suaddhead				",
NULL};

/* Credits:
 *	SEP: Einar Kjartansson  c. 1985
 *	CWP: Jack K. Cohen        April 1990
 *
 * Trace header fields accessed: ns, dt
 */
/**************** end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	cwp_String head;	/* name of file holding headers		*/
	FILE *headfp; 		/* ... its file pointer			*/
	cwp_String outpar;	/* name of file holding output parfile	*/
	FILE *outparfp;		/* ... its file pointer			*/
	int ns;			/* number of data samples on the segys	*/
	size_t nsbytes;		/* ... in bytes				*/
	int ftn;		/* fortran flag				*/
	int ntr = 0;		/* number of traces written		*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	switch(filestat(STDOUT)) {
	case BADFILETYPE:
		warn("stdout is illegal filetype");
		pagedoc();
	break;
	case TTY:
		warn("stdout can't be tty");
		pagedoc();
	break;
	default: /* other cases are OK */
	break;
	}

	/* Get parameters */
	if (!getparstring("head"  , &head))	head   = "/dev/null";
	if (!getparstring("outpar", &outpar))	outpar = "/dev/tty" ;
	if (!getparint   ("ftn"   , &ftn))	ftn = 0;
	if (ftn != 0 && ftn != 1)  err("ftn=%d must be 0 or 1", ftn);


        checkpars();
	/* Open files to save headers and parameters */
	headfp = efopen(head, "w");
	outparfp = efopen(outpar, "w");


	/* Get info from first trace */
	if (!gettr(&tr)) err("can't get first trace");
	ns = tr.ns;
	nsbytes = ns * FSIZE;


	/* Write the data portion of the records--if the ftn	*/
	/* option is selected, write an int before and after	*/
	/* each trace giving the length of the trace in bytes	*/
	/* as per the Fortran unformatted record format.	*/
	do {

		if (ftn) efwrite(&nsbytes, ISIZE, 1, stdout);
		switch(tr.trid) {
		case CHARPACK:  efwrite(tr.data, sizeof(char), ns, stdout);
		break;
		case SHORTPACK: efwrite(tr.data, sizeof(short), ns, stdout);
		break;
		default:        efwrite(tr.data, FSIZE, ns, stdout);
		}     
		if (ftn) efwrite(&nsbytes, ISIZE, 1, stdout);

		efwrite(&tr, 1, HDRBYTES, headfp);

		++ntr;

	} while (gettr(&tr));

	/* Make par file for headerless file */
	fprintf(outparfp, "n1=%d n2=%d d1=%f\nnt=%d ntr=%d dt=%f\nns=%d\n",
		tr.ns, ntr, ((double) tr.dt)/1000000.0,
		tr.ns, ntr, ((double) tr.dt)/1000000.0,
		tr.ns);


	return(CWP_Exit());
}
