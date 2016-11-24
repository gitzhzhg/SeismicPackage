/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUGETHW: $Revision: 1.18 $ ; $Date: 2011/11/16 22:10:29 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUGETHW - sugethw writes the values of the selected key words		",
" 									",
"   sugethw key=key1,... [output=] <infile [>outfile]			",
" 									",
" Required parameters:							",
" key=key1,...		At least one key word.				",
" 									",
" Optional parameters:							",
" output=ascii		output written as ascii for display		",
" 			=binary for output as binary floats		",
" 			=geom   ascii output for geometry setting	",
" verbose=0 		quiet						",
" 			=1 chatty					",
" 									",
" Output is written in the order of the keys on the command		",
" line for each trace in the data set.					",
" 									",
" Example:								",
" 	sugethw <stdin key=sx,gx					",
" writes sx, gx values as ascii trace by trace to the terminal.		",
" 									",
" Comment: 								",
" Users wishing to edit one or more header field (as in geometry setting)",
" may do this via the following sequence:				",
"     sugethw < sudata output=geom key=key1,key2,... > hdrfile 		",
" Now edit the ASCII file hdrfile with any editor, setting the fields	",
" appropriately. Convert hdrfile to a binary format via:		",
"     a2b < hdrfile n1=nfields > binary_file				",
" Then set the header fields via:					",
"     sushw < sudata infile=binary_file key=key1,key2,... > sudata.edited",
" 									",
NULL};

/* Credits:
 *
 *	SEP: Shuki Ronen
 *	CWP: Jack K. Cohen
 *      CWP: John Stockwell, added geom stuff, and getparstringarray
 */
/**************** end self doc ***********************************/

#define ASCII 0
#define BINARY 1
#define GEOM 2

segy tr;

int
main(int argc, char **argv)
{
	cwp_String key[SU_NKEYS];	/* array of keywords		*/
	int nkeys;		/* number of keywords to be gotten 	*/
	int iarg;		/* arguments in argv loop		*/
	int countkey=0;		/* counter of keywords in argc loop	*/
	cwp_String output;	/* string representing output format	*/
	int ioutput=ASCII;	/* integer representing output format	*/
	int verbose;		/* verbose?				*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get key values */
	if (!getparint("verbose",&verbose))	verbose=0;
	if ((nkeys=countparval("key"))!=0) {
		getparstringarray("key",key);
	} else {
		/* support old fashioned method for inputting key fields */
		/* as single arguments:  sugethw key1 key2 ... 		*/
		if (argc==1) err("must set at least one key value!");

		for (iarg = 1; iarg < argc; ++iarg) {
			cwp_String keyword;	     /* keyword */

			keyword = argv[iarg];

			if (verbose) warn("argv=%s",argv[iarg]);
			/* get array of types and indexes to be set */
			if ((strncmp(keyword,"output=",7)!=0)) {
				key[countkey] = keyword;
				++countkey;
			}

			if (countkey==0)
				err("must set at least one key value!");
		}
		nkeys=countkey;
	}

	/* Set and check output format; recall ioutput initialized to ASCII */
	if (!getparstring("output", &output))   output = "ascii";

        checkpars();

	if      (STREQ(output, "binary"))	ioutput = BINARY;
	else if (STREQ(output, "geom"))		ioutput = GEOM;
	else if (!STREQ(output, "ascii"))
		err("unknown format output=\"%s\", see self-doc", output);

	/* Loop over traces writing selected header field values */
	while (gettr(&tr)) {
		register int ikey;

		for (ikey = 0; ikey < nkeys; ++ikey) {
			Value val;
			float fval;

			gethdval(&tr, key[ikey], &val);

			switch(ioutput) {
			case ASCII:
				printf("%6s=", key[ikey]);
				printfval(hdtype(key[ikey]), val);
				putchar('\t');
			break;
			case BINARY:
				fval = vtof(hdtype(key[ikey]), val);
				efwrite((char *) &fval, FSIZE, 1, stdout);
			break;
			case GEOM:
				printfval(hdtype(key[ikey]), val);
				putchar(' ');
			break;
			}
		}

		switch(ioutput) {
		case GEOM:
			printf("\n");
		break;
		case ASCII:
			printf("\n\n");
		break;
		}
	}

	return(CWP_Exit());
}
