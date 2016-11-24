/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* BHEDTOPAR: $Revision: 1.11 $ ; $Date: 2011/11/16 22:10:29 $	*/
#include "su_xdr.h"
#include "bhdr.h"
#include "bheader.h"
#include "tapesegy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" BHEDTOPAR - convert a Binary tape HEaDer file to PAR file format	",
" 									",
"     bhedtopar < stdin outpar=parfile					",
" 									",
" Required parameter:							",
" 	none								",
" Optional parameters:							",
"	swap=0 			=1 to swap bytes			",
" 	outpar=/dev/tty		=parfile  name of output param file	",
" 									",
" Notes: 								",
" This program dumps the contents of a SEGY binary tape header file, as ",
" would be produced by segyread and segyhdrs to a file in \"parfile\" format.",
" A \"parfile\" is an ASCII file containing entries of the form param=value.",
" Here \"param\" is the keyword for the binary tape header field and	",
" \"value\" is the value of that field. The parfile may be edited as	",
" any ASCII file. The edited parfile may then be made into a new binary tape ",
" header file via the program    setbhed.				",
" 									",
" See    sudoc  setbhed   for examples					",
" 									",
NULL};

/* Credits:
 *
 *	CWP: John Stockwell  11 Nov 1994
 */
/**************** end self doc ***********************************/
/*
 * Revised:  7/5/95  Stewart A. Levin  (Mobil)
 *           Use new xdr routines for portability.
 */

/* prototypes of functions used internally */
static char *getbhkey(const int index);
static void fprintfparval(FILE *stream, cwp_String key,
				cwp_String type, Value val);

static bhed bh;		/* binary header read from file */

int main(int argc, char **argv)
{

	/* Declarations */
	int index; 		/* counter				*/
	int swap; 		/* swap bytes				*/
	char *outpar;		/* name of outpar			*/
	FILE *outparfp;		/*  ... its file pointer		*/
	FILE *infp=stdin;	/* input file pointer			*/
#ifdef SUXDR
	XDR  in_xdr;		/* for bhed conversion			*/
#endif

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);
	
	/* Get outpar */
	if (!getparstring("outpar", &outpar))  outpar = "/dev/tty" ;
	if (!getparint("swap", &swap))  swap = 0 ;
        checkpars();
	/* Open outpar for writing or parfile for reading   */
	outparfp=efopen(outpar,"w");

	/* read binary file from stdin */
#ifdef SUXDR
	xdrstdio_create(&in_xdr,stdin, XDR_DECODE);
	xdrbhdrsub(&in_xdr, &bh);
	xdr_destroy(&in_xdr);
#else
	efread((char *) &bh, 1, BNYBYTES, infp);
#endif

	/* convert binary header, field by field */
	for (index = 0; index < BHED_NKEYS; ++index) {
		Value val;
		char *key = getbhkey(index);

		/* if byte swapping is desired */
		if (swap) swapbhval(&bh,index);

		/* get values from binary header */
		getbhval(&bh,index,&val);


		/* print out in ascii */
		fprintfparval(outparfp, key, bhdr[index].type, val);

       	}

	efclose(outparfp);
                
	return(CWP_Exit());
}

static char *getbhkey(const int index)
/***************************************************************************
getbhkey - get Binary Header KEY
****************************************************************************
Notes:
****************************************************************************
Author: CWP: John Stockwell, 11 Nov 1994
***************************************************************************/
{
        return (index < BHED_NKEYS && index >= 0) ? bhdr[index].key : NULL;
}

static void fprintfparval(FILE *stream, cwp_String key,
				cwp_String type, Value val)
/**************************************************************************
fprintfparval - use fprintf to print the value of  Value in parfile format 
****************************************************************************
Notes:
****************************************************************************
Author: CWP: John Stockwell, 11 Nov 1994
***************************************************************************/
{

	switch(*type) {
	case 's':
		(void) fprintf(stream, "%s=%s\n",key,val.s);
	break;
	case 'h':
		(void) fprintf(stream, "%s=%d\n",key,val.h);
	break;
	case 'u':
		(void) fprintf(stream, "%s=%d\n",key,val.u);
	break;
	case 'i':
		(void) fprintf(stream, "%s=%d\n",key,val.i);
	break;
	case 'p':
		(void) fprintf(stream, "%s=%d\n",key,val.p);
	break;
	case 'l':
		(void) fprintf(stream, "%s=%ld\n",key,val.l);
	break;
	case 'v':
		(void) fprintf(stream, "%s=%ld\n",key,val.v);
	break;
	case 'f':
		(void) fprintf(stream, "%s=%f\n",key,val.f);
	break;
	case 'd':
		(void) fprintf(stream, "%s=%f\n",key,val.d);
	break;
	case 'U':
		(void) fprintf(stream, "%s=%d\n",key,val.U);
	break;
	case 'P':
		(void) fprintf(stream, "%s=%d\n",key,val.P);
	break;
	default:
		err("fprintfparval: unknown type %s", type);
	}

	return;
}
