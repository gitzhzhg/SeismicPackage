/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SETBHED: $Revision: 1.8 $ ; $Date: 2011/11/16 17:43:20 $	*/

#include <stdint.h>
#include "su.h"
#include "segy.h"
#include "bheader.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SETBHED - SET the fields in a SEGY Binary tape HEaDer file, as would be",
" 	    produced by segyread and segyhdrs				",
" 									",
" setbhed par= [optional parameters]					",
" 									",
" Required parameter:							",
" 	none								",
" Optional parameters:							",
"	bfile=binary		output binary tape header file		",
"	par=			=parfile				",
" Set field by field, if desired:					",
" 	jobid=			job id field				",
" 	lino=			line number (only one line per reel)	",
" 	reno=			reel number				",
" 	format=			data format				",
" ... etc....								",
" To set any binary header field, use sukeyword to find out		",
" the appropriate keyword, then use the getpar form:			",
" 	keyword=value	to set keyword to value				",
" Notes:								",
" As with all other programs in the CWP/SU package that use getpars, 	",
" (GET PARameters from the command line) a file filled with such	",
" statments may be included via option par=parfile. In particular, a	",
" parfile created by   \"bhedtopar\"  may be used as input for the program",
" \"setbhed\".								",
" 									",
" The binary header file that results from running segyread may have the",
" wrong byte order. You will need to use \"swapbhed\" to change the byte,"
" order before applying this program. 					",
" 									",
" Example:								",
"   segyread tape=yourdata.segy bfile=yourdata.b > yourdata.su		",
" If  									",
"   bhedtopar < yourdata.b | more 					",
" shows impossible values, then apply 					",
"   swapbhed < yourdata.b > swapped.b					",
" then apply 								",
"   bhedtopar < swapped.b | more 					",
"   bhedtopar < swapped.b outpar=parfile				",
" hand edit parfile, and then apply 					",
"  setbhed par=parfile bfile=swapped.b > new.b				",
" then apply 								",
"   segywrite tape=fixeddata.segy bfile=new.b < yourdata.su		",
" 									",
" Caveat: This program breaks if a \"short\" isn't 2 bytes since	",
"         the SEG-Y standard demands a 2 byte integer for ns.		",
NULL};

/* Credits:
 *
 *	CWP: John Stockwell  11 Nov 1994
 */
/**************** end self doc ***********************************/

segy tr;
bhed bh;

int
main(int argc, char **argv)
{
	char *bfile;		/* name of binary header file		*/
	FILE *binaryfp;		/* file pointer for bfile		*/

	/* bhed fields the user might wish to set, taken right out of segy.h */
	int32_t jobid;	/* job identification number */
	int32_t lino;	/* line number (only one line per reel) */
	int32_t reno;	/* reel number */
	int32_t ntrpr;	/* number of data traces per record */
        int32_t nart;	/* number of auxiliary traces per record */
	int32_t hdt;	/* sample interval in microsecs for this reel */
	int32_t dto;	/* same for original field recording */

	/* hns replaces ns in original version of setbhed */
	int32_t hns;	/* number of samples per trace for this reel */

	int32_t nso;	/* same for original field recording */
	int32_t format;	/* data sample format code:
				1 = floating point (4 bytes)
				2 = fixed point (4 bytes)
				3 = fixed point (2 bytes)
				4 = fixed point w/gain code (4 bytes) */
	int32_t fold;	/* CDP fold expected per CDP ensemble */
	int32_t tsort;	/* trace sorting code: 
				1 = as recorded (no sorting)
				2 = CDP ensemble
				3 = single fold continuous profile
				4 = horizontally stacked */
	int32_t vscode;	/* vertical sum code:
				1 = no sum
				2 = two sum ...
				N = N sum (N = 32,767) */
	int32_t hsfs;	/* sweep frequency at start */
	int32_t hsfe;	/* sweep frequency at end */
	int32_t hslen;	/* sweep length (ms) */
	int32_t hstyp;	/* sweep type code:
				1 = linear
				2 = parabolic
				3 = exponential
				4 = other */
	int32_t schn;	/* trace number of sweep channel */
	int32_t hstas;	/* sweep trace taper length at start if
			   tapered (the taper starts at zero time
			   and is effective for this length) */
	int32_t hstae;	/* sweep trace taper length at end (the ending
			   taper starts at sweep length minus the taper
			   length at end) */
	int32_t htatyp;	/* sweep trace taper type code:
				1 = linear
				2 = cos-squared
				3 = other */
	int32_t hcorr;	/* correlated data traces code:
				1 = no
				2 = yes */
	int32_t bgrcv;	/* binary gain recovered code:
				1 = yes
				2 = no */
	int32_t rcvm;	/* amplitude recovery method code:
				1 = none
				2 = spherical divergence
				3 = AGC
				4 = other */
	int32_t mfeet;	/* measurement system code:
				1 = meters
				2 = feet */
	int32_t polyt;	/* impulse signal polarity code:
				1 = increase in pressure or upward
				    geophone case movement gives
				    negative number on tape
				2 = increase in pressure or upward
				    geophone case movement gives
				    positive number on tape */
	int32_t vpol;	/* vibratory polarity code:
				code	seismic signal lags pilot by
				1	337.5 to  22.5 degrees
				2	 22.5 to  67.5 degrees
				3	 67.5 to 112.5 degrees
				4	112.5 to 157.5 degrees
				5	157.5 to 202.5 degrees
				6	202.5 to 247.5 degrees
				7	247.5 to 292.5 degrees
				8	293.5 to 337.5 degrees */


	/* Initialize */
	initargs(argc, argv);
	requestdoc(0); /* stdin not used */

	
	/* Get parameters */
	if (!getparstring("bfile", &bfile))	bfile = "binary";

	/* Optional settings; user has acess to all named fields of bhed */
	if (!getparint("jobid", &jobid))	jobid = 0;
	if (!getparint("lino", &lino))		lino = 0;
	if (!getparint("reno", &reno))		reno = 0;
	if (!getparint("ntrpr", &ntrpr))	ntrpr = 0;
	if (!getparint("hdt", &hdt))		hdt = 0;
	if (!getparint("nart", &nart))		nart = 0;
	if (!getparint("dto", &dto))		dto = 0;
	if (!getparint("hns", &hns))		hns = 0;
	if (!getparint("nso", &nso))		nso = 0;
	if (!getparint("format", &format))	format = 0;
	if (!getparint("fold", &fold))		fold = 0;
	if (!getparint("tsort", &tsort))	tsort = 0;
	if (!getparint("vscode", &vscode))	vscode = 0;
	if (!getparint("hsfs", &hsfs))		hsfs = 0;
	if (!getparint("hsfe", &hsfe))		hsfe = 0;
	if (!getparint("hslen", &hslen))	hslen = 0;
	if (!getparint("hstyp", &hstyp))	hstyp = 0;
	if (!getparint("schn", &schn))		schn = 0;
	if (!getparint("hstas", &hstas))	hstas = 0;
	if (!getparint("hstae", &hstae))	hstae = 0;
	if (!getparint("htatyp", &htatyp))	htatyp = 0;
	if (!getparint("hcorr", &hcorr))	hcorr = 0;
	if (!getparint("bgrcv", &bgrcv))	bgrcv = 0;
	if (!getparint("rcvm", &rcvm))		rcvm = 0;
	if (!getparint("mfeet", &mfeet))	mfeet = 0;
	if (!getparint("polyt", &polyt))	polyt = 0;
	if (!getparint("vpol", &vpol))		vpol = 0;
        checkpars();

	/* Open file for writing */
	binaryfp = efopen(bfile, "w");


	/* Create binary header; set all named fields */
	memset((void *) &bh, 0, BNYBYTES);
	bh.jobid = (int32_t) jobid;
	bh.lino = (int32_t) lino;
	bh.reno = (int32_t) reno;
	bh.ntrpr = (short) ntrpr;
	bh.nart = (short) nart;
	bh.hdt = (short) hdt;
	bh.dto = (short) dto;
	bh.hns = (short) hns;
	bh.nso = (short) nso;
	bh.format = (short) format;
	bh.fold = (short) fold;
	bh.tsort = (short) tsort;
	bh.vscode = (short) vscode;
	bh.hsfs = (short) hsfs;
	bh.hsfe = (short) hsfe;
	bh.hslen = (short) hslen;
	bh.hstyp = (short) hstyp;
	bh.schn = (short) schn;
	bh.hstas = (short) hstas;
	bh.hstae = (short) hstae;
	bh.htatyp = (short) htatyp;
	bh.hcorr = (short) hcorr;
	bh.bgrcv = (short) bgrcv;
	bh.rcvm = (short) rcvm;
	bh.mfeet = (short) mfeet;
	bh.polyt = (short) polyt;
	bh.vpol = (short) vpol;

	/* Write binary header from bh structure to designated file */
	efwrite( (char *) &bh, 1, BNYBYTES, binaryfp);


	/* Clean up */
	efclose(binaryfp);

	return(CWP_Exit());
}
