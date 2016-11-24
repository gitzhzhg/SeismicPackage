/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSPLIT */

#include "su.h"
#include "segy.h"
#include "header.h"
#include "bheader.h"
#include "tapebhdr.h"


/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUSPLIT - Split traces into different output files by keyword value	",
"									",
"     susplit <stdin >stdout [options]					",
"									",
" Required Parameters:							",
"	none								",
"									",
" Optional Parameters:							",
"	key=cdp		Key header word to split on (see segy.h)	",
"	stem=split_	Stem name for output files			",
"	middle=key	middle of name of output files			",
"	suffix=.su	Suffix for output files				",
"	numlength=7	Length of numeric part of filename		",
"	verbose=0	=1 to echo filenames, etc.			",
"	close=1		=1 to close files before opening new ones	",
"									",
" Notes:								",
" The most efficient way to use this program is to presort the input data",
" into common keyword gathers, prior to using susplit.			"
"									",
" Use \"suputgthr\" to put SU data into SU data directory format.	",
NULL};

/* Credits:
 *	Geocon: Garry Perratt hacked together from various other codes
 * 
 */
/**************** end self doc *******************************************/


segy tr;

int
main(int argc, char **argv)
{
	cwp_String key;		/* header key word from segy.h		*/
	int ns;			/* ns as an  int			*/
	int numlength;		/* length of split key number format	*/
	char format[BUFSIZ];	/* output filename format		*/

	int index;		/* index of key				*/
	Value val;		/* value of key				*/
	int val_i=0;		/* key value as integer 		*/
	int lastval_i=0;	/* last key value as integer	 	*/

	cwp_String type;	/* key's type				*/
	char filename[BUFSIZ];	/* output file name			*/

	cwp_String stem;	/* output file stem			*/
	cwp_String middle;	/* output file middle			*/
	cwp_String suffix;	/* output file suffix			*/
	FILE *outfp=NULL;	/* pointer to output file		*/

	int init;		/* initialisation flag for first efopen	*/
	int verbose;		/* =1 to echo filenames, etc.		*/
	int close;	/* =1 to close files before opening a new one	*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);
	init=0;

	/* Default parameters;  User-defined overrides */
	if (!getparstring("key", &key))	 key="cdp";
	if (!getparstring("stem", &stem)) stem="split_";
	if (!getparstring("middle", &middle)) middle=key;
	if (!getparstring("suffix", &suffix)) suffix=".su";
	if (!getparint("numlength", &numlength)) numlength=7;
	if (!getparint("verbose",&verbose)) verbose=0;
	if (!getparint("close",&close)) close=1;
        checkpars();

	/* Evaluate time bounds from getpars and first header */
	if (!gettr(&tr)) err("can't get first trace");
	ns = tr.ns;

	type = hdtype(key);
	index = getindex(key);

	if (verbose) warn("key is %s",key);

	/* Main loop over traces */
	do {
		gethval(&tr, index, &val);
		val_i = vtoi(type, val);
		
		if (val_i!=lastval_i || init==0) {
			if (init==0) init=1;
			else if (close) efclose(outfp);
			(void)sprintf(format, "%%s%%s%%0%dd%%s",numlength);
			(void)sprintf(filename, format, stem, middle, val_i, suffix);
			if (verbose) warn("output file is %s",filename);
			outfp = efopen(filename, "ab");
		}
		fputtr(outfp,&tr);
		lastval_i = val_i;

	} while (gettr(&tr));

	return(CWP_Exit());
}
