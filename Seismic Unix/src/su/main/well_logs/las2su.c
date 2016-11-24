/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* LAS2SU: $Revision: 1.9 $ ; $Date: 2011/11/16 17:43:20 $	*/

#include "par.h"
#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"								",
" LAS2SU - convert las2 format well log curves to su traces	",
"								",
"  las2su <stdin nskip= ncurve= >stdout [optional params]	",
"								",
" Required parameters:						",
" none								",
" Optional parameters:						",
" ncurve=automatic	number of log curves in the las file	",
" dz=0.5		input depth sampling (ft)		",
" m=0			output (d1,f1) in feet			",
"			=1 output (d1,f1) in meters		",
" ss=0			do not subsample (unless nz > 32767 )	",
"			=1 pass every other sample		",
" verbose=0		=1 to echo las header lines to screen	",
" outhdr=las_header.asc	name of file for las headers		",
"								",
" Notes:							",
" 1. It is recommended to run LAS_CERTIFY available from CWLS	",
"    at http://cwls.org.					",
" 2. First log curve MUST BE depth.				",
" 3. If number of depth levels > 32767 (segy NT limit)		",
"    then input is subsampled by factor of 2 or 4 as needed	",
" 4. Logs may be isolated using tracl header word (1,2,...,ncurve) ",
"    tracl=1 is depth						",
"								",
" If the input LAS file contains sonic data as delta t or interval",
" transit time and you plan to use these data for generating a ",
" reflection coefficient time series in suwellrf, convert the sonic",
" trace to velocity using suop with op=s2v (sonic to velocity) ",
" before input of the velocity trace to suwellrf.		", 
"								",
" Caveat:							", 
" No trap is made for the commonly used null value in LAS files ",
" (-999.25). The null value will be output as ?999.25, which	",
" really messes up a suxwigb display of density data because the",
" ?999.25 skews the more or less 2.5 values of density.		",
" The user needs to edit out null values (-999.25) before running",
" other programs, such as \"suwellrf\".				",
"								",
NULL};

/* Credits:
  *	Chris Liner based on code by Ferhan Ahmed and a2b.c (June 2005)
  *            (Based on code by Ferhan Ahmed and a2b.c)
  *            I gratefully acknowledge Saudi Aramco for permission
  *            to release this code developed while I worked for the
  *            EXPEC-ARC research division.
  *	CWP: John Stockwell 31 Oct 2006, combining lasstrip and
  *	CENPET: lasstrip 2006 by Werner Heigl
  *
  *     Rob Hardy: allow the ncurve parameter to work correctly if set
  *    - change string length to 400 characters to allow more curves
  *    - note nskip in header is totally ignored !
  *
  * Ideas for improvement:
  *	add option to chop off part of logs (often shallow
  *	   portions are not of interest
  *	cross check sampling interval from header against
  *	   values found from first log curve (=depth)
  *
  */

/**************** end self doc ***********************************/

/* defined quantities */
#define LAS_MAXLINE 1000
#define LAS_HDR_DEF "las_header.asc"

/* function prototype for subroutine used internally */
int las_getnewline(char line[], int maxline);

segy tr;	/* output trace structure */

int
main(int argc, char **argv)
{
	int ncurve=0;		/* number of log curves in las file */
	int discard=1;		/* count to discard */
	float **x=NULL;		/* binary floats */
	float len=0;		/* line length			*/
	char string[300];	/* one line of input ascii file	*/
	char c1[30];		/* ascii form of log value */
	int m;			/* flag for metric (d1,f1) output     */
	int nzmax;	 	/* max number of depth levels	*/
	int nz,nzold;	 	/* actual number of depth levels */
	int iz,izz,icurve,i;	/* counters */
	int  verbose;	/* if 1(yes) echo las header lines to stderr */
	float dz;	/* depth sample rate */
	int ss;		/* subsample flag */
	char line[LAS_MAXLINE];
	 char *outhdr=NULL;	/* name of file holding LAS header */
	 FILE *outhdrfp=NULL;    /* ... its file pointer */

	/* booleans for curve portion */
	cwp_Bool is_ncurve_set=cwp_false;	/* is ncurve set manually?*/
	cwp_Bool in_tilde_C_block=cwp_false;	/* are we inside the ~C block?*/


	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* get parameters */
	if (!getparint("verbose", &verbose))  verbose = 0;
	if (!getparint("m", &m)) 	m = 0;
	if (!getparint("ss", &ss)) 	ss = 0;
	if (!getparfloat("dz", &dz)) 	dz = 0.5;
	if (!getparstring("outhdr", &outhdr))  outhdr = LAS_HDR_DEF;
	outhdrfp = efopen(outhdr, "w");

	/* WMH: read LAS header, extract ncurve, and copy to outhdr */
	/* hardyr: flip the booleans here */
	if (!getparint("ncurve",&ncurve)) {
		is_ncurve_set=cwp_true;
		ncurve = 0;
	} else {
		is_ncurve_set=cwp_false;
	}

        checkpars();

	do {
		len = las_getnewline(line,LAS_MAXLINE);
		i = 0;
				while (line[i]!='\0') {
					 fputc(line[i],outhdrfp);
					 ++i;
		}
				fputc('\n',outhdrfp);

		/* have we reached the curve section yet? */
		if (strncmp("~C",line,2)==0) {
			in_tilde_C_block=cwp_true;
		} else if(strncmp("~P",line,2)==0)  {
			in_tilde_C_block=cwp_false;
		}

		/* count discards */
		/* hardyr: if ncurve not set */
		if ((strncmp("#",line,1)==0) && (in_tilde_C_block) ) ++discard;

		/* count lines */
		/* hardyr: if ncurve not set */
		if ((in_tilde_C_block) && (is_ncurve_set) ) ++ncurve;

	/* are we through the curve section? */
	} while (strncmp("~A",line,2)!=0 );

	/* discard # and ~ line counts */
		/* hardyr: if ncurve not set */
	if ((is_ncurve_set)) ncurve = ncurve - discard;

	/* close output file */
	efclose(outhdrfp);

	/* max depth levels: 32767 (segy limit) */
	nzmax = SU_NFLTS;

	if (verbose) warn("ncurve %d nzmax %d",ncurve,nzmax);

	/* alloc array to hold float log values */
	x = ealloc2float(nzmax,ncurve);

	/* zero array */
	memset((void *) x[0], 0, ncurve*nzmax*FSIZE);

	/* initialize depth counter */
	nz = 0;
	iz = 0;

	/* get each line as a string */
	/* hardyr change line length max to 400 characters */
	while(fgets(string,400,stdin) != NULL) {

		/* read first token */
		strcpy(c1,strtok(string,"    \n\t"));

		/* load log value into float array */
		x[0][iz] = atof(c1);

		for (icurve = 1; icurve < ncurve; ++icurve) {
			/* read next token get ascii log value */
			strcpy(c1,strtok(NULL,"    \n\t"));

			/* load this log value into float array */
			x[icurve][iz] = atof(c1);
		}

		/* bump depth counter */
		++iz;

	}

	/* number of depth values in log */
	nz = iz;
	warn("nz=%i",nz);

	/* check that nz limit is not exceeded, or subsampling requested */
	if ( nz > nzmax || ss == 1 ) {
		nzold = nz;
		/* reset number of depth samples and sample rate */
		nz = nz/2;
		dz = 2.0 * dz;
		/* subsample */
		for (icurve=0 ; icurve < ncurve ; ++icurve) {
			for (iz = 0 ; iz < nz ; ++iz){
				izz = 2*iz;
				x[icurve][iz] = x[icurve][izz];
			}
		}
		if (verbose) warn("New: nz=%i dz=%g ft\n",nz,dz);
	}

	/* check again (possible deep well with 0.25 ft sampling) */
	if ( nz > nzmax || ss == 1 ) {
		nzold = nz;
		/* reset number of depth samples and sample rate */
		nz = nz/2;
		dz = 2.0 * dz;
		/* subsample */
		for (icurve=0 ; icurve < ncurve ; ++icurve){
			for (iz = 0 ; iz < nz ; ++iz){
				izz = 2*iz;
				x[icurve][iz] = x[icurve][izz];
			}
		}
		if (verbose) warn("New: nz=%i dz=%g ft\n",nz,dz);
	}

	/* set up output trace headers */
	tr.trid = 1;		/* su time traces (trick) */
	tr.ns = nz;		/* samples per trace */
	tr.dt = 1000*dz;	/* time sample rate (trick) */
	if (m == 0) {
		tr.d1 = dz;		/* actual dz (in ft) */
		tr.f1 = x[0][0];	/* first depth value (in ft) */
	} else {
		tr.d1 = dz/3.28084;		/* actual dz (in m) */
		tr.f1 = x[0][0]/3.28084;	/* first depth value (in m) */
	}

	for (icurve=0 ; icurve < ncurve ; ++icurve){
		tr.tracl = icurve+1;

		for (iz = 0 ; iz < nz ; ++iz){
			tr.data[iz] = x[icurve][iz];
		}

		puttr(&tr);
	}


	return EXIT_SUCCESS;
}

int las_getnewline(char s[], int lim)
/***************************************************************************
las_getnewline: read a line from stdin into s[] and return length of line 
****************************************************************************
Input:
s[]	input string
lim	maximum length of line
Returns:
i	length of line 
****************************************************************************
Author: CENPET: Werner Heigl  2005
****************************************************************************/
{
	 int c=0,i;

	 for (i=0; i<lim-1 && (c=getchar())!=EOF && c!='\n'; ++i)
		 s[i] = c;
	 if (c=='\n') {
		 s[i] = c;
		 ++i;
	 }
	 s[i] = '\0';
	 return i;
}
