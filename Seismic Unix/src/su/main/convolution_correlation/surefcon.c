/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUREFCON: $Revision: 1.7 $ ; $Date: 2015/08/07 21:57:27 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"
/*********************** self documentation **************************/
char *sdoc[] = {
"									",
" SUREFCON -  Convolution of user-supplied Forward and Reverse		",
"		refraction shots using XY trace offset in reverse shot	",
"									",
"	surefcon <forshot sufile=revshot  xy=trace offseted  >stdout	",
"									",
" Required parameters:						 	",
" sufile=	file containing SU trace to use as reverse shot		",
" xy=		Number of traces offseted from the 1st trace in sufile	",
"									",
" Optional parameters:						 	",
" none								 	",
"									",
" Trace header fields accessed: ns					",
" Trace header fields modified: ns					",
"									",
" Notes:								",
" This code implements the Refraction Convolution Section (RCS)	method	",
" of generalized reciprocal refraction traveltime analysis developed by ",
" Derecke Palmer and Leoni Jones.					",
"									",
" The time sampling interval on the output traces is half of that on the",
" traces in the input files.		  	",
"									",
" Example:								",
"									",
"	 surefcon <DATA sufile=DATA xy=1 | ...				",
"									",
" Here, the su data file, \"DATA\", convolved the nth trace by		",
" (n+xy)th trace in the same file					",
"									",
"									",
NULL};

/* Credits: (based on suconv)
 *	CWP: Jack K. Cohen, Michel Dietrich
 *	UNSW: D. Palmer, K.T. LEE
 *  CAVEATS: no space-variable or time-variable capacity.
 *	The more than one trace allowed in sufile is the
 *	beginning of a hook to handle the spatially variant case.
 *
 * Trace header fields accessed: ns
 * Trace header fields modified: ns
 * Notes:
 * This code implements the refraction convolution 
 * section (RCS) method
 * method described in:
 *
 * Palmer, D, 2001a, Imaging refractors with the convolution section,
 *           Geophysics 66, 1582-1589.
 * Palmer, D, 2001b, Resolving refractor ambiguities with amplitudes,
 *           Geophysics 66, 1590-1593.
 *
 * Exploration Geophysics (2005) 36, 18­25
 * Butsuri-Tansa (Vol. 58, No.1)
 * Mulli-Tamsa (Vol. 8,
 *    A simple approach to refraction statics with the 
 * Generalized Main Reciprocal Method and the Refraction 
 * Convolution Section Heading
 *        by Derecke Palmer  Leonie Jones
 *
 */
/**************** end self doc ********************************/

segy intrace, outtrace, sutrace;

int
main(int argc, char **argv)
{
	int nt;		/* number of points on input traces	*/
	int ntout;	/* number of points on output traces	*/
	int xy;		/* the offset number for GRM		*/
	float *forshot;	/* forward shot			*/
	int nforshot;	/* length of input wavelet in samples  */
	cwp_String sufile;	/* name of file of forward SU traces	*/
	FILE *fp;		/* ... its file pointer		*/
	int itr;		/* trace counter			 */

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get info from first trace */ 
	if (!gettr(&intrace) ) err("can't get 1st reverse shot trace");
	nt = intrace.ns;

	/* Default parameters;  User-defined overrides */
	if (!getparint("xy", &xy) )  xy = 0;
		
	/* Check xy values */
	if (xy < 0)	err("xy=%d should be positive", xy);
	 
	if (!getparstring("sufile", &sufile)) {
		err("must specify sufile= desired forward shot");
	} else {
                checkpars();

		/* Get parameters and set up forshot array */
		fp = efopen(sufile, "r");
	 	for (itr = 0; itr <= xy; ++itr) {
			if (!fgettr(fp, &sutrace) ) { 
				err("can't get 1st requested forward trace");
			}
	 	}	

	 	nforshot = sutrace.ns;
	 	forshot = ealloc1float(nforshot);

		 /* Set output trace length */
		 ntout = nt + nforshot - 1;
		
		 /* Main loop over reverse shot traces */
		 do {  
			warn("rev==%d\t , for=%d", intrace.tracf, sutrace.tracf);
			 memcpy((void *) forshot,
			(const void *) sutrace.data, nforshot*FSIZE);
	
			/* Convolve for shot with reverse shot trace */
			convolve_cwp(nforshot, 0, forshot,
					 nt, 0, intrace.data, 
					ntout, 0, outtrace.data);	

			/* Output convolved trace */
			memcpy((void *) &outtrace, (const void *) &intrace, HDRBYTES);
			outtrace.ns = ntout;
			outtrace.dt = outtrace.dt/2;
			/*outtrace.cdp = 2*intrace.tracf + xy;*/ 
			fprintf(stderr,"out_cdp=%d\n", 2*intrace.tracf + xy);
			puttr(&outtrace);
		
		 } while ( gettr(&intrace) && fgettr(fp, &sutrace) ); 
	}
	
	return(CWP_Exit());
}
