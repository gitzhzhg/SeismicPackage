/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSTATIC: $Revision: 1.26 $ ; $Date: 2011/11/16 23:16:23 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUSTATIC - Elevation static corrections, apply corrections from	",
"	      headers or from a source and receiver statics file	",
"									",
"     sustatic <stdin >stdout  [optional parameters]	 		",
"									",
" Required parameters:							",
"	none								",
" Optional Parameters:							",
"	v0=v1 or user-defined	or from header, weathering velocity	",
"	v1=user-defined		or from header, subweathering velocity	",
"	hdrs=0			=1 to read statics from headers		",
" 				=2 to read statics from files		",
"				=3 to read from output files of suresstat",
"	sign=1			apply static correction (add tstat values)",
"				=-1 apply negative of tstat values	",
" Options when hdrs=2 and hdrs=3:					",
"	sou_file=		input file for source statics (ms) 	",
"	rec_file=		input file for receiver statics (ms) 	",
"	ns=240 			number of souces 			",
"	nr=335 			number of receivers 			",
"	no=96 			number of offsets			",
"									",
" Notes:								",
" For hdrs=1, statics calculation is not performed, statics correction  ",
" is applied to the data by reading statics (in ms) from the header.	",
"									",
" For hdrs=0, field statics are calculated, and				",
" 	input field sut is assumed measured in ms.			",
" 	output field sstat = 10^scalel*(sdel - selev + sdepth)/swevel	",
" 	output field gstat = sstat - sut/1000.				",
" 	output field tstat = sstat + gstat + 10^scalel*(selev - gelev)/wevel",
"									",
" For hdrs=2, statics are surface consistently obtained from the 	",
" statics files. The geometry should be regular.			",
" The source- and receiver-statics files should be unformated C binary 	",
" floats and contain the statics (in ms) as a function of surface location.",
"									",
" For hdrs=3, statics are read from the output files of suresstat, with ",
" the same options as hdrs=2 (but use no=max traces per shot and assume ",
" that ns=max shot number and nr=max receiver number).			",
" For each shot number (trace header fldr) and each receiver number     ",
" (trace header tracf) the program will look up the appropriate static  ",
" correction.  The geometry need not be regular as each trace is treated",
" independently.							",
"									",
" Caveat:  The static shifts are computed with the assumption that the  ",
" desired datum is sea level (elevation=0). You may need to shift the	",
" selev and gelev header values via  suchw.				",
" Example: subtracting min(selev,gelev)=25094431			",
"									",
" suchw < CR290.su key1=selev,gelev key2=selev,gelev key3=selev,gelev \\ ",
"            a=-25094431,-25094431 b=1,1 c=0,0 > CR290datum.su		",
NULL};

/* Credits:
 *	CWP: Jamie Burns
 *
 *	CWP: Modified by Mohammed Alfaraj, 11/10/1992, for reading
 *	     statics from headers and including sign (+-) option
 *
 *      CWP: Modified by Timo Tjan, 29 June 1995, to include input of
 *           source and receiver statics from files. 
 *
 *	modified by Thomas Pratt, USGS, Feb, 2000 to read statics from
 * 	     the output files of suresstat
 *
 * Trace header fields accessed:  ns, dt, delrt, gelev, selev,
 *	sdepth, gdel, sdel, swevel, sut, scalel, fldr, tracf
 * Trace header fields modified:  sstat, gstat, tstat
 */

/************************ end self doc ***********************************/


segy intrace, outtrace;

int
main(int argc, char **argv)
{
	int nt;		/* number of samples on output trace	*/
	float dt;	/* sample rate on outpu trace		*/
	int itime;	/* counter          			*/
	float tmin;	/* first time sample on output trace	*/
	float tsd=0.0;	/* time to move source to datum         */
	float trd=0.0;	/* time to move 0 offset receiver       */
	float v0;	/* weathering velocity			*/
	float v1;	/* subweathering velocity		*/
	int hdrs; 	/* flag to read statics from headers	*/ 
	float *t;	/* array of output times		*/
	float tstat;	/* total (source and receiver) statics	*/
	int sign;	/* to add (+) or subtract (-) statics	*/
	int no;		/* number of offsets per shot 		*/
	int io;		/* offset counter 			*/
	int is;		/* source counter 			*/
	int ir;		/* receiver counter 			*/
	int ns;		/* number of sources = number of source statics */
	int nr;		/* number of receiver = number of rec. statics	*/
	float *sou_statics=NULL;	/* array of source statics	*/
	float *rec_statics=NULL;	/* array of receiver statics	*/
	FILE *fps, *fpr;	/* file pointers for statics input 	*/
	cwp_String sou_file, rec_file; /* statics filenames 		*/

	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* Get information from first trace */
	if (!gettr(&intrace)) err("can't get first trace");
	nt   = intrace.ns;
	tmin = intrace.delrt/1000.0;
	dt   = ((double) intrace.dt)/1000000.0;
	
	/* Get parameters */
	if (!getparfloat("v1", &v1))          v1 = (float) intrace.swevel;
	if (!getparfloat("v0", &v0))
                v0 = (float) ((intrace.wevel) ? intrace.wevel : v1);
	if (!getparint("hdrs", &hdrs))        hdrs = 0;
	if (!getparint("sign", &sign))        sign = 1;

	/* Allocate vector of output times */
	t = ealloc1float(nt);

	/* reading source and receiver statics from files */
	if ((hdrs == 2) || (hdrs == 3)){

		/* getpar statics file related parameters */
		if (!getparint("ns", &ns))        ns = 240;
		if (!getparint("nr", &nr))        nr = 335;
		if (!getparint("no", &no))        no = 96;

		/* getpar statics file names */
        	getparstring("sou_file",&sou_file);
        	getparstring("rec_file",&rec_file);
                checkpars();

		/* allocate space */
		rec_statics = alloc1float(nr);
        	sou_statics = alloc1float(ns);

		/* open and read from receiver statics file */
        	if((fpr=efopen(rec_file,"rb"))==NULL)
                	err("cannot open stat_file=%s\n",rec_file);
        	efread(rec_statics, sizeof(float),nr,fpr);
        	efclose(fpr);

		/* open and read from source statics file */
        	if((fps=efopen(sou_file,"rb"))==NULL)
                	err("cannot open stat_file=%s\n",sou_file);
        	efread(sou_statics, sizeof(float),ns,fps);
        	efclose(fps);
	}


	/* Loop on traces */	
	io = 0; is = 0;
	do {
		int temp = SGN(intrace.scalel)*log10(abs((int) intrace.scalel));
		float scale;
                scale = pow(10., (float)temp);
		
		/* copy and adjust header */
		memcpy( (void *) &outtrace, (const void *) &intrace, HDRBYTES);
	
		/* compute static correction if necessary */
		if(!hdrs) {
		    	tsd = scale *
			(-intrace.selev + intrace.sdel + intrace.sdepth)/v1;
			trd = tsd - intrace.sut/1000.0;
			tstat = tsd + trd +
				scale * (intrace.selev - intrace.gelev)/v0;

		/* else, read statics from headers */
		} else { 

			if (hdrs == 2) {
				ir = is + io;
				if (is <= ns) tsd = sou_statics[is]/1000;
				if (ir > 0 && ir <= nr)
					trd = rec_statics[ir]/1000;

				intrace.tstat = tsd + trd;
				io ++;
				if (io > no-1) {
					io = 0; is++;
				}
			}
			if (hdrs == 3) {
				ir = is + io;
				tsd = sou_statics[intrace.fldr];
				trd = rec_statics[intrace.tracf];

				intrace.tstat = tsd + trd;
				io ++;
				if (io > no-1) {
					io = 0; is++;
				}
			}
			

			/* if total statics not supplied, calculate it */
			if(intrace.tstat==0) {
				outtrace.tstat = intrace.sstat+intrace.gstat;
				tstat = outtrace.tstat/1000.0;
			} else
				tstat = intrace.tstat/1000.0;
		}
		
		/* Compute output times */
		for (itime=0; itime<nt; ++itime)
			t[itime] = tmin + itime*dt + sign*tstat;

		/* sinc interpolate new data */
		ints8r(nt, dt, tmin, intrace.data, 
				0.0, 0.0, nt, t, outtrace.data);
		
		/* set header field for output trace */
		if(hdrs == 0 || hdrs == 2 || hdrs == 3){

			/* value is added to existing header values */
			/* this permits multiple static corrections */
			outtrace.sstat += (1000.0 * tsd);
			outtrace.gstat += (1000.0 * trd);
			outtrace.tstat += (1000.0 * tstat);
		} 
		
		puttr(&outtrace);
	} while (gettr(&intrace));


	return(CWP_Exit());
}
