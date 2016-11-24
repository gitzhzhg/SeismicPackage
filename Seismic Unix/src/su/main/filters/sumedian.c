/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUMEDIAN: $Revision: 1.16 $ ; $Date: 2011/11/12 00:09:00 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 	   								",
" SUMEDIAN - MEDIAN filter about a user-defined polygonal curve with	",
"	   the distance along the curve specified by key header word 	",
" 	   								",
" sumedian <stdin >stdout xshift= tshift= [optional parameters]		",
" 									",
" Required parameters:							",
" xshift=               array of position values as specified by	",
"                       the `key' parameter				",
" tshift=               array of corresponding time values (sec)	",
"  ... or input via files:						",
" nshift=               number of x,t values defining median times	",
" xfile=                file containing position values as specified by	",
"                       the `key' parameter				",
" tfile=                file containing corresponding time values (sec)	",
" 									",
" Optional parameters:							",
" key=tracl             Key header word specifying trace number 	",
"                       =offset  use trace offset instead		",
" 									",
" mix=.6,1,1,1,.6       array of weights for mix (weighted moving average)",
" median=0              =0  for mix					",
"                       =1  for median filter				",
" nmed=5                odd no. of traces to median filter		",
" sign=-1               =-1  for upward shift				",
"                       =+1  for downward shift				",
" subtract=1            =1  subtract filtered data from input		",
"                       =0  don't subtract				",
" verbose=0             =1  echoes information				",
"									",
" tmpdir= 	 if non-empty, use the value as a directory path	",
"		 prefix for storing temporary files; else if the	",
"	         the CWP_TMPDIR environment variable is set use		",
"	         its value for the path; else use tmpfile()		",
" 									",
" Notes: 								",
" ------								",
" Median filtering is a process for suppressing a particular moveout on ",
" seismic sections. Its advantage over traditional dip filtering is that",
" events with arbitrary moveout may be suppressed. Median filtering is	",
" commonly used in up/down wavefield separation of VSP data.		",
"									",
" The process generally consists of 3 steps:				",
" 1. A copy of the data panel is shifted such that the polygon in x,t	",
"    specifying moveout is flattened to horizontal. The x,t pairs are 	",
"    specified either by the vector xshift,tshift or by the values in	",
"    the datafiles xfile,tfile.	For fractional shift, the shifted data	",
"    is interpolated.							",
" 2. Then a mix (weighted moving average) is performed over the shifted	",
"    panel to emphasize events with the specified moveout and suppress	",
"    events with other moveouts.					",
" 3. The panel is then shifted back (and interpolated) to its original	",
"    moveout, and subtracted from the original data. Thus all events	",
"    with the user-specified moveout are removed.			",
" 									",
" For VSP data the following modifications are provided:		",
" 1. The moveout polygon in x,t is usually the first break times for	",
"    each trace. The parameter sign allows for downward shift in order	",
"    align upgoing events.						",
" 2. Alternative to a mix, a median filter can be applied by setting	",
"    the parameter median=1 and nmed= to the number of traces filtered.	",
" 3. By setting subtract=0 the filtered panel is only shifted back but	",
"    not subtracted from the original data.				",
"									",
" The values of tshift are linearly interpolated for traces falling	",
" between given xshift values. The tshift interpolant is extrapolated	",
" to the left by the smallest time sample on the trace and to the right	",
" by the last value given in the tshift array. 				",
" 									",
" The files tfile and xfile are files of binary (C-style) floats.	",
" 									",
" The number of values defined by mix=val1,val2,... determines the	",
" number of traces to be averaged, the values determine the weights.	",
" 									",
" Caveat:								",
" The median filter may perform poorly on the edges of a section.	",
" Choosing larger beginning and ending mix values may help, but may	",
" also introduce additional artifacts.					",
"									",
" Examples:								",
" 									",
" 									",
NULL};

/* Credits:
 *
 * CWP: John Stockwell, based in part on sumute, sureduce, sumix
 * CENPET: Werner M. Heigl - fixed various errors, added VSP functionality
 *
 * U of Durham, UK: Richard Hobbs - fixed the program so it applies the
 *                                   median filter
 * ideas for improvement:
 *	a versatile median filter needs to do:
 *	shift traces by fractional amounts -> needs sinc interpolation
 *	positive and negative shifts similar to SUSTATIC
 *	make subtraction of filtered events a user choice
 *	provide a median stack as well as a weighted average stack
 * Trace header fields accessed: ns, dt, delrt, key=keyword
 *
 */
/**************** end self doc ***********************************/

/* default weighting values */
#define VAL0	0.6
#define VAL1	1.0
#define VAL2	1.0
#define VAL3	1.0
#define VAL4	0.6

static void closefiles(void);

/* Globals (so can trap signal) defining temporary disk files */
char tracefile[BUFSIZ];	/* filename for the file of traces	*/
char headerfile[BUFSIZ];/* filename for the file of headers	*/
FILE *tracefp;		/* fp for trace storage file		*/
FILE *headerfp;		/* fp for header storage file		*/

segy tr;

int
main(int argc, char **argv)
{
	char *key=NULL;		/* header key word from segy.h		*/
	char *type=NULL;	/* ... its type				*/
	int index;		/* ... its index			*/
	Value val;		/* ... its value			*/
	float fval;		/* ... its value cast to float		*/

	float *xshift=NULL;	/* array of key shift curve values	*/
	float *tshift=NULL;	/* ...		shift curve time values */

	int nxshift;		/* number of key shift values		*/
	int ntshift;		/* ...		shift time values 	*/

	int nxtshift;		/* number of shift values 		*/

	int it;			/* sample counter			*/
	int itr;		/* trace counter			*/
	int nt;			/* number of time samples 		*/
	int ntr=0;		/* number of traces			*/
	int *inshift=NULL;	/* array of (integer) time shift values
				   used for positioning shifted trace in
				   data[][]				*/

	float dt;		/* time sampling interval		*/

	cwp_String xfile="";	/* file containing positions by key	*/
	FILE *xfilep=NULL;	/* ... its file pointer			*/
	cwp_String tfile="";	/* file containing times	 	*/
	FILE *tfilep=NULL;	/* ... its file pointer			*/

	int verbose;		/* flag for printing information	*/
	char *tmpdir=NULL;	/* directory path for tmp files		*/
	cwp_Bool istmpdir=cwp_false;/* true for user-given path		*/

	int median;		/* flag for median filter		*/
	int nmed;		/* no. of traces to median filter	*/
	int nmix;		/* number of traces to mix over		*/
	int imix;		/* mixing counter			*/
	float *mix=NULL;	/* array of mix values			*/
	int sign;		/* flag for up/down shift		*/
	int shiftmin=0;		/* minimum time shift (in samples)	*/
	int shiftmax=0;		/* maximum time shift (in samples)	*/
	int ntdshift;		/* nt + shiftmax			*/

	size_t mixbytes;	/* size of mixing array			*/
	size_t databytes;	/* size of data array			*/
	size_t shiftbytes;	/* size of data array			*/
	float *temp=NULL;	/* temporary array			*/
	float *dtemp=NULL;	/* temporary array			*/
	float *stemp=NULL;	/* rwh median sort array		*/
	float **data=NULL;	/* mixing array 			*/
	int subtract;		/* flag for subtracting shifted data	*/

	/* rwh extra pointers for median sort */
	int first;		/* start pointer in ring buffer */
	int middle;		/* middle pointer in ring buffer */
	int last;		/* last pointer in ring buffer */
	int halfwidth;		/* mid point */
	int trcount;		/* pointer to current start trace number */
	float tmp;		/* temp storage for bubble sort */
	int rindex;		/* wrap around index for ring buffer */
	int jmix;		/* internal pointer for bubble sort */
	

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get parameters */
	if (!(getparstring("xfile",&xfile) && getparstring("tfile",&tfile))) {
		if (!(nxshift = countparval("xshift")))
			err("must give xshift= vector");
		if (!(ntshift = countparval("tshift")))
			err("must give tshift= vector");
		if (nxshift != ntshift)
			err("lengths of xshift, tshift must be the same");
		xshift = ealloc1float(nxshift);	getparfloat("xshift", xshift);
		tshift = ealloc1float(nxshift);	getparfloat("tshift", tshift);
	} else {
		MUSTGETPARINT("nshift",&nxtshift);
		nxshift = nxtshift;
		xshift = ealloc1float(nxtshift);
		tshift = ealloc1float(nxtshift);

		if((xfilep=fopen(xfile,"r"))==NULL)
			err("cannot open xfile=%s\n",xfile);
		if (fread(xshift,sizeof(float),nxtshift,xfilep)!=nxtshift)
			err("error reading xfile=%s\n",xfile);
		fclose(xfilep);

		if((tfilep=fopen(tfile,"r"))==NULL)
			err("cannot open tfile=%s\n",tfile);
		if (fread(tshift,sizeof(float),nxtshift,tfilep)!=nxtshift)
			err("error reading tfile=%s\n",tfile);
		fclose(tfilep);
	}
	if (!getparstring("key", &key))		key = "tracl";

	/* Get key type and index */
	type = hdtype(key);
	index = getindex(key);   

	/* Get mix weighting values values */
	if ((nmix = countparval("mix"))!=0) {
		mix = ealloc1float(nmix);
		getparfloat("mix",mix);
		/* rwh check nmix is odd */
		if (nmix%2==0) {
			err("number of mixing coefficients must be odd");
		}		
	} else {
		nmix = 5;
		mix = ealloc1float(nmix);
		mix[0] = VAL0;
		mix[1] = VAL1;
		mix[2] = VAL2;
		mix[3] = VAL3;
		mix[4] = VAL4;
	}
	
	/* Get remaning parameters */
	if (!getparint("median",&median))	median = 0;
	if (!getparint("nmed",&nmed) && median)	nmed = 5;
	if (!getparint("sign",&sign))		sign = -1;
	if (!getparint("subtract",&subtract))	subtract = 1;
	if (!getparint("verbose", &verbose))	verbose = 0;

	/* rwh check nmed is odd */
	if (median && nmed%2==0) {
		nmed=nmed+1;
		warn("increased nmed by 1 to ensure it is odd");
	}

	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);

	/* rwh fix for median filter if median true set nmix=nmed */
	if (!median) {
		/* Divide mixing weights by number of traces to mix */
		for (imix = 0; imix < nmix; ++imix)
			mix[imix]=mix[imix]/((float) nmix);
	} else {
		nmix=nmed;
	}

	/* Get info from first trace */
	if (!gettr(&tr)) err("can't read first trace");
	if (!tr.dt) err("dt header field must be set");
	dt   = ((double) tr.dt)/1000000.0;
	nt = (int) tr.ns;
	databytes = FSIZE*nt;

	/* Tempfiles */
	if (STREQ(tmpdir,"")) {
		tracefp = etmpfile();
		headerfp = etmpfile();
		if (verbose) warn("using tmpfile() call");
	} else { /* user-supplied tmpdir */
		char directory[BUFSIZ];
		strcpy(directory, tmpdir);
		strcpy(tracefile, temporary_filename(directory));
		strcpy(headerfile, temporary_filename(directory));
		/* Trap signals so can remove temp files */
		signal(SIGINT,  (void (*) (int)) closefiles);
		signal(SIGQUIT, (void (*) (int)) closefiles);
		signal(SIGHUP,  (void (*) (int)) closefiles);
		signal(SIGTERM, (void (*) (int)) closefiles);
		tracefp = efopen(tracefile, "w+");
		headerfp = efopen(headerfile, "w+");
      		istmpdir=cwp_true;		
		if (verbose) warn("putting temporary files in %s", directory);
	}

	/* Read headers and data while getting a count */
	do {
		++ntr;
		efwrite(&tr, 1, HDRBYTES, headerfp);
		efwrite(tr.data, 1, databytes, tracefp);   

	} while (gettr(&tr));
	rewind(headerfp);
	rewind(tracefp);
	
	/* Allocate space for inshift vector */
	inshift = ealloc1int(ntr);

	/* Loop over headers */
 	for (itr=0; itr<ntr; ++itr) {
		float tmin=tr.delrt/1000.0;
		float t;

		/* Read header values */
		efread(&tr, 1, HDRBYTES, headerfp);

		/* Get value of key and convert to float */
		gethval(&tr, index, &val);
		fval = vtof(type,val);

		/* Linearly interpolate between (xshift,tshift) values */
		intlin(nxshift,xshift,tshift,tmin,tshift[nxshift-1],1,&fval,&t);
		
		/* allow for fractional shifts -> requires interpolation */ 
		inshift[itr] = NINT((t - tmin)/dt);
		
		/* Find minimum and maximum shifts */
		if (itr==0) {
			 shiftmax=inshift[0];
			 shiftmin=inshift[0];
		} else {
			shiftmax = MAX(inshift[itr],shiftmax);
			shiftmin = MIN(inshift[itr],shiftmin);
		}
	}
	rewind(headerfp);
	rewind(tracefp);

	if (verbose) {
		for (itr=0;itr<ntr;itr++)
			warn("inshift[%d]=%d",itr,inshift[itr]);
	}

	/* Compute databytes per trace and bytes in mixing panel */
	ntdshift = nt + shiftmax;
	shiftbytes = FSIZE*ntdshift;
	mixbytes = shiftbytes*nmix;
	if (verbose) {
		warn("nt=%d  shiftmax=%d  shiftmin=%d",nt,shiftmax,shiftmin);
		warn("ntdshift=%d  shiftbytes=%d  mixbytes=%d",
						ntdshift,shiftbytes,mixbytes);
	}
	
	/* Allocate space and zero  data array */
	data = ealloc2float(ntdshift,nmix);
	temp = ealloc1float(ntdshift);
	dtemp = ealloc1float(nt);
	memset( (void *) data[0], 0, mixbytes);

	/* rwh array for out of place bubble sort (so we do not corrupt order in ring buffer */ 
	stemp = ealloc1float(nmix);

	/* rwh first preload ring buffer symmetrically (now you know why nmix must be odd) */
	trcount=-1;
	halfwidth=(nmix-1)/2+1;
	first = 0;
	last  = nmix-1;
	middle = (nmix-1)/2;

	for (itr=0; itr<halfwidth; itr++) {
		efread(tr.data, 1, databytes, tracefp);
		trcount++;
		for(it=0; it<nt; ++it) {
			/* sign to account for positive or negative shift */
			/* tr.data needs to be interpolated for non-integer shifts */
			data[middle-itr][it + shiftmax + sign*inshift[itr]] = tr.data[it];
			data[middle+itr][it + shiftmax + sign*inshift[itr]] = tr.data[it];
		}
	}
	
	/* Loop over traces performing median filtering  */
 	for (itr=0; itr<ntr; ++itr) {

		/* paste header and data on output trace */
		efread(&tr, 1, HDRBYTES, headerfp);

		/* Zero out temp and dtemp */
		memset((void *) temp, 0, shiftbytes);
		memset((void *) dtemp, 0, databytes);

		/* Loop over time samples */
		for (it=0; it<nt; ++it) {

			/* Weighted moving average (mix) ? */
			if (!median) {
				for(imix=0; imix<nmix; ++imix) {
					temp[it] += data[imix][it] * mix[imix];
				}
			} else {
			
			/* inlcude median stack */
			/* rwh do bubble sort and choose median value */
				for(imix=0; imix<nmix; ++imix) {
					stemp[imix]=data[imix][it];
				}
				for (imix=0; imix<nmix-1; imix++) {
					for (jmix=0; jmix<nmix-1-imix; jmix++) {
						if (stemp[jmix+1] < stemp[jmix]) {
							tmp = stemp[jmix];
							stemp[jmix] = stemp[jmix+1];
							stemp[jmix+1] = tmp;
						}
					}
				}
				temp[it] = stemp[middle];
			}

			/* shift back mixed data and put into dtemp */
			if (subtract) {
				if ((it - shiftmax - sign*inshift[itr])>=0)
					dtemp[it - shiftmax - sign*inshift[itr]] = data[middle][it]-temp[it];
			} else {
				if ((it - shiftmax)>=0)
				dtemp[it - shiftmax - sign*inshift[itr]] = temp[it];
			}
		}
		memcpy((void *) tr.data,(const void *) dtemp,databytes);
			
		/* Bump rows of data[][] over by 1 to free first row for next tr.data */
		for (imix=nmix-1; 0<imix; --imix)
			memcpy((void *) data[imix],(const void *) data[imix-1],shiftbytes);
			/*for (it=0; it<nt; ++it)
				data[imix][it] = data[imix-1][it];*/

		/* Write output trace */
		tr.ns = nt;
		puttr(&tr);

		/* read next trace into buffer */
		if (trcount < ntr) {
			efread(tr.data, 1, databytes, tracefp);
			trcount++;

			/* read tr.data into first row of mixing array */
			/* WMH: changed ntdshift to nt */
			for(it=0; it<nt; ++it) {
				/* sign to account for positive or negative shift */
				/* tr.data needs to be interpolated for non-integer shifts */
				data[0][it + shiftmax + sign*inshift[trcount]] = tr.data[it];
			}
		} else {
			rindex=2*(trcount-ntr);
			memcpy((void *) data[0],(const void *) data[rindex],shiftbytes);
			trcount++;
		}

	}

	if (verbose && subtract)	warn("filtered data subtracted from input");

	/* Clean up */
	efclose(headerfp);
	if (istmpdir) eremove(headerfile);
	efclose(tracefp);
	if (istmpdir) eremove(tracefile);

	return(CWP_Exit());
}

/* for graceful interrupt termination */
static void closefiles(void)
{
	efclose(headerfp);
	efclose(tracefp);
	eremove(headerfile);
	eremove(tracefile);
	exit(EXIT_FAILURE);
}
