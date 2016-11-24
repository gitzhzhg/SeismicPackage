/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUWIND: $Revision: 1.55 $ ; $Date: 2015/03/18 17:50:30 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUWIND - window traces by key word					",
"									",
"  suwind <stdin >stdout [options]					",
"									",
" Required Parameters:							",
"  none 								",
"									",
" Optional Parameters:							",
" verbose=0		=1 for verbose					",
" key=tracl		Key header word to window on (see segy.h)	",
" min=LONG_MIN		min value of key header word to pass		",
" max=LONG_MAX		max value of key header word to pass		",
"									",
" abs=0			=1 to take absolute value of key header word	",
" j=1			Pass every j-th trace ...			",
" s=0			... based at s  (if ((key - s)%j) == 0)		",
" skip=0		skip the initial N traces                       ",
" count=ULONG_MAX	... up to count traces				",
" reject=none		Skip traces with specified key values		",
" accept=none		Pass traces with specified key values(see notes)",
"			processing, but do no window the data		",
" ordered=0		=1 if traces sorted in increasing keyword value ",
"			=-1  if traces are sorted in a decreasing order ",
"									",
" Options for vertical windowing (time gating):				",
" dt=tr.dt (from header) time sampling interval (sec)	(seismic data)	",
" 			 =tr.d1  (nonseismic)				",
" f1=tr.delrt (from header) first sample		(seismic data)	",
" 			 =tr.f1  (nonseismic)				",
"									",
" tmin=0.0		min time to pass				",
" tmax=(from header)	max time to pass				",
" itmin=0		min time sample to pass				",
" itmax=(from header)   max time sample to pass				",
" nt=itmax-itmin+1	number of time samples to pass			",
"									",
" Notes:								",
" On large data sets, the count parameter should be set if		",
" possible.  Otherwise, every trace in the data set will be		",
" examined.  However, the count parameter overrides the accept		",
" parameter, so you can't specify count if you want true		",
" unconditional acceptance.						",
"                                                                       ",
" The skip= option allows the user to skip over traces, which helps	",
" for selecting traces far from the beginning of the dataset.		",
" Caveat: skip only works with disk input.                        	",
"                                                                       ",
" The ordered= option will speed up the process if the data are   	",
" sorted in according to the key.                                 	",
"									",
" The accept option is a bit strange--it does NOT mean accept ONLY	",
" the traces on the accept list!  It means accept these traces,   	",
" even if they would otherwise be rejected (except as noted in the	",
" previous paragraph).  To implement accept-only, you can use the 	",
" max=0 option (rejecting everything).  For example, to accept    	",
" only the tracl values 4, 5 and 6:					",
"	... | suwind max=0 accept=4,5,6 | ...		   		",
"									",
" Another example is the case of suppressing nonseismic traces in 	",
" a seismic data set. By the SEGY standard header field trace id, 	",
" trid=1 designates traces as being seismic traces. Other traces, 	",
" such as calibration traces may be designated by another value.  	",
"									",
" Example:  trid=1 seismic and trid=0 is nonseismic. To reject    	",
"       nonseismic traces						",
"       ... | suwind key=trid reject=0 | ...				",
"      									",
" On most 32 bit machines, LONG_MIN, LONG_MAX and ULONG_MAX are   	",
" about -2E9,+2E9 and 4E9, they are defined in limits.h.		",
"									",
" Selecting times beyond the maximum in the data induces		",
" zero padding (up to SU_NFLTS).					",
"									",
" The time gating here is to the nearest neighboring sample or    	",
" time value. Gating to the exact temporal value requires	 	",
" resampling if the selected times fall between samples on the    	",
" trace. Use suresamp to perform the time gating in this case.    	",
"									",
" It doesn't really make sense to specify both itmin and tmin,		",
" but specifying itmin takes precedence over specifying tmin.		",
" Similarly, itmax takes precedence over tmax and tmax over nt.		",
" If dt in header is not set, then dt is mandatory			",
"									",
NULL};

/* Credits:
 *	SEP: Einar Kjartansson
 *	CWP: Shuki Ronen, Jack Cohen, Chris Liner
 *	Warnemuende: Toralf Foerster
 *	CENPET: Werner M. Heigl (modified to include well log data)
 *
 * Trace header fields accessed: ns, dt, delrt, keyword
 * Trace header fields modified: ns, delrt, ntr
 */
/**************** end self doc *******************************************/


segy tr;

int
main(int argc, char **argv)
{
        FILE* infp=stdin;
	cwp_Bool seismic;	/* is this seismic data? */
	cwp_String key; /* header key word from segy.h		*/
	Value val;	/* value of key				*/
	int ival;	/* ... cast to int			*/
	cwp_String type;/* type of key				*/
	int indx;	/* index of key				*/
	int ordered;   /* order of key				*/
	long min;	/* smallest key value to accept		*/
	long max;	/* largest key value to accept		*/
	int j;		/* take every jth trace ...		*/
	int s;		/* ... starting at the sth trace ...	*/

	unsigned
	 long count;    /* ... up to a total of count traces    */
	Value *badptr=NULL;  /* pointer to list of traces to reject  */
	unsigned
	 int nbad;	/* number of rejected traces		*/
	Value *goodptr=NULL; /* pointer to list of traces to accept  */
	unsigned
	 int ngood;     /* number unconditionally accepted	*/
	short ab;	/* absolute value flag (1=YES, 0=NO)    */
	short verbose;  /* if 1(yes) echo parameters to stderr  */
	register int i; /* counter				*/

	float tmin=0.0;     /* minimum time to pass			*/
	float tmax;     /* maximum time to pass			*/
	float dt;	/* sampling interval:
			   from tr.dt for seismic data (sec)
			   from tr.d1 for well log data (m,ft)	*/
	float f1;	/* first sample:
			   from tr.delrt for seismic data (sec)
			   from tr.f1 for well log data (m,ft)	*/
	int itmin;	/* smallest time sample (zero-based)    */
	int itmax;	/* largest time sample (zero-based)     */
	int ntfirst;	/* number of time samples on first trace*/
	int nt;		/* number of time samples on output     */
        int ntr=0;       /* number of traces                     */
        int itr=0;      /* trace counter                        */
        int skip=0;     /* traces to skip                       */
	size_t nzeros;  /* number of zeroes to pad		*/
	char *pzeros;   /* pointer to zero pad			*/
        


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Default parameters;  User-defined overrides */
	if (!getparstring("key", &key))		key = "tracl";
	if (!getparlong("min", &min))		min = INT_MIN;
	if (!getparlong("max", &max))		max = INT_MAX;
	if (!getparint("j", &j))		j = 1;
	if (!getparint("s", &s))		s = 0;
	if (!getparulong("count", &count))	count = ULONG_MAX;
	if (!getparshort("abs", &ab))		ab = 0;
	if (!getparshort("verbose", &verbose))	verbose = 0;
        if (!getparint("skip" ,&skip ))         skip=0;
	if (!getparint("ordered", &ordered))    ordered = 0;

	/* Evaluate time bounds from getpars and first header */

        if( skip ){
           if( !(ntr = fgettra(infp, &tr, itr) ) ){
               err("can't get first trace");
           }
           if( skip > ntr-1 ){
              err( "can't skip past last trace" );
           }else{
              itr=skip;
              fgettra(infp, &tr, itr);
           }

        }else{
           if (!gettr(&tr)) err("can't get first trace"); 
        }

	ntfirst = tr.ns;
	
	/* check for seismic or well log data */
	seismic = ISSEISMIC(tr.trid);		
	if (seismic) {
		if (verbose) warn("input is seismic data, trid=%d",tr.trid);
		if (!getparfloat("dt",&dt))	dt = ((double) tr.dt)/1000000.0;
		if (dt==0)  err("dt not set in header or not getparred");
		if (!getparfloat("f1",&f1))	f1 = ((double) tr.delrt)/1000.0;  
		if (!f1) {
			if (verbose)
			warn("delrt not set in header or not getparred");
		}
	} else {	/* is not seismic */
		if (verbose) warn("input is not seismic data, trid=%d",tr.trid);
		if (!getparfloat("dt",&dt)) {
			if (tr.d1) {
				dt = tr.d1;
			} else { 
				if  (verbose) {
					warn("d1 field not set");
					warn("using dt field");
				}

				dt = tr.dt;

				if (dt==0)
				err("d1 and dt not set in header or dt not getparred");
			}
		}
		if (!getparfloat("f1",&f1)) f1 = tr.f1;
		if (!f1) {       
			if (verbose)
				warn("f1 not set in header or not getparred");
		}
    }

	/* Time gating parameters */
	if (getparint("itmin", &itmin)) {
		tmin = itmin*dt + f1;
	} else if (getparfloat("tmin", &tmin)) {
		itmin = NINT((tmin - f1) / dt);
	} else {
		itmin = 0;
		tmin = f1;
	}
	if (getparint("itmax", &itmax)) {
		tmax = itmax*dt + f1;
		nt = itmax - itmin + 1;
	} else if (getparfloat("tmax", &tmax)) {
		itmax = NINT((tmax - f1) / dt);
		nt = itmax - itmin + 1;
	} else if (getparint("nt", &nt)) {
		itmax = itmin + nt - 1;
		tmax = itmax*dt + f1;
	} else {
		itmax = ntfirst - 1;
		tmax = itmax*dt + f1;
		nt = itmax - itmin + 1;
	}
	

	/* Check time gating values */
	if (itmin < 0)
		err("itmin=%d should be positive", itmin);
	if (nt > SU_NFLTS)
		err("nt=%d exceeds SU_NFLTS=%d", nt, SU_NFLTS);
	if (itmin > itmax)
		err("itmin=%d, itmax=%d conflict", itmin, itmax);


	/* compute number of zeros and padding */
	nzeros = (nt - ntfirst > 0) ? (nt - ntfirst) * FSIZE : 0;
	pzeros = (char *) (tr.data + ntfirst - itmin);

	type = hdtype(key);
	indx = getindex(key);


	/* Getpar the reject vector */
	if ((nbad = countparval("reject"))) {
		badptr = (Value *) ealloc1(nbad, sizeof(Value));
		getparval("reject", type, nbad, badptr);
	}


	 /* Getpar the accept vector */
	if ((ngood = countparval("accept"))) {
		goodptr = (Value *) ealloc1(ngood, sizeof(Value));
		getparval("accept", type, ngood, goodptr);
	}
        checkpars();


	/* Echo parameters */
	if (verbose) {
		warn("key = %s", key);
		warn("type = %s", type);
		warn("min = %ld", min);
		warn("max = %ld", max);
		warn("j = %d", j);
		warn("s = %d", s);
		warn("count = %lu", count);
		warn("abs = %d", ab);
		warn("ordered = %d", ordered);
		for (i = 0; i < ngood; i++) {
			(void) fprintf(stderr, "accept[%d] = ", i);
			fprintfval(stderr, type, goodptr[i]);
			putc('\n', stderr);
		}
		for (i = 0; i < nbad; i++) {
			(void) fprintf(stderr, "reject[%d] = ", i);
			fprintfval(stderr, type, badptr[i]);
			putc('\n', stderr);
		}
		
		warn("tmin=%f tmax=%f", tmin, tmax);
		warn("itmin=%d itmax=%d nt=%u", itmin, itmax, nt);
		warn("Padding %d zeroes", nzeros/FSIZE);
	}


        if( skip ){

		/* Main loop over traces */
		do {
			cwp_Bool isbad = cwp_false;  /* flag for unconditional reject */
			cwp_Bool isgood = cwp_false; /* flag for unconditional accept */
	 
			gethval(&tr, indx, &val);
			
			for (i = 0; i < nbad; i++) {
				if (!valcmp(type, val, badptr[i])) {
					isbad = cwp_true;
					break;  /* found */
				}
			}
			
			for (i = 0; i < ngood; i++) {
				if (!valcmp(type, val, goodptr[i])) {
					isgood = cwp_true;
					break;  /* found */
				}
			}
	
			if (ab) val = valtoabs(type, val);
	
			ival = vtoi(type, val);
	
			/* If trace selected, put it out */

			if (((ordered == 1 ) && (max < ival)) || ((ordered == (-1) ) && (min > ival ))) break;


			if ( isgood || 
			    ((min <= ival) && (ival <= max) &&
			     !((ival - s) % j) && !isbad ) ) {
	
				/* Perform time windowing */
				if (itmin > 0) {
					for (i = itmin; i <= itmax; i++) {
						tr.data[i-itmin] = tr.data[i];
					}
					if (seismic)
						tr.delrt = NINT((itmin*((int) tr.dt))*.001 + ((int) (tr.delrt)));
					else	tr.f1 = itmin*(tr.d1) + tr.f1;
				}
				if (nzeros) memset(pzeros, 0, nzeros);
				tr.ns = nt;
	
				/* zero ntr field to keep sugraphics from choking */
				tr.ntr = 0; 
	
				puttr(&tr);
				if (!(--count)) break; /* all done */
			}
	
	          itr++;
		} while (itr < ntr && fgettra(infp, &tr, itr));


        }else{

		/* Main loop over traces */
		do {
			cwp_Bool isbad = cwp_false;  /* flag for unconditional reject */
			cwp_Bool isgood = cwp_false; /* flag for unconditional accept */
	 
			gethval(&tr, indx, &val);
			
			for (i = 0; i < nbad; i++) {
				if (!valcmp(type, val, badptr[i])) {
					isbad = cwp_true;
					break;  /* found */
				}
			}
			
			for (i = 0; i < ngood; i++) {
				if (!valcmp(type, val, goodptr[i])) {
					isgood = cwp_true;
					break;  /* found */
				}
			}
	
			if (ab) val = valtoabs(type, val);
	
			ival = vtoi(type, val);
	
			/* If trace selected, put it out */

			if (((ordered == 1 ) && (max < ival)) || ((ordered == (-1) ) && (min > ival ))) break; 

			if ( isgood || 
			    ((min <= ival) && (ival <= max) &&
			     !((ival - s) % j) && !isbad ) ) {
	
				/* Perform time windowing */
				if (itmin > 0) {
					for (i = itmin; i <= itmax; i++) {
						tr.data[i-itmin] = tr.data[i];
					}
					if (seismic)
						tr.delrt = NINT((itmin*((int) tr.dt))*.001 + ((int) (tr.delrt)));
					else	tr.f1 = itmin*(tr.d1) + tr.f1;
				}
				if (nzeros) memset(pzeros, 0, nzeros);
				tr.ns = nt;
	
				/* zero ntr field to keep sugraphics from choking */
				tr.ntr = 0; 
	
				puttr(&tr);
				if (!(--count)) break; /* all done */
			}
	
		} while (gettr(&tr));
        }

	return(CWP_Exit());
}
