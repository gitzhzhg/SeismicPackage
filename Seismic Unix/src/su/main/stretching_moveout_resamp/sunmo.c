/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUNMO: $Revision: 1.31 $ ; $Date: 2013/03/06 20:35:27 $		*/
 
#include "su.h"
#include "segy.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"									     ",
" SUNMO - NMO for an arbitrary velocity function of time and CDP	     ",
"									     ",
"  sunmo <stdin >stdout [optional parameters]				     ",
"									     ",
" Optional Parameters:							     ",
" tnmo=0,...		NMO times corresponding to velocities in vnmo	     ",
" vnmo=1500,...		NMO velocities corresponding to times in tnmo	     ",
" cdp=			CDPs for which vnmo & tnmo are specified (see Notes) ",
" smute=1.5		samples with NMO stretch exceeding smute are zeroed  ",
" lmute=25		length (in samples) of linear ramp for stretch mute  ",
" sscale=1		=1 to divide output samples by NMO stretch factor    ",
" invert=0		=1 to perform (approximate) inverse NMO		     ",
" upward=0		=1 to scan upward to find first sample to kill	     ",
" voutfile=		if set, interplolated velocity function v[cdp][t] is ",
"			output to named file.			     	     ",
" Notes:								     ",
" For constant-velocity NMO, specify only one vnmo=constant and omit tnmo.   ",
"									     ",
" NMO interpolation error is less than 1% for frequencies less than 60% of   ",
" the Nyquist frequency.						     ",
"									     ",
" Exact inverse NMO is impossible, particularly for early times at large     ",
" offsets and for frequencies near Nyquist with large interpolation errors.  ",
" 								     	     ",
" The \"offset\" header field must be set.				     ",
" Use suazimuth to set offset header field when sx,sy,gx,gy are all	     ",
" nonzero. 							   	     ",
"									     ",
" For NMO with a velocity function of time only, specify the arrays	     ",
"	   vnmo=v1,v2,... tnmo=t1,t2,...				     ",
" where v1 is the velocity at time t1, v2 is the velocity at time t2, ...    ",
" The times specified in the tnmo array must be monotonically increasing.    ",
" Linear interpolation and constant extrapolation of the specified velocities",
" is used to compute the velocities at times not specified.		     ",
" The same holds for the anisotropy coefficients as a function of time only. ",
"									     ",
" For NMO with a velocity function of time and CDP, specify the array	     ",
"	   cdp=cdp1,cdp2,...						     ",
" and, for each CDP specified, specify the vnmo and tnmo arrays as described ",
" above. The first (vnmo,tnmo) pair corresponds to the first cdp, and so on. ",
" Linear interpolation and constant extrapolation of 1/velocity^2 is used    ",
" to compute velocities at CDPs not specified.				     ",
"									     ",
" The format of the output interpolated velocity file is unformatted C floats",
" with vout[cdp][t], with time as the fast dimension and may be used as an   ",
" input velocity file for further processing.				     ",
"									     ",
" Note that this version of sunmo does not attempt to deal with	anisotropy.  ",
" The version of sunmo with experimental anisotropy support is \"sunmo_a\"   ",
"									     ",
NULL};

/* Credits:
 *	SEP: Shuki Ronen, Chuck Sword
 *	CWP: Shuki Ronen, Jack, Dave Hale, Bjoern Rommel
 *      Modified: 08/08/98 - Carlos E. Theodoro - option for lateral offset
 *      Modified: 07/11/02 - Sang-yong Suh -
 *	  added "upward" option to handle decreasing velocity function.
 *      CWP: Sept 2010: John Stockwell
 *	  1. replaced Carlos Theodoro's fix 
 *	  2. added  the instruction in the selfdoc to use suazimuth to set 
 *	      offset so that it accounts for lateral offset. 
 *        3. removed  Bjoren Rommel's anisotropy stuff. sunmo_a is the 
 *           version with the anisotropy parameters left in.
 *        4. note that scalel does not scale the offset field in
 *           the segy standard.
 * Technical Reference:
 *	The Common Depth Point Stack
 *	William A. Schneider
 *	Proc. IEEE, v. 72, n. 10, p. 1238-1254
 *	1984
 *
 * Trace header fields accessed: ns, dt, delrt, offset, cdp, scalel
 */
/**************** end self doc *******************************************/

static void interpovv (int nt, int ncdp, float *cdp, 
	float **ovv, float cdpt, float *ovvt );

segy tr;

int
main(int argc, char **argv)
{
	int nt;			/* number of time samples per trace */
	float dt;		/* time sampling interval */
	float ft;		/* time of first sample */
	int it;			/* time sample index */

	int ncdp;		/* number of cdps specified */
	float *cdp=NULL;	/* array[ncdp] of cdps */
	int icdp;		/* index into cdp array */
	int jcdp;		/* index into cdp array */

	int nvnmo;		/* number of vnmos specified */
	float *vnmo=NULL;	/* array[nvnmo] of vnmos */
	int ntnmo;		/* number of tnmos specified */
	float *tnmo=NULL;	/* array[ntnmo] of tnmos */
	float **ovv=NULL;	/* array[ncdp][nt] of sloth */
				/*      i.e. (1/velocity^2) functions */
	float *ovvt=NULL;	/* array[nt] of sloth for a particular trace */

	float smute;		/* zero samples with NMO stretch exceeding */
				/*  smute */
	float osmute;		/* 1/smute */
	int lmute;		/* length in samples of linear ramp for mute */
	int itmute=0;		/* zero samples with indices less than itmute */
	int sscale;		/* if non-zero, apply NMO stretch scaling */
	int invert;		/* if non-zero, do inverse NMO */
	
	long oldoffset;		/* offset of previous trace */
	long oldcdp;		/* cdp of previous trace */

	int newsloth;		/* if non-zero, new sloth function was */
				/* computed */

	float tn;		/* NMO time (time after NMO correction) */
	float v;		/* velocity */
	float *qtn=NULL;	/* NMO-corrected trace q(tn) */
	float *ttn=NULL;	/* time t(tn) for NMO */
	float *atn=NULL;	/* amplitude a(tn) for NMO */
	float *qt=NULL;		/* inverse NMO-corrected trace q(t) */
	float *tnt=NULL;	/* time tn(t) for inverse NMO */
	float *at=NULL;		/* amplitude a(t) for inverse NMO */
	float acdp;		/* temporary used to sort cdp array */

	float *aovv;		/* temporary used to sort ovv array */
	float temp;		/* temporary float */
	float tsq;		/* temporary float */
	int upward;		/* scans upward if it's nonzero. */

	float offset;		/* value of offset honoring scalel */

	char *voutfile="";	/* name of interpolated output vel file */
	FILE *voutfp=NULL;	/* ... its file pointer */
	cwp_Bool isvoutfile=cwp_false; /* is output vel file specified? */
	float voutt[1];	/* output velocities */

	
	/* hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = ((double) tr.dt)/1000000.0;
	ft = tr.delrt/1000.0;

	/* if specified, open output velocity file */
	getparstring("voutfile",&voutfile);
	if (*voutfile!='\0') {
		isvoutfile=cwp_true;
		
		if((voutfp=fopen(voutfile,"w"))==NULL)
                        err("cannot open voutfile=%s\n",voutfile);
	}
	


	/* get velocity functions, linearly interpolated in time */
	ncdp = countparval("cdp");
	if (ncdp>0) {
		if (countparname("vnmo")!=ncdp)
			err("a vnmo array must be specified for each cdp");
		if (countparname("tnmo")!=ncdp)
			err("a tnmo array must be specified for each cdp");
	} else {
		ncdp = 1;
		if (countparname("vnmo")>1)
			err("only one (or no) vnmo array must be specified");
		if (countparname("tnmo")>1)
			err("only one (or no) tnmo array must be specified");
	}
	cdp = ealloc1float(ncdp);
	if (!getparfloat("cdp",cdp)) cdp[0] = tr.cdp;
	ovv = ealloc2float(nt,ncdp);


	for (icdp=0; icdp<ncdp; ++icdp) {

		nvnmo = countnparval(icdp+1,"vnmo");
		ntnmo = countnparval(icdp+1,"tnmo");

		if (nvnmo!=ntnmo && !(ncdp==1 && nvnmo==1 && ntnmo==0))
			err("number of vnmo and tnmo values must be equal");

		if (nvnmo==0) nvnmo = 1;
		if (ntnmo==0) ntnmo = nvnmo;

		/* equal numbers of parameters vnmo, tnmo */
		vnmo = ealloc1float(nvnmo);
		tnmo = ealloc1float(nvnmo);

		if (!getnparfloat(icdp+1,"vnmo",vnmo)) vnmo[0] = 1500.0;
		if (!getnparfloat(icdp+1,"tnmo",tnmo)) tnmo[0] = 0.0;
		for (it=1; it<ntnmo; ++it)
			if (tnmo[it]<=tnmo[it-1])
				err("tnmo values must increase monotonically");
		for (it=0,tn=ft; it<nt; ++it,tn+=dt) {
			intlin(ntnmo,tnmo,vnmo,vnmo[0],vnmo[nvnmo-1],1,&tn,&v);
			ovv[icdp][it] = 1.0/(v*v);
		}

		free1float(vnmo);
		free1float(tnmo);
	}

	/* sort (by insertion) sloth and anis functions by increasing cdp */
	for (jcdp=1; jcdp<ncdp; ++jcdp) {
		acdp = cdp[jcdp];
		aovv = ovv[jcdp];
		for (icdp=jcdp-1; icdp>=0 && cdp[icdp]>acdp; --icdp) {
			cdp[icdp+1] = cdp[icdp];
			ovv[icdp+1] = ovv[icdp];
		}
		cdp[icdp+1] = acdp;
		ovv[icdp+1] = aovv;
	}

	/* get other optional parameters */
	if (!getparfloat("smute",&smute)) smute = 1.5;
	if (smute<=0.0) err("smute must be greater than 0.0");
	if (!getparint("lmute",&lmute)) lmute = 25;
	if (!getparint("sscale",&sscale)) sscale = 1;
	if (!getparint("invert",&invert)) invert = 0;
	if (!getparint("upward",&upward)) upward = 0;
        checkpars();

	/* allocate workspace */
	ovvt = ealloc1float(nt);
	ttn = ealloc1float(nt);
	atn = ealloc1float(nt);
	qtn = ealloc1float(nt);
	tnt = ealloc1float(nt);
	at = ealloc1float(nt);
	qt = ealloc1float(nt);

	/* interpolate sloth and anis function for first trace */
	interpovv(nt,ncdp,cdp,ovv,(float)tr.cdp,ovvt);

	/* if specified output output velocity for first trace */
	if(isvoutfile) {
			for (it=0; it<nt; ++it) {	
				float veltemp=ovvt[it];
				voutt[0]=sqrt(1.0/veltemp);
				efwrite(voutt,FSIZE,1,voutfp);
			}
	}

	/* set old cdp and old offset for first trace */
	oldcdp = tr.cdp;
	oldoffset = tr.offset-1; /* here offset is used as a marker */
				 /* there is no need to have it honor scalel */

	/* loop over traces */
	do {
		/* if necessary, compute new sloth and anis function */
		if (tr.cdp!=oldcdp && ncdp>1) {
			interpovv(nt,ncdp,cdp,ovv,(float)tr.cdp,
				  ovvt);
			newsloth = 1;
			/* if specified output output velocity */
			if(isvoutfile) {
				for (it=0; it<nt; ++it) {	
					float veltemp=ovvt[it];
					voutt[0]=sqrt(1.0/veltemp);
					efwrite(voutt,FSIZE,1,voutfp);
				}
			}
		} else {
			newsloth = 0;
		}

		/* if sloth function or offset has changed */
		if (newsloth || tr.offset!=oldoffset) {

			offset = (float) (tr.offset);
			

			/* compute time t(tn) (normalized) */
			temp = ((float) offset*offset)/(dt*dt);
			for (it=0,tn=ft/dt; it<nt; ++it,tn+=1.0) {
				tsq = temp*ovvt[it];
				ttn[it] = sqrt (tn*tn + tsq);

	
			}
			/* compute inverse of stretch factor a(tn) */
			atn[0] = ttn[1]-ttn[0];
			for (it=1; it<nt; ++it)
				atn[it] = ttn[it]-ttn[it-1];
			
			/* determine index of first sample to survive mute */
			osmute = 1.0/smute;
			if(!upward) {
				for (it=0; it<nt-1 && atn[it]<osmute; ++it);
			} else {
				/* scan samples from bottom to top */
				for (it=nt-1; it>0 && atn[it]>=osmute; --it);
			}
			itmute = it;


			/* if inverse NMO will be performed */
			if (invert) {
							
				/* compute tn(t) from t(tn) */
				yxtoxy(nt-itmute,1.0,ft/dt+itmute,&ttn[itmute],
					nt-itmute,1.0,ft/dt+itmute,
					ft/dt-nt,ft/dt+nt,&tnt[itmute]);
			
				/* adjust mute time */
				itmute = 1.0+ttn[itmute]-ft/dt;
				itmute = MIN(nt-2,itmute);
								
				/* compute a(t) */
				if (sscale) {
					for (it=itmute+1; it<nt; ++it)
						at[it] = tnt[it]-tnt[it-1];
					at[itmute] = at[itmute+1];
				}
			}
		}
		
		/* if forward (not inverse) nmo */
		if (!invert) {
	
			/* do nmo via 8-point sinc interpolation */
			ints8r(nt,1.0,ft/dt,tr.data,0.0,0.0,
				nt-itmute,&ttn[itmute],&qtn[itmute]);
			
			/* apply mute */
			for (it=0; it<itmute; ++it)
				qtn[it] = 0.0;
			
			/* apply linear ramp */
			for (it=itmute; it<itmute+lmute && it<nt; ++it)
				qtn[it] *= (float)(it-itmute+1)/(float)lmute;
			
			/* if specified, scale by the NMO stretch factor */
			if (sscale)
				for (it=itmute; it<nt; ++it)
					qtn[it] *= atn[it];
			
			/* copy NMO corrected trace to output trace */
			memcpy( (void *) tr.data,
					(const void *) qtn, nt*sizeof(float));
		
		/* else inverse nmo */
		} else {
	
			/* do inverse nmo via 8-point sinc interpolation */
			ints8r(nt,1.0,ft/dt,tr.data,0.0,0.0,
				nt-itmute,&tnt[itmute],&qt[itmute]);
			
			/* apply mute */
			for (it=0; it<itmute; ++it)
				qt[it] = 0.0;
			
			/* if specified, undo NMO stretch factor scaling */
			if (sscale)
				for (it=itmute; it<nt; ++it)
					qt[it] *= at[it];
			
			/* copy inverse NMO corrected trace to output trace */
			memcpy( (void *) tr.data,
					(const void *) qt,nt*sizeof(float));
		}

		/* write output trace */
		puttr(&tr);

		/* remember offset and cdp */
		oldoffset = tr.offset;
		oldcdp = tr.cdp;

	} while (gettr(&tr));

	if (isvoutfile) efclose(voutfp);
	return(CWP_Exit());
}


/* linearly interpolate/extrapolate sloth between cdps */
static void interpovv (int nt, int ncdp, float *cdp, float **ovv, 
	float cdpt, float *ovvt)
{
	static int index=0;
	int it;
	float a1,a2;

	/* if before first cdp, constant extrapolate */
	if (cdpt<=cdp[0]) {
		for (it=0; it<nt; ++it)
			ovvt[it] = ovv[0][it];
	
	/* else if beyond last cdp, constant extrapolate */
	} else if (cdpt>=cdp[ncdp-1]) {
		for (it=0; it<nt; ++it)
			ovvt[it] = ovv[ncdp-1][it];
	
	/* else, linearly interpolate */
	} else {
		xindex(ncdp,cdp,cdpt,&index);
		a1 = (cdp[index+1]-cdpt)/(cdp[index+1]-cdp[index]);
		a2 = (cdpt-cdp[index])/(cdp[index+1]-cdp[index]);
		for (it=0; it<nt; ++it)
			ovvt[it] = a1*ovv[index][it]+a2*ovv[index+1][it];
	}
}
