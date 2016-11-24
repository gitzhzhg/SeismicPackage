/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUNMO: $Revision: 1.5 $ ; $Date: 2012/01/03 19:45:15 $		*/
 
#include "su.h"
#include "segy.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"									     ",
" SUNMO_a - NMO for an arbitrary velocity function of time and CDP with	     ",
"		experimental Anisotropy options				     ",
"  sunmo <stdin >stdout [optional parameters]				     ",
"									     ",
" Optional Parameters:							     ",
" tnmo=0,...		NMO times corresponding to velocities in vnmo	     ",
" vnmo=1500,..		NMO velocities corresponding to times in tnmo	     ",
" anis1=0		two anisotropy coefficients making up quartic term   ",
" anis2=0		in traveltime curve, corresponding to times in tnmo  ",
" cdp=			CDPs for which vnmo & tnmo are specified (see Notes) ",
" smute=1.5		samples with NMO stretch exceeding smute are zeroed  ",
" lmute=25		length (in samples) of linear ramp for stretch mute  ",
" sscale=1		=1 to divide output samples by NMO stretch factor    ",
" invert=0		=1 to perform (approximate) inverse NMO		     ",
" upward=0		=1 to scan upward to find first sample to kill	",
"									     ",
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
" Anisotropy option:							     ",
" Caveat, this is an experimental option,				     ",
"									     ",
" The anisotropy coefficients anis1, anis2 permit non-hyperbolicity due	     ",
" to layering, mode conversion, or anisotropy. Default is isotropic NMO.     ",
"									     ",
" The same holds for the anisotropy coefficients as a function of time and   ",
" CDP.									     ",
"									     ",
" Moveout is defined by							     ",
"									     ",
"   1		 anis1							     ",
"  --- x^2 + ------------- x^4.						     ",
"  v^2	     1 + anis2 x^2						     ",
"									     ",
" Note: In general, the user should set the cdp parameter.  The default is   ",
"	to use tr.cdp from the first trace and assume only one cdp.	  ",
" Caveat:								     ",
" Nmo cannot handle negative moveout as in triplication caused by	     ",
" anisotropy. But negative moveout happens necessarily for negative anis1 at ",
" sufficiently large offsets. Then the error-negative moveout- is printed.   ",
" Check anis1. An error (anis2 too small) is also printed if the	     ",
" denominator of the quartic term becomes negative. Check anis2. These errors",
" are prompted even if they occur in traces which would not survive the	     ",
" NMO-stretch threshold. Chop off enough far-offset traces (e.g. with suwind)",
" if anis1, anis2 are fine for near-offset traces.			     ",
"									     ",
NULL};

/* Credits:
 *	SEP: Shuki, Chuck Sword
 *	CWP: Shuki, Jack, Dave Hale, Bjoern Rommel
 *      Modified: 08/08/98 - Carlos E. Theodoro - option for lateral offset
 *      Modified: 07/11/02 - Sang-yong Suh -
 *	  added "upward" option to handle decreasing velocity function.
 *      CWP: Sept 2010: John Stockwell
 *	  replaced Carlos Theodoro's fix
 *	  and added the instruction in the selfdoc to use suazimuth to set 
 *	    offset so that it accounts for lateral offset
 *      note that by the segy standard "scalel" does not scale the offset
 *      field
 * Technical Reference:
 *	The Common Depth Point Stack
 *	William A. Schneider
 *	Proc. IEEE, v. 72, n. 10, p. 1238-1254
 *	1984
 *
 * Trace header fields accessed: ns, dt, delrt, offset, cdp, scalel
 */
/**************** end self doc *******************************************/

static void interpovv_a (int nt, int ncdp, float *cdp, 
	float **ovv, float **oa1, float **oa2, float cdpt, 
	float *ovvt, float *oa1t, float *oa2t);

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

	int nanis1;		/* number of anis1's specified */
	int nanis2;		/* number of anis2's specified */
	float *anis1=NULL;	/* array[nanis1] of anis1's */
	float *anis2=NULL;	/* array[nanis2] of anis2's */
	float **oa1=NULL;	/* array[ncdp][nt] of anis1 functions */
	float **oa2=NULL;	/* array[ncdp][nt] of anis2 functions */
	float *oa1t=NULL;	/* array[nt] of anis1 for a particular trace */
	float *oa2t=NULL;	/* array[nt] of anis2 for a particular trace */

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
	float *aovv=NULL;	/* temporary used to sort ovv array */
	float *aoa1=NULL;	/* temporary used to sort oa1 array */
	float *aoa2=NULL;	/* temporary used to sort oa2 array */

	float temp;		/* temporary float */
	float tsq;		/* temporary float */
	int i;			/* index used in loop */
	int upward;		/* scans upward if it's nonzero. */

	float offset;		/* value of offset honoring scalel */

	/* hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = ((double) tr.dt)/1000000.0;
	ft = tr.delrt/1000.0;

	/* get velocity functions, linearly interpolated in time */
	ncdp = countparval("cdp");
	if (ncdp>0) {
		if (countparname("vnmo")!=ncdp)
			err("a vnmo array must be specified for each cdp");
		if (countparname("tnmo")!=ncdp)
			err("a tnmo array must be specified for each cdp");
		if (countparname("anis1")!=ncdp &&
		    countparname("anis1")!=0)
			err("an anis1 array must be specified for each cdp, "
			    "or omitted at all");
		if (countparname("anis2")!=ncdp &&
		    countparname("anis2")!=0)
			err("an anis2 array must be specified for each cdp, "
			    "or omitted at all");
	} else {
		ncdp = 1;
		if (countparname("vnmo")>1)
			err("only one (or no) vnmo array must be specified");
		if (countparname("tnmo")>1)
			err("only one (or no) tnmo array must be specified");
		if (countparname("anis1")>1)
			err("only one (or no) anis1 array must be specified");
		if (countparname("anis2")>1)
			err("only one (or no) anis2 array must be specified");
	}
	cdp = ealloc1float(ncdp);
	if (!getparfloat("cdp",cdp)) cdp[0] = tr.cdp;
	ovv = ealloc2float(nt,ncdp);
	oa1 = ealloc2float(nt,ncdp);
	oa2 = ealloc2float(nt,ncdp);
	for (icdp=0; icdp<ncdp; ++icdp) {

		nvnmo = countnparval(icdp+1,"vnmo");
		ntnmo = countnparval(icdp+1,"tnmo");
		nanis1 = countnparval(icdp+1,"anis1");
		nanis2 = countnparval(icdp+1,"anis2");

		if (nvnmo!=ntnmo && !(ncdp==1 && nvnmo==1 && ntnmo==0))
			err("number of vnmo and tnmo values must be equal");
		if (nanis1!=nvnmo && nanis1 != 0)
			err("number of vnmo and anis1 values must be equal");
		if (nanis2!=nvnmo && nanis2 != 0)
			err("number of vnmo and anis2 values must be equal");

		if (nvnmo==0) nvnmo = 1;
		if (ntnmo==0) ntnmo = nvnmo;
		if (nanis1==0) nanis1 = nvnmo;
		if (nanis2==0) nanis2 = nvnmo;

		/* equal numbers of parameters vnmo, tnmo, anis1, anis2 */
		vnmo = ealloc1float(nvnmo);
		tnmo = ealloc1float(nvnmo);
		anis1 = ealloc1float(nvnmo);
		anis2 = ealloc1float(nvnmo);

		if (!getnparfloat(icdp+1,"vnmo",vnmo)) vnmo[0] = 1500.0;
		if (!getnparfloat(icdp+1,"tnmo",tnmo)) tnmo[0] = 0.0;
		if (!getnparfloat(icdp+1,"anis1",anis1)) 
			for (i=0; i<nvnmo; i++) anis1[i] = 0.0;
		if (!getnparfloat(icdp+1,"anis2",anis2))
			for (i=0; i<nvnmo; i++) anis2[i] = 0.0;
		for (it=1; it<ntnmo; ++it)
			if (tnmo[it]<=tnmo[it-1])
				err("tnmo values must increase monotonically");
		for (it=0,tn=ft; it<nt; ++it,tn+=dt) {
			intlin(ntnmo,tnmo,vnmo,vnmo[0],vnmo[nvnmo-1],1,&tn,&v);
			ovv[icdp][it] = 1.0/(v*v);
		}
		for (it=0,tn=ft; it<nt; ++it,tn+=dt) {
			intlin(ntnmo,tnmo,anis1,anis1[0],anis1[nanis1-1],1,&tn,
				&oa1[icdp][it]);
		}
		for (it=0,tn=ft; it<nt; ++it,tn+=dt) {
			intlin(ntnmo,tnmo,anis2,anis2[0],anis2[nanis2-1],1,&tn,
				&oa2[icdp][it]);
		}
		free1float(vnmo);
		free1float(tnmo);
		free1float(anis1);
		free1float(anis2);
	}

	/* sort (by insertion) sloth and anis functions by increasing cdp */
	for (jcdp=1; jcdp<ncdp; ++jcdp) {
		acdp = cdp[jcdp];
		aovv = ovv[jcdp];
		aoa1 = oa1[jcdp];
		aoa2 = oa2[jcdp];
		for (icdp=jcdp-1; icdp>=0 && cdp[icdp]>acdp; --icdp) {
			cdp[icdp+1] = cdp[icdp];
			ovv[icdp+1] = ovv[icdp];
			oa1[icdp+1] = oa1[icdp];
			oa2[icdp+1] = oa2[icdp];
		}
		cdp[icdp+1] = acdp;
		ovv[icdp+1] = aovv;
		oa1[icdp+1] = aoa1;
		oa2[icdp+1] = aoa2;
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
	oa1t = ealloc1float(nt);
	oa2t = ealloc1float(nt);
	ttn = ealloc1float(nt);
	atn = ealloc1float(nt);
	qtn = ealloc1float(nt);
	tnt = ealloc1float(nt);
	at = ealloc1float(nt);
	qt = ealloc1float(nt);

	/* interpolate sloth and anis function for first trace */
	interpovv_a(nt,ncdp,cdp,ovv,oa1,oa2,(float)tr.cdp,ovvt,oa1t,oa2t);

	/* set old cdp and old offset for first trace */
	oldcdp = tr.cdp;
	oldoffset = tr.offset-1; /* here offset is used as a marker */
				 /* there is no need to have it honor scalel */

	/* loop over traces */
	do {
		/* if necessary, compute new sloth and anis function */
		if (tr.cdp!=oldcdp && ncdp>1) {
			interpovv_a(nt,ncdp,cdp,ovv,oa1,oa2,(float)tr.cdp,
				  ovvt,oa1t,oa2t);
			newsloth = 1;
		} else {
			newsloth = 0;
		}

		/* if sloth and anis function or offset has changed */
		if (newsloth || tr.offset!=oldoffset) {

			offset = (float) (tr.offset);
			

			/* compute time t(tn) (normalized) */
			temp = ((float) offset*offset)/(dt*dt);
			for (it=0,tn=ft/dt; it<nt; ++it,tn+=1.0) {
				tsq = temp*ovvt[it] + \
				      oa1t[it]*temp*temp / (1.0+oa2t[it]*temp);
				if (tsq<0.0)
					err("negative moveout; check anis1, "
					    "anis2, or suwind far-offset "
					    "traces");
				if ((1.0+oa2t[it]*temp)<=0.0)
					err("anis2 negative and too small; "
					    "check anis2, or suwind far-offset"
					    " traces");
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

	return(CWP_Exit());
}


/* linearly interpolate/extrapolate sloth and anis between cdps */
static void interpovv_a (int nt, int ncdp, float *cdp, float **ovv, float **oa1, 
	float **oa2, float cdpt, float *ovvt, float *oa1t, float *oa2t)
{
	static int indx=0;
	int it;
	float a1,a2;

	/* if before first cdp, constant extrapolate */
	if (cdpt<=cdp[0]) {
		for (it=0; it<nt; ++it) {
			ovvt[it] = ovv[0][it];
			oa1t[it] = oa1[0][it];
			oa2t[it] = oa2[0][it];
		      };
	
	/* else if beyond last cdp, constant extrapolate */
	} else if (cdpt>=cdp[ncdp-1]) {
		for (it=0; it<nt; ++it) {
			ovvt[it] = ovv[ncdp-1][it];
			oa1t[it] = oa1[ncdp-1][it];
			oa2t[it] = oa2[ncdp-1][it];
		      };
	
	/* else, linearly interpolate */
	} else {
		xindex(ncdp,cdp,cdpt,&indx);
		a1 = (cdp[indx+1]-cdpt)/(cdp[indx+1]-cdp[indx]);
		a2 = (cdpt-cdp[indx])/(cdp[indx+1]-cdp[indx]);
		for (it=0; it<nt; ++it) {
			ovvt[it] = a1*ovv[indx][it]+a2*ovv[indx+1][it];
			oa1t[it] = a1*oa1[indx][it]+a2*oa1[indx+1][it];
			oa2t[it] = a1*oa2[indx][it]+a2*oa2[indx+1][it];
		      };
	}
}
