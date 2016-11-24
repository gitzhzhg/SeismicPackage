/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUTAUPNMO: $Revision: 1.3 $ ; $Date: 2011/11/16 23:21:55 $		*/
 
#include "su.h"
#include "segy.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUTAUPNMO - NMO for an arbitrary velocity function of tau and CDP	",
"									",
"  sutaupnmo <stdin >stdout [optional parameters]			",
"									",
" Optional Parameters:							",
" tnmo=0,...		NMO times corresponding to velocities in vnmo	",
" vnmo=1500,...		NMO velocities corresponding to times in tnmo	",
" cdp=			CDPs for which vnmo & tnmo are specified (see Notes) ",
" smute=1.5		samples with NMO stretch exceeding smute are zeroed  ",
" lmute=25		length (in samples) of linear ramp for stretch mute  ",
" sscale=1		=1 to divide output samples by NMO stretch factor    ",
"									",
" Notes:								",
"									",
" For constant-velocity NMO, specify only one vnmo=constant and omit tnmo.",
"									",
" For NMO with a velocity function of tau only, specify the arrays	",
"	   vnmo=v1,v2,... tnmo=t1,t2,...				",
" where v1 is the velocity at tau t1, v2 is the velocity at tau t2, ...    ",
" The taus specified in the tnmo array must be monotonically increasing.    ",
" Linear interpolation and constant extrapolation of the specified velocities",
" is used to compute the velocities at taus not specified.		",
"									",
" For NMO with a velocity function of tau and CDP, specify the array	",
"	   cdp=cdp1,cdp2,...						",
" and, for each CDP specified, specify the vnmo and tnmo arrays as described ",
" above. The first (vnmo,tnmo) pair corresponds to the first cdp, and so on. ",
" Linear interpolation and constant extrapolation of velocity^2 is used	 ",
" to compute velocities at CDPs not specified.				",
"									",
" Moveout is defined by							",
"									",
"  tau^2 + tau^2.p^2.vel^2						",
"									",
" Note: In general, the user should set the cdp parameter.  The default is   ",
"	to use tr.cdp from the first trace and assume only one cdp.	 ",
" Caveat:								",
" Taunmo should handle triplication					",
"									",
" NMO interpolation error is less than 1% for frequencies less than 60% of   ",
" the Nyquist frequency.						",
"									",
" Exact inverse NMO is not implemented, nor has anisotropy		",
" Example implementation:						",
"   sutaup dx=25 option=2 pmin=0 pmax=0.0007025 < cmpgather.su |	",
"   supef minlag=0.2 maxlag=0.8 |					",
"   sutaupnmo tnmo=0.5,2,4 vnmo=1500,2000,3200 smute=1.5 |		",
"   sumute key=tracr mode=1 ntaper=20 xmute=1,30,40,50,85,15  		",
"				 tmute=7.8,7.8,4.5,3.5,2.0,0.35 |	",
"   sustack key=cdp | ... [...]						",
"									",
NULL};

/* Credits:
 *	 Durham, Richard Hobbs modified from SUNMO credited below
 *	SEP: Shuki Ronen, Chuck Sword
 *	CWP: Shuki Ronen, Jack K. Cohen , Dave Hale
 *
 * Technical Reference:
 *	van der Baan papers in geophysics (2002 & 2004)
 *
 * Trace header fields accessed: ns, dt, delrt, offset, cdp, sy
 */
/**************** end self doc *******************************************/

static void interpvv (int nt, int ncdp, float *cdp, 
	float **vv, float cdpt, float *vvt);

segy tr;

int
main(int argc, char **argv)
{
	int nt;		/* number of time samples per trace */
	float dt;	/* time sampling interval */
	float ft;	/* time of first sample */
	int it;		/* time sample index */
	int ncdp;	/* number of cdps specified */
	float *cdp=NULL;	/* array[ncdp] of cdps */
	int icdp;	/* index into cdp array */
	int jcdp;	/* index into cdp array */
	int nvnmo;	/* number of vnmos specified */
	float *vnmo=NULL;	/* array[nvnmo] of vnmos */
	int ntnmo;	/* number of tnmos specified */
	float *tnmo=NULL;	/* array[ntnmo] of tnmos */
	float **vv=NULL; /* array[ncdp][nt] of vel (velocity^2) functions */
	float *vvt=NULL;	/* array[nt] of vel2 for a particular trace */
	float smute;	/* zero samples with NMO stretch exceeding smute */
	float osmute;	/* 1/smute */

	int lmute;	/* length in samples of linear ramp for mute */
	int itmute=0;	/* zero samples with indices less than itmute */
	int sscale;	/* if non-zero, apply NMO stretch scaling */
	long oldcdp;	/* cdp of previous trace */
	int newvel2;	/* if non-atzero, new vel2 function was computed */

	float tn;	/* NMO time (time after NMO correction) */
	float v;	/* velocity */
	float newp;	/* current ray parameter */
	float oldp;	/* previous ray parameter */

	float *qtn=NULL;	/* NMO-corrected trace q(tn) */
	float *ttn=NULL;	/* time t(tn) for NMO */
	float *atn=NULL;	/* amplitude a(tn) for NMO */
	float *qt=NULL;	/* inverse NMO-corrected trace q(t) */
	float *tnt=NULL;	/* time tn(t) for inverse NMO */
	float *at=NULL;	/* amplitude a(t) for inverse NMO */

	float acdp;	/* temporary used to sort cdp array */
	float *avv=NULL;	/* temporary used to sort vv array */
	float tsq;	/* temporary float */

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
	} else {
		ncdp = 1;
		if (countparname("vnmo")>1)
			err("only one (or no) vnmo array must be specified");
		if (countparname("tnmo")>1)
			err("only one (or no) tnmo array must be specified");
	}
	cdp = ealloc1float(ncdp);
	if (!getparfloat("cdp",cdp)) cdp[0] = tr.cdp;
	vv = ealloc2float(nt,ncdp);
	for (icdp=0; icdp<ncdp; ++icdp) {
		nvnmo = countnparval(icdp+1,"vnmo");
		ntnmo = countnparval(icdp+1,"tnmo");
		if (nvnmo!=ntnmo && !(ncdp==1 && nvnmo==1 && ntnmo==0))
			err("number of vnmo and tnmo values must be equal");
		if (nvnmo==0) nvnmo = 1;
		if (ntnmo==0) ntnmo = nvnmo;
		/* equal numbers of parameters vnmo, tnmo, anis1, anis2 */
		vnmo = ealloc1float(nvnmo);
		tnmo = ealloc1float(nvnmo);
		if (!getnparfloat(icdp+1,"vnmo",vnmo)) vnmo[0] = 1500.0;
		if (!getnparfloat(icdp+1,"tnmo",tnmo)) tnmo[0] = 0.0;
		for (it=1; it<ntnmo; ++it)
			if (tnmo[it]<=tnmo[it-1])
				err("tnmo values must increase monotonically");
		for (it=0,tn=ft; it<nt; ++it,tn+=dt) {
			intlin(ntnmo,tnmo,vnmo,vnmo[0],vnmo[nvnmo-1],1,&tn,&v);
			vv[icdp][it] = v*v;
		}
		free1float(vnmo);
		free1float(tnmo);
	}

	/* sort (by insertion) vel2 functions by increasing cdp */
	for (jcdp=1; jcdp<ncdp; ++jcdp) {
		acdp = cdp[jcdp];
		avv = vv[jcdp];
		for (icdp=jcdp-1; icdp>=0 && cdp[icdp]>acdp; --icdp) {
			cdp[icdp+1] = cdp[icdp];
			vv[icdp+1] = vv[icdp];
		}
		cdp[icdp+1] = acdp;
		vv[icdp+1] = avv;
	}

	/* get other optional parameters */
	if (!getparfloat("smute",&smute)) smute = 1.5;
	if (smute<=0.0) err("smute must be greater than 0.0");
	if (!getparint("lmute",&lmute)) lmute = 25;
	if (!getparint("sscale",&sscale)) sscale = 1;
        checkpars();

	/* allocate workspace */
	vvt = ealloc1float(nt);
	ttn = ealloc1float(nt);
	atn = ealloc1float(nt);
	qtn = ealloc1float(nt);
	tnt = ealloc1float(nt);
	at = ealloc1float(nt);
	qt = ealloc1float(nt);

	/* interpolate vel2 and anis function for first trace */
	interpvv(nt,ncdp,cdp,vv,(float)tr.cdp,vvt);

	/* set old cdp and old p for first trace */
	oldcdp = tr.cdp;
	oldp = -99999.0;

	/* loop over traces */
	do {
		/* if necessary, compute new vel2 function */
		if (tr.cdp!=oldcdp && ncdp>1) {
			interpvv(nt,ncdp,cdp,vv,(float)tr.cdp,vvt);
			newvel2 = 1;
		} else {
			newvel2 = 0;
		}

		/* if vel2 and anis function or offset has changed */
		newp = tr.f2 + ((float)tr.tracr-1.0) * tr.d2;
		if (newvel2 || newp!=oldp) {
			/* compute time t(tn) (normalized) */
			for (it=0,tn=ft/dt; it<nt; ++it,tn+=1.0) {
				tsq = tn*tn - newp*newp*tn*tn*vvt[it];
				if (tsq < 0.0) {
					ttn[it] = 0;
				} else {	
					ttn[it] = sqrt(tsq);
				}
			}

			/* compute inverse of stretch factor a(tn) */
			atn[0] = ttn[1]-ttn[0];
			for (it=1; it<nt; ++it) {
				atn[it] = ttn[it]-ttn[it-1];
			}
			/* determine index of first sample to survive mute */
			osmute = 1.0/smute;
			for (it=0; it<nt-1 && atn[it]>osmute; ++it) 
				;
			itmute = it;
		}
		
		/* forward nmo */
		/* do nmo via 8-point sinc interpolation */
		ints8r(nt,1.0,ft/dt,tr.data,0.0,0.0,
		itmute,&ttn[0],&qtn[0]);
			
		/* apply mute */
		for (it=itmute; it<nt; ++it)
			qtn[it] = 0.0;
			
		/* apply linear ramp */
		for (it=itmute-lmute; it<itmute && it<nt; ++it)
			qtn[it] *= (float)(itmute-it)/(float)lmute;
			
		/* if specified, scale by the NMO stretch factor */
		if (sscale)
			for (it=itmute; it<nt; ++it)
				qtn[it] *= atn[it];
			
		/* copy NMO corrected trace to output trace */
		memcpy( (void *) tr.data,
			(const void *) qtn, nt*sizeof(float));

		/* write output trace */
		puttr(&tr);

		/* remember offset and cdp */
		oldp = newp;
		oldcdp = tr.cdp;

	} while (gettr(&tr));

	return(CWP_Exit());
}


/* linearly interpolate/extrapolate vel2 between cdps */
static void interpvv (int nt, int ncdp, float *cdp, float **vv, float cdpt, float *vvt)
{
	static int indx=0;
	int it;
	float a1,a2;

	/* if before first cdp, constant extrapolate */
	if (cdpt<=cdp[0]) {
		for (it=0; it<nt; ++it) {
			vvt[it] = vv[0][it];
			 };
	
	/* else if beyond last cdp, constant extrapolate */
	} else if (cdpt>=cdp[ncdp-1]) {
		for (it=0; it<nt; ++it) {
			vvt[it] = vv[ncdp-1][it];
			 };
	
	/* else, linearly interpolate */
	} else {
		xindex(ncdp,cdp,cdpt,&indx);
		a1 = (cdp[indx+1]-cdpt)/(cdp[indx+1]-cdp[indx]);
		a2 = (cdpt-cdp[indx])/(cdp[indx+1]-cdp[indx]);
		for (it=0; it<nt; ++it) {
			vvt[it] = a1*vv[indx][it]+a2*vv[indx+1][it];
			 };
	}
}
