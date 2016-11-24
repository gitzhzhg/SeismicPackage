/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUBACKUSH: $Revision: 1.6 $ ; $Date: 2011/11/16 17:24:58 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUBACKUSH - calculate Thomsen anisotropy parameters from 	",
" 	     well log (vp,vs,rho) data and optionally include	",
" 	     intrinsic VTI shale layers based on gramma ray log	",
" 	     via BACKUS averaging				",
" subackush < vp_vs_rho.su >stdout [options]			",
" subackush < vp_vs_rho_gr.su  gr=1 >stdout [options]		",
" 								",
" Required parameters:						",
" none								",
"								",
" Optional parameter:						",
" navg=101	number of depth samples in Backus avg window 	",
"								",
" 	Intrinsic anisotropy of shale layers can be included ...",
" gr=0		no gamma ray log input for shale 		",
"		=1 input is vp_vs_rho_gr			",
" grs=100	pure shale gamma ray value (API units)		",
" grc=10	0% shale gamma ray value (API units)		",
" smode=1	include shale anis params prop to shale volume 	",
"		=0 include shale anis for pure shale only	",
" se=0.209	shale epsilon (Thomsen parameter)		",
" sd=0.033	shale delta (Thomsen parameter)			",
" sg=0.203	shale gamma (Thomsen parameter)			",
"								",
" Notes:							",
" 1. Input are (vp,vs,rho) traces in metric units		",
" 2. Output are  						",
"    tracl	=(1,2,3,4,5,6)					",
"    quantity	=(vp0,vs0,<rho>,epsilon,delta,gamma) 		",
"    units	=(m/s,m/s,kg/m^3,nd,nd,nd) nd=dimensionless	",
"    tracl	=(7,8)						",
"    quantity	=(Vsh,shaleEps) Vsh=shale volume fraction	",
"    units	=(nd,nd) 					",
" 3. (epsilon,delta,etc.) can be isolated by tracl header field ",
" 4. (vp0,vs0) are backus averaged vertical wavespeeds		",
" 5. <rho> is backus averaged density, etc.			",
"								",
" Example:							",
" las2su < logs.las nskip=34 nlog=4 > logs.su 			",
" suwind < logs.su  key=tracl min=2 max=3 | suop op=s2vm > v.su	",
" suwind < logs.su  key=tracl min=4 max=4 | suop op=d2m > d.su	",
" fcat v.su d.su > vp_vs_rho.su					",
" subackus < vp_vs_rho.su > vp0_vs0_rho_eps_delta_gamma.su	",
" In this example we start with a well las file containing 	",
" 34 header lines and 4 log tracks (depth,p_son,s_son,den).	",
" This is converted to su format by las2su.  Then we pull off	",
" the sonic logs and convert them to velocity in metric units.	",
" Then the density log is pulled off and converted to metric.	",
" All three metric curves are bundled into one su file which 	",
" is the input to subackus. 					", 
"								",
" Related programs: subackus, sulprime				",
"								",
NULL};

/* Credits:
 *
 *	UHouston: Chris Liner 
 *              I gratefully acknowledge Saudi Aramco for permission
 *              to release this code developed while I worked for the 
 *              EXPEC-ARC research division.
 *
 * References:
 * Anisotropy parameters: Thomsen, 2002, DISC Notes (SEG)
 * Backus Method: Berryman, Grechka, and Berge, 1997, SEP94
 * Shale params: Wang, 2002, Geophysics, p. 1427	
 */
/**************** end self doc ***********************************/

static void dobackus(int navg, int nz, float *p, float *avg);
static void handlEnds(int navg, int nz, float *p);

segy tr;

int
main(int argc, char **argv) {
	float *vp;		/* p-wave velocity model 	*/
	float *vs;		/* s-wave velocity model 	*/
	float *vp0;		/* vertical p-wave speed 	*/
	float *vs0;		/* vertical s-wave speed 	*/
	int nz;			/* number of depth levels in log*/
	int i;			/* counters 			*/
	int navg;		/* length of averaging window	*/
	float *tmp,*btmp,*tmp3;	/* temp array for avg subroutine */
	float d1,f1,dt;
	float *ao=NULL,*fo=NULL,*co=NULL,*lo=NULL,*mo=NULL;	
		/* original stiffness params from logs 	*/
	float *ab=NULL,*fb=NULL,*cb=NULL,*lb=NULL,*mb=NULL;
		/* backus-average stiffness params	*/
	float *rhoo=NULL, *rhob=NULL;	/* orig and backus avg density	*/
	float epso,deltao,gammao; 	/* orig thomsen params 	*/
	float *epsb,*deltab,*gammab;	/* backus avg thomsen params	*/ 
	float ftmp,ftmp1,ftmp2,ftmp3;	/* temp float variables		*/
	int gr;			/* gamma ray log input flag 	*/
	float *grl=NULL;	/* gamma ray log values		*/
	float *vsh;		/* shale volume (z)		*/
	float se,sd,sg;		/* shale anisotropy params	*/
	float grs;		/* pure shale gr value 		*/
	float grc;		/* 0% shale gr value   		*/
	int smode;		/* shale mode 1=propToVsh 0=pureShaleOnly */

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* get parameters */
	if (!getparint("navg",&navg)) 	navg = 101;
	if (!getparint("gr",&gr)) 	gr   = 0;
	if (!getparfloat("grs",&grs)) 	grs  = 100.0;
	if (!getparfloat("grc",&grc)) 	grc  = 10.0;
	if (!getparfloat("se",&se)) 	se   = 0.209;
	if (!getparfloat("sd",&sd)) 	sd   = 0.033;
	if (!getparfloat("sg",&sg)) 	sg   = 0.203;
	if (!getparint("smode",&smode)) smode = 1;
        checkpars();

	/* make sure it is odd */
	if (navg != 201) navg = navg/2*2 + 1;

	/* Get information from first trace */
	if (!gettr(&tr)) err("can't get first trace");
	nz = tr.ns;
	d1 = tr.d1;
	f1 = tr.f1;
	dt = tr.dt;

	/* alloc input model vectors */
	vp 	= alloc1float(nz);
	vs 	= alloc1float(nz);
	rhoo 	= alloc1float(nz);
	grl 	= alloc1float(nz);
	vsh 	= alloc1float(nz);

	/* load input trace 1 into vp */
	for (i = 0; i < nz; ++i) {
		vp[i]  = tr.data[i];
	}

	/* load input trace 2 into vs */
	gettr(&tr);
	for (i = 0; i < nz; ++i) {
		vs[i]  = tr.data[i];
	}

	/* load input trace 3 into rho */
	gettr(&tr);
	for (i = 0; i < nz; ++i) {
		rhoo[i]  = tr.data[i];
	}
	
	if (gr == 1) {
		/* load input trace 4 into grl */
		gettr(&tr);
		for (i = 0; i < nz; ++i) {
			grl[i]  = tr.data[i];
		}
		
		/* calc shale volume 
		   vsh >= 1 for pure shale*/
		for (i = 0; i < nz; ++i) {
			ftmp = (grl[i] - grc)/(grs - grc);
			vsh[i] = 0.33 * ( pow(2,2*ftmp) - 1);
			/* vsh > 1 is pure shale */
			if ( vsh[i] > 1.0 ) vsh[i] = 1.0;
			/* vsh < 0 has no shale */
			if ( vsh[i] < 0.0 ) vsh[i] = 0.0;
		}
	}
	
	

	/* allocate other model vectors */
	/* orig quantities */
	ao 	= alloc1float(nz);
	fo 	= alloc1float(nz);
	co 	= alloc1float(nz);
	lo 	= alloc1float(nz);
	mo 	= alloc1float(nz);
	/* averaged quantities */
	ab 	= alloc1float(nz);
	fb 	= alloc1float(nz);
	cb 	= alloc1float(nz);
	lb 	= alloc1float(nz);
	mb 	= alloc1float(nz);
	rhob 	= alloc1float(nz);
	/* misc                */
	tmp 	= alloc1float(nz);
	btmp 	= alloc1float(nz);
	tmp3 	= alloc1float(nz);
	epsb 	= alloc1float(nz);
	deltab 	= alloc1float(nz);
	gammab 	= alloc1float(nz);
	vp0 	= alloc1float(nz);
	vs0 	= alloc1float(nz);

	/* fill orig stiffness arrays */
	for (i = 0; i < nz; ++i) {
	
		/* default is isotropic layers */
		epso   = 0.0;
		deltao = 0.0;
		gammao = 0.0;
		
		/* allow for shale intrinsic anis if requested */
		if (gr == 1) {
			/* only include anis from pure shale */
			if (smode == 0 && vsh[i] >= 1) {
				epso   = se;
				deltao = sd;
				gammao = sg;
			} else {
				epso   = 0.0;
				deltao = 0.0;
				gammao = 0.0;
			}
			/* only include anis from partial shale */
			if (smode == 1 ) {
				epso   = se * vsh[i];
				deltao = sd * vsh[i];
				gammao = sg * vsh[i];
			} 
		}
		/* orig stiffnesses */
		ao[i]	= (1.0 + 2.0 * epso ) * rhoo[i] * vp[i] * vp[i];
		ftmp	= (vp[i] * vp[i] - vs[i] * vs[i]);
		ftmp	= ftmp * ( (1.0 + 2.0 * deltao) * vp[i]*vp[i] - vs[i]*vs[i]);
		fo[i]	= rhoo[i] * ( sqrt(ftmp) - vs[i] * vs[i] );
		co[i]	= rhoo[i] * vp[i] * vp[i];
		lo[i] 	= rhoo[i] * vs[i] * vs[i];
		mo[i]	= (1.0 + 2.0 * gammao) * rhoo[i]*vs[i]*vs[i];
	}
	
	/*********begin: calc backus avg quantities **********************/
	
	/* calc C=cb */
	for (i = 0; i < nz; ++i) {
		tmp[i] = 1.0/co[i];
	}
	dobackus(navg,nz,tmp,cb);
	for (i = 0; i < nz; ++i) {
		cb[i] = 1.0/cb[i];
	}
	
	/* calc F=fb ... uses cb*/
	for (i = 0; i < nz; ++i) {
		tmp[i] = fo[i]/co[i];
	}
	dobackus(navg,nz,tmp,btmp);
	for (i = 0; i < nz; ++i) {
		fb[i] = cb[i] * btmp[i];
		/* needed for ab below */
		tmp3[i] = btmp[i] * btmp[i];
	}
	
	/* calc A=ab ... uses fb*/
	for (i = 0; i < nz; ++i) {
		tmp[i] = ao[i] - fo[i]*fo[i]/co[i];
	}
	dobackus(navg,nz,tmp,btmp);
	for (i = 0; i < nz; ++i) {
		ab[i] = btmp[i] + cb[i] * tmp3[i];
	}
		
	/* calc L=lb */
	for (i = 0; i < nz; ++i) {
		tmp[i] = 1.0/lo[i];
	}
	dobackus(navg,nz,tmp,lb);
	for (i = 0; i < nz; ++i) {
		lb[i] = 1.0/lb[i];
	}
	
	/* calc M=mb */
	dobackus(navg,nz,mo,mb);

	/* calc rhob (averaged density)	*/
	dobackus(navg,nz,rhoo,rhob);
	
	/*********end:   calc backus avg quantities **********************/

	/* calculate anis params and vertical wave speeds 	*/
	for (i = 0; i < nz; ++i) {
		vp0[i] = sqrt( cb[i] / rhob[i] );
		vs0[i] = sqrt( lb[i] / rhob[i] );
		epsb[i] = (ab[i] - cb[i]) / (2.0 * cb[i]);
		ftmp1 = (fb[i] + lb[i]) * (fb[i] + lb[i]);
		ftmp2 = (cb[i] - lb[i]) * (cb[i] - lb[i]);
		ftmp3 = 2.0 * cb[i] * (cb[i] - lb[i]);
		deltab[i] = (ftmp1 - ftmp2) / ftmp3;
		gammab[i] = (mb[i] - lb[i]) / (2.0 * lb[i]);
	}

	/* set up output trace headers */
	tr.trid = 1;			/* su time traces (trick) */
	tr.ns = nz;			/* samples per trace */
	tr.dt = dt;			/* time sample rate (trick) */
	tr.d1 = d1;		
	tr.f1 = f1;

	/* write vp0 trace */
	tr.tracl = 1;
	for (i = 0 ; i < nz ; ++i){
		tr.data[i] = vp0[i];
	}
	handlEnds(navg,nz,tr.data);
	puttr(&tr);

	/* write vs0 trace */
	tr.tracl = 2;
	for (i = 0 ; i < nz ; ++i){
		tr.data[i] = vs0[i];
	}
	handlEnds(navg,nz,tr.data);
	puttr(&tr);

	/* write avg'd density trace */
	tr.tracl = 3;
	for (i = 0 ; i < nz ; ++i){
		tr.data[i] = rhob[i];
	}
	handlEnds(navg,nz,tr.data);
	puttr(&tr);

	/* write epsilon trace */
	tr.tracl = 4;
	for (i = 0 ; i < nz ; ++i){
		tr.data[i] = epsb[i];
	}
	handlEnds(navg,nz,tr.data);
	puttr(&tr);

	/* write delta trace */
	tr.tracl = 5;
	for (i = 0 ; i < nz ; ++i){
		tr.data[i] = deltab[i];
	}
	handlEnds(navg,nz,tr.data);
	puttr(&tr);

	/* write gamma trace */
	tr.tracl = 6;
	for (i = 0 ; i < nz ; ++i){
		tr.data[i] = gammab[i];
	}
	handlEnds(navg,nz,tr.data);
	puttr(&tr);

	/* write vsh trace */
	tr.tracl = 7;
	for (i = 0; i < nz; ++i) {
		tr.data[i]= vsh[i];
	}
	puttr(&tr);

	/* write shale anisotropy (eps) */
	tr.tracl = 8;
	for (i = 0; i < nz; ++i) {
		/* only include anis from pure shale */
		if (smode == 0 && vsh[i] >= 1) {
			epso   = se;
			deltao = sd;
			gammao = sg;
		} else {
			epso   = 0.0;
			deltao = 0.0;
			gammao = 0.0;
		}
		/* only include anis from partial shale */
		if (smode == 1 && vsh[i] < 1) {
			epso   = se * vsh[i];
			deltao = sd * vsh[i];
			gammao = sg * vsh[i];
		} else {
			/* vsh > 1 is pure shale */
			epso   = se;
			deltao = sd;
			gammao = sg;
		}
		tr.data[i]= epso;
	}
	puttr(&tr);


	return(CWP_Exit());
}

static void handlEnds(int navg, int nz, float *p)
/*****************************************************************************
copy first valid backus value on each end for a distance of navg/2
*****************************************************************************/
{
	int i, m;
	float gt, gb;

	/* half width of rms window */
	m = (navg-1)/2 + 1;
	
	/* good value at top and bottom */
	gt = p[m];
	gb = p[nz-m];
	
	/* fix top */
	for (i = 0; i < m; ++i) {
		p[i] = gt;
	}
	
	/* fix bottom */
	for (i = nz - m; i < nz; ++i) {
		p[i] = gb;
	}
	
}

static void dobackus(int navg, int nz, float *p, float *avg)
/*****************************************************************************
Do Backus averaging of parameter vector p[nz] using a 
centered window of navg samples.

avg_j = (1/m) ( sum_{i=j-m/s}^{j+m/2} p_i )

where m=navg
*****************************************************************************/
{
	int i, j, m;
	float val;

	/* half width of rms window */
	m = (navg-1)/2;

	/* loop over output times */
	for (i = 0; i < nz; ++i) {
		val = 0.0;
		/* check we are not off the data ends */
		if (i-m > 0 && i+m < nz) {
			/* loop over window samples */
			for (j = i-m; j <= i+m; ++j) {
				val += p[j];
			}
			avg[i] = val/navg;
		}
		/* quick fix on edge effects */
		if (i <= m) avg[i] = p[i];
		if (i >= nz-m) avg[i] = p[i];
	}
		
}
