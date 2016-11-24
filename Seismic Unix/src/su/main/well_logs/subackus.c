/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUBACKUS: $Revision: 1.6 $ ; $Date: 2011/11/16 17:24:58 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUBACKUS - calculate Thomsen anisotropy parameters from 	",
" 	     well log (vp,vs,rho) data via BACKUS averaging	",
" 								",
" subackus < vp_vs_rho.su >stdout [options]			",
" 								",
" Required parameters:						",
" none								",
"								",
" Optional parameter:						",
" navg=201	number of depth samples in Backus avg window 	",
" all=0		=1 to output extra parameters 			",
"		(<vp0>,<vs0>,<rho>,eta,vang,a,f,c,l,A,B,<lam>,<mu>)",
" ang=30	angle (deg) for use in vang			",
"								",
" Notes:							",
" 1. Input are (vp,vs,rho) traces in metric units		",
" 2. Output are (epsilon,delta,gamma) dimensionless traces	",
"    tracl=(1,2,3)=(epsilon,delta,gamma) 			",
"	(all=1 output optional traces listed below)		",
"    tracl=(4,5,6,7,8)=(vp0,vs0,<rho>,eta,vang)			",
"    tracl=(9,10,11,12)=(a,f,c,l)=(c11,c13,c33,c44) backus avg'd",
"    tracl=(13,14)=(<lam/lamp2mu>^2,4<mu*lampmu/lamp2mu>)=(A,B)	",
"       used to analyze eps=(a-c)/2c; a=c11=A*x+B;  c=c33=x	",
"    tracl=(15,16)=(<lambda>,<mu>)				",
"       for fluid analysis (lambda affected by fluid, mu not)   ",
"    tracl=(17,18,19)=(vp,vs,rho)  orig log values		",
"    tracl=(20)=(m)=(c66) Backus avg'd 				",
"    tracl=(21,22,23,24,25)=(a,f,c,l,m)=(c11,c13,c33,c44,c66) orig",
" 3. (epsilon,delta,etc.) can be isolated by tracl header field ",
" 4. (vp0,vs0) are backus averaged vertical wavespeeds		",
" 5. <rho> is backus averaged density, etc.			",
" 6. eta = (eps - delta) / (1 + 2 delta)			",
" 7. vang=vp(ang_deg)/vp0  phase velocity ratio			",
"    The idea being that if vang~vp0 there are small time effects",
"    (30 deg comes from ~ max ray angle preserved in processing)",
"								",
" Example:							",
" las2su < logs.las nskip=34 nlog=4 > logs.su 			",
" suwind < logs.su  key=tracl min=2 max=3 | suop op=s2vm > v.su	",
" suwind < logs.su  key=tracl min=4 max=4 | suop op=d2m > d.su	",
" fcat v.su d.su > vp_vs_rho.su					",
" subackus < vp_vs_rho.su > eps_delta_gamma.su			",
" In this example we start with a well las file containing 	",
" 34 header lines and 4 log tracks (depth,p_son,s_son,den).	",
" This is converted to su format by las2su.  Then we pull off	",
" the sonic logs and convert them to velocity in metric units.	",
" Then the density log is pulled off and converted to metric.	",
" All three metric curves are bundled into one su file which 	",
" is the input to subackus. 					", 
"								",
" Related codes: sulprime subackush				",
NULL};

/* Credits:
 *
 *	UHouston: Chris Liner 
 *              I gratefully acknowledge Saudi Aramco for permission
 *              to release this code developed while I worked for the 
 *              EXPEC-ARC research division.
 * References:		
 * Anisotropy parameters: Thomsen, 2002, DISC Notes (SEG)
 * Backus Method: Berryman, Grechka, and Berge, 1997, SEP94
 *
 */
/**************** end self doc ***********************************/

static void dobackus(int navg, int nz, float *p, float *avg);
static void handlEnds(int navg, int nz, float *p);

segy tr;

int
main(int argc, char **argv) {
	float *vp=NULL;		/* p-wave velocity model 	*/
	float *vs=NULL;		/* s-wave velocity model 	*/
	float *rho=NULL;	/* density model 		*/
	float *mu=NULL;		/* shear modulus  		*/
	float *lp2mu=NULL;	/* lambda + 2 mu  		*/
	float *lam=NULL;	/* lambda  			*/
	float *lpmu=NULL;	/* lambda + mu  		*/
	float *eps=NULL;	/* anisotropy param epsilon 	*/
	float *delta=NULL;	/* anisotropy param delta 	*/
	float *gamma=NULL;	/* anisotropy param gamma	*/
	float *vp0=NULL;	/* vertical p-wave speed 	*/
	float *vs0=NULL;	/* vertical s-wave speed 	*/
	float *eta=NULL;	/* anisotropy param eta		*/
	float *rhoa=NULL;	/* averaged density		*/
	float *vang=NULL;	/* vp(ang_deg)/vp0		*/
	float *t1=NULL,*t2=NULL,*t3=NULL,*t4=NULL,*t5=NULL,*t6=NULL;
			/* temp vectors	*/
	int nz;			/* number of depth levels in log*/
	int i;			/* counters 			*/
	int navg;		/* length of averaging window	*/
	int all;		/* eta output flag		*/
	float *c11=NULL,*c13=NULL,*c33=NULL,*c44=NULL,*c66=NULL;
			/* effective stiffness coefficients */
	float *aa=NULL, *bb=NULL;	/* factors in epsilon calc	*/
	float *blam=NULL, *bmu=NULL;	/* backus avg'd lambda and mu	*/
	float *tmp=NULL;		/* temp array for avg subroutine */
	float d1,f1,dt;
	float sina, cosa, sina2, cosa2, sina4, u, ds, dds, vpa;
	float ang;

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* get parameters */
	if (!getparint("navg",&navg)) navg = 201;
	if (!getparint("all",&all)) all = 0;
	if (!getparfloat("ang",&ang)) ang = 30;

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
	rho 	= alloc1float(nz);

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
		rho[i]  = tr.data[i];
	}

	/* allocate other model vectors */
	mu 	= alloc1float(nz);
	lp2mu 	= alloc1float(nz);
	lam 	= alloc1float(nz);
	lpmu 	= alloc1float(nz);
	eps 	= alloc1float(nz);
	delta 	= alloc1float(nz);
	gamma 	= alloc1float(nz);
	vp0 	= alloc1float(nz);
	vs0 	= alloc1float(nz);
	eta 	= alloc1float(nz);
	rhoa 	= alloc1float(nz);
	vang 	= alloc1float(nz);
	t1 	= alloc1float(nz);
	t2 	= alloc1float(nz);
	t3 	= alloc1float(nz);
	t4 	= alloc1float(nz);
	t5 	= alloc1float(nz);
	t6 	= alloc1float(nz);
	tmp 	= alloc1float(nz);
	c11 	= alloc1float(nz);
	c13 	= alloc1float(nz);
	c33 	= alloc1float(nz);
	c44 	= alloc1float(nz);
	c66 	= alloc1float(nz);
	aa 	= alloc1float(nz);
	bb 	= alloc1float(nz);
	/* backus averaged lambda and mu */
	blam	= alloc1float(nz);
	bmu	= alloc1float(nz);

	/* fill aux arrays */
	for (i = 0; i < nz; ++i) {
		mu[i]    = rho[i]*vs[i]*vs[i];
		lp2mu[i] = rho[i]*vp[i]*vp[i];
		lam[i]   = rho[i]*(vp[i]*vp[i] - 2*vs[i]*vs[i]);
		lpmu[i]  = rho[i]*(vp[i]*vp[i] - vs[i]*vs[i]);
	}

	/* backus averaged lambda and mu */
	dobackus(navg,nz,lam,blam);
	dobackus(navg,nz,mu,bmu);


	/* calc c11 			*/
	for (i = 0; i < nz; ++i) {
		t1[i] = lam[i]/lp2mu[i];
		t2[i] = 1.0/lp2mu[i];
		t3[i] = mu[i]*lpmu[i]/lp2mu[i];
	}
	dobackus(navg,nz,t1,t4);
	dobackus(navg,nz,t2,t5);
	dobackus(navg,nz,t3,t6);
	
	/* finish */
	for (i = 0; i < nz; ++i) {
		t4[i] = t4[i]*t4[i];
		t5[i] = 1.0/t5[i];
		c11[i] = t4[i]*t5[i] + 4.0*t6[i];
		aa[i] = t4[i];
		bb[i] = 4*t6[i];
	}

	/* calc c13 			*/
	for (i = 0; i < nz; ++i) {
		t1[i] = lam[i]/lp2mu[i];
		t2[i] = 1.0/lp2mu[i];
	}
	dobackus(navg,nz,t1,t3);
	dobackus(navg,nz,t2,t4);	
	/* recip */
	for (i = 0; i < nz; ++i) {
		t4[i] = 1.0/t4[i];
		c13[i] = t3[i]*t4[i];
	}

	/* calc c33 			*/
	for (i = 0; i < nz; ++i) {
		t1[i] = 1.0/lp2mu[i];
	}
	dobackus(navg,nz,t1,c33);
	/* recip */
	for (i = 0; i < nz; ++i) {
		c33[i] = 1.0/c33[i];
	}

	/* calc c44 			*/
	for (i = 0; i < nz; ++i) {
		t1[i] = 1.0/mu[i];
	}
	dobackus(navg,nz,t1,c44);
	/* recip */
	for (i = 0; i < nz; ++i) {
		c44[i] = 1.0/c44[i];
	}

	/* calc c66 			*/
	for (i = 0; i < nz; ++i) {
		t1[i] = mu[i];
	}
	dobackus(navg,nz,t1,c66);

	/* calc rhoa (averaged density)	*/
	dobackus(navg,nz,rho,rhoa);

	/* set up trig functions */
	sina = sin(ang*3.1415927/180.);
	cosa = cos(ang*3.1415927/180.);
	sina2= sina*sina;
	cosa2= cosa*cosa;
	sina4= sina2*sina2;

	/* calculate anis params and vertical wave speeds 	*/
	for (i = 0; i < nz; ++i) {
		eps[i] = (c11[i] - c33[i]) / (2.0 * c33[i]);
		t1[i] = (c13[i] + c44[i]) * (c13[i] + c44[i]);
		t2[i] = (c33[i] - c44[i]) * (c33[i] - c44[i]);
		t3[i] = 2.0 * c33[i] * (c33[i] - c44[i]);
		delta[i] = (t1[i] - t2[i]) / t3[i];
		gamma[i] = (c66[i] - c44[i]) / (2.0 * c44[i]);
		vp0[i] = sqrt( c33[i] / rhoa[i] );
		vs0[i] = sqrt( c44[i] / rhoa[i] );
		eta[i] = (eps[i] - delta[i]) / (1.0 + 2.0 * delta[i]);
		u = 1 - (vs0[i]*vs0[i])/(vp0[i]*vp0[i]);
		ds = (2*delta[i] - eps[i])*u;
		dds = 0.5*
		  (sqrt(
		  u*u + 4*ds*sina2*cosa2 + 4*(u+eps[i])*eps[i]*sina4
		) - u); 
		vpa = vp0[i]*sqrt(1 + eps[i]*sina2 + dds);
		vang[i] = vpa/vp0[i];
	}

	/* set up output trace headers */
	tr.trid = 1;			/* su time traces (trick) */
	tr.ns = nz;			/* samples per trace */
	tr.dt = dt;			/* time sample rate (trick) */
	tr.d1 = d1;		
	tr.f1 = f1;

	/* write epsilon trace */
	tr.tracl = 1;
	for (i = 0 ; i < nz ; ++i){
		tr.data[i] = eps[i];
	}
	handlEnds(navg,nz,tr.data);
	puttr(&tr);

	/* write delta trace */
	tr.tracl = 2;
	for (i = 0 ; i < nz ; ++i){
		tr.data[i] = delta[i];
	}
	handlEnds(navg,nz,tr.data);
	puttr(&tr);

	/* write gamma trace */
	tr.tracl = 3;
	for (i = 0 ; i < nz ; ++i){
		tr.data[i] = gamma[i];
	}
	handlEnds(navg,nz,tr.data);
	puttr(&tr);

	/* optionally write other params */
	if (all==1) {
		/* vp0 */
		tr.tracl = 4;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = vp0[i];
		}
		handlEnds(navg,nz,tr.data);
		puttr(&tr);
		
		/* vs0 */
		tr.tracl = 5;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = vs0[i];
		}
		handlEnds(navg,nz,tr.data);
		puttr(&tr);

		/* rhoa */
		tr.tracl = 6;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = rhoa[i];
		}
		handlEnds(navg,nz,tr.data);
		puttr(&tr);
		
		/* eta */
		tr.tracl = 7;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = eta[i];
		}
		handlEnds(navg,nz,tr.data);
		puttr(&tr);
		
		/* vang */
		tr.tracl = 8;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = vang[i];
		}
		handlEnds(navg,nz,tr.data);
		puttr(&tr);
		
		/* a = c11 */
		tr.tracl = 9;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = c11[i];
		}
		handlEnds(navg,nz,tr.data);
		puttr(&tr);
		
		/* f = c13 */
		tr.tracl = 10;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = c13[i];
		}
		handlEnds(navg,nz,tr.data);
		puttr(&tr);
		
		/* c = c33 */
		tr.tracl = 11;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = c33[i];
		}
		handlEnds(navg,nz,tr.data);
		puttr(&tr);
		
		/* l = c44 */
		tr.tracl = 12;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = c44[i];
		}
		handlEnds(navg,nz,tr.data);
		puttr(&tr);
		
		/* aa = <lam/lamp2mu>^2 */
		tr.tracl = 13;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = aa[i];
		}
		handlEnds(navg,nz,tr.data);
		puttr(&tr);
		
		/* bb = 4*<mu*ampmu/lamp2mu> */
		tr.tracl = 14;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = bb[i];
		}
		handlEnds(navg,nz,tr.data);
		puttr(&tr);
		
		/* <lambda> */
		tr.tracl = 15;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = blam[i];
		}
		handlEnds(navg,nz,tr.data);
		puttr(&tr);
		
		/* <mu> */
		tr.tracl = 16;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = bmu[i];
		}
		handlEnds(navg,nz,tr.data);
		puttr(&tr);
		
		/* vp */
		tr.tracl = 17;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = vp[i];
		}
		puttr(&tr);
		
		/* vs */
		tr.tracl = 18;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = vs[i];
		}
		puttr(&tr);
		
		/* rho */
		tr.tracl = 19;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = rho[i];
		}
		puttr(&tr);
		
		/* c66 */
		tr.tracl = 20;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = c66[i];
		}
		handlEnds(navg,nz,tr.data);
		puttr(&tr);
		
		/* output original cij */		

		/* a = c11 = c22 = lambda + 2 mu */
		tr.tracl = 21;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = lp2mu[i];
		}
		puttr(&tr);

		/* f = c13 = c33 = lambda */
		tr.tracl = 22;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = lam[i];
		}
		puttr(&tr);

		/* c = c33 = lambda + 2 mu */
		tr.tracl = 23;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = lp2mu[i];
		}
		puttr(&tr);

		/* l = c44 = c55 = mu */
		tr.tracl = 24;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = mu[i];
		}
		puttr(&tr);

		/* m = c66 = mu */
		tr.tracl = 25;
		for (i = 0 ; i < nz ; ++i){
			tr.data[i] = mu[i];
		}
		puttr(&tr);

	}

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
