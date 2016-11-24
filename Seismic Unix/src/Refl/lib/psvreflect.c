/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


#include "par.h"
#include "Reflect/reflpsvsh.h"
#define BLK 100
#define IMAX 300
#define SZERO 0.000000000001

/********************************Self-Documentation***************************/
/******************************************************************************
REFLECTIVITIES -	Subroutines to do matrix propagation as the hard core of 
						a reflectivity modeling code for PSV and SH wavefields
psv_reflectivities		applies matrix propagation to compute the PSV 
							reflectivity response of a horizontally stratified 
							earth model
*******************************************************************************
Function Prototypes:
void psv_reflectivities (int int_type, int verbose, int wtype, int nw,
	int nlayers, int nx, int layern, int nor, int np, float bp1, float m1,
	float m2, float m3, float h1, float h2, int lsource, float bx, float dx,
	float xmax, float decay, float fref, float wrefp, float wrefs, float tsec,
	float p2w, float fs, float epsp, float epss, float sigp, float sigs,
	float pw1, float pw2, float pw3, float pw4, int *acoustic, int *flag,
	int *lobs, float *rho, float *t, float *cl, float *ct, float *ql, float *qt,
	complex ***response1, complex ***response2, complex ***response3,
	FILE *outfp);
*******************************************************************************
psv_reflectivities
Input:
nw  			number of frequencies
nlayers 		number of reflecting layers
nx  			number of ranges
nor 			number of receivers
lsource 		layer on top of which the source is located
bx  			beginning range
fx  			final range
dx  			range increment
acoustic		array[nor] of flags for receiver type
flag			array[nor] of flags for source type
rho 			array[nlayers] of densities
t   			array[nlayers] of layer thicknesses

Output
response1   	array[nw][nx][nor] of computed pressure wavefield
response2   	array[nw][nx][nor] of computed radial wavefield
response3   	array[nw][nx][nor] of computed vertical wavefield
*******************************************************************************
Credits:


******************************************************************************/
/*****************************End Self-Documentation**************************/





/******************************************************************************

		Subroutine to compute the propagation matrices for a single
							frequency step

******************************************************************************/
void psv_reflectivities (int int_type, int verbose, int wtype,
		int nw, int nlayers, int nx, int layern, int nor,
		int np, float bp1, float m1, float m2, float m3,
		float h1, float h2, int lsource, float bx, float dx,
		float xmax, float decay, float fref, float wrefp, 
		float wrefs, float tsec, float p2w, float fs,
		float epsp, float epss, float sigp, float sigs,
		float pw1, float pw2, float pw3, float pw4, 
		int *acoustic, int *flag, int *lobs, float *rho,
		float *t, float *cl, float *ct, float *ql, float *qt,
		complex ***response1, complex ***response2, 
		complex ***response3, FILE *outfp)
/******************************************************************************
Iunput:
nw  			number of frequencies
nlayers 		number of reflecting layers
nx  			number of ranges
nor 			number of receivers
lsource 		layer on top of which the source is located
bx  			beginning range
fx  			final range
dx  			range increment
acoustic		array[nor] of flags for receiver type
flag			array[nor] of flags for source type
rho 			array[nlayers] of densities
t   			array[nlayers] of layer thicknesses

Output
response1   	array[nw][nx][nor] of computed pressure wavefield
response2   	array[nw][nx][nor] of computed radial wavefield
response3   	array[nw][nx][nor] of computed vertical wavefield
******************************************************************************/
{
	int iw,iz,ip,ix;		/* loop counters */
	int ijk1=0,ik1,ik2,il,jl;	/* auxiliary indices */
	int ijk=0,ijk2,jj;		/* more auxiliary indices */
	int flg1,lrec;				
	float bp=0.0;			/* smallest ray parameter */
	float fp;			/* final ray parameter */
	float dp;			/* ray parameter increment */
	int nblock;			/* number of blocks to process */
	int block_size;			/* size of current block */
	int left;			/* remainder */
	int *prv,*ibl;			/* scratch arrays */
	float x1,w;
	float wbeg=0.0,wfin=0.0;	/* aux variables for output */
	complex cdp,wpie;		/* complex cdp and frequency */

	/* complex auxiliary variables */
	complex divfac,g,h,e,f,mu,x,y,z,a,b,c,d,t1,tem,temr,sig,sigh,rho1,rho2;
	complex det,psq,gl1,gt1,gl2,gt2,gam1,gam2,at1,at2,al1,al2;
	complex rvrb111,rvrb112,rvrb121,rvrb122,rvrb211,rvrb212,rvrb221;
	complex ewigh11,ewigh22,rvrb222;
	complex wrn011,wrn012,wrn021,wrn022,ewrd11,ewrd12,ewrd21,ewrd22;
	complex ewd0211,ewd0212,ewd0221,ewd0222,ewtu211,ewtu212;
	complex ewtu221,ewtu222,rtb111,rtb112,rtb121,rtb122,rtb211;
	complex rtb212,rtb221,rtb222,rtb311,rtb312,rtb321,rtb322;
	complex func1,func2,func3,func4,func5,func6;
	complex tun0p11,tun0p12,tun0p21,tun0p22,rd0np11,rd0np12,rd0np21;
	complex rd0np22,td0np11,td0np12,td0np21,td0np22;
	complex rd11,rd12,rd21,rd22,td11,td12,td21,td22,tu11,tu12,tu21,tu22;
	complex ru11,ru12,ru21,ru22,ruf11,ruf12,ruf21,ruf22;
	complex fact11,fact12,vuzs1,vuzs2,vdzs1,vdzs2;

	/* complex scratch arrays */
	complex *rd0n11=NULL,*rd0n12=NULL,*rd0n21=NULL,*rd0n22=NULL,
		*td0n11=NULL,*td0n12=NULL;
	complex *td0n21=NULL,*td0n22=NULL,*tun011=NULL,*tun012=NULL,
		*tun021=NULL,*tun022=NULL;
	complex *run011=NULL,*run012=NULL,*run021=NULL,*run022=NULL,
		*tusf11=NULL,*tusf12=NULL;
	complex *tusf21=NULL,*tusf22=NULL,*rdfs11=NULL,*rdfs12=NULL,
		*rdfs21=NULL,*rdfs22=NULL;
	complex *tdfs11=NULL,*tdfs12=NULL,*tdfs21=NULL,*tdfs22=NULL,
		*rurf11=NULL,*rurf12=NULL;
	complex *rurf21=NULL,*rurf22=NULL,*turf11=NULL,*turf12=NULL,
		*turf21=NULL,*turf22=NULL;
	complex *rdfr11=NULL,*rdfr12=NULL,*rdfr21=NULL,*rdfr22=NULL,
		*tdfr11=NULL,*tdfr12=NULL;
	complex *tdfr21=NULL,*tdfr22=NULL,*rnr11=NULL,*rnr12=NULL,
		*rnr21=NULL,*rnr22=NULL;
	complex *rdsr11=NULL,*rdsr12=NULL,*rdsr21=NULL,*rdsr22=NULL;
	/* complex *r11=NULL,*r12=NULL,*r21=NULL,*r22=NULL; */
	complex *texp=NULL,*vos1=NULL,*vos2=NULL,*vos3=NULL,*ux=NULL,
		*uz=NULL,*up=NULL;
	complex *alpha=NULL,*betha=NULL,*sigmad1=NULL,*sigmad2=NULL,
		*sigmau1=NULL,*sigmau2=NULL;
	complex *al=NULL,*at=NULL,*gl=NULL,*gt=NULL,*gam=NULL,*p=NULL,
		*pp=NULL,*prs=NULL,*pwin=NULL;

	/* complex pointers */	
	complex *rusf11=NULL,*rusf12=NULL,*rusf21=NULL,*rusf22=NULL;
	complex *tusr11=NULL,*tusr12=NULL,*tusr21=NULL,*tusr22=NULL;
	complex *rusr11=NULL,*rusr12=NULL,*rusr21=NULL,*rusr22=NULL;
	complex *tdsr11=NULL,*tdsr12=NULL,*tdsr21=NULL,*tdsr22=NULL;
	complex *rdnr11=NULL,*rdnr12=NULL,*rdnr21=NULL,*rdnr22=NULL;

	/* allocate working space */
	ux=alloc1complex(BLK);uz=alloc1complex(BLK);
	up=alloc1complex(BLK);rd0n11=alloc1complex(BLK);
	rd0n12=alloc1complex(BLK);rd0n21=alloc1complex(BLK);
	rd0n22=alloc1complex(BLK);td0n11=alloc1complex(BLK);
	td0n12=alloc1complex(BLK);td0n21=alloc1complex(BLK);
	td0n22=alloc1complex(BLK);tun011=alloc1complex(BLK);
	tun012=alloc1complex(BLK);tun021=alloc1complex(BLK);
	tun022=alloc1complex(BLK);run011=alloc1complex(BLK);
	run012=alloc1complex(BLK);run021=alloc1complex(BLK);
	run022=alloc1complex(BLK);tusf11=alloc1complex(BLK);
	tusf12=alloc1complex(BLK);tusf21=alloc1complex(BLK);
	tusf22=alloc1complex(BLK);rdfs11=alloc1complex(BLK);
	rdfs12=alloc1complex(BLK);rdfs21=alloc1complex(BLK);
	rdfs22=alloc1complex(BLK);tdfs11=alloc1complex(BLK);
	tdfs12=alloc1complex(BLK);tdfs21=alloc1complex(BLK);
	tdfs22=alloc1complex(BLK);rurf11=alloc1complex(IMAX);
	rurf12=alloc1complex(IMAX);rurf21=alloc1complex(IMAX);
	rurf22=alloc1complex(IMAX);turf11=alloc1complex(IMAX);
	turf12=alloc1complex(IMAX);turf21=alloc1complex(IMAX);
	turf22=alloc1complex(IMAX);rdfr11=alloc1complex(IMAX);
	rdfr12=alloc1complex(IMAX);rdfr21=alloc1complex(IMAX);
	rdfr22=alloc1complex(IMAX);tdfr11=alloc1complex(IMAX);
	tdfr12=alloc1complex(IMAX);tdfr21=alloc1complex(IMAX);
	tdfr22=alloc1complex(IMAX);rnr11=alloc1complex(BLK);
	rnr12=alloc1complex(BLK);rnr21=alloc1complex(BLK);
	rnr22=alloc1complex(BLK); rdsr11=alloc1complex(BLK);
	rdsr12=alloc1complex(BLK);rdsr21=alloc1complex(BLK);
	rdsr22=alloc1complex(BLK);vos1=alloc1complex(BLK);
	rusf21=alloc1complex(BLK);rusf22=alloc1complex(BLK);
	vos2=alloc1complex(BLK);vos3=alloc1complex(BLK);
/*	r11=alloc1complex(BLK);r12=alloc1complex(BLK);
	r21=alloc1complex(BLK);r22=alloc1complex(BLK);
*/
	texp=alloc1complex(BLK);

	/* allocate other working space */
	rdnr11=alloc1complex(BLK);rdnr12=alloc1complex(BLK);
	rdnr21=alloc1complex(BLK);rdnr22=alloc1complex(BLK);
	prv=alloc1int(nor);
	prs=alloc1complex(nor);
	ibl=alloc1int(BLK);
	p=alloc1complex(BLK);
	pp=alloc1complex(BLK);
	pwin=alloc1complex(BLK);
	al=alloc1complex(nlayers);
	at=alloc1complex(nlayers);
	gl=alloc1complex(BLK*nlayers);
	gt=alloc1complex(BLK*nlayers);
	gam=alloc1complex(BLK*nlayers);
	alpha=alloc1complex(BLK);
	betha=alloc1complex(BLK);
	sigmau1=alloc1complex(BLK);
	sigmau2=alloc1complex(BLK);
	sigmad1=alloc1complex(BLK);
	sigmad2=alloc1complex(BLK);

	/* set pointers */
	rusf11=alpha;
	rusf12=betha;
	tusr11=tun011;
	tusr12=tun012;
	tusr21=tun021;
	tusr22=tun022;
	rusr11=run011;
	rusr12=run012;
	rusr21=run021;
	rusr22=run022;
	tdsr11=td0n11;
	tdsr12=td0n12;
	tdsr21=td0n21;
	tdsr22=td0n22;
/*
	rdnr11=r11;
	rdnr12=r12;
	rdnr21=r21;
	rdnr22=r22;
*/

	/* assign initial value to working variables */
    tem=cdiv(cmplx(1.0,0.0),cwp_cexp(crmul(cmplx(0.0,1.0),PI/4)));
    ewigh11=cmplx(0.0,0.0);
    ewigh22=cmplx(0.0,0.0);

	/* initialize complex output arrays */
	for (iw=0; iw<nw; iw++) 
		for (ix=0; ix<nx; ix++)
			for (iz=0; iz<nor; iz++) {
				response1[iw][ix][iz]=cmplx(0.0,0.0);
				response2[iw][ix][iz]=cmplx(0.0,0.0);
				response3[iw][ix][iz]=cmplx(0.0,0.0);
			}

	/**************************************************************************
	*						Main loop over frequencies			*	
	**************************************************************************/
	for (iw=0; iw<nw; iw++) {
		w=(iw+1)*PI*2/tsec;			/* w in radians/sec */
		if (iw==0) wbeg=w;
		if (iw==nw-1) wfin=w;
		wpie=cmplx(w,decay);		/* complex w */

		/* compute frequency-dependent auxiliary variables for reflectivity */
		compute_w_aux_arrays (wtype, layern, nlayers, nor, lsource, &np,
			&block_size, &nblock, &left, fref, wrefp, wrefs, lobs, tsec, p2w,
			fs, xmax, w, decay, epsp, epss, sigp, sigs, &pw1, &pw2, &pw3, &pw4,
			&dp, &fp, wpie, &cdp, cl, ct, ql, qt, rho, t, al, at, prs);

		/* if requested, output processing information */
		if (verbose==1||verbose==3) {
			fprintf(stderr,"%3d%6.1f%7.3f%7.3f%6d%7.3f%7.3f%7.3f"
			"%7.3f%7.3f\n",iw,w,bp,fp,np,dp,pw1,pw2,pw3,pw4);
		} 
		if (verbose==2||verbose==3) {
			fprintf(outfp,"%3d%6.1f%7.3f%7.3f%6d%7.3f%7.3f%7.3f"
			"%7.3f%7.3f\n",iw,w,bp,fp,np,dp,pw1,pw2,pw3,pw4);
		}


		/**********************************************************************
		* 		loop over slopes to compute the reflectivities (by blocks)    *
	    **********************************************************************/
		while (1) {

			if ((nblock==1) && (left !=0)) block_size=left;

			/* compute p_aux_arrays */
			compute_p_aux_arrays (wtype, nlayers, lsource, block_size, bp, dp,
				w, decay, pw1, pw2, pw3, pw4, pwin, m1, m2, m3, h1, h2,
				wpie, &divfac, p, pp, al, at, rho, gl, gt, gam, alpha, betha,
				sigmad1, sigmad2, sigmau1, sigmau2);


			/******************************************************************
			*			start computation of reflectivity series			  *
			******************************************************************/
			/* loop over reflecting layers */
			ijk=0;
			for (il=0; il<nlayers-1; il++) {
				jl=il+1;
				ik1=il*block_size;
				ik2=(il+1)*block_size;
				al1=al[il];
				al2=al[jl];
				at1=at[il];
				at2=at[jl];
				rho1=cmplx(rho[il],0.0);
				rho2=cmplx(rho[jl],0.0);
				t1=cmplx(t[il],0.0);
				if (il==0) {				/* initialize, first layer only */
					if ((at1.r==0.0)&&(at1.i==0.0)) {
						for (ip=0; ip<block_size; ip++) {
							ijk1=ik1+ip;
							gl1=gl[ijk1];
							ewigh11=cwp_cexp(cmul(cmplx(0.0,1.0),cmul(wpie,cmul(gl1,t1))));
							rd0n11[ip]=cmplx(0.0,0.0);
							rd0n12[ip]=cmplx(0.0,0.0);
							rd0n21[ip]=cmplx(0.0,0.0);
							rd0n22[ip]=cmplx(0.0,0.0);
							td0n11[ip]=ewigh11;
							td0n12[ip]=cmplx(0.0,0.0);
							td0n21[ip]=cmplx(0.0,0.0);
							td0n22[ip]=cmplx(0.0,0.0);
							tun011[ip]=ewigh11;
							tun012[ip]=cmplx(0.0,0.0);
							tun021[ip]=cmplx(0.0,0.0);
							tun022[ip]=cmplx(0.0,0.0);
							run011[ip]=cneg(cmul(ewigh11,ewigh11));
							run012[ip]=cmplx(0.0,0.0);
							run021[ip]=cmplx(0.0,0.0);
							run022[ip]=cmplx(0.0,0.0);
						}	
					} else {
						for (ip=0; ip<block_size; ip++) {
							ijk1=ik1+ip;
							psq=pp[ip];
							gl1=gl[ijk1];
							gt1=gt[ijk1];
							gam1=gam[ijk1];
							mu=cmul(cdiv(cmplx(1.0,0.0),cmul(at1,at1)),rho1);
							rtb211=crmul(cmul(p[ip],cmul(mu,gl1)),2.);
							rtb212=gam1;
							rtb221=cneg(gam1);
							rtb222=crmul(cmul(p[ip],cmul(mu,gt1)),2.);
							det=csub(cmul(rtb211,rtb222),cmul(rtb212,rtb221));
							rtb111=cdiv(rtb222,det);
							rtb112=cdiv(rtb212,det);
							rtb121=cneg(cdiv(rtb221,det));
							rtb122=cdiv(rtb211,det);
							ruf11=cadd(cmul(rtb211,rtb222),cmul(rtb212,rtb221));
							ruf12=cmul(rtb222,crmul(rtb212,2.));
							ruf21=cmul(rtb211,crmul(rtb221,2.));
							ruf22=ruf11;
							ruf11=cdiv(ruf11,det);
							ruf12=cdiv(ruf12,det);
							ruf21=cdiv(ruf21,det);
							ruf22=cdiv(ruf22,det);
							ewigh11=cwp_cexp(cmul(gl1,cmul(wpie,cmul(cmplx(0.0,1.0),t1))));
							ewigh22=cwp_cexp(cmul(gt1,cmul(wpie,cmul(cmplx(0.0,1.0),t1))));
							rd0n11[ip]=cmplx(0.0,0.0);
							rd0n12[ip]=cmplx(0.0,0.0);
							rd0n21[ip]=cmplx(0.0,0.0);
							rd0n22[ip]=cmplx(0.0,0.0);
							td0n11[ip]=ewigh11;
							td0n12[ip]=cmplx(0.0,0.0);
							td0n21[ip]=cmplx(0.0,0.0);
							td0n22[ip]=ewigh22;
							tun011[ip]=ewigh11;
							tun012[ip]=cmplx(0.0,0.0);
							tun021[ip]=cmplx(0.0,0.0);
							tun022[ip]=ewigh22;
							rtb111=cmul(ewigh11,ruf11);	
							rtb112=cmul(ewigh11,ruf12);	
							rtb121=cmul(ewigh22,ruf21);	
							rtb122=cmul(ewigh22,ruf22);	
							run011[ip]=cmul(ewigh11,rtb111);	
							run012[ip]=cmul(ewigh22,rtb112);	
							run021[ip]=cmul(ewigh11,rtb121);	
							run022[ip]=cmul(ewigh22,rtb122);	
						}
					}
				} else {				/* layers after the first one */

					/* case 1: liquid-liquid */
					if ((at1.r==0.)&&(at1.i==0.)&&(at2.r==0.)&&(at2.i==0.)) {
						if ((al1.r==al2.r)&&(al1.i==al2.i)) {
							for (ip=0; ip<block_size; ip++) {
								ijk1=ik1+ip;
								gl1=gl[ijk1];
								ewigh11=cwp_cexp(cmul(wpie,cmul(cmplx(0.0,1.0),cmul(t1,gl1))));
								tun0p11=cmul(ewigh11,tun011[ip]);
								tun0p21=cmul(ewigh11,tun021[ip]);
								rtb111=cmul(ewigh11,run011[ip]);
								run011[ip]=cmul(ewigh11,rtb111);
								run012[ip]=cmplx(0.0,0.0);
								run021[ip]=cmplx(0.0,0.0);
								run022[ip]=cmplx(0.0,0.0);
								td0np11=cmul(ewigh11,td0n11[ip]);
								td0np12=cmul(ewigh11,td0n12[ip]);
								tun011[ip]=tun0p11;
								tun012[ip]=cmplx(0.0,0.0);
								tun021[ip]=tun0p21;
								tun022[ip]=cmplx(0.0,0.0);
								td0n11[ip]=td0np11;
								td0n12[ip]=td0np12;
								td0n21[ip]=cmplx(0.0,0.0);
								td0n22[ip]=cmplx(0.0,0.0);
							}
						} else {
							for (ip=0; ip<block_size; ip++) {
								ijk1=ik1+ip;
								ijk2=ik2+ip;
								psq=pp[ip];
								gl1=gl[ijk1];
								gt1=gt[ijk1];
								gam1=gam[ijk1];
								gam2=gam[ijk2];
								gl2=gl[ijk2];
								gt2=gt[ijk2];
								ewigh11=cwp_cexp(cmul(wpie,cmul(cmplx(0.0,1.0),cmul(t1,gl1))));
								a=csub(gam2,gam1);
								b=cadd(rho1,a);
								c=csub(rho2,a);
								e=cadd(cmul(b,gl1),cmul(c,gl2));
								y=cadd(cmul(gl2,rho1),cmul(gl1,rho2));
								td11=cmul(cdiv(gl1,y),crmul(rho1,2.));
								rd11=cdiv(csub(cmul(gl1,rho2),
									cmul(gl2,rho1)),y);
								tu11=cmul(cdiv(gl2,y),crmul(rho2,2.));
								ru11=cneg(rd11);
								ewrd11=cmul(ewigh11,rd11);
								wrn011=cmul(ewigh11,run011[ip]);
								rvrb111=cdiv(cmplx(1.0,0.0),csub(cmplx(1.0,0.0),cmul(wrn011,ewrd11)));
								rvrb211=cadd(cmplx(1.0,0.0),cmul(rvrb111,
									cmul(ewrd11,wrn011)));
								ewd0211=cmul(td0n11[ip],cmul(rvrb111,ewigh11));
								ewd0212=cmul(td0n12[ip],cmul(rvrb111,ewigh11));
								td0n11[ip]=cmul(td11,ewd0211);
								td0n12[ip]=cmul(td11,ewd0212);
								td0n21[ip]=cmplx(0.0,0.0);
								td0n22[ip]=cmplx(0.0,0.0);
								ewtu211=cmul(rvrb211,cmul(ewigh11,tu11));
								tun0p11=cmul(ewtu211,tun011[ip]);
								tun0p21=cmul(ewtu211,tun021[ip]);
								rtb111=cmul(ewrd11,ewd0211);
								rtb112=cmul(ewrd11,ewd0212);
								rd0np11=cadd(rd0n11[ip],cmul(tun011[ip],
									rtb111));
								rd0np12=cadd(rd0n12[ip],cmul(tun011[ip],
									rtb112));
								rd0np21=cadd(rd0n21[ip],cmul(tun021[ip],
									rtb111));
								rd0np22=cadd(rd0n22[ip],cmul(tun022[ip],
									rtb112));
								run011[ip]=cadd(ru11,cmul(ewtu211,
									cmul(wrn011,td11)));
								run012[ip]=cmplx(0.0,0.0);
								run021[ip]=cmplx(0.0,0.0);
								run022[ip]=cmplx(0.0,0.0);
								tun011[ip]=tun0p11;
								tun012[ip]=cmplx(0.0,0.0);
								tun021[ip]=tun0p21;
								tun022[ip]=cmplx(0.0,0.0);
								rd0n11[ip]=rd0np11;
								rd0n12[ip]=rd0np12;
								rd0n21[ip]=rd0np21;
								rd0n22[ip]=rd0np22;
							}
						}
					} else if (((at1.r==0.)&&(at1.i==0.))&&((at2.r!=0.)||
								(at2.i!=0.))) {

						/* case 2: liquid-solid */
						for (ip=0; ip<block_size; ip++) {
							ijk1=ik1+ip;
							ijk2=ik2+ip;
							psq=pp[ip];
							gl1=gl[ijk1];
							gt1=gt[ijk1];
							gam1=gam[ijk1];
							gl2=gl[ijk2];
							gt2=gt[ijk2];
							gam2=gam[ijk2];
							ewigh11=cwp_cexp(cmul(wpie,cmul(cmplx(0.0,1.0),cmul(t1,gl1))));
							ewigh22=cmplx(0.0,0.0);
							a=csub(gam2,gam1);
							b=cadd(rho1,a);
							c=csub(rho2,a);
							e=cadd(cmul(gl1,b),cmul(gl2,c));
							d=cdiv(crmul(rho2,2.),cmul(at2,at2));
							f=b;
							g=csub(a,cmul(d,cmul(gl1,gt2)));
							h=cneg(cmul(d,gl2));
							z=cdiv(cmplx(1.0,0.0),cadd(cmul(e,f),cmul(g,cmul(h,psq))));
							y=cmul(cmul(z,gl1),crmul(rho1,2.0));
							td11=cmul(y,f);
							td12=cmplx(0.0,0.0);
							td21=cneg(cmul(y,cmul(h,p[ip])));
							td22=cmplx(0.0,0.0);
							rd11=cmul(z,csub(cmul(f,csub(cmul(gl1,b),
								cmul(gl2,c))),
							cmul(cmul(psq,h),cadd(a,cmul(gl1,cmul(gt2,d))))));
							rd12=cmplx(0.0,0.0);
							rd21=cmplx(0.0,0.0);
							rd22=cmplx(0.0,0.0);
							y=cmul(z,crmul(rho2,2.0));
							tu11=cmul(y,cmul(f,gl2));
							tu12=cmul(p[ip],cmul(y,cmul(gt2,h)));
							tu21=cmplx(0.0,0.0);
							tu22=cmplx(0.0,0.0);
							y=crmul(cmul(p[ip],cmul(gl1,cmul(b,
								cmul(d,z)))),2.0);
							ru11=cmul(z,csub(cmul(f,csub(cmul(gl2,c),
								cmul(gl1,b))),cmul(cmul(gl2,d),cmul(g,psq))));
							ru12=cmul(gt2,y);
							ru21=cneg(cmul(gl2,y));
							ru22=cneg(cmul(z,cadd(cmul(b,e),cmul(cmul(psq,h),
								cadd(a,cmul(gl1,cmul(gt2,d)))))));
							ewrd11=cmul(ewigh11,rd11);
							ewrd12=cmul(ewigh11,rd12);
							ewrd21=cmul(ewigh22,rd21);
							ewrd22=cmul(ewigh22,rd22);
							wrn011=cmul(ewigh11,run011[ip]);
							wrn012=cmul(ewigh11,run012[ip]);
							wrn021=cmul(ewigh22,run021[ip]);
							wrn022=cmul(ewigh22,run022[ip]);
							rtb111=cadd(cmul(wrn011,ewrd11),
								cmul(wrn012,ewrd21));
							rtb112=cadd(cmul(wrn011,ewrd12),
								cmul(wrn012,ewrd22));
							rtb121=cadd(cmul(wrn021,ewrd11),
								cmul(wrn022,ewrd21));
							rtb122=cadd(cmul(wrn021,ewrd12),
								cmul(wrn022,ewrd22));
							x=csub(cmplx(1.0,0.0),rtb111);
							y=csub(cmplx(1.0,0.0),rtb122);
							det=csub(cmul(x,y),cmul(rtb112,rtb121));
							rvrb111=cdiv(y,det);
							rvrb112=cdiv(rtb112,det);
							rvrb121=cdiv(rtb121,det);
							rvrb122=cdiv(x,det);
							rtb111=cadd(cmul(rvrb111,wrn011),
								cmul(rvrb112,wrn021));
							rtb112=cadd(cmul(rvrb111,wrn012),
								cmul(rvrb112,wrn022));
							rtb121=cadd(cmul(rvrb121,wrn011),
								cmul(rvrb122,wrn021));
							rtb122=cadd(cmul(rvrb121,wrn012),
								cmul(rvrb122,wrn022));
							rtb211=cadd(cmul(ewrd11,rtb111),
								cmul(ewrd12,rtb121));
							rtb212=cadd(cmul(ewrd11,rtb112),
								cmul(ewrd12,rtb122));
							rtb221=cadd(cmul(ewrd21,rtb111),
								cmul(ewrd22,rtb121));
							rtb222=cadd(cmul(ewrd21,rtb112),
								cmul(ewrd22,rtb122));
							rvrb211=cadd(rtb211,cmplx(1.0,0.0));
							rvrb212=rtb212;
							rvrb221=rtb221;
							rvrb222=cadd(rtb222,cmplx(1.0,0.0));
							rtb111=cmul(ewigh11,td0n11[ip]);
							rtb112=cmul(ewigh11,td0n12[ip]);
							rtb121=cmul(ewigh22,td0n21[ip]);
							rtb122=cmul(ewigh22,td0n22[ip]);
							ewd0211=cadd(cmul(rtb111,rvrb111),
								cmul(rvrb112,rtb121));
							ewd0212=cadd(cmul(rtb112,rvrb111),
								cmul(rvrb112,rtb122));
							ewd0221=cadd(cmul(rtb111,rvrb121),
								cmul(rvrb122,rtb121));
							ewd0222=cadd(cmul(rtb112,rvrb121),
								cmul(rvrb122,rtb122));
							td0n11[ip]=cadd(cmul(td11,ewd0211),
								cmul(td12,ewd0221));
							td0n12[ip]=cadd(cmul(td11,ewd0212),
								cmul(td12,ewd0222));
							td0n21[ip]=cadd(cmul(td21,ewd0211),
								cmul(td22,ewd0221));
							td0n22[ip]=cadd(cmul(td21,ewd0212),
								cmul(td22,ewd0222));
							rtb111=cadd(ewigh11,tu11);
							rtb112=cadd(ewigh11,tu12);
							rtb121=cadd(ewigh22,tu21);
							rtb122=cadd(ewigh22,tu22);
							ewtu211=cadd(cmul(rvrb211,rtb111),
								cmul(rvrb212,rtb121));
							ewtu212=cadd(cmul(rvrb211,rtb112),
								cmul(rvrb212,rtb122));
							ewtu221=cadd(cmul(rvrb221,rtb111),
								cmul(rvrb222,rtb121));
							ewtu222=cadd(cmul(rvrb221,rtb112),
								cmul(rvrb222,rtb122));
							tun0p11=cadd(cmul(tun011[ip],ewtu211),
								cmul(tun012[ip],ewtu221));
							tun0p12=cadd(cmul(tun011[ip],ewtu212),
								cmul(tun012[ip],ewtu222));
							tun0p21=cadd(cmul(tun021[ip],ewtu211),
								cmul(tun022[ip],ewtu221));
							tun0p22=cadd(cmul(tun021[ip],ewtu212),
								cmul(tun022[ip],ewtu222));
							rtb111=cadd(cmul(ewrd11,ewd0211),
								cmul(ewrd12,ewd0221));
							rtb112=cadd(cmul(ewrd11,ewd0212),
								cmul(ewrd12,ewd0222));
							rtb121=cadd(cmul(ewrd21,ewd0211),
								cmul(ewrd22,ewd0221)); 
							rtb122=cadd(cmul(ewrd21,ewd0212),
								cmul(ewrd22,ewd0222));
							rtb211=cadd(cmul(rtb111,tun011[ip]),cmul(rtb121,
								tun012[ip]));
							rtb212=cadd(cmul(rtb112,tun011[ip]),cmul(rtb122,
								tun012[ip]));
							rtb221=cadd(cmul(rtb111,tun021[ip]),cmul(rtb121,
								tun022[ip]));
							rtb222=cadd(cmul(rtb112,tun021[ip]),cmul(rtb122,
								tun022[ip]));
							rd0np11=cadd(rtb211,rd0n11[ip]);
							rd0np12=cadd(rtb212,rd0n12[ip]);
							rd0np21=cadd(rtb221,rd0n21[ip]);
							rd0np22=cadd(rtb222,rd0n22[ip]);
							rtb111=cadd(cmul(wrn011,ewtu211),
								cmul(ewtu221,wrn012));
							rtb112=cadd(cmul(wrn011,ewtu212),
								cmul(ewtu222,wrn012));
							rtb121=cadd(cmul(wrn021,ewtu211),
								cmul(ewtu221,wrn022));
							rtb122=cadd(cmul(wrn021,ewtu212),
								cmul(ewtu222,wrn022));
							rtb211=cadd(cmul(td11,rtb111),cmul(td12,rtb121));
							rtb212=cadd(cmul(td11,rtb112),cmul(td12,rtb122));
							rtb221=cadd(cmul(td21,rtb111),cmul(td22,rtb121));
							rtb222=cadd(cmul(td21,rtb112),cmul(td22,rtb122));
							run011[ip]=cadd(ru11,rtb211);
							run012[ip]=cadd(ru12,rtb212);
							run021[ip]=cadd(ru21,rtb221);
							run022[ip]=cadd(ru22,rtb222);
							rd0n11[ip]=rd0np11;
							rd0n12[ip]=rd0np12;
							rd0n21[ip]=rd0np21;
							rd0n22[ip]=rd0np22;
							tun011[ip]=tun0p11;
							tun012[ip]=tun0p12;
							tun021[ip]=tun0p21;
							tun022[ip]=tun0p22;
						}
					} else if ((at1.r !=0.)&&(at1.i !=0.)&&(at2.r==0.)&&
						(at2.i==0.)) {

						/* case 3: solid-liquid */
						for (ip=0; ip<block_size; ip++) {
							ijk1=ik1+ip;
							ijk2=ik2+ip;
							psq=pp[ip];
							gl1=gl[ijk1];
							gt1=gt[ijk1];
							gam1=gam[ijk1];
							gl2=gl[ik2];
							gt2=gt[ik2];
							gam2=gam[ijk2];
							ewigh11=cwp_cexp(cmul(cmplx(0.0,1.0),cmul(wpie,cmul(gl1,t1))));
							ewigh22=cwp_cexp(cmul(cmplx(0.0,1.0),cmul(wpie,cmul(gt1,t1))));
							a=csub(gam2,gam1);
							b=cadd(rho1,a);
							c=csub(rho2,a);
							e=cadd(cmul(b,gl1),cmul(c,gl2));
							d=cdiv(crmul(rho1,-2.0),cmul(at1,at1));
							f=c;
							g=cneg(cmul(gl1,d));
							h=csub(a,cmul(gl2,cmul(d,gt1)));
							z=cdiv(cmplx(1.0,0.0),cadd(cmul(e,f),cmul(g,cmul(h,psq))));
							y=cmul(z,crmul(rho1,2));
							td11=cmul(y,cmul(gl1,f));
							td12=cmul(y,cmul(p[ip],cmul(gt1,g)));
							td21=cmplx(0.0,0.0);
							td22=cmplx(0.0,0.0);
							y=crmul(cmul(z,cmul(c,cmul(d,
								cmul(p[ip],gl2)))),2.0);
							rd11=cmul(z,csub(cmul(f,csub(cmul(gl1,b),
								cmul(gl2,c))),cmul(cmul(psq,h),cmul(gl1,d))));
							rd12=cmul(y,gt1);	
							rd21=cneg(cmul(gl1,y));
							rd22=cneg(cmul(z,cadd(cmul(c,e),cmul(cadd(a,
								cmul(gt1,cmul(gl2,d))),cmul(psq,g)))));
							y=cmul(cmul(z,gl2),crmul(rho2,2.));
							tu11=cmul(y,f);
							tu12=cmplx(0.0,0.0);
							tu21=cneg(cmul(y,cmul(p[ip],y)));
							tu22=cmplx(0.0,0.0);
							ru11=cmul(z,csub(cmul(f,csub(cmul(gl2,c),
								cmul(gl1,b))),cmul(cadd(a,cmul(gl2,
								cmul(d,gt1))),cmul(psq,g))));
							ru12=cmplx(0.0,0.0);
							ru21=cmplx(0.0,0.0);
							ru22=cmplx(0.0,0.0);
							ewrd11=cmul(ewigh11,rd11);
							ewrd12=cmul(ewigh11,rd12);
							ewrd21=cmul(ewigh22,rd21);
							ewrd22=cmul(ewigh22,rd22);
							wrn011=cmul(ewigh11,run011[ip]);
							wrn012=cmul(ewigh11,run012[ip]);
							wrn021=cmul(ewigh22,run021[ip]);
							wrn022=cmul(ewigh22,run022[ip]);
							rtb111=cadd(cmul(wrn011,ewrd11),
								cmul(ewrd21,wrn012));
							rtb112=cadd(cmul(wrn011,ewrd12),
								cmul(ewrd22,wrn012));
							rtb121=cadd(cmul(wrn021,ewrd11),
								cmul(ewrd21,wrn022));
							rtb122=cadd(cmul(wrn021,ewrd12),
								cmul(ewrd22,wrn022));
							x=csub(cmplx(1.0,0.0),rtb111);
							y=csub(cmplx(1.0,0.0),rtb122);
							det=csub(cmul(x,y),cmul(rtb112,rtb121));
							rvrb111=cdiv(y,det);
							rvrb112=cdiv(rtb112,det);
							rvrb121=cdiv(rtb121,det);
							rvrb122=cdiv(x,det);
							rtb111=cadd(cmul(rvrb111,wrn011),
								cmul(rvrb112,wrn021));
							rtb112=cadd(cmul(rvrb111,wrn012),
								cmul(rvrb112,wrn022));
							rtb121=cadd(cmul(rvrb121,wrn011),
								cmul(rvrb122,wrn021));
							rtb122=cadd(cmul(rvrb121,wrn012),
								cmul(rvrb122,wrn022));
							rtb211=cadd(cmul(ewrd11,rtb111),
								cmul(ewrd12,rtb121));
							rtb212=cadd(cmul(ewrd11,rtb112),
								cmul(ewrd12,rtb122));
							rtb221=cadd(cmul(ewrd21,rtb111),
								cmul(ewrd22,rtb121));
							rtb222=cadd(cmul(ewrd21,rtb112),
								cmul(ewrd22,rtb122));
							rvrb211=cadd(cmplx(1.0,0.0),rtb211);
							rvrb212=rtb212;
							rvrb221=rtb221;
							rvrb222=cadd(cmplx(1.0,0.0),rtb222);
							rtb111=cadd(ewigh11,td0n11[ip]);
							rtb112=cadd(ewigh11,td0n12[ip]);
							rtb121=cadd(ewigh22,td0n21[ip]);
							rtb122=cadd(ewigh22,td0n22[ip]);
							ewd0211=cadd(cmul(rvrb111,rtb111),
								cmul(rvrb112,rtb121));
							ewd0212=cadd(cmul(rvrb111,rtb112),
								cmul(rvrb112,rtb122));
							ewd0221=cadd(cmul(rvrb121,rtb111),
								cmul(rvrb122,rtb121));
							ewd0222=cadd(cmul(rvrb121,rtb112),
								cmul(rvrb122,rtb122));
							td0n11[ip]=cadd(cmul(ewd0211,td11),
								cmul(ewd0221,td12));
							td0n12[ip]=cadd(cmul(ewd0212,td11),
								cmul(ewd0222,td12));
							td0n21[ip]=cadd(cmul(ewd0211,td21),
								cmul(ewd0221,td22));
							td0n22[ip]=cadd(cmul(ewd0212,td21),
								cmul(ewd0222,td22));
							rtb111=cmul(ewigh11,tu11);
							rtb112=cmul(ewigh11,tu12);
							rtb121=cmul(ewigh22,tu21);
							rtb122=cmul(ewigh22,tu22);
							ewtu211=cadd(cmul(rvrb211,rtb111),
								cmul(rvrb212,rtb121));
							ewtu212=cadd(cmul(rvrb211,rtb112),
								cmul(rvrb212,rtb122));
							ewtu221=cadd(cmul(rvrb221,rtb111),
								cmul(rvrb222,rtb121));
							ewtu222=cadd(cmul(rvrb221,rtb112),
								cmul(rvrb222,rtb122));
							tun0p11=cadd(cmul(tun011[ip],ewtu211),
								cmul(tun012[ip],ewtu221));
							tun0p12=cadd(cmul(tun011[ip],ewtu212),
								cmul(tun012[ip],ewtu222));
							tun0p21=cadd(cmul(tun021[ip],ewtu211),
								cmul(tun022[ip],ewtu221));
							tun0p22=cadd(cmul(tun021[ip],ewtu212),
								cmul(tun022[ip],ewtu222));
							rtb111=cadd(cmul(ewrd11,ewd0211),
								cmul(ewd0221,ewrd12));
							rtb112=cadd(cmul(ewrd11,ewd0212),
								cmul(ewd0222,ewrd12));
							rtb121=cadd(cmul(ewrd21,ewd0211),
								cmul(ewd0221,ewrd22));
							rtb122=cadd(cmul(ewrd21,ewd0212),
								cmul(ewd0222,ewrd22));
							rtb211=cadd(cmul(rtb111,tun011[ip]),cmul(rtb121,
								tun012[ip]));
							rtb212=cadd(cmul(rtb112,tun011[ip]),cmul(rtb122,
								tun012[ip]));
							rtb221=cadd(cmul(rtb111,tun021[ip]),cmul(rtb121,
								tun022[ip]));
							rtb222=cadd(cmul(rtb112,tun021[ip]),cmul(rtb122,
								tun022[ip]));
							rd0np11=cadd(rd0n11[ip],rtb211);
							rd0np12=cadd(rd0n12[ip],rtb212);
							rd0np21=cadd(rd0n21[ip],rtb221);
							rd0np22=cadd(rd0n22[ip],rtb222);
							rtb111=cadd(cmul(wrn011,ewtu211),
								cmul(wrn012,ewtu221));
							rtb112=cadd(cmul(wrn011,ewtu212),
								cmul(wrn012,ewtu222));
							rtb121=cadd(cmul(wrn021,ewtu211),
								cmul(wrn022,ewtu221));
							rtb122=cadd(cmul(wrn021,ewtu212),
								cmul(wrn022,ewtu222));
							rtb211=cadd(cmul(td11,rtb111),cmul(td12,rtb121));
							rtb212=cadd(cmul(td11,rtb112),cmul(td12,rtb122));
							rtb221=cadd(cmul(td21,rtb111),cmul(td22,rtb121));
							rtb222=cadd(cmul(td21,rtb112),cmul(td22,rtb122));
							run011[ip]=cadd(ru11,rtb211);
							run012[ip]=cadd(ru12,rtb212);
							run021[ip]=cadd(ru21,rtb221);
							run022[ip]=cadd(ru22,rtb222);
							rd0n11[ip]=rd0np11;
							rd0n12[ip]=rd0np12;
							rd0n21[ip]=rd0np21;
							rd0n22[ip]=rd0np22;
							tun011[ip]=tun0p11;
							tun012[ip]=tun0p12;
							tun021[ip]=tun0p21;
							tun022[ip]=tun0p22;
						}
					} else if (((at1.r!=0.)||(at1.i!=0.))&&((at2.r!=0.)||
						(at2.i!=0.))){
			
						/* case 4: solid-solid */
						if ((al1.r==al2.r)&&(al1.i==al2.i)&&(at1.r==at2.r)
							&&(at1.i==at2.i)) {		
							for(ip=0;ip<block_size;ip++) {
								ijk1=ik1+ip;
								gl1=gl[ijk1];
								gt1=gt[ijk1];
								ewigh11=cwp_cexp(cmul(cmplx(0.0,1.0),cmul(t1,cmul(wpie,gl1))));
								ewigh22=cwp_cexp(cmul(cmplx(0.0,1.0),cmul(t1,cmul(wpie,gt1))));
								tun0p11=cmul(tun011[ip],ewigh11);
								tun0p12=cmul(tun012[ip],ewigh22);
								tun0p21=cmul(tun021[ip],ewigh11);
								tun0p22=cmul(tun022[ip],ewigh22);
								rtb111=cmul(ewigh11,run011[ip]);
								rtb112=cmul(ewigh11,run012[ip]);
								rtb121=cmul(ewigh22,run021[ip]);
								rtb122=cmul(ewigh22,run022[ip]);
								run011[ip]=cmul(ewigh11,rtb111);
								run012[ip]=cmul(ewigh22,rtb112);
								run021[ip]=cmul(ewigh11,rtb121);
								run022[ip]=cmul(ewigh22,rtb122);
								td0np11=cmul(ewigh11,td0n11[ip]);
								td0np12=cmul(ewigh11,td0n12[ip]);
								td0np21=cmul(ewigh22,td0n21[ip]);
								td0np22=cmul(ewigh22,td0n22[ip]);
								tun011[ip]=tun0p11;
								tun012[ip]=tun0p12;
								tun021[ip]=tun0p21;
								tun022[ip]=tun0p22;
								td0n11[ip]=td0np11;
								td0n12[ip]=td0np12;
								td0n21[ip]=td0np21;
								td0n22[ip]=td0np22;
							}	
						} else {
							d=crmul(csub(cdiv(rho2,cmul(at2,at2)),cdiv(rho1,
								cmul(at1,at1))),2.0);
							for (ip=0;ip<block_size;ip++) {
								ijk1=ik1+ip;
								ijk2=ik2+ip;
								psq=pp[ip];
								gl1=gl[ijk1];
								gt1=gt[ijk1];
								gam1=gam[ijk1];
								gl2=gl[ijk2];
								gt2=gt[ijk2];
								gam2=gam[ijk2];
								ewigh11=cwp_cexp(cmul(cmplx(0.0,1.0),cmul(wpie,cmul(gl1,t1))));
								ewigh22=cwp_cexp(cmul(cmplx(0.0,1.0),cmul(wpie,cmul(gt1,t1))));
								a=csub(gam2,gam1);
								b=cadd(rho1,a);
								c=csub(rho2,a);
								e=cadd(cmul(gl1,b),cmul(gl2,c));
								f=cadd(cmul(gt1,b),cmul(gt2,c));
								g=csub(a,cmul(gl1,cmul(gt2,d)));
								h=csub(a,cmul(gl2,cmul(gt1,d)));
								z=cdiv(cmplx(1.0,0.0),cadd(cmul(e,f),cmul(g,cmul(psq,h))));
								y=cmul(z,crmul(rho1,2.0));
								td11=cmul(y,cmul(gl1,f));
								td12=cmul(y,cmul(gt1,cmul(p[ip],g)));
								td21=cneg(cmul(y,cmul(gl1,cmul(p[ip],h))));
								td22=cmul(y,cmul(gt1,e));
								y=crmul(cmul(z,cmul(p[ip],cadd(cmul(a,b),
									cmul(gl2,cmul(gt2,cmul(c,d)))))),2.0);
								rd11=cmul(z,csub(cmul(f,csub(cmul(gl1,b),
									cmul(gl2,c))),cmul(cadd(a,cmul(gl1,
									cmul(gt2,d))),cmul(psq,h))));
								rd12=cmul(y,gt1);
								rd21=cneg(cmul(y,gl1));
								rd22=cmul(z,csub(cmul(e,csub(cmul(gt1,b),
									cmul(gt2,c))),cmul(cadd(a,cmul(gt1,
									cmul(gl2,d))),cmul(psq,g))));
								y=cmul(z,crmul(rho2,2.0));
								tu11=cmul(y,cmul(gl2,f));
								tu12=cmul(y,cmul(gt2,cmul(p[ip],h)));
								tu21=cneg(cmul(y,cmul(gl2,cmul(p[ip],g))));
								tu22=cmul(y,cmul(gt2,e));
								y=crmul(cmul(z,cmul(p[ip],cadd(cmul(a,c),
									cmul(gl1,cmul(gt1,cmul(b,d)))))),2.0);
								ru11=cmul(z,csub(cmul(f,csub(cmul(gl2,c),
									cmul(gl1,b))),cmul(cadd(a,cmul(gl2,
									cmul(gt1,d))),cmul(psq,g))));
								ru12=cmul(y,gt2);
								ru21=cneg(cmul(y,gl2));
								ru22=cmul(z,csub(cmul(e,csub(cmul(gt2,c),
									cmul(gt1,b))),cmul(cadd(a,cmul(gl1,
									cmul(gt2,d))),cmul(psq,h))));
								ewrd11=cmul(ewigh11,rd11);
								ewrd12=cmul(ewigh11,rd12);
								ewrd21=cmul(ewigh22,rd21);
								ewrd22=cmul(ewigh22,rd22);
								wrn011=cmul(ewigh11,run011[ip]);
								wrn012=cmul(ewigh11,run012[ip]);
								wrn021=cmul(ewigh22,run021[ip]);
								wrn022=cmul(ewigh22,run022[ip]);
								rtb111=cadd(cmul(wrn011,ewrd11),
									cmul(wrn012,ewrd21));
								rtb112=cadd(cmul(wrn011,ewrd12),
									cmul(wrn012,ewrd22));
								rtb121=cadd(cmul(wrn021,ewrd11),
									cmul(wrn022,ewrd21));
								rtb122=cadd(cmul(wrn021,ewrd12),
									cmul(wrn022,ewrd22));
								x=csub(cmplx(1.0,0.0),rtb111);
								y=csub(cmplx(1.0,0.0),rtb122);
								det=csub(cmul(x,y),cmul(rtb112,rtb121));
								rvrb111=cdiv(y,det);
								rvrb112=cdiv(rtb112,det);
								rvrb121=cdiv(rtb121,det);
								rvrb122=cdiv(x,det);
								rtb111=cadd(cmul(wrn011,rvrb111),
									cmul(rvrb112,wrn021));
								rtb112=cadd(cmul(wrn012,rvrb111),
									cmul(rvrb112,wrn022));
								rtb121=cadd(cmul(wrn011,rvrb121),
									cmul(rvrb122,wrn021));
								rtb122=cadd(cmul(wrn012,rvrb121),
									cmul(rvrb122,wrn022));
								rtb211=cadd(cmul(ewrd11,rtb111),
									cmul(ewrd12,rtb121));
								rtb212=cadd(cmul(ewrd11,rtb112),
									cmul(ewrd12,rtb122));
								rtb221=cadd(cmul(ewrd21,rtb111),
									cmul(ewrd22,rtb121));
								rtb222=cadd(cmul(ewrd21,rtb112),
									cmul(ewrd22,rtb122));
								rvrb211=cadd(cmplx(1.0,0.0),rtb211);
								rvrb212=rtb212;
								rvrb221=rtb221;
								rvrb222=cadd(cmplx(1.0,0.0),rtb222);
								rtb111=cmul(ewigh11,td0n11[ip]);
								rtb112=cmul(ewigh11,td0n12[ip]);
								rtb121=cmul(ewigh22,td0n21[ip]);
								rtb122=cmul(ewigh22,td0n22[ip]);
								ewd0211=cadd(cmul(rvrb111,rtb111),
									cmul(rvrb112,rtb121));
								ewd0212=cadd(cmul(rvrb111,rtb112),
									cmul(rvrb112,rtb122));
								ewd0221=cadd(cmul(rvrb121,rtb111),
									cmul(rvrb122,rtb121));
								ewd0222=cadd(cmul(rvrb121,rtb112),
									cmul(rvrb122,rtb122));
								td0n11[ip]=cadd(cmul(td11,ewd0211),
									cmul(ewd0221,td12));
								td0n12[ip]=cadd(cmul(td11,ewd0212),
									cmul(ewd0222,td12));
								td0n21[ip]=cadd(cmul(td21,ewd0211),
									cmul(ewd0221,td22));
								td0n22[ip]=cadd(cmul(td21,ewd0212),
									cmul(ewd0222,td22));
								rtb111=cmul(ewigh11,tu11);
								rtb112=cmul(ewigh11,tu12);
								rtb121=cmul(ewigh22,tu21);
								rtb122=cmul(ewigh22,tu22);
								ewtu211=cadd(cmul(rvrb211,rtb111),
									cmul(rvrb212,rtb121));
								ewtu212=cadd(cmul(rvrb211,rtb112),
									cmul(rvrb212,rtb122));
								ewtu221=cadd(cmul(rvrb221,rtb111),
									cmul(rvrb222,rtb121));
								ewtu222=cadd(cmul(rvrb221,rtb112),
									cmul(rvrb222,rtb122));
								tun0p11=cadd(cmul(tun011[ip],ewtu211),
									cmul(tun012[ip],ewtu221));
								tun0p12=cadd(cmul(tun011[ip],ewtu212),	
									cmul(tun012[ip],ewtu222));
								tun0p21=cadd(cmul(tun021[ip],ewtu211),
									cmul(tun022[ip],ewtu221));
								tun0p22=cadd(cmul(tun021[ip],ewtu212),
									cmul(tun022[ip],ewtu222));
								rtb111=cadd(cmul(ewrd11,ewd0211),
									cmul(ewrd12,ewd0221));
								rtb112=cadd(cmul(ewrd11,ewd0212),
									cmul(ewrd12,ewd0222));
								rtb121=cadd(cmul(ewrd21,ewd0211),
									cmul(ewrd22,ewd0221));
								rtb122=cadd(cmul(ewrd21,ewd0212),
									cmul(ewrd22,ewd0222));
								rtb211=cadd(cmul(rtb111,tun011[ip]),cmul(rtb121,
									tun012[ip]));
								rtb212=cadd(cmul(rtb112,tun011[ip]),cmul(rtb122,
									tun012[ip]));
								rtb221=cadd(cmul(rtb111,tun021[ip]),cmul(rtb121,
									tun022[ip]));
								rtb222=cadd(cmul(rtb112,tun021[ip]),cmul(rtb122,
									tun022[ip]));
								rd0np11=cadd(rd0n11[ip],rtb211);
								rd0np12=cadd(rd0n12[ip],rtb212);
								rd0np21=cadd(rd0n21[ip],rtb221);
								rd0np22=cadd(rd0n22[ip],rtb222);
								rtb111=cadd(cmul(wrn011,ewtu211),
									cmul(ewtu221,wrn012));
								rtb112=cadd(cmul(wrn011,ewtu212),
									cmul(ewtu222,wrn012));
								rtb121=cadd(cmul(wrn021,ewtu211),
									cmul(ewtu221,wrn022));
								rtb122=cadd(cmul(wrn021,ewtu212),
									cmul(ewtu222,wrn022));
								rtb211=cadd(cmul(td11,rtb111),
									cmul(td12,rtb121));
								rtb212=cadd(cmul(td11,rtb112),
									cmul(td12,rtb122));
								rtb221=cadd(cmul(td21,rtb111),
									cmul(td22,rtb121));
								rtb222=cadd(cmul(td21,rtb112),
									cmul(td22,rtb122));
								run011[ip]=cadd(ru11,rtb211);
								run012[ip]=cadd(ru12,rtb212);
								run021[ip]=cadd(ru21,rtb221);
								run022[ip]=cadd(ru22,rtb222);
								rd0n11[ip]=rd0np11;
								rd0n12[ip]=rd0np12;
								rd0n21[ip]=rd0np21;
								rd0n22[ip]=rd0np22;
								tun011[ip]=tun0p11;
								tun012[ip]=tun0p12;
								tun021[ip]=tun0p21;
								tun022[ip]=tun0p22;
							}
						}
					}	
				}					/* closes "if" of layers after first one*/

				if (il==lobs[ijk]-2) {
					ik1=ijk*block_size;
					if (acoustic[ijk]==2.||acoustic[ijk]==4.) {
						for (ip=0; ip<block_size; ip++) {
							iz=ik1+ip;
							rurf11[iz]=run011[ip];
							rurf12[iz]=run012[ip];
							rurf21[iz]=run021[ip];
							rurf22[iz]=run022[ip];
							rdfr11[iz]=rd0n11[ip];
							rdfr12[iz]=rd0n12[ip];
							rdfr21[iz]=rd0n21[ip];
							rdfr22[iz]=rd0n22[ip];
							turf11[iz]=tun011[ip];
							turf12[iz]=tun012[ip];
							turf21[iz]=tun021[ip];
							turf22[iz]=tun022[ip];
							tdfr11[iz]=td0n11[ip];
							tdfr12[iz]=td0n12[ip];
							tdfr21[iz]=td0n21[ip];
							tdfr22[iz]=td0n22[ip];
							tun011[ip]=cmplx(1.0,0.0);
							tun012[ip]=cmplx(0.0,0.0);
							tun021[ip]=cmplx(0.0,0.0);
							tun022[ip]=cmplx(1.0,0.0);
							td0n11[ip]=cmplx(1.0,0.0);
							td0n12[ip]=cmplx(0.0,0.0);
							td0n21[ip]=cmplx(0.0,0.0);
							td0n22[ip]=cmplx(1.0,0.0);
							run011[ip]=cmplx(0.0,0.0);
							run012[ip]=cmplx(0.0,0.0);
							run021[ip]=cmplx(0.0,0.0);
							run022[ip]=cmplx(0.0,0.0);
							rd0n11[ip]=cmplx(0.0,0.0);
							rd0n12[ip]=cmplx(0.0,0.0);
							rd0n21[ip]=cmplx(0.0,0.0);
							rd0n22[ip]=cmplx(0.0,0.0);
						}
					} else {
						for (ip=0; ip<block_size; ip++) {
							iz=ik1+ip;
							rurf11[iz]=run011[ip];
							rurf12[iz]=run012[ip];
							rurf21[iz]=run021[ip];
							rurf22[iz]=run022[ip];
							rdfr11[ip]=rd0n11[ip];
							rdfr12[ip]=rd0n12[ip];
							rdfr21[ip]=rd0n21[ip];
							rdfr22[ip]=rd0n22[ip];
							turf11[iz]=tun011[ip];
							turf12[iz]=tun012[ip];
							turf21[iz]=tun021[ip];
							turf22[iz]=tun022[ip];
							tdfr11[iz]=td0n11[ip];
							tdfr12[iz]=td0n12[ip];
							tdfr21[iz]=td0n21[ip];
							tdfr22[iz]=td0n22[ip];
						}
					}
					ijk++;
				}
				if (il==lsource-2) {
					for (ip=0; ip<block_size; ip++) {
						rusf11[ip]=run011[ip];
						rusf12[ip]=run012[ip];
						rusf21[ip]=run021[ip];
						rusf22[ip]=run022[ip];
						rdfs11[ip]=rd0n11[ip];
						rdfs12[ip]=rd0n12[ip];
						rdfs21[ip]=rd0n21[ip];
						rdfs22[ip]=rd0n22[ip];
						run011[ip]=cmplx(0.0,0.0);
						run012[ip]=cmplx(0.0,0.0);
						run021[ip]=cmplx(0.0,0.0);
						run022[ip]=cmplx(0.0,0.0);
						rd0n11[ip]=cmplx(0.0,0.0);
						rd0n12[ip]=cmplx(0.0,0.0);
						rd0n21[ip]=cmplx(0.0,0.0);
						rd0n22[ip]=cmplx(0.0,0.0);
						tusf11[ip]=tun011[ip];
						tusf12[ip]=tun012[ip];
						tusf21[ip]=tun021[ip];
						tusf22[ip]=tun022[ip];
						tdfs11[ip]=td0n11[ip];
						tdfs12[ip]=td0n12[ip];
						tdfs21[ip]=td0n21[ip];
						tdfs22[ip]=td0n22[ip];
						tun011[ip]=cmplx(1.0,0.0);
						tun012[ip]=cmplx(0.0,0.0);
						tun021[ip]=cmplx(0.0,0.0);
						tun022[ip]=cmplx(1.0,0.0);
						td0n11[ip]=cmplx(1.0,0.0);
						td0n12[ip]=cmplx(0.0,0.0);
						td0n21[ip]=cmplx(0.0,0.0);
						td0n22[ip]=cmplx(1.0,0.0);
					}
				}
			}

			/* loop over receivers */
			flg1=0;
			for (iz=0; iz<nor; iz++) {
				ik1=iz*block_size;
				for (ip=0; ip<block_size; ip++) {
					ijk1=ik1+ip;	
					if (acoustic[iz]==1) {
						det=csub(rd0n11[ip],rdfr11[ijk1]);
					} else {
						det=csub(cmul(csub(rd0n11[ip],rdfr11[ijk1]),
							csub(rd0n22[ip],rdfr22[ijk1])),cmul(csub(rd0n12[ip],
							rdfr12[ijk1]),csub(rd0n21[ip],rdfr21[ijk1])));
					}	
					if (rcabs(det)<=SZERO) {
						prv[iz]=flag[iz];
						flag[iz]=2;
						ibl[iz]=ip;
						break;
					}
				}
				if (acoustic[iz]==2) {
					flg1=1;
					for (ip=0; ip<block_size; ip++) {
						ijk1=ik1+ip;
						rnr11[ip]=rd0n11[ip];
						rnr12[ip]=rd0n12[ip];
						rnr21[ip]=rd0n21[ip];
						rnr22[ip]=rd0n22[ip];
						tdsr11[ip]=tdfr11[ijk1];
						tdsr12[ip]=tdfr12[ijk1];
						tdsr21[ip]=tdfr21[ijk1];
						tdsr22[ip]=tdfr22[ijk1];
						tusr11[ip]=turf11[ijk1];
						tusr12[ip]=turf12[ijk1];
						tusr21[ip]=turf21[ijk1];
						tusr22[ip]=turf22[ijk1];
						rdsr11[ip]=rdfr11[ijk1];	
						rdsr12[ip]=rdfr12[ijk1];	
						rdsr21[ip]=rdfr21[ijk1];	
						rdsr22[ip]=rdfr22[ijk1];	
						rusr11[ip]=rurf11[ijk1];
						rusr12[ip]=rurf12[ijk1];
						rusr21[ip]=rurf21[ijk1];
						rusr22[ip]=rurf22[ijk1];
						rtb111=cadd(cmul(rurf11[ijk1],rnr11[ip]),
							cmul(rurf12[ijk1],rnr21[ip]));
						rtb112=cadd(cmul(rurf11[ijk1],rnr12[ip]),
							cmul(rurf12[ijk1],rnr22[ip]));
						rtb121=cadd(cmul(rurf21[ijk1],rnr11[ip]),
							cmul(rurf22[ijk1],rnr21[ip]));
						rtb122=cadd(cmul(rurf21[ijk1],rnr12[ip]),
							cmul(rurf22[ijk1],rnr22[ip]));
						x=csub(cmplx(1.0,0.0),rtb111);
						y=csub(cmplx(1.0,0.0),rtb122);
						det=csub(cmul(x,y),cmul(rtb112,rtb121));
						rvrb111=cdiv(y,det);
						rvrb112=cdiv(rtb112,det);
						rvrb121=cdiv(rtb121,det);
						rvrb122=cdiv(x,det);
						rtb111=cadd(cmul(rvrb111,tdfr11[ijk1]),cmul(rvrb112,
							tdfr21[ijk1]));
						rtb112=cadd(cmul(rvrb111,tdfr12[ijk1]),cmul(rvrb112,
							tdfr22[ijk1]));
						rtb121=cadd(cmul(rvrb121,tdfr11[ijk1]),cmul(rvrb122,
							tdfr21[ijk1]));
						rtb122=cadd(cmul(rvrb121,tdfr12[ijk1]),cmul(rvrb122,
							tdfr22[ijk1]));
						rtb211=cadd(cmul(rnr11[ip],rtb111),
							cmul(rnr12[ip],rtb121));	
						rtb212=cadd(cmul(rnr11[ip],rtb112),
							cmul(rnr12[ip],rtb122));	
						rtb221=cadd(cmul(rnr21[ip],rtb111),
							cmul(rnr22[ip],rtb121));	
						rtb222=cadd(cmul(rnr21[ip],rtb112),
							cmul(rnr22[ip],rtb122));	
						rd0n11[ip]=cadd(cmul(rtb211,turf11[ijk1]),
							cadd(cmul(rtb221,turf12[ijk1]),rdfr11[ijk1]));
						rd0n12[ip]=cadd(cmul(rtb212,turf11[ijk1]),
							cadd(cmul(rtb222,turf12[ijk1]),rdfr12[ijk1]));
						rd0n21[ip]=cadd(cmul(rtb211,turf21[ijk1]),
							cadd(cmul(rtb221,turf22[ijk1]),rdfr21[ijk1]));
						rd0n22[ip]=cadd(cmul(rtb212,turf21[ijk1]),
							cadd(cmul(rtb222,turf22[ijk1]),rdfr22[ijk1]));
					}
				}
			}

			/* loop over receivers */
			for (iz=0; iz<nor; iz++) {
				ik1=iz*block_size;
				lrec=lobs[iz];
				ik2=(lrec-1)*block_size;
			
				/* receiver above the source */
				if (lsource>lrec) {
					if (acoustic[iz]==4) {
						for (ip=0; ip<block_size; ip++) {
							ijk1=ik1+ip;
							ijk2=ik2+ip;
							rtb111=cadd(cmul(rdfs11[ip],rurf11[ijk1]),
								cmul(rdfs12[ip],rurf21[ijk1]));
							rtb112=cadd(cmul(rdfs11[ip],rurf12[ijk1]),
								cmul(rdfs12[ip],rurf22[ijk1]));
							rtb121=cadd(cmul(rdfs21[ip],rurf11[ijk1]),
								cmul(rdfs22[ip],rurf21[ijk1]));
							rtb122=cadd(cmul(rdfs21[ip],rurf12[ijk1]),
								cmul(rdfs22[ip],rurf22[ijk1]));
							x=csub(cmplx(1.0,0.0),rtb111);
							y=csub(cmplx(1.0,0.0),rtb122);
							det=csub(cmul(x,y),cmul(rtb112,rtb121));
							rvrb111=cdiv(y,det);
							rvrb112=cdiv(rtb112,det);
							rvrb121=cdiv(rtb121,det);
							rvrb122=cdiv(x,det);
							rtb111=cadd(cmul(rvrb111,tusf11[ip]),cmul(rvrb112,
								tusf21[ip]));
							rtb112=cadd(cmul(rvrb111,tusf12[ip]),cmul(rvrb112,
								tusf22[ip]));
							rtb121=cadd(cmul(rvrb121,tusf11[ip]),cmul(rvrb122,
								tusf21[ip]));
							rtb122=cadd(cmul(rvrb121,tusf12[ip]),cmul(rvrb122,
								tusf22[ip]));
							rtb211=cadd(cmul(rurf11[ijk1],rtb111),
								cmul(rurf12[ijk1],rtb121));
							rtb212=cadd(cmul(rurf11[ijk1],rtb112),
								cmul(rurf12[ijk1],rtb122));
							rtb221=cadd(cmul(rurf21[ijk1],rtb111),
								cmul(rurf22[ijk1],rtb121));
							rtb222=cadd(cmul(rurf21[ijk1],rtb112),
								cmul(rurf22[ijk1],rtb122));
							rtb311=cadd(cmul(tdfs11[ip],rtb211),cmul(tdfs12[ip],
								rtb221));
							rtb312=cadd(cmul(tdfs11[ip],rtb212),cmul(tdfs12[ip],
								rtb222));
							rtb321=cadd(cmul(tdfs21[ip],rtb211),cmul(tdfs22[ip],
								rtb221));
							rtb322=cadd(cmul(tdfs21[ip],rtb212),cmul(tdfs22[ip],
								rtb222));
							rusf11[ip]=cadd(rusf11[ip],rtb311);
							rusf12[ip]=cadd(rusf12[ip],rtb312);
							rusf21[ip]=cadd(rusf21[ip],rtb321);
							rusf22[ip]=cadd(rusf22[ip],rtb322);
							rtb311=cadd(cmul(rd0n11[ip],rusf11[ip]),
								cmul(rd0n12[ip],rusf21[ip]));
							rtb312=cadd(cmul(rd0n11[ip],rusf12[ip]),
								cmul(rd0n12[ip],rusf22[ip]));
							rtb321=cadd(cmul(rd0n21[ip],rusf11[ip]),
								cmul(rd0n22[ip],rusf21[ip]));
							rtb322=cadd(cmul(rd0n21[ip],rusf12[ip]),
								cmul(rd0n22[ip],rusf22[ip]));
							x=csub(cmplx(1.0,0.0),rtb311);
							y=csub(cmplx(1.0,0.0),rtb322);
							det=csub(cmul(x,y),cmul(rtb312,rtb321));
							rvrb111=cdiv(y,det);
							rvrb112=cdiv(rtb312,det);
							rvrb121=cdiv(rtb321,det);
							rvrb122=cdiv(x,det);
							fact11=cadd(cmul(sigmad1[ip],rd0n11[ip]),
								cadd(sigmau1[ip],cmul(rd0n12[ip],sigmad2[ip])));
							fact12=cadd(cmul(rd0n21[ip],sigmad1[ip]),
								cadd(sigmau2[ip],cmul(rd0n22[ip],sigmad2[ip])));
							vuzs1=cadd(cmul(rvrb111,fact11),
								cmul(rvrb112,fact12));
							vuzs2=cadd(cmul(rvrb121,fact11),
								cmul(rvrb122,fact12));
							up[ip]=cmul(prs[iz],cadd(cmul(vuzs1,cadd(rtb111,
								rtb211)),cmul(vuzs2,cadd(rtb112,rtb212))));
							rtb311=cadd(cmul(rurf11[ijk1],p[ip]),
								csub(cmul(p[ip],gt[ijk2]),rurf21[ijk1]));
							rtb312=cadd(cmul(rurf12[ijk1],p[ip]),
								csub(cmul(p[ip],gt[ijk2]),rurf22[ijk1]));
							rtb321=cadd(cmul(rurf11[ijk1],gl[ijk2]),
								csub(cmul(p[ip],rurf21[ijk1]),gl[ijk2]));
							rtb322=cadd(cmul(rurf12[ijk1],gl[ijk2]),
								cadd(cmul(p[ip],rurf22[ijk1]),p[ip]));
							rtb211=cadd(cmul(rtb311,rtb111),
								cmul(rtb312,rtb121));
							rtb212=cadd(cmul(rtb311,rtb112),
								cmul(rtb312,rtb122));
							rtb221=cadd(cmul(rtb321,rtb111),
								cmul(rtb322,rtb121));
							rtb222=cadd(cmul(rtb321,rtb112),
								cmul(rtb322,rtb122));
							ux[ip]=cadd(cmul(rtb211,vuzs1),cmul(rtb212,vuzs2));
							uz[ip]=cadd(cmul(rtb221,vuzs1),cmul(rtb222,vuzs2));
						}
					} else {
	
						/* source elastic,receiver elastic or*/
						/* source acoustic, receiver elastic */
						if (acoustic[iz]==0||acoustic[iz]==3) {
							for (ip=0; ip<block_size; ip++) {
								ijk1=ik1+ip;
								det=csub(cmul(turf11[ijk1],turf22[ijk1]),
									cmul(turf21[ijk1],turf12[ijk1]));
								rtb211=cdiv(turf22[ijk1],det);
								rtb212=cneg(cdiv(turf12[ijk1],det));
								rtb221=cneg(cdiv(turf21[ijk1],det));
								rtb222=cdiv(turf11[ijk1],det);
								rdnr11[ip]=cadd(cmul(rtb211,tusf11[ip]),
									cmul(rtb212,tusf21[ip]));
								rdnr12[ip]=cadd(cmul(rtb211,tusf12[ip]),
									cmul(rtb212,tusf22[ip]));
								rdnr21[ip]=cadd(cmul(rtb221,tusf11[ip]),
									cmul(rtb222,tusf21[ip]));
								rdnr22[ip]=cadd(cmul(rtb221,tusf12[ip]),
									cmul(rtb222,tusf22[ip]));
							}
						} else {
			
							/* source elastic,receiver acoustic */
							/* or source and receiver acoustic */
							for (ip=0; ip<block_size; ip++) {
								ijk1=ik1+ip;
								rtb111=cdiv(cmplx(1.0,0.0),turf11[ijk1]);
								rdnr11[ip]=cmul(rtb111,tusf11[ip]);
								rdnr12[ip]=cmul(rtb111,tusf12[ip]);
								rdnr21[ip]=cmplx(0.0,0.0);
								rdnr22[ip]=cmplx(0.0,0.0);
							}
						}
	
						for (ip=0; ip<block_size; ip++) {
							ijk1=ik1+ip;
							ijk2=ik2+ip;
							rtb111=rdnr11[ip];
							rtb112=rdnr12[ip];
							rtb121=rdnr21[ip];
							rtb122=rdnr22[ip];
							rtb211=cadd(cmul(rurf11[ijk1],rtb111),
								cmul(rurf12[ijk1],rtb121));
							rtb212=cadd(cmul(rurf11[ijk1],rtb112),
								cmul(rurf12[ijk1],rtb122));
							rtb221=cadd(cmul(rurf21[ijk1],rtb111),
								cmul(rurf22[ijk1],rtb121));
							rtb222=cadd(cmul(rurf21[ijk1],rtb112),
								cmul(rurf22[ijk1],rtb122));
							rtb311=cadd(cmul(rd0n11[ip],rusf11[ip]),
								cmul(rd0n12[ip],rusf21[ip]));
							rtb312=cadd(cmul(rd0n11[ip],rusf12[ip]),
								cmul(rd0n12[ip],rusf22[ip]));
							rtb321=cadd(cmul(rd0n21[ip],rusf11[ip]),
								cmul(rd0n22[ip],rusf21[ip]));
							rtb322=cadd(cmul(rd0n21[ip],rusf12[ip]),
								cmul(rd0n22[ip],rusf22[ip]));
							x=csub(cmplx(1.0,0.0),rtb311);
							y=csub(cmplx(1.0,0.0),rtb322);	
							det=csub(cmul(x,y),cmul(rtb312,rtb321));
							rvrb111=cdiv(y,det);
							rvrb112=cdiv(rtb312,det);
							rvrb121=cdiv(rtb321,det);
							rvrb122=cdiv(x,det);
							fact11=cadd(cmul(rd0n11[ip],sigmad1[ip]),
								cadd(cmul(rd0n12[ip],sigmad2[ip]),sigmau1[ip]));
							fact12=cadd(cmul(rd0n21[ip],sigmad1[ip]),
								cadd(cmul(rd0n22[ip],sigmad2[ip]),sigmau2[ip]));
							vuzs1=cadd(cmul(rvrb111,fact11),
								cmul(rvrb112,fact12));
							vuzs2=cadd(cmul(rvrb121,fact11),
								cmul(rvrb122,fact12));
							up[ip]=cmul(prs[iz],cadd(cmul(cadd(rtb111,rtb211),
								vuzs1),cmul(cadd(rtb112,rtb212),vuzs2)));
							rtb211=csub(cmul(rurf11[ijk1],p[ip]),cmul(gt[ijk2],
								rurf21[ijk1]));
							rtb212=csub(cmul(rurf12[ijk1],p[ip]),cmul(gt[ijk2],
								rurf22[ijk1]));
							rtb221=cadd(cmul(rurf11[ijk1],gl[ijk2]),
								cmul(rurf21[ijk1],p[ip]));
							rtb222=cadd(cmul(rurf12[ijk1],gl[ijk2]),
								cmul(rurf22[ijk1],p[ip]));
							rtb311=cadd(p[ip],rtb211);
							rtb312=cadd(gt[ijk2],rtb212);
							rtb321=csub(rtb221,gl[ijk2]);
							rtb322=cadd(p[ip],rtb222);
							rtb211=cadd(cmul(rtb311,rtb111),
								cmul(rtb312,rtb121));
							rtb212=cadd(cmul(rtb311,rtb112),
								cmul(rtb312,rtb122));
							rtb221=cadd(cmul(rtb321,rtb111),
								cmul(rtb322,rtb121));
							rtb222=cadd(cmul(rtb321,rtb112),
								cmul(rtb322,rtb122));
							ux[ip]=cadd(cmul(rtb211,vuzs1),cmul(rtb212,vuzs2));
							uz[ip]=cadd(cmul(rtb221,vuzs1),cmul(rtb222,vuzs2));
						}
					}
				} else {
		
					/* receiver below the source */
					if (flag[iz]==2) {
						if (acoustic[iz]==3) {
							for (ip=0; ip<ibl[iz]-1; ip++) {
								ijk1=ik1+ip;
								rtb111=csub(rnr11[ip],rdfr11[ijk1]);
								rtb112=csub(rnr12[ip],rdfr12[ijk1]);
								rtb121=csub(rnr21[ip],rdfr21[ijk1]);
								rtb122=csub(rnr22[ip],rdfr22[ijk1]);
								det=csub(cmul(rtb111,rtb122),
									cmul(rtb112,rtb121));
								rtb211=cdiv(rtb122,det);
								rtb212=cneg(cdiv(rtb112,det));
								rtb221=cneg(cdiv(rtb121,det));
								rtb222=cdiv(rtb111,det);
								rtb311=cadd(cmul(turf11[ijk1],rtb211),
									cmul(rtb212,turf21[ijk1]));
								rtb312=cadd(cmul(turf12[ijk1],rtb211),
									cmul(rtb212,turf22[ijk1]));
								rtb321=cadd(cmul(turf11[ijk1],rtb221),
									cmul(rtb222,turf21[ijk1]));
								rtb322=cadd(cmul(turf12[ijk1],rtb221),
									cmul(rtb222,turf22[ijk1]));
								rtb111=cadd(cmul(tdfr11[ijk1],rtb311),cadd(
									cmul(rtb321,tdfr12[ijk1]),rurf11[ijk1])); 
								rtb112=cadd(cmul(tdfr11[ijk1],rtb312),cadd(
									cmul(rtb322,tdfr12[ijk1]),rurf12[ijk1]));
								rtb121=cadd(cmul(tdfr21[ijk1],rtb311),cadd(
									cmul(rtb321,tdfr22[ijk1]),rurf21[ijk1]));
								rtb122=cadd(cmul(tdfr21[ijk1],rtb312),cadd(
									cmul(rtb322,tdfr22[ijk1]),rurf22[ijk1]));
								det=csub(cmul(rtb111,rtb122),
									cmul(rtb112,rtb121));
								rdnr11[ip]=cdiv(rtb122,det);
								rdnr12[ip]=cneg(cdiv(rtb112,det));
								rdnr21[ip]=cneg(cdiv(rtb121,det));
								rdnr22[ip]=cdiv(rtb111,det);
								rtb111=cadd(cmul(rusr11[ip],rdfr11[ijk1]),
									cmul(rusr12[ip],rdfr21[ijk1]));
								rtb112=cadd(cmul(rusr11[ip],rdfr12[ijk1]),
									cmul(rusr12[ip],rdfr22[ijk1]));
								rtb121=cadd(cmul(rusr21[ip],rdfr11[ijk1]),
									cmul(rusr22[ip],rdfr21[ijk1]));
								rtb122=cadd(cmul(rusr21[ip],rdfr12[ijk1]),
									cmul(rusr22[ip],rdfr22[ijk1]));
								x=csub(cmplx(1.0,0.0),rtb111);
								y=csub(cmplx(1.0,0.0),rtb122);
								det=csub(cmul(x,y),cmul(rtb112,rtb121));
								rvrb111=cdiv(y,det);
								rvrb112=cdiv(rtb112,det);
								rvrb121=cdiv(rtb121,det);
								rvrb122=cdiv(x,det);
								rtb211=cadd(cmul(tdfr11[ijk1],rvrb111),
									cmul(tdfr12[ijk1],rvrb121));
								rtb212=cadd(cmul(tdfr11[ijk1],rvrb112),
									cmul(tdfr12[ijk1],rvrb122));
								rtb221=cadd(cmul(tdfr21[ijk1],rvrb111),
									cmul(tdfr22[ijk1],rvrb121));
								rtb222=cadd(cmul(tdfr21[ijk1],rvrb112),
									cmul(tdfr22[ijk1],rvrb122));
								tdfr11[ijk1]=cadd(cmul(rtb211,tdsr11[ip]),
									cmul(rtb212,tdsr21[ip]));
								tdfr12[ijk1]=cadd(cmul(rtb211,tdsr12[ip]),
									cmul(rtb212,tdsr22[ip]));
								tdfr21[ijk1]=cadd(cmul(rtb221,tdsr11[ip]),
									cmul(rtb222,tdsr21[ip]));
								tdfr22[ijk1]=cadd(cmul(rtb221,tdsr12[ip]),
									cmul(rtb222,tdsr22[ip]));
								rtb111=cadd(cmul(rusr11[ip],rtb211),cmul(rtb212,
									rusr21[ip]));
								rtb112=cadd(cmul(rusr12[ip],rtb211),cmul(rtb212,
									rusr22[ip]));
								rtb121=cadd(cmul(rusr11[ip],rtb221),cmul(rtb222,
									rusr21[ip]));
								rtb122=cadd(cmul(rusr12[ip],rtb221),cmul(rtb222,
									rusr22[ip]));
								rurf11[ijk1]=cadd(cmul(rtb111,rurf11[ijk1]),
									cadd(cmul(rtb112,turf21[ijk1]),
									rurf11[ijk1]));
								rurf12[ijk1]=cadd(cmul(rtb111,rurf12[ijk1]),
									cadd(cmul(rtb112,turf22[ijk1]),
									rurf12[ijk1]));
								 rurf21[ijk1]=cadd(cmul(rtb121,rurf11[ijk1]),
									cadd(cmul(rtb122,turf21[ijk1]),
									rurf21[ijk1]));
								rurf22[ijk1]=cadd(cmul(rtb121,rurf12[ijk1]),
									cadd(cmul(rtb122,turf22[ijk1]),
									rurf22[ijk1]));
							}
							for (ip=ibl[iz]-1; ip<block_size; ip++) {
								ijk1=ik1+ip;
								rdnr11[ip]=cmplx(0.0,0.0);
								rdnr12[ip]=cmplx(0.0,0.0);
								rdnr21[ip]=cmplx(0.0,0.0);
								rdnr22[ip]=cmplx(0.0,0.0);
								rtb111=cadd(cmul(rusr11[ip],rdfr11[ijk1]),
									cmul(rusr12[ip],rdfr21[ijk1]));
								rtb112=cadd(cmul(rusr11[ip],rdfr12[ijk1]),
									cmul(rusr12[ip],rdfr22[ijk1]));
								rtb121=cadd(cmul(rusr21[ip],rdfr11[ijk1]),
									cmul(rusr22[ip],rdfr21[ijk1]));
								rtb122=cadd(cmul(rusr21[ip],rdfr12[ijk1]),
									cmul(rusr22[ip],rdfr22[ijk1]));
								x=csub(cmplx(1.0,0.0),rtb111);
								y=csub(cmplx(1.0,0.0),rtb122);
								det=csub(cmul(x,y),cmul(rtb112,rtb121));
								rvrb111=cdiv(y,det);
								rvrb112=cdiv(rtb112,det);
								rvrb121=cdiv(rtb121,det);
								rvrb122=cdiv(x,det);
								rtb211=cadd(cmul(tdfr11[ijk1],rvrb111),
									cmul(rvrb121,tdfr12[ijk1]));
								rtb212=cadd(cmul(tdfr11[ijk1],rvrb112),
									cmul(rvrb122,tdfr12[ijk1]));
								rtb221=cadd(cmul(tdfr21[ijk1],rvrb111),
									cmul(rvrb121,tdfr22[ijk1]));
								rtb222=cadd(cmul(tdfr21[ijk1],rvrb112),
									cmul(rvrb122,tdfr22[ijk1]));
								tdfr11[ijk1]=cadd(cmul(rtb211,tdsr11[ip]),	
									cmul(rtb212,tdsr21[ip]));
								tdfr12[ijk1]=cadd(cmul(rtb211,tdsr12[ip]),
									cmul(rtb212,tdsr22[ip]));
								tdfr21[ijk1]=cadd(cmul(rtb221,tdsr11[ip]),
									cmul(rtb222,tdsr21[ip]));
								tdfr22[ijk1]=cadd(cmul(rtb221,tdsr12[ip]),
									cmul(rtb222,tdsr22[ip]));
								rtb111=cadd(cmul(rtb211,rusr11[ip]),
									cmul(rtb212,rusr21[ip]));
								rtb112=cadd(cmul(rtb211,rusr12[ip]),
									cmul(rtb212,rusr22[ip]));
								rtb121=cadd(cmul(rtb221,rusr11[ip]),
									cmul(rtb222,rusr21[ip]));
								rtb122=cadd(cmul(rtb221,rusr12[ip]),
									cmul(rtb222,rusr22[ip]));
								rurf11[ijk1]=cadd(cmul(rtb111,turf11[ijk1]),
									cadd(cmul(rtb112,turf21[ijk1]),
									rurf11[ijk1]));
								rurf12[ijk1]=cadd(cmul(rtb111,turf12[ijk1]),
									cadd(cmul(rtb112,turf22[ijk1]),
									rurf12[ijk1]));
								rurf21[ijk1]=cadd(cmul(rtb121,turf11[ijk1]),
									cadd(cmul(rtb122,turf21[ijk1]),
								rurf21[ijk1]));
								rurf22[ijk1]=cadd(cmul(rtb121,turf12[ijk1]),
									cadd(cmul(rtb122,turf22[ijk1]),
									rurf22[ijk1]));
							}
						} else if (acoustic[iz]==1) {
							for (ip=0; ip<ibl[iz]-1; ip++) {
								ijk1=ik1+ip;
								rtb211=cdiv(turf11[ijk1],csub(rd0n11[ip],
									rdfr11[ijk1]));
								rdnr11[ip]=cdiv(cmplx(1.0,0.0),cadd(cmul(tdfr11[ijk1],
									rtb211),rurf11[ijk1]));
								rdnr12[ip]=cmplx(0.0,0.0);   
								rdnr21[ip]=cmplx(0.0,0.0);   
								rdnr22[ip]=cmplx(0.0,0.0);   
							}
							for (ip=ibl[iz]-1; ip<block_size; ip++) {
								rdnr11[ip]=cmplx(0.0,0.0);
								rdnr12[ip]=cmplx(0.0,0.0);
								rdnr21[ip]=cmplx(0.0,0.0);
								rdnr22[ip]=cmplx(0.0,0.0);
							}
						} else if (acoustic[iz]==2) {
							for (ip=0; ip<block_size; ip++) {
								rdnr11[ip]=rnr11[ip];
								rdnr12[ip]=rnr12[ip];
								rdnr21[ip]=rnr21[ip];
								rdnr22[ip]=rnr22[ip];
							}
						} else {
							for (ip=0; ip<ibl[iz]-1; ip++) {
								ijk1=ik1+ip;
								rtb111=csub(rd0n11[ip],rdfr11[ijk1]);
								rtb112=csub(rd0n12[ip],rdfr12[ijk1]);
								rtb121=csub(rd0n21[ip],rdfr21[ijk1]);
								rtb122=csub(rd0n22[ip],rdfr22[ijk1]);
								det=csub(cmul(rtb111,rtb122),
									cmul(rtb112,rtb121));
								rtb211=cdiv(rtb122,det);
								rtb212=cneg(cdiv(rtb112,det));
								rtb221=cneg(cdiv(rtb121,det));
								rtb222=cdiv(rtb111,det);
								rtb311=cadd(cmul(rtb211,turf11[ijk1]),
									cmul(rtb212,turf21[ijk1]));
								rtb312=cadd(cmul(rtb211,turf12[ijk1]),
									cmul(rtb212,turf22[ijk1]));
								rtb321=cadd(cmul(rtb221,turf11[ijk1]),
									cmul(rtb222,turf21[ijk1]));
								rtb322=cadd(cmul(rtb221,turf12[ijk1]),
									cmul(rtb222,turf22[ijk1]));
								rtb111=cadd(cmul(rtb311,tdfr11[ijk1]),cadd(
									cmul(rtb321,tdfr12[ijk1]),rurf11[ijk1]));
								rtb112=cadd(cmul(rtb312,tdfr11[ijk1]),cadd(
									cmul(rtb322,tdfr12[ijk1]),rurf12[ijk1]));
								rtb121=cadd(cmul(rtb311,tdfr21[ijk1]),cadd(
									cmul(rtb321,tdfr22[ijk1]),rurf21[ijk1]));
								rtb122=cadd(cmul(rtb312,tdfr21[ijk1]),cadd(
									cmul(rtb322,tdfr22[ijk1]),rurf22[ijk1]));
								det=csub(cmul(rtb111,rtb122),
									cmul(rtb112,rtb121));
								rdnr11[ip]=cdiv(rtb122,det);
								rdnr12[ip]=cneg(cdiv(rtb112,det));
								rdnr21[ip]=cneg(cdiv(rtb121,det));
								rdnr22[ip]=cdiv(rtb111,det);
							}
							for (ip=ibl[iz]-1; ip<block_size; ip++) {
								rdnr11[ip]=cmplx(0.0,0.0);
								rdnr12[ip]=cmplx(0.0,0.0);
								rdnr21[ip]=cmplx(0.0,0.0);
								rdnr22[ip]=cmplx(0.0,0.0);
							}
						}
					} else if (flag[iz]==0) {
						/* source acoustic, 1st elastic rec */
						if (acoustic[iz]==2) {
							for (ip=0; ip<block_size; ip++) {
								rdnr11[ip]=rnr11[ip];
								rdnr12[ip]=rnr12[ip];
								rdnr21[ip]=rnr21[ip];
								rdnr22[ip]=rnr22[ip];
							}
						} else if (acoustic[iz]==3) {
							for (ip=0; ip<block_size; ip++) {
								ijk1=ik1+ip;
								rtb111=csub(rnr11[ip],rdfr11[ijk1]);
								rtb112=csub(rnr12[ip],rdfr12[ijk1]);
								rtb121=csub(rnr21[ip],rdfr21[ijk1]);
								rtb122=csub(rnr22[ip],rdfr22[ijk1]);
								det=csub(cmul(rtb111,rtb122),
									cmul(rtb112,rtb121));
								rtb211=cdiv(rtb122,det);
								rtb212=cneg(cdiv(rtb112,det));
								rtb221=cneg(cdiv(rtb121,det));
								rtb222=cdiv(rtb111,det);
								rtb311=cadd(cmul(turf11[ijk1],rtb211),
									cmul(rtb212,turf21[ijk1]));
								rtb312=cadd(cmul(turf12[ijk1],rtb211),
									cmul(rtb212,turf22[ijk1]));
								rtb321=cadd(cmul(turf11[ijk1],rtb221),
									cmul(rtb222,turf21[ijk1]));
								rtb322=cadd(cmul(turf12[ijk1],rtb221),
									cmul(rtb222,turf22[ijk1]));
								rtb111=cadd(cmul(tdfr11[ijk1],rtb311),cadd(
									cmul(rtb321,tdfr12[ijk1]),rurf11[ijk1]));
								rtb112=cadd(cmul(tdfr11[ijk1],rtb312),cadd(
									cmul(rtb322,tdfr12[ijk1]),rurf12[ijk1]));
								rtb121=cadd(cmul(tdfr21[ijk1],rtb311),cadd(
									cmul(rtb321,tdfr22[ijk1]),rurf21[ijk1]));
								rtb122=cadd(cmul(tdfr21[ijk1],rtb312),cadd(
									cmul(rtb322,tdfr22[ijk1]),rurf22[ijk1]));
								det=csub(cmul(rtb111,rtb122),
									cmul(rtb112,rtb121));
								rdnr11[ip]=cdiv(rtb122,det);
								rdnr12[ip]=cneg(cdiv(rtb112,det));
								rdnr21[ip]=cneg(cdiv(rtb121,det));
								rdnr22[ip]=cdiv(rtb111,det);
								rtb111=cadd(cmul(rusr11[ip],rdfr11[ijk1]),
									cmul(rusr12[ip],rdfr21[ijk1]));
								rtb112=cadd(cmul(rusr11[ip],rdfr12[ijk1]),
									cmul(rusr12[ip],rdfr22[ijk1]));
								rtb121=cadd(cmul(rusr21[ip],rdfr11[ijk1]),
									cmul(rusr22[ip],rdfr21[ijk1]));
								rtb122=cadd(cmul(rusr21[ip],rdfr12[ijk1]),
									cmul(rusr22[ip],rdfr22[ijk1]));
								x=csub(cmplx(1.0,0.0),rtb111);
								y=csub(cmplx(1.0,0.0),rtb122);
								det=csub(cmul(x,y),cmul(rtb112,rtb121));
								rvrb111=cdiv(y,det);
								rvrb112=cdiv(rtb112,det);
								rvrb121=cdiv(rtb121,det);
								rvrb122=cdiv(x,det);
								rtb211=cadd(cmul(tdfr11[ijk1],rvrb111),
									cmul(rvrb121,tdfr12[ijk1]));
								rtb212=cadd(cmul(tdfr11[ijk1],rvrb112),
									cmul(rvrb122,tdfr12[ijk1]));
								rtb221=cadd(cmul(tdfr21[ijk1],rvrb111),
									cmul(rvrb121,tdfr22[ijk1]));
								rtb222=cadd(cmul(tdfr21[ijk1],rvrb112),
									cmul(rvrb122,tdfr22[ijk1]));
								tdfr11[ijk1]=cadd(cmul(rtb211,tdsr11[ip]),
									cmul(rtb212,tdsr21[ip]));
								tdfr12[ijk1]=cadd(cmul(rtb211,tdsr12[ip]),
									cmul(rtb212,tdsr22[ip]));
								tdfr21[ijk1]=cadd(cmul(rtb221,tdsr11[ip]),
									cmul(rtb222,tdsr21[ip]));
								tdfr22[ijk1]=cadd(cmul(rtb221,tdsr12[ip]),
									cmul(rtb222,tdsr22[ip]));
								rtb111=cadd(cmul(rtb211,rusr11[ip]),
									cmul(rtb212,rusr21[ip]));
								rtb112=cadd(cmul(rtb211,rusr12[ip]),cmul(rtb212,
									rusr22[ip]));
								rtb121=cadd(cmul(rtb221,rusr11[ip]),cmul(rtb222,
									rusr21[ip]));
								rtb122=cadd(cmul(rtb221,rusr12[ip]),cmul(rtb222,
									rusr22[ip]));
								rurf11[ijk1]=cadd(cmul(rtb111,turf11[ijk1]),
									cadd(cmul(rtb112,turf21[ijk1]),
									rurf11[ijk1]));
								rurf12[ijk1]=cadd(cmul(rtb111,turf12[ijk1]),
									cadd(cmul(rtb112,turf22[ijk1]),
									rurf12[ijk1]));
								rurf21[ijk1]=cadd(cmul(rtb121,turf11[ijk1]),
									cadd(cmul(rtb122,turf21[ijk1]),
									rurf21[ijk1]));
								rurf22[ijk1]=cadd(cmul(rtb121,turf12[ijk1]),
									cadd(cmul(rtb122,turf22[ijk1]),
									rurf22[ijk1]));
							}
						} else if (acoustic[iz]==0) {
							for (ip=0; ip<block_size; ip++) {
								ijk1=ik1+ip;
								rtb111=csub(rd0n11[ip],rdfr11[ijk1]);
								rtb112=csub(rd0n12[ip],rdfr12[ijk1]);
								rtb121=csub(rd0n21[ip],rdfr21[ijk1]);
								rtb122=csub(rd0n22[ip],rdfr22[ijk1]);
								det=csub(cmul(rtb111,rtb122),
									cmul(rtb112,rtb121));
								rtb211=cdiv(rtb122,det);
								rtb212=cneg(cdiv(rtb112,det));
								rtb221=cneg(cdiv(rtb121,det));
								rtb222=cdiv(rtb111,det);
								rtb311=cadd(cmul(rtb211,turf11[ijk1]),
									cmul(rtb212,turf21[ijk1]));
								rtb312=cadd(cmul(rtb211,turf12[ijk1]),
									cmul(rtb212,turf22[ijk1]));
								rtb321=cadd(cmul(rtb221,turf11[ijk1]),
									cmul(rtb222,turf21[ijk1]));
								rtb322=cadd(cmul(rtb221,turf12[ijk1]),	
									cmul(rtb222,turf22[ijk1]));
								rtb111=cadd(cmul(tdfr11[ijk1],rtb311),cadd(
									cmul(tdfr12[ijk1],rtb321),rurf11[ijk1]));
								rtb112=cadd(cmul(tdfr11[ijk1],rtb312),cadd(
									cmul(tdfr12[ijk1],rtb322),rurf12[ijk1]));
								rtb121=cadd(cmul(tdfr21[ijk1],rtb311),cadd(
									cmul(tdfr22[ijk1],rtb321),rurf21[ijk1]));
								rtb122=cadd(cmul(tdfr21[ijk1],rtb312),cadd(
									cmul(tdfr22[ijk1],rtb322),rurf22[ijk1]));
								det=csub(cmul(rtb111,rtb122),
									cmul(rtb112,rtb121));
								rdnr11[ip]=cdiv(rtb122,det);
								rdnr12[ip]=cneg(cdiv(rtb112,det));
								rdnr21[ip]=cneg(cdiv(rtb121,det));
								rdnr22[ip]=cdiv(rtb111,det);
							}
						} else if (acoustic[iz]==1) {
							for (ip=0; ip<block_size; ip++) {
								ijk1=ik1+ip;
								rtb211=cdiv(turf11[ijk1],csub(rd0n11[ip],
									rdfr11[ijk1]));
								rdnr11[ip]=cdiv(cmplx(1.0,0.0),cadd(cmul(tdfr11[ijk1],
									rtb211),rurf11[ijk1]));
								rdnr12[ip]=cmplx(0.0,0.0);
								rdnr21[ip]=cmplx(0.0,0.0);
								rdnr22[ip]=cmplx(0.0,0.0);
							}
						}
					} else {
						if (acoustic[iz]==0||acoustic[iz]==1||acoustic[iz]==2) {
							for (ip=0; ip<block_size; ip++) {
								rdnr11[ip]=cmplx(0.0,0.0);
								rdnr12[ip]=cmplx(0.0,0.0);
								rdnr21[ip]=cmplx(0.0,0.0);
								rdnr22[ip]=cmplx(0.0,0.0);
							}
						} else {
							for (ip=0; ip<block_size; ip++) {
								ijk1=ik1+ip;
								rdnr11[ip]=cmplx(0.0,0.0);
								rdnr12[ip]=cmplx(0.0,0.0);
								rdnr21[ip]=cmplx(0.0,0.0);
								rdnr22[ip]=cmplx(0.0,0.0);
								rtb111=cadd(cmul(rusr11[ip],rdfr11[ijk1]),
									cmul(rusr12[ip],rdfr21[ijk1]));
								rtb112=cadd(cmul(rusr11[ip],rdfr12[ijk1]),
									cmul(rusr12[ip],rdfr22[ijk1]));
								rtb121=cadd(cmul(rusr21[ip],rdfr11[ijk1]),
									cmul(rusr22[ip],rdfr21[ijk1]));
								rtb122=cadd(cmul(rusr21[ip],rdfr12[ijk1]),
									cmul(rusr22[ip],rdfr22[ijk1]));
								x=csub(cmplx(1.0,0.0),rtb111);
								y=csub(cmplx(1.0,0.0),rtb122);
								det=csub(cmul(x,y),cmul(rtb112,rtb121));
								rvrb111=cdiv(y,det);
								rvrb112=cdiv(rtb112,det);
								rvrb121=cdiv(rtb121,det);
								rvrb122=cdiv(x,det);
								rtb211=cadd(cmul(tdfr11[ijk1],rvrb111),
									cmul(rvrb121,tdfr12[ijk1]));
								rtb212=cadd(cmul(tdfr11[ijk1],rvrb112),
									cmul(rvrb122,tdfr12[ijk1]));
								rtb221=cadd(cmul(tdfr21[ijk1],rvrb111),
									cmul(rvrb121,tdfr22[ijk1]));
								rtb222=cadd(cmul(tdfr21[ijk1],rvrb112),
									cmul(rvrb122,tdfr22[ijk1]));
								tdfr11[ijk1]=cadd(cmul(rtb211,tdsr11[ip]),
									cmul(rtb212,tdsr21[ip]));
								tdfr12[ijk1]=cadd(cmul(rtb211,tdsr12[ip]),
									cmul(rtb212,tdsr22[ip]));
								tdfr21[ijk1]=cadd(cmul(rtb221,tdsr11[ip]),
									cmul(rtb222,tdsr21[ip]));
								tdfr22[ijk1]=cadd(cmul(rtb221,tdsr12[ip]),
									cmul(rtb222,tdsr22[ip]));
								rtb111=cadd(cmul(rtb211,rusr11[ip]),
									cmul(rtb212,rusr21[ip]));
								rtb112=cadd(cmul(rtb211,rusr12[ip]),
									cmul(rtb212,rusr22[ip]));
								rtb121=cadd(cmul(rtb221,rusr11[ip]),
									cmul(rtb222,rusr21[ip]));
								rtb122=cadd(cmul(rtb221,rusr12[ip]),
									cmul(rtb222,rusr22[ip]));
								rurf11[ijk1]=cadd(cmul(rtb111,turf11[ijk1]),
									cadd(cmul(rtb112,turf21[ijk1]),
									rurf11[ijk1]));
								rurf12[ijk1]=cadd(cmul(rtb111,turf12[ijk1]),
									cadd(cmul(rtb112,turf22[ijk1]),
									rurf12[ijk1]));
								rurf21[ijk1]=cadd(cmul(rtb121,turf11[ijk1]),
									cadd(cmul(rtb122,turf21[ijk1]),
									rurf21[ijk1]));
								rurf22[ijk1]=cadd(cmul(rtb121,turf12[ijk1]),
									cadd(cmul(rtb122,turf22[ijk1]),
									rurf22[ijk1]));
							}
						}
					}	
					if (flag[iz]==2) {
						flag[iz]=prv[iz];	
					}
					for (ip=0; ip<block_size; ip++) {
						ijk1=ik1+ip;
						ijk2=ik2+ip;
						rtb111=cadd(cmul(rurf11[ijk1],rdnr11[ip]),
							cmul(rurf12[ijk1],rdnr21[ip]));
						rtb112=cadd(cmul(rurf11[ijk1],rdnr12[ip]),
							cmul(rurf12[ijk1],rdnr22[ip]));
						rtb121=cadd(cmul(rurf21[ijk1],rdnr11[ip]),
							cmul(rurf22[ijk1],rdnr21[ip]));
						rtb122=cadd(cmul(rurf21[ijk1],rdnr12[ip]),
							cmul(rurf22[ijk1],rdnr22[ip]));
						x=csub(cmplx(1.0,0.0),rtb111);
						y=csub(cmplx(1.0,0.0),rtb122);
						det=csub(cmul(x,y),cmul(rtb112,rtb121));
						rvrb111=cdiv(y,det);
						rvrb112=cdiv(rtb112,det);
						rvrb121=cdiv(rtb121,det);
						rvrb122=cdiv(x,det);
						rtb111=cadd(cmul(rvrb111,tdfr11[ijk1]),cmul(rvrb112,
							tdfr21[ijk1]));
						rtb112=cadd(cmul(rvrb111,tdfr12[ijk1]),cmul(rvrb112,
							tdfr22[ijk1])); 
						rtb121=cadd(cmul(rvrb121,tdfr11[ijk1]),cmul(rvrb122,
							tdfr21[ijk1]));
						rtb122=cadd(cmul(rvrb121,tdfr12[ijk1]),cmul(rvrb122,
							tdfr22[ijk1]));
						rtb211=cadd(cmul(rdnr11[ip],rtb111),cmul(rdnr12[ip],
							rtb121));
						rtb212=cadd(cmul(rdnr11[ip],rtb112),cmul(rdnr12[ip],
							rtb122));
						rtb221=cadd(cmul(rdnr21[ip],rtb111),cmul(rdnr22[ip],
							rtb121));
						rtb222=cadd(cmul(rdnr21[ip],rtb112),cmul(rdnr22[ip],
							rtb122));
						rtb311=cadd(cmul(rusf11[ip],rd0n11[ip]),cmul(rusf12[ip],
							rd0n21[ip]));
						rtb312=cadd(cmul(rusf11[ip],rd0n12[ip]),cmul(rusf12[ip],
							rd0n22[ip]));
						rtb321=cadd(cmul(rusf21[ip],rd0n11[ip]),cmul(rusf22[ip],
							rd0n21[ip]));
						rtb322=cadd(cmul(rusf21[ip],rd0n12[ip]),cmul(rusf22[ip],
							rd0n22[ip]));
						x=csub(cmplx(1.0,0.0),rtb311);
						y=csub(cmplx(1.0,0.0),rtb322);
						det=csub(cmul(x,y),cmul(rtb312,rtb321));
						rvrb111=cdiv(y,det);
						rvrb112=cdiv(rtb312,det);
						rvrb121=cdiv(rtb321,det);
						rvrb122=cdiv(x,det);
						fact11=cadd(cmul(rusf11[ip],sigmau1[ip]),
							cadd(cmul(rusf12[ip],sigmau2[ip]),sigmad1[ip]));
						fact12=cadd(cmul(rusf21[ip],sigmau1[ip]),
							cadd(cmul(rusf22[ip],sigmau2[ip]),sigmad2[ip]));
						vdzs1=cadd(cmul(rvrb111,fact11),cmul(rvrb112,fact12));
						vdzs2=cadd(cmul(rvrb121,fact11),cmul(rvrb122,fact12));
						up[ip]=cmul(prs[iz],cadd(cmul(cadd(rtb111,rtb211),
							vdzs1),cmul(cadd(rtb112,rtb212),vdzs2)));
						rtb211=cadd(cmul(p[ip],rdnr11[ip]),cmul(gt[ijk2],
							rdnr21[ip]));
						rtb212=cadd(cmul(p[ip],rdnr12[ip]),cmul(gt[ijk2],
							rdnr22[ip]));
						rtb221=csub(cmul(rdnr21[ip],p[ip]),cmul(gl[ijk2],
							rdnr11[ip]));
						rtb222=csub(cmul(rdnr22[ip],p[ip]),cmul(gl[ijk2],
							rdnr12[ip]));
						rtb311=cadd(p[ip],rtb211);
						rtb312=csub(rtb212,gt[ijk2]);
						rtb321=cadd(gl[ijk2],rtb221);
						rtb322=cadd(p[ip],rtb222);
						rtb211=cadd(cmul(rtb311,rtb111),cmul(rtb312,rtb121));
						rtb212=cadd(cmul(rtb311,rtb112),cmul(rtb312,rtb122));
						rtb221=cadd(cmul(rtb321,rtb111),cmul(rtb322,rtb121));
						rtb222=cadd(cmul(rtb321,rtb112),cmul(rtb322,rtb122));
						ux[ip]=cadd(cmul(rtb211,vdzs1),cmul(rtb212,vdzs2));
						uz[ip]=cadd(cmul(rtb221,vdzs1),cmul(rtb222,vdzs2));
					}
				}

				/* loop over ranges (traces) */
				for (ix=0; ix<nx; ix++) {
					x1=bx+dx*ix;
					if (x1==0.0) { /* zero offset traces are a special case */
						for (ip=1; ip<block_size-1; ip++) {
							t1=cdiv(cmplx(1.0,0.0),cwp_csqrt(p[ip]));
							vos1[ip]=cmul(up[ip],t1);
							vos2[ip]=cmul(ux[ip],t1);
							vos3[ip]=cmul(uz[ip],t1);
						}
						vos1[0]=cmplx(0.0,0.0);
						vos1[1]=crmul(vos1[1],0.5);
						vos1[block_size-2]=crmul(vos1[block_size-2],.5);
					} else {
						temr=crmul(wpie,2*PI*x1);
						temr=cdiv(tem,cwp_csqrt(temr));
						sig=crmul(divfac,x1);
						sigh=cmul(sig,cdp);
						for (ip=0; ip<block_size; ip++) {
							texp[ip]=cwp_cexp(cmul(sig,p[ip]));
						}

						/******************************************************
						*		compute the slowness inytegral				*
						******************************************************/
						if (int_type==1) { 		/* use trapezoidal rule */
							temr=crmul(cmul(temr,cdp),0.5);
							for (ip=0; ip<block_size-1; ip++) {
								jj=ip+1;
								vos1[ip]=cmul(temr,cadd(cmul(up[ip],texp[ip]),
									cmul(up[jj],texp[jj])));
								vos2[ip]=cmul(temr,cadd(cmul(ux[ip],texp[ip]),
									cmul(ux[jj],texp[jj])));
								vos3[ip]=cmul(temr,cadd(cmul(uz[ip],texp[ip]),
									cmul(uz[jj],texp[jj])));
							}
						} else if (int_type==2) { /* use first order Filon */
							for (ip=0; ip<block_size-1; ip++) {
								jj=ip+1;
								t1=csub(texp[jj],texp[ip]);
								func1=csub(cmul(up[jj],texp[jj]),
									cmul(up[ip],texp[ip]));
								func2=cmul(csub(up[jj],up[ip]),t1);
								func3=csub(cmul(ux[jj],texp[jj]),
									cmul(ux[ip],texp[ip]));
								func4=cmul(csub(ux[jj],ux[ip]),t1);
								func5=csub(cmul(uz[jj],texp[jj]),
									cmul(uz[ip],texp[ip]));
								func6=cmul(csub(uz[jj],uz[ip]),t1);
								vos1[ip]=cmul(cdiv(cdp,sigh),
									cmul(csub(func1,cdiv(func2,sigh)),temr));
								vos2[ip]=cmul(cdiv(cdp,sigh),
									cmul(csub(func3,cdiv(func4,sigh)),temr));
								vos3[ip]=cmul(cdiv(cdp,sigh),
									cmul(csub(func5,cdiv(func6,sigh)),temr));
							}
						}
					}
				
					/**********************************************************
					* 				update output arrays			
					**********************************************************/
					for (ip=0; ip<block_size-1; ip++) {
						response1[iw][ix][iz]=cadd(response1[iw][ix][iz],
							vos1[ip]);
						response2[iw][ix][iz]=cadd(response2[iw][ix][iz],
							vos2[ip]);
						response3[iw][ix][iz]=cadd(response3[iw][ix][iz],
							vos3[ip]);
					}
				}
			}

			/******************************************************************
			* 						update loop variables					*
			*****************************************************************/
			nblock--;
			if (nblock != 0) {

				bp=p[block_size-1].r;
				left++;
				if (left > block_size) {
					left -=block_size;
					nblock++;
				}

			} else {
				if (int_type==1) bp=bp1;
				else if (int_type==2) bp=0.0;
				break;		/* last block, exit while loop */
			}
		}
	}

	/* if requested, output some important processing information */
	if (verbose==1||verbose==3) {
   		fprintf(stderr,"\nA total of %d frequencies, from %g to %g (Hz)\n"
			"and a total of %d ray parameters, from %g to %g (s/km) were "
			"processed\n",nw,wbeg,wfin,np,bp,fp);
	} 
	if (verbose==2||verbose==3) {
   		fprintf(outfp,"\nA total of %d frequencies, from %g to %g (Hz)\n"
			"and a total of %d ray parameters, from %g to %g (s/km) were "
			"processed\n",nw,wbeg,wfin,np,bp,fp);
	} 

	/* free allocated space */
	free1complex(ux);free1complex(uz);free1complex(up);
	free1complex(rd0n11);free1complex(rd0n12);free1complex(rd0n21);
	free1complex(rd0n22);free1complex(td0n11);free1complex(td0n12);
	free1complex(td0n22);free1complex(tun011);free1complex(tun012);
	free1complex(tun021);free1complex(tun022);free1complex(run011);
	free1complex(run012);free1complex(run021);free1complex(run022);
	free1complex(tusf11);free1complex(tusf12);free1complex(tusf21);
	free1complex(tusf22);free1complex(rdfs11);free1complex(rdfs12);
	free1complex(rdfs21);free1complex(rdfs22);free1complex(tdfs11);
	free1complex(tdfs12);free1complex(tdfs21);free1complex(tdfs22);
	free1complex(rurf11);free1complex(rurf12);free1complex(rurf21);
	free1complex(rurf22);free1complex(turf11);free1complex(turf12);
	free1complex(turf21);free1complex(turf22);free1complex(rdfr11);
	free1complex(rdfr12);free1complex(rdfr21);free1complex(rdfr22);
	free1complex(tdfr11);free1complex(tdfr12);free1complex(tdfr21);
	free1complex(tdfr22);free1complex(rnr11);free1complex(rnr12);
	free1complex(rnr21);free1complex(rnr22);free1complex(rdsr11);
	free1complex(rdsr12);free1complex(rdsr21);free1complex(rdsr22);
/*	free1complex(r11);free1complex(r12);free1complex(r21);free1complex(r22);
*/
	free1complex(vos1);free1complex(vos2);free1complex(vos3);

	/* free more working space */ 
	free1complex(p);
	free1complex(pp);
	free1complex(pwin);
	free1complex(gl);
	free1complex(gt);
	free1complex(gam);
	free1complex(alpha);
	free1complex(betha);
	free1complex(sigmau1);
	free1complex(sigmau2);
	free1complex(sigmad1);
	free1complex(sigmad2);
}
