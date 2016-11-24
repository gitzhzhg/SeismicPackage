/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "par.h"
#include "Reflect/reflpsvsh.h"

#define BLK 100
#define IMAX 300
#define SZERO 0.000000000001

/*********************** self documentation **********************/
/******************************************************************************
SH_REFLECTIVITIES -	Subroutines to do matrix propagation as the hard core of 
						a reflectivity modeling code for SH wavefields

sh_reflectivities		applies matrix propagation to compute the SH 
						reflectivity response of a horizontally stratified 
						earth model

*******************************************************************************
Function Prototypes:
void sh_reflectivities (int int_type, int verbose, int wtype, int nw,
	int nlayers, int nx, int layern, int nor, int np, float bp1, float m1,
	float m2, float m3, float h1, float h2, int lsource, float bx, float dx,
	float xmax, float decay, float fref, float wrefp, float wrefs, float tsec,
	float p2w, float fs, float epsp, float epss, float sigp, float sigs,
	float pw1, float pw2, float pw3, float pw4, int *flag, int *lobs,
	float *rho, float *t, float *cl, float *ct, float *ql, float *qt,
	complex ***response1, FILE *outfp);
*******************************************************************************
sh_reflectivities
Input:
nw  			number of frequencies
nlayers 		number of reflecting layers
nx  			number of ranges
nor 			number of receivers
lsource 		layer on top of which the source is located
bx  			beginning range
fx  			final range
dx  			range increment
flag			array[nor] of flags for source type
rho 			array[nlayers] of densities
t   			array[nlayers] of layer thicknesses

Output
response1   	array[nw][nor][nx] of updated tangential field response
*******************************************************************************
Credits:



******************************************************************************/
/**************** end self doc ********************************/



/******************************************************************************

		Subroutine to compute the SH propagation matrices for a single
							frequency step

******************************************************************************/
void sh_reflectivities (int int_type, int verbose, int wtype, int nw,
	int nlayers, int nx, int layern, int nor, int np, float bp1, float m1,
	float m2, float m3, float h1, float h2, int lsource, float bx, float dx,
	float xmax, float decay, float fref, float wrefp, float wrefs, float tsec,
	float p2w, float fs, float epsp, float epss, float sigp, float sigs,
	float pw1, float pw2, float pw3, float pw4, int *flag, int *lobs,
	float *rho, float *t, float *cl, float *ct, float *ql, float *qt,
	complex ***response1, FILE *outfp)
/******************************************************************************
Input:
nw  			number of frequencies
nlayers 		number of reflecting layers
nx  			number of ranges
nor 			number of receivers
lsource 		layer on top of which the source is located
bx  			beginning range
fx  			final range
dx  			range increment
flag			array[nor] of flags for source type
rho 			array[nlayers] of densities
t   			array[nlayers] of layer thicknesses

Output
response1   	array[nw][nor][nx] of updated tangencialfield response
******************************************************************************/
{
	int iw,iz,ip,ix;		/* loop counters */
	int ijk1=0,ik1,ik2,il,jl;	/* auxiliary indices */
	int ijk=0,ijk2,jj=0;		/* more auxiliary indices */
	int lrec;			
	float bp=0.0;		/* beginning ray parameter */
	float fp;		/* final ray parameter */
	float dp;		/* ray parameter increment */
	int nblock;		/* number of blocks to process */
	int block_size;		/* size of processing blocks */
	int left;		/* remainder */
	int *prv,*ibl;		/* scratch arrays */
	float x1;
	float w;
	complex wpie,cdp;	/* complex frecuency and ray parameter inc */
		
	/* complex auxiliary variables */
	complex x,y,t1,divfac;
	complex det,psq,gl1,gt1,gl2,gt2,gam1,gam2,at1,at2,al1,al2;
	complex tem,temr,sig,sigh,rho1,rho2;
	complex rvrb111,rvrb112,rvrb121,rvrb122,rvrb211;
	complex ewigh11,ewigh22,wrn011,ewrd11;
	complex ewd0211,ewd0212,ewtu211;
	complex rtb111,rtb112,rtb121,rtb122,rtb211;
	complex rtb212,rtb221,rtb222,rtb311,rtb312,rtb321,rtb322;
	complex func1,func2;
	complex tun0p11,tun0p21,rd0np11,rd0np12,rd0np21;
	complex rd0np22,td0np11,td0np12;
	complex rd11,td11,tu11,ru11;
	complex fact11,fact12,vuzs1,vuzs2,vdzs1,vdzs2;

	/* complex scratch arrays */
	complex *rd0n11,*rd0n12,*rd0n21,*rd0n22,*td0n11,*td0n12;
	complex *td0n21,*td0n22,*tun011,*tun012,*tun021,*tun022;
	complex *run011,*run012,*run021,*run022;
	complex *rusf21,*rusf22,*tusf11,*tusf12,*tusf21,*tusf22;
	complex *rdfs11,*rdfs12,*rdfs21,*rdfs22,*rurf11,*rurf12;
	complex *rurf21,*rurf22,*turf11,*turf12,*turf21,*turf22;
	complex *rdfr11,*rdfr12,*rdfr21,*rdfr22,*tdfr11,*tdfr12;
	complex *tdfr21,*tdfr22,*r11,*r12,*r21,*r22;
	complex *texp,*vos1,*vos2,*vos3,*up;
	complex *sigmau1,*sigmau2,*sigmad1,*sigmad2,*alpha,*betha;
	complex *al=NULL,*at=NULL,*gl,*gt,*gam,*p,*pp,*prs=NULL,*pwin;
	
	/* complex pointers */
	complex *rusf11,*rusf12;
	complex *tusr11,*tusr12,*tusr21,*tusr22;
	complex *rusr11,*rusr12,*rusr21,*rusr22;
	complex *tdsr11,*tdsr12,*tdsr21,*tdsr22;
	complex *rdnr11,*rdnr12,*rdnr21,*rdnr22;

	/* allocate working space */

	/* allocate working space */
	up=alloc1complex(BLK);
	rd0n11=alloc1complex(BLK);rd0n12=alloc1complex(BLK);
	rd0n21=alloc1complex(BLK);rd0n22=alloc1complex(BLK);
	td0n11=alloc1complex(BLK);td0n12=alloc1complex(BLK);
	td0n21=alloc1complex(BLK);td0n22=alloc1complex(BLK);
	tun011=alloc1complex(BLK);tun012=alloc1complex(BLK);
	tun021=alloc1complex(BLK);tun022=alloc1complex(BLK);
	run011=alloc1complex(BLK);run012=alloc1complex(BLK);
	run021=alloc1complex(BLK);run022=alloc1complex(BLK);
	tusf11=alloc1complex(BLK);tusf12=alloc1complex(BLK);
	tusf21=alloc1complex(BLK);tusf22=alloc1complex(BLK);
	rdfs11=alloc1complex(BLK);rdfs12=alloc1complex(BLK);
	rdfs21=alloc1complex(BLK);rdfs22=alloc1complex(BLK);
	rurf11=alloc1complex(IMAX);rurf12=alloc1complex(IMAX);
	rurf21=alloc1complex(IMAX);rurf22=alloc1complex(IMAX);
	turf11=alloc1complex(IMAX);turf12=alloc1complex(IMAX);
	turf21=alloc1complex(IMAX);turf22=alloc1complex(IMAX);
	rdfr11=alloc1complex(IMAX);rdfr12=alloc1complex(IMAX);
	rdfr21=alloc1complex(IMAX);rdfr22=alloc1complex(IMAX);
	tdfr11=alloc1complex(IMAX);tdfr12=alloc1complex(IMAX);
	tdfr21=alloc1complex(IMAX);tdfr22=alloc1complex(IMAX);
	rusf21=alloc1complex(BLK);rusf22=alloc1complex(BLK);
	vos1=alloc1complex(BLK);vos2=alloc1complex(BLK);vos3=alloc1complex(BLK);
	r11=alloc1complex(BLK);r12=alloc1complex(BLK);
	r21=alloc1complex(BLK);r22=alloc1complex(BLK);
	texp=alloc1complex(BLK);

	/* allocate other working space */
	prv=alloc1int(nor);
	ibl=alloc1int(nor);
	/* ibl=alloc1int(BLK); */
    p=alloc1complex(BLK);
    pp=alloc1complex(BLK);
    pwin=alloc1complex(BLK);
    /* gl=alloc1complex(BLK); */
    gl=alloc1complex(BLK*nlayers);
    gt=alloc1complex(BLK*nlayers);
    gam=alloc1complex(BLK*nlayers);
    alpha=alloc1complex(BLK);
    betha=alloc1complex(BLK);
    sigmau1=alloc1complex(BLK);
    sigmau2=alloc1complex(BLK);
    sigmad1=alloc1complex(BLK);
    sigmad2=alloc1complex(BLK);
	at=alloc1complex(nlayers);
	al=alloc1complex(nlayers);
	/* prs=ealloc1complex(nlayers); */
	prs=alloc1complex(nor);

	/* initialize pointers */
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
	rdnr11=r11;
	rdnr12=r12;
	rdnr21=r21;
	rdnr22=r22;

	/* assign initial value to working variables */
	tem=cdiv(cmplx(1.0,0.0),cwp_cexp(crmul(cmplx(0.0,1.0),PI/4)));
	ewigh11=cmplx(0.0,0.0);
	ewigh22=cmplx(0.0,0.0);

	/* initialize complex output arrays */
    for (iw=0; iw<nw; iw++)
	for (ix=0; ix<nx; ix++)
	    for (iz=0; iz<nor; iz++) {
		response1[iw][ix][iz]=cmplx(0.0,0.0);
	    }


	/**************************************************************************
	*					Main loop over frequencies			    *	
	**************************************************************************/
	for (iw=0; iw<nw; iw++) {
		w=(iw+1)*PI*2/tsec;		/* w in radians/sec */
		wpie=cmplx(w,decay);		/* complex w */

		/* compute frequency-dependent auxiliary variables for reflectivity */
		compute_w_aux_arrays (wtype, layern, nlayers, nor, lsource,
			&np, &block_size, &nblock, &left, fref, wrefp, wrefs, 
			lobs, tsec, p2w, fs, xmax, w, decay, epsp, epss, sigp, 
			sigs, &pw1, &pw2, &pw3, &pw4, &dp, &fp, wpie, &cdp, cl,
			ct, ql, qt, rho, t, al, at, prs);

		/**********************************************************************
		* 	loop over slopes to compute the reflectivities (by blocks)		  *
		**********************************************************************/
		while (1) {

			if ((nblock==1) && (left !=0)) block_size=left;

			/* compute p_aux_arrays */
			compute_p_aux_arrays (wtype, nlayers, lsource, block_size, bp, dp,
		w, decay, pw1, pw2, pw3, pw4, pwin, m1, m2, m3, h1, h2,
		wpie, &divfac, p, pp, al, at, rho, gl, gt, gam, alpha, betha,
				sigmad1, sigmad2, sigmau1, sigmau2);

			/* if requested, output processing information */
			if (verbose==1||verbose==3) {
				fprintf(stderr,"%3d%6.1f%7.3f%7.3f%6d%7.3f%7.3f%7.3f"
				"%7.3f%7.3f\n",iw,w,bp,fp,np,dp,pw1,pw2,pw3,pw4);
			} if (verbose==2||verbose==3) {
				fprintf(outfp,"%3d%6.1f%7.3f%7.3f%6d%7.3f%7.3f%7.3f"
				"%7.3f%7.3f\n",iw,w,bp,fp,np,dp,pw1,pw2,pw3,pw4);
			}



			/******************************************************************
			* 			start the computation of the reflectivities 		*
			******************************************************************/
			/* loop over reflecting layers */
			/* for (il=0; il<nlayers; il++) { */
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
				if (il==0) {
					for (ip=0; ip<block_size; ip++) {
						ijk1=ik1+ip;
						gt1=gt[ijk1];
						ewigh11=cwp_cexp(cmul(cmplx(0.0,1.0),cmul(wpie,cmul(gt1,t1))));
						rd0n11[ip]=cmplx(0.0,0.0);
						rd0n12[ip]=cmplx(0.0,0.0);
						rd0n21[ip]=cmplx(0.0,0.0);
						rd0n22[ip]=cmplx(0.0,0.0);

						/* free surface */
						td0n11[ip]=ewigh11;
						td0n12[ip]=cmplx(0.0,0.0);
						td0n21[ip]=cmplx(0.0,0.0);
						td0n22[ip]=cmplx(0.0,0.0);
						tun011[ip]=ewigh11;
						tun012[ip]=cmplx(0.0,0.0);
						tun021[ip]=cmplx(0.0,0.0);
						tun022[ip]=cmplx(0.0,0.0);
						run011[ip]=cmul(ewigh11,ewigh11);
						run012[ip]=cmplx(0.0,0.0);
						run021[ip]=cmplx(0.0,0.0);
						run022[ip]=cmplx(0.0,0.0);
					}	
				} else {
					if ((at1.r==at2.r)&&(at1.i==at2.i)) {
						for (ip=0; ip<block_size; ip++) {
							ijk1=ik1+ip;
							gt1=gt[ijk1];
							ewigh11=cwp_cexp(cmul(cmplx(0.0,1.0),cmul(wpie,cmul(gt1,t1))));
							tun0p11=cmul(tun011[ip],ewigh11);
							tun0p21=cmul(tun021[ip],ewigh11);
						
							/* -R/T coefficients betha1=betha2 */
							rtb111=cmul(ewigh11,run011[ip]);
							run011[ip]=cmul(rtb111,ewigh11);
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
							gl2=gl[ijk2];
							gt2=gt[ijk2];
							gam2=gam[ijk2];
							ewigh11=cwp_cexp(cmul(wpie,cmul(cmplx(0.0,1.0),cmul(t1,gt1))));
							y=cadd(cmul(rho1,cdiv(gt1,cmul(at1,at1))),
								cmul(rho2,cdiv(gt2,cmul(at2,at2))));
							td11=crmul(cmul(rho1,cdiv(gt1,cmul(y,
								cmul(at1,at1)))),2.);
							rd11=cdiv(csub(cmul(rho1,cdiv(at1,at1)),
								cmul(rho1,cdiv(gt2,cmul(at2,at2)))),y);
							tu11=crmul(cmul(rho2,cdiv(gt2,cmul(y,
								cmul(at2,at2)))),2.);
							ru11=cneg(rd11);
							ewrd11=cmul(ewigh11,rd11);
							wrn011=cmul(ewigh11,run011[ip]);
							rvrb111=cdiv(cmplx(1.0,0.0),csub(cmplx(1.0,0.0),cmul(wrn011,ewrd11)));
							rvrb211=cadd(cmplx(1.0,0.0),cmul(rvrb111,cmul(ewrd11,wrn011)));
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
							rd0np11=cadd(rd0n11[ip],cmul(tun011[ip],rtb111));
							rd0np12=cadd(rd0n12[ip],cmul(tun011[ip],rtb112));
							rd0np21=cadd(rd0n21[ip],cmul(tun021[ip],rtb111));
							rd0np22=cadd(rd0n22[ip],cmul(tun022[ip],rtb112));
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
				} 
				/* if (il==lobs[ijk]-1) { */
				if (il==lobs[ijk]-2) {
					ik1=ijk*block_size;		
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
					}
					ijk++;
				} 

				/* if (il==lsource-1) { */
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

			/* loop over receiver depths */
		    for (iz=0; iz<nor; iz++) {
			ik1=iz*block_size;
			for (ip=0; ip<block_size; ip++) {
			    ijk1=ik1+ip;    
			    det=csub(rd0n11[ip],rdfr11[ijk1]);
			    if (rcabs(det)<=SZERO) {
				prv[iz]=flag[iz];
				flag[iz]=2;
				ibl[iz]=ip;
				break;
			    }
			}
    		}


 		   for (iz=0; iz<nor; iz++) {
		ik1=iz*block_size;
			lrec=lobs[iz];
			ik2=(lrec-1)*block_size;

			/* receiver above the source */
			if (lsource>lrec) {
			    for (ip=0; ip<block_size; ip++) {
				ijk1=ik1+ip;
						ijk2=ik2+ip;
						rtb111=cdiv(cmplx(1.0,0.0),turf11[ijk1]);
						rdnr11[ip]=cmul(rtb111,tusf11[ip]);
						rdnr12[ip]=cmul(rtb111,tusf12[ip]);
						rdnr21[ip]=cmplx(0.0,0.0);
						rdnr22[ip]=cmplx(0.0,0.0);
						rtb111=rdnr11[ip];
						rtb112=rdnr12[ip];
						rtb121=rdnr21[ip];
						rtb122=rdnr22[ip];
						rtb211=cadd(cmul(rurf11[ijk1],rtb111),cmul(rurf12[ijk1],
							rtb121));
						rtb212=cadd(cmul(rurf11[ijk1],rtb112),cmul(rurf12[ijk1],
							rtb122));
						rtb221=cadd(cmul(rurf21[ijk1],rtb111),cmul(rurf22[ijk1],
							rtb121));
						rtb222=cadd(cmul(rurf21[ijk1],rtb112),cmul(rurf22[ijk1],
							rtb122));
						rtb311=cadd(cmul(rd0n11[ip],rusf11[ip]),cmul(rd0n12[ip],
							rusf21[ip]));
						rtb312=cadd(cmul(rd0n11[ip],rusf12[ip]),cmul(rd0n12[ip],
							rusf22[ip]));
						rtb321=cadd(cmul(rd0n21[ip],rusf11[ip]),cmul(rd0n22[ip],
							rusf21[ip]));
						rtb322=cadd(cmul(rd0n21[ip],rusf12[ip]),cmul(rd0n22[ip],
							rusf22[ip]));
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
						vuzs1=cadd(cmul(rvrb111,fact11),cmul(rvrb112,fact12));
						vuzs2=cadd(cmul(rvrb121,fact11),cmul(rvrb122,fact12));
						up[ip]=cmul(p[ip],cadd(cmul(cadd(rtb111,rtb211),vuzs1),
							cmul(cadd(rtb112,rtb212),vuzs2)));
					}
				} else {
				
					/* receiver below the source */
					if (flag[iz]==2) {
						for (ip=0; ip<ibl[iz]-1; ip++) {
							ijk1=ik1+ip;
							rtb211=cdiv(turf11[ijk1],csub(rd0n11[ip],
							rdfr11[ijk1]));
							rdnr11[ip]=cdiv(cmplx(1.0,0.0),cadd(cmul(tdfr11[ijk1],rtb211),
								rurf11[ijk1]));
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
					} else if (flag[iz]==0) {
			
						for (ip=0; ip<block_size; ip++) {
							ijk1=ik1+ip;
							rtb211=cdiv(turf11[ijk1],csub(rd0n11[ip],
								rdfr11[ijk1]));
							rdnr11[ip]=cdiv(cmplx(1.0,0.0),cadd(cmul(tdfr11[ijk1],rtb211),
								rurf11[ijk1]));
							rdnr12[ip]=cmplx(0.0,0.0);
							rdnr21[ip]=cmplx(0.0,0.0);
							rdnr22[ip]=cmplx(0.0,0.0);
						}
					} else {
						for (ip=0; ip<block_size; ip++) {
							rdnr11[ip]=cmplx(0.0,0.0);
							rdnr12[ip]=cmplx(0.0,0.0);
							rdnr21[ip]=cmplx(0.0,0.0);
							rdnr22[ip]=cmplx(0.0,0.0);
						}
					}	
					if (flag[iz]==2) flag[iz]=prv[iz];	
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
						fact11=cadd(cmul(rusf11[ip],sigmau1[ip]),cadd(
							cmul(rusf12[ip],sigmau2[ip]),sigmad1[ip]));
						fact12=cadd(cmul(rusf21[ip],sigmau1[ip]),cadd(
							cmul(rusf22[ip],sigmau2[ip]),sigmad2[ip]));
						vdzs1=cadd(cmul(rvrb111,fact11),cmul(rvrb112,fact12));
						vdzs2=cadd(cmul(rvrb121,fact11),cmul(rvrb122,fact12));
						up[ip]=cmul(p[ip],cadd(cmul(cadd(rtb111,rtb211),vdzs1),
							cmul(cadd(rtb112,rtb212),vdzs2)));
					}
				}
				for (ix=0; ix<nx; ix++) {
					x1=bx+dx*ix;
					if (x1==0.0) {
						for (ip=1; ip<block_size-1; ip++) {
							t1=cdiv(cmplx(1.0,0.0),cwp_csqrt(p[ip]));
							vos1[ip]=cmul(up[ip],t1);
						}
						vos1[0]=cmplx(0.0,0.0);
						vos1[1]=crmul(vos1[1],0.5);
						vos1[block_size-2]=crmul(vos1[block_size-2],.5);
					} else {
						temr=crmul(wpie,2*PI*x1);
						temr=cdiv(tem,cwp_csqrt(temr));
						sig=crmul(divfac,x1);
						sigh=cmul(sig,cdp);
						for (ip=0; ip<block_size-1; ip++) {
							texp[ip]=cwp_cexp(cmul(sig,p[ip]));
						}

						/******************************************************
						*			Compute the slowness integral			  *
						******************************************************/
						if (int_type==1) {		/* use trapezoidal rule */
							temr=crmul(cmul(temr,cdp),0.5);
							for (ip=0; ip<block_size-1; ip++) {
								jj=ip+1;
								vos1[ip]=cmul(temr,cadd(cmul(up[ip],texp[ip]),
									cmul(up[jj],texp[jj])));
							}
						} else if (int_type==2) {
							for (ip=0; ip<block_size-1; ip++) {
								jj=ip+1;
								t1=csub(texp[jj],texp[ip]);
								func1=csub(cmul(up[jj],texp[jj]),
									cmul(up[ip],texp[ip]));
								func2=cmul(csub(up[jj],up[ip]),t1);
								vos1[ip]=cmul(cdiv(cdp,sigh),cmul(csub(func1,
									cdiv(func2,sigh)),temr));
							}
						}
					}
		/**********************************************************
	    *		update output array			*
	    **********************************************************/
				for (ip=0; ip<block_size-1; ip++) {
						response1[iw][ix][iz]=cadd(response1[iw][ix][iz],
							vos1[ip]);
					}
				}
			}

	/* update loop variables */
            nblock--;
	    if (nblock != 0) {

	/*	bp=p[block_size].r; */
		bp=p[block_size-1].r;
		left++;
		if (left > block_size) {
		    left -=block_size;
		    nblock++;
		}

	    } else {
				if (int_type==1) bp=bp1;
				else if (int_type==2) bp=0.0;
				break;	/* last block, exit while loop */
			}
		}
	}
	
	/* free allocated space */
	free1complex(up);
	free1complex(rd0n11);free1complex(rd0n12);free1complex(rd0n21);
	free1complex(rd0n22);free1complex(td0n11);free1complex(td0n12);
	free1complex(td0n22);free1complex(tun011);free1complex(tun012);
	free1complex(tun021);free1complex(tun022);free1complex(run011);
	free1complex(run012);free1complex(run021);free1complex(run022);
	free1complex(tusf11);free1complex(tusf12);free1complex(tusf21);
	free1complex(tusf22);free1complex(rdfs11);free1complex(rdfs12);
	free1complex(rdfs21);free1complex(rdfs22);free1complex(rurf11);
	free1complex(rurf12);free1complex(rurf21);free1complex(rurf22);
	free1complex(turf11);free1complex(turf12);free1complex(turf21);
	free1complex(turf22);free1complex(rdfr11);free1complex(rdfr12);
	free1complex(rdfr21);free1complex(rdfr22);free1complex(tdfr11);
	free1complex(tdfr12);free1complex(tdfr21);free1complex(tdfr22);
	free1complex(r11);free1complex(r12);free1complex(r21);free1complex(r22);
	free1complex(vos1);free1complex(vos2);free1complex(vos3);

	/* free working space */
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

