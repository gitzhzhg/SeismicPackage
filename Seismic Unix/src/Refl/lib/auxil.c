/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


#include "par.h"
#include "Reflect/reflpsvsh.h"
#define BLK 100

/*********************** self documentation **********************/
/******************************************************************************
AUX_REFLEXTIVITY - Set of subroutines to compute auxiliary stuff to be
	used by the reflectivity modeling code in producing synthetic seismograms
*******************************************************************************
Function prototypes:
void compute_w_aux_arrays (int wtype, int layern, int nlayers, int nor, 
	int lsource, int *np, int *block_size, int *nblock, int *left, float fw,
	float wrefp, float wrefs, int *lobs, float tsec, float p2w, float fs, 
	float xmax, float w, float decay, float epsp, float epss, float sigp, 
	float sigs, float *pw1, float *pw2, float *pw3, float *pw4, float *dp, 
	float *fp, complex wpie, complex *cdp, float *cl, float *ct, float*ql, 
	float *qt, float *rho, float *t, complex *al, complex *at, complex *prs);

void compute_p_aux_arrays (int wtype, int nlayers, int lsource, int block_size,
	float bp, float dp, float w, float decay, float pw1, float pw2,
	float pw3, float pw4, complex *pwin, float m1, float m2, float m3, float h1,
	float h2, complex wpie, complex *divfac, complex *p, complex *pp,
	complex *al, complex *at, float *rho, complex *gl, complex *gt,
	complex *gam, complex *alpha, complex *betha, complex *sigmad1,
	complex *sigmad2, complex *sigmau1, complex *sigmau2) ;

void source_receiver_type (int nor, int nlayers, int lsource, int  *lobs, 
	float *cl, float *ct, int *acoust, int *flag);

void compute_slowness (int wtype, int lsource, float w, float p2w, int *np, 
	float *fp, float fs, float e4, float dk, float decay, float xmax, float *dp,
	float *pw1, float *pw2, float *pw3, float *pw4, complex *cdp, float *cl, 
	float *ct, float *t);
		
void compute_al_at (int wtype, int nlayers, int layern, float eps, float epsp, 
	float epss, float sigma, float sigp, float sigs, float *wrefp, float *wrefs,
	float fw, float *cl, float *ct, float *ql, float *qt, float *rho, 
	complex wpie, complex *al, complex *at);

void compute_prs (int nor, int *lobs, complex *al, complex *at, float *rho,
	complex *prs);

void compute_block_size (int np, int *block_size, int *nblock, int *left);

void compute_pwin (int wtype, int block_size, float bp, float dp, float w, 
	float decay, float pw1, float pw2, float pw3, float pw4, complex *pwin, 
	complex *p, complex *pp);

void compute_gl_gt_gam (int wtype, int nlayers, int block_size, complex *al, 
	complex *at, float *rho, complex *pp, complex *gl, complex *gt, 
	complex *gam);

void compute_alpha_betha (int lsource, int block_size, complex ats, float rhos, 
	complex *gl, complex *gt, complex *alpha, complex *betha);

void compute_sigmas (int wtype, int block_size, int lsource, float m1, 
	float m2, complex meu, complex *alpha, complex *betha, complex *p, 
	complex *gl, complex *gt, complex *gam, complex a2, complex a3, 
	complex *pwin, complex s1, complex s2, complex s4, complex *sigmau1, 
	complex *sigmau2, complex *sigmad1, complex *sigmad2);

void compute_moment_tensor (int wtype, float phi, float lambda, float delta, 
	float phis, float m0, float *m1, float *m2, float *m3);

void parameter_interpolation (int nlayers, int *intlayers, int *nintlayers,
	float *intlayth, float *cl, float *ql, float *ct, float *qt, float *rho, 
	float *t) ;

void random_velocity_layers (int *nlayers, int *lsource, int nrand_layers,
	float sdcl, float sdct, float layer, float zlayer, float *cl, float *ql, 
	float *ct, float *qt, float *rho, float *t);

void apply_earth_flattening (int nlayers, float z0, float *cl, float *ct, 
	float *rho, float *t);
*******************************************************************************
Notes:

******************************************************************************/
/********************************End Self-Documentation***********************/


/******************************************************************************

				Subroutine to compute auxiliary arrays

******************************************************************************/
void compute_w_aux_arrays (int wtype, int layern, int nlayers, int nor, 
	int lsource, int *np, int *block_size, int *nblock, int *left, float fref,
	float wrefp, float wrefs, int *lobs, float tsec, float p2w, float fs, 
	float xmax, float w, float decay, float epsp, float epss, float sigp, 
	float sigs, float *pw1, float *pw2, float *pw3, float *pw4, float *dp, 
	float *fp, complex wpie, complex *cdp, float *cl, float *ct, float*ql, 
	float *qt, float *rho, float *t, complex *al, complex *at, complex *prs)
/******************************************************************************
Input:
layern			
nlayers		number of reflecting layers
tsec		time series length in seconds (the maximum frequency
			of the seismogram will be (nw/tsec) Hz
w			frequency in rad/sec
decay		decay factor, used to avoid time series wraparound
epsp
epss
sigp
sigs 
pw1			left lower side window ray paramter for tapering
pw2			right lower side window ray paramter for tapering
pw3			left upper side window ray paramter for tapering
pw4			right upper side window ray paramter for tapering
cl			array[nlayers] of compressional wave velocities km/s
ct			array[nlayers] of shear wave velocities km/s
ql			array[nlayers] of compressional Q-values
qt			array[nlayers] of shear Q-values
rho			array[nlayers] of densities g/cc
t			array[nlayers] of layer thicknesses km

Output:
******************************************************************************/
{
	float wref;			/* reference frequency in rads/sec */
	float eps,sigma,e4,dk;		/* auxiliary variables */

	/* initialize varaibles */ 
	eps=0.001;
	sigma=0.1;
	dk=0.8;
	e4=0.8;
	wref=2*PI*fref;

	/* compute al and at */
	compute_al_at (wtype, nlayers, layern, eps, epsp, epss, sigma, sigp, 
		sigs, &wrefp, &wrefs, fref, cl, ct, ql, qt, rho, wpie, al, at);

	/* compute prs for every receiver */
	compute_prs (nor, lobs, al, at, rho, prs);

	/* compute number of slowness, slowness increment and maximum slowness */
	compute_slowness (wtype, lsource, w, p2w, np, fp, fs, e4, dk, decay, 
		xmax, dp, pw1, pw2, pw3, pw4, cdp, cl, ct, t);

	/* compute number of blocks and block size for matrix computations */
	compute_block_size (*np, block_size, nblock, left);
}

/******************************************************************************

		Subroutine to compute auxiliary arrays

******************************************************************************/
void compute_p_aux_arrays (int wtype, int nlayers, int lsource, int block_size,
	float bp, float dp, float w, float decay, float pw1, float pw2,
	float pw3, float pw4, complex *pwin, float m1, float m2, float m3, float h1,
	float h2, complex wpie, complex *divfac, complex *p, complex *pp,
	complex *al, complex *at, float *rho, complex *gl, complex *gt,
	complex *gam, complex *alpha, complex *betha, complex *sigmad1,
	complex *sigmad2, complex *sigmau1, complex *sigmau2) 
/******************************************************************************
Input:







Output:


******************************************************************************/
{
	float rhos;
	complex als,ats;
	complex alphasq,bethasq;
	complex meu,lambda;
	complex s1,s2,s4,a1,a2,a3;
	
	/* save al, at and rho values for the source layer */
	als=al[lsource-1];
	ats=at[lsource-1];
	rhos=rho[lsource-1];

	/* compute complex auxiliary variables */
	alphasq=cdiv(cmplx(1.0,0.0),cmul(als,als));
	if ((ats.r==0.)&&(ats.i==0.)) bethasq=cmplx(0.0,0.0);
	else bethasq=cdiv(cmplx(1.0,0.0),cmul(ats,ats));
	meu=crmul(bethasq,rhos);
	if ((meu.r != 0.)||(meu.i !=0.)) a1=cdiv(cmplx(1.0,0.0),meu);
	else a1=cmplx(0.0,0.0);
	lambda=crmul(cmul(csub(alphasq,cmplx(2,0)),bethasq),rhos);
	*divfac=cmul(cmplx(0.0,1.0),wpie);
	s1=crmul(a1,m2);
	s2=crmul(crmul(cmul(als,als),1./rhos),m3);
	s4=cdiv(cmplx(h2,0.),*divfac);
	a2=cdiv(cmplx(h1,0.),*divfac);
	a3=csub(cmul(lambda,s2),cmplx(m1,0.));

	/* compute pwin */
	compute_pwin (wtype, block_size, bp, dp, w, decay, pw1, pw2, pw3, pw4, 
		pwin, p, pp);

	/* compute gl and gt and gam */
	compute_gl_gt_gam (wtype, nlayers, block_size, al, at, rho, pp, gl, gt, 
		gam);

	/* compute alpha and betha */
	compute_alpha_betha (lsource, block_size, ats, rhos, gl, gt, alpha, betha);

	/* compute sigmau1,sigmau2,sigmad1,sigmad2 */
	compute_sigmas (wtype, block_size, lsource, m1, m2, meu, alpha, betha, p, 
		gl, gt, gam, a2, a3, pwin, s1, s2, s4, sigmau1, sigmau2, sigmad1,
		sigmad2); 
}
	
/******************************************************************************

	Subroutine to determine the type (acoustic or elastic) of sources
				and receivers 

******************************************************************************/
void source_receiver_type (int nor, int nlayers, int lsource, int  *lobs, 
	float *cl, float *ct, int *acoustic, int *flag)
/******************************************************************************
Input:
nor	    	number of recievers
nlayers		number of reflecting layers
cl 	    	array[nlayers] of compressional velocities km/s
ct 	    	array[nlayers] of shear velocities km/s
acoust  	array[nor] of receiver-type flags
		=1 for receiver acoustic
		=2 for first receiver elastic below source
		=3 for receiver elastic below source
		=4 for non-physical conditions ??
flag    	array[nor] of flags ??
		=1 if the compressional and shear velocities in the
		receiver layer are the same as their corresponding
		values for the last layer

Output:
acoust   	array[nor] of updated flags according to reciver type
flag
******************************************************************************/
{
	int k=0,iz;			/* loop counters */
	int jkl,lmn;			/* auxiliary indices */

	/* loop over the receivers */
	for (iz=0; iz<nor; iz++) {
		jkl=lsource-1;
		lmn=lobs[iz]-1;
		acoustic[iz]=0;
		flag[iz]=0;

		/* test condition for flag */
		if ((cl[nlayers-1]==cl[lmn])&&(ct[nlayers-1]==ct[lmn]))
			flag[iz]=1;

		/* test condition for source and receiver type */
		if (ct[jkl]==0.0) {		/* source acoustic */

			if (ct[lmn]==0.0) {	/* receiver acoustic */
				acoustic[iz]=1;	
			} else {
				if (lmn>lsource-1) k++;

				if (k==1) { 	/* 1st elastic rec below sorce*/
					acoustic[iz]=2;	
				} else {    	/* elastic receiver below surface */
					acoustic[iz]=3;	
				}
			}

		} else {    			/* source elastic */

			if (ct[lmn]==0.0) { /* receiver acoustic */

				acoustic[iz]=1;

			}
		}
	}
	
	/* only for Po/So */
	if ((nor<=1)&&(lsource>lobs[0])&&(ct[0]==0.0)) acoustic[0]=4;
}

/******************************************************************************

		Subroutine to compute the number of slowness values, final 
				slownesses, slownes increment and np3

******************************************************************************/
void compute_slowness (int wtype, int lsource, float w, float p2w, int *np, 
	float *fp, float fs, float e4, float dk, float decay, float xmax, float *dp,
	float *pw1, float *pw2, float *pw3, float *pw4, complex *cdp, float *cl, 
	float *ct, float *t)
/*****************************************************************************
Input:
lsource 		layer on top of which the source is located 
w			frequency in rad/sec
p2w			maximum ray parameter value to which the synthetics
			are computed
fs			sampling parameter, usually between 0.07 and 0.12
e4
dk
decay   		decay factor to avoid time series wraparound
xmax    		maximum range
dp			pointer to ray parameter increment
np			pointer to number of ray parameters
pw1			pointer to left lower side window ray parameter
pw2			pointer to right lower side window ray parameter
pw3			pointer to left higher side window ray prameter		
pw4			pointer to right higher side window ray parameter
cdp			pointer to complex ray parameter increment
cl			array of compressional velocities
t			array of layer thicknesses

Output:
			calculated values for np,bp,fp,dp,pw1,pw2,pw3 and pw4
******************************************************************************/
{
	int numw;		/* number of frequencies */
	int index=0;
	float fq;		/* frequency in hertz */
	float fp1,fp2,dkt;   	/* auxiliary variables */
	float bp;		/* beginning and final ray parameters */

	/* initialize variables */
	fq=w/(PI*2.0);
	bp=0.0;
	if (wtype==1) index=lsource-1;
	else if (wtype==2) index=0;

	/* compute fp1 and fp2 */
	if (ct[index]<=0.0005) {
		fp1=sqrt(pow(1.0/cl[lsource-1],2)+pow(50.0/(w*(t[lsource-1]+
				0.00001)),2));
		fp2=sqrt(pow(1.0/cl[lsource-2],2)+pow(50.0/(w*(t[lsource-2]+
				0.00001)),2));
	} else {
		fp1=sqrt(pow(1.0/ct[lsource-1],2)+pow(50.0/(w*(t[lsource-1]+
				0.00001)),2));
		fp2=sqrt(pow(1.0/ct[lsource-2],2)+pow(50.0/(w*(t[lsource-2]+
				0.00001)),2));
	}

	/* compute the slowness parameters */
	*fp=MAX(fp1,fp2);
	dkt=dk*PI/xmax;
	*dp=(dkt/w)*pow((1.0+fq/fs),e4/2.);
	*dp=MIN(0.002,*dp);
	if ((p2w>0.0)&&(*fp>=p2w)) *fp=p2w;
	*np=*fp/ *dp;
	*fp=*np* *dp; 
	*cdp=cmplx(*dp,-(*dp)*(decay/w));

	/* if required, use default values for window slowness parameters */
	if ((*pw2<=0.) && (*pw3<=0.)) {
		numw=*np*0.15-1;
		*pw1=bp;
		*pw2=bp+*dp*numw;
		*pw3=*fp-*dp*numw;
		*pw4=*fp;
	}
}

/******************************************************************************

			Subroutine to compute the al and at arrays 

******************************************************************************/
void compute_al_at (int wtype, int nlayers, int layern, float eps, float epsp, 
	float epss, float sigma, float sigp, float sigs, float *wrefp, float *wrefs,
	float fw, float *cl, float *ct, float *ql, float *qt, float *rho, 
	complex wpie, complex *al, complex *at)
/******************************************************************************
Input:
nlayers 		number of reflecting layers
nlayern	
eps
epsp
spss
wpie    		complex frequency
sigma
sigp
sigs
cl			array of compressional wave velocities
ct			array of shear wave velocities
ql			array of compressional Q-values
qt			array of shear Q-values
rho			array of densities

Output:
al			array of ??
at			array of ??
******************************************************************************/
{
	int i;  					/* loop counter */
	float qwif,qwifs,qwifp;		/* auxilairy variables */
	float wref;    				/* reference frequency in rad/sec */	
	float wrefpp,wrefss;		/* reference p ands frequencies in rad/sec */	
	complex eiw,eiwp,eiws;		/* auxiliary variables */

	/* compute reference frequencies for p and s waves in rad/s */
	wrefpp=*wrefp;
	wrefss=*wrefs;
	wref=2.0*PI*fw;
	wrefpp *=2.0*PI;
	wrefss *=2.0*PI;

	/* compute the new p and s q values */
	qwif=(pow((eps*eps+wref*wref),sigma/2.)/(sin(sigma*PI/2.)*2.));
	qwifp=(pow((epsp*epsp+wrefpp*wrefpp),sigp/2.)/(sin(sigp*PI/2.)*2.));
	qwifs=(pow((epss*epss+wrefss*wrefss),sigs/2.)/(sin(sigs*PI/2.)*2.));

	/* compute wie */
	eiw=crpow(csub(cmplx(eps,0.0),cmul(cmplx(0.0,1.0),wpie)),sigma);

	/* check to see if layern was provided as an input */
	if (layern != 0) {
		eiwp=crpow(csub(cmplx(epsp,0.0),cmul(cmplx(0.0,1.0),wpie)),sigp);
		eiws=crpow(csub(cmplx(epss,0.0),cmul(cmplx(0.0,1.0),wpie)),sigs);

		/* compute al and at until just before layern */
		for (i=0; i<layern-1; i++) {
			al[i]=cmul(cmplx(1./cl[i],0.),cadd(cdiv(cmplx(qwif/ql[i],0.),
				eiw),cmplx(1.0,0.0)));
			if (ct[i]==0.0) {
				at[i]=cmplx(0.0,0.0);
			} else {
				at[i]=cmul(cmplx(1./ct[i],0.),cadd(cdiv(cmplx(qwif/qt[i],0.),
				eiw),cmplx(1.0,0.0)));
			}
		}

		/* compute al and at for the remaining layers */
		for (i=layern-1; i<nlayers; i++) {
			al[i]=cmul(cmplx(1./cl[i],0.),cadd(cdiv(cmplx(qwifp/ql[i],0.),
				eiwp),cmplx(1.0,0.0)));
			if (ct[i]==0.0) {
				at[i]=cmplx(0.0,0.0);
			} else {
				at[i]=cmul(cmplx(1./ct[i],0.),cadd(cdiv(cmplx(qwifs/qt[i],0.),
				eiws),cmplx(1.0,0.0)));
			}
		}

	} else {

		/* compute al and at for all layers */
		for (i=0; i<nlayers; i++) {
			al[i]=cmul(cmplx(1./cl[i],0.),cadd(cdiv(cmplx(qwif/ql[i],0.),
				eiw),cmplx(1.0,0.0)));
			if (ct[i]==0.0) {
				at[i]=cmplx(0.0,0.0);
			} else {
				at[i]=cmul(cmplx(1./ct[i],0.),cadd(cdiv(cmplx(qwif/qt[i],0.),
				eiw),cmplx(1.0,0.0)));
			}
		}
	}
	
	/* update pointers */
	*wrefp=wrefpp;
	*wrefs=wrefss;
}

/******************************************************************************

		Subroutine to compute the prs array

******************************************************************************/
void compute_prs (int nor, int *lobs, complex *al, complex *at, float *rho,
	complex *prs)
/******************************************************************************
Input:
nor			number of receivers
lobs		array[nor] layers on top of which the receivers are locate
al			arrar....
at			arrar....
rho			array of densities

Output
prs			array  ....
******************************************************************************/
{
	int ijk;
	int ijk1;
	complex ctemp;
	float rp;

	/* loop over the receivers */
	for (ijk=0; ijk<nor; ijk++) {
		ijk1=lobs[ijk]-1;
		rp=at[ijk1].r;
		if (rp==0.0) {
			prs[ijk]=crmul(cdiv(cmplx(1.0,0.0),cmul(al[ijk1],al[ijk1])),rho[ijk1]);
		} else {
			ctemp=cmul(cdiv(al[ijk1],at[ijk1]),cdiv(al[ijk1],
				at[ijk1]));
			prs[ijk]=crmul(csub(cmplx(1.0,0.0),crmul(ctemp,4./3.)),rho[ijk1]);
		}
	}
}

/******************************************************************************

	Subroutine to compute the number of blocks and block size for
			matrix computations 

******************************************************************************/
void compute_block_size (int np, int *block_size, int *nblock, int *left)
/******************************************************************************
Input:
np				Number of ray parameters
block_size		pointer to block size
nblock			pointer to number of blocks
left			pointer to remainder blocks

Output
				computed values for block_size, nblock and left
******************************************************************************/
{
	if (np<=BLK) {		 

		/* one block is enough and nothing is left */
		*block_size=np;
		*nblock=1;
		*left=0;

	} else {
		
		/* define block size and compute number of required blocks */
		*block_size=BLK;
		*left=np%BLK;
		*nblock=np/BLK+1;
	}
}

/*****************************************************************************

		Subroutine to compute the pwin array 

*****************************************************************************/
void compute_pwin (int wtype, int block_size, float bp, float dp, float w, 
	float decay, float pw1, float pw2, float pw3, float pw4, complex *pwin, 
	complex *p, complex *pp)
/******************************************************************************
Input:
block_size	block size for matrix computations
bp			begin ray parameter
dp			ray parameter increment
pw1			left lower window ray parameter for tapering
pw2			right lower window ray parameter for tapering
pw3			left upper window ray parameter for tapering
pw4			right upper window ray parameter for tapering

Output:
p			array[block_size] of complex ray parameters
pp			array[block_size] of complex squared ray parameters
pwin		array[block_size] of windowed complex ray parameters
******************************************************************************/
{
	int ip;				/* loop counter			*/
	float rp;			/* current ray parameter	*/
	float win=0.0,win1,win2;	/* auxiliary variables		*/

	/* loop over blocks */
	for (ip=0; ip<block_size; ip++) {

		/* compute array of complex slownesses */
		rp=bp+ip*dp;
		if (wtype==1) {
			p[ip]=cmplx(rp,0.0);
		} else if (wtype==2) {
			p[ip]=cmplx(rp, -decay*rp/w);
		}
		pp[ip]=cmul(p[ip],p[ip]);

		/* compute Hanning tapered window */
		if ((rp>=pw1)&&(rp<pw2)) {
			win1=0.5*(1.+cos(PI*((rp-pw1)/(pw2-pw1)-1.)));
			win=win1;
		}
		if ((rp>=pw2)&&(rp<=pw3)) win=1.0;
		if ((rp>pw3)&&(rp<pw4)) {
			win2=0.5*(1.+cos(PI*((rp-pw3)/(pw4-pw3))));
			win=win2;
		}
		if ((rp<pw1)||(rp>=pw4)) win=0.0;

		/* multiply complex ray parameter by window to compute pwin */
		pwin[ip]=crmul(cwp_csqrt(p[ip]),win);
	}
}

/******************************************************************************

	Subroutine to compute arrays gl, gt and gam

******************************************************************************/
void compute_gl_gt_gam (int wtype, int nlayers, int block_size, complex *al, 
	complex *at, float *rho, complex *pp, complex *gl, complex *gt, 
	complex *gam)
/******************************************************************************
Input:
nlayers 		number of reflecting layers
block_size	block size for matrix computations
al			array[nlayers] of ...
at			array[nlayers] of ...
rho			array[nlayers] of densities
pp			array[nlayers] of complex squared ray parameters

Output:
gl			array[block_size] of ...
gt			array[block_size] of ...
gam			array[block_size] of ...
*****************************************************************************/
{
	int il,ip,ijk=0;	/* loop counters */
	int ik1,ijk1;		/* auxiliary indices */
	float rho1;		/* current density */
	complex p1,p2;

	/* loop over layers */
	for (il=0; il<nlayers; il++) {
		p1=cmul(al[il],al[il]);
		p2=cmul(at[il],at[il]);
		rho1=rho[il];
		ik1=il*block_size;

		/* if p2 is not complex number zero */
		if ((p2.r !=0.0)||(p2.i !=0.0)) {
			for (ip=0; ip<block_size; ip++) {
				ijk1=ik1+ip;

				/* compute gl and gt */
				gl[ijk1]=cwp_csqrt(csub(p1,pp[ip]));
				gt[ijk1]=cwp_csqrt(csub(p2,pp[ip]));

				/* make imaginary parts of gl and gt positive */
				if (gl[ijk1].i<0.0) gl[ijk1].i *=-1.0;
				if (gt[ijk1].i<0.0) gt[ijk1].i *=-1.0;
				if (wtype==2) {
					/* make imaginary parts of gl and gt positive */
					if (gl[ijk1].i<0.0) gl[ijk1].r *=-1.0;
					if (gt[ijk1].i<0.0) gt[ijk1].r *=-1.0;
				}
	
				/* compute gam */
				gam[ijk1]=crmul(csub(cmplx(1.0,0.0),crmul(cdiv(pp[ip],p2),2.)),rho1);
			}

		} else {		/* p2 is complex number zero */

			for (ip=0; ip<block_size; ip++) {
				ijk1=ik1+ip;

				/* compute gl and set gt to zero */
				gl[ijk1]=cwp_csqrt(csub(p1,pp[ip]));
				gt[ijk1]=cmplx(0.0,0.0);

				/* if necessary, make imaginary part of gl positive */
				if (gl[ijk].i<0.0) gl[ijk1]=cneg(gl[ijk1]);
				gam[ijk1]=cmplx(rho1,0.0);
			}
		}
	}
}

/******************************************************************************

		Subroutine to compute alpha and betha

******************************************************************************/
void compute_alpha_betha (int lsource, int block_size, complex ats, float rhos, 
	complex *gl, complex *gt, complex *alpha, complex *betha)
/******************************************************************************
Input:
lsource		layer on top of which the source is located
block_size	block size for matrix computaions
ats			al[lsource-1]
rhos		rho[lsource-1] (density at the source layer)
gl			complex array[nlayers] of ...
gt			complex array[nlayers] of ...

Output
alpha		complex array[block_size] of ...
betha		complex array[block_size] of ...
******************************************************************************/
{
	int ip;				/* loop counter */
	int ik1,ijk1;		/* auxiliary indices */

	ik1=(lsource-1)*block_size;

	if ((ats.r==0.0)&&(ats.i==0.0)) {
		for (ip=0; ip<block_size; ip++) {
			ijk1=ik1+ip;
			alpha[ip]=cdiv(cmplx(1.0,0.0),crmul(crmul(gl[ijk1],rhos),2.0));
			betha[ip]=cmplx(0.0,0.0);
		}
	} else {
		for (ip=0; ip<block_size; ip++) {
			ijk1=ik1+ip;
			alpha[ip]=cdiv(cmplx(1.0,0.0),crmul(crmul(gl[ijk1],rhos),2.0));
			betha[ip]=cdiv(cmplx(1.0,0.0),crmul(crmul(gt[ijk1],rhos),2.0));
		}
	}
}	

/******************************************************************************

	Subroutine to compute arrays for sigmau1, sigmau2, sigmad1
						and sigmad2

******************************************************************************/
void compute_sigmas (int wtype, int block_size, int lsource, float m1, 
	float m2, complex meu, complex *alpha, complex *betha, complex *p, 
	complex *gl, complex *gt, complex *gam, complex a2, complex a3, 
	complex *pwin, complex s1, complex s2, complex s4, complex *sigmau1, 
	complex *sigmau2, complex *sigmad1, complex *sigmad2) 
/******************************************************************************
Input:
block_size	block size for matrix computations
meu
alpha
betha
p			array of complex ray parameters
gl
gt
gam
a2			
a3
pwin
s1
s2
s4
sigmau1		pointer to sigmau1
sigmau2		pointer to sigmau2
sigmad1		pointer to sigmad1
sigmad2		pointer to sigmad2

Output:
			Computed values for sigmau1, sigmau2, sigmad1, sigmad2
******************************************************************************/
{
	int ip;								/* loop counter */
	int ijk1,ik1;						/* auxiliary indices */
	complex d1,d2,d3,d4,d5,d6,d7,d8,s3;	/* auxiliary variables */	

	/* compute auxiliary index */
	ik1=(lsource-1)*block_size;

	/* loop over layers in a block */	
	for (ip=0; ip<block_size; ip++) {
		ijk1=ik1+ip;

		if (wtype==1) {
			/* compute auxiliary variables */
			d1=crmul(cmul(cmul(meu,gl[ijk1]),cmul(alpha[ip],p[ip])),2.0);
			d2=cneg(cmul(gam[ijk1],alpha[ip]));
			d3=cmul(p[ip],alpha[ip]);
			d4=cneg(cmul(gl[ijk1],alpha[ip]));
			d5=cmul(gam[ijk1],betha[ip]);
			d6=crmul(cmul(cmul(meu,gt[ijk1]),cmul(betha[ip],p[ip])),2.0);
			d7=cmul(gt[ijk1],betha[ip]);
			d8=cmul(p[ip],betha[ip]);
			s3=cadd(a2,cmul(p[ip],a3));

			/* compute sigmas */
			sigmau1[ip]=cneg(cmul(cadd(cadd(cmul(d1,s1),cmul(d2,s2)),
				cadd(cmul(d3,s3),cmul(d4,s4))),pwin[ip]));
			sigmau2[ip]=cneg(cmul(cadd(cadd(cmul(d5,s1),cmul(d6,s2)),
				cadd(cmul(d7,s3),cmul(d8,s4))),pwin[ip]));
			sigmad1[ip]=cmul(cadd(csub(cmul(d1,s1),cmul(d2,s2)),
				csub(cmul(d4,s4),cmul(d3,s3))),pwin[ip]);
			sigmad2[ip]=cmul(cadd(csub(cmul(d6,s2),cmul(d5,s1)),
				csub(cmul(d7,s3),cmul(d8,s4))),pwin[ip]);

		} else if (wtype==2) {
		
			/* avoid p and gt being zero */
			if ((p[ip].r==0.)&&(p[ip].i==0.)) p[ip]=cmplx(0.0001,0.0);
			if ((gt[ijk1].r==0.)&&(gt[ijk1].i==0.)) gt[ijk1]=cmplx(0.0001,0.0);

			/* compute sigmas */
			sigmau2[ip]=cmplx(0.0,0.0);
			sigmad2[ip]=cmplx(0.0,0.0);
			sigmau1[ip]=cneg(cmul(cadd(crmul(cinv(cmul(meu,p[ip])),m2/2.),
				crmul(cinv(cmul(meu,gt[ijk1])),m1/2.)),pwin[ip]));
			sigmad1[ip]=sigmau1[ip];
		}
	}
}

/******************************************************************************

			Subroutine to compute moment tensor components 

******************************************************************************/
void compute_moment_tensor (int wtype, float phi, float lambda, float delta, 
	float phis, float m0, float *m1, float *m2, float *m3)
/******************************************************************************
Input:
phi			azimuth of the receiver location (degrees)
lambda		rake (degrees)
delta		dip (degrees)
phis		azimuth of the fault plane (degrees)

Output
m1			[1][1] component of moment tensor
m2			[1][2] and [2][1] components of moment tensor
m3			[2][2] component of moment tensor
******************************************************************************/ 
{
	float a,b,c,d,e,f;			/* auxiliary variables */

	/* update fault plane azimuth */
	phis -=phi;

	/* convert angles to radians */
	delta *=PI/180.;
	lambda *=PI/180.;
	phis *=PI/180.;

	if (wtype==1) {
		/* compute auxiliary variables */
		a=sin(phis);
		b=a*a;
		c=sin(delta)*cos(lambda)*sin(2.*phis);
		d=sin(2.*delta)*sin(lambda);
		e=cos(delta)*cos(lambda)*cos(phis);
		f=cos(2.*delta)*sin(lambda)*a;

		/* compute the moment tensor components */
		*m1=-m0*(c+d*b);
		*m2=-m0*(e+f);
		*m3=m0*d;
	} else if (wtype==2) {
		*m1=m0*(sin(delta)*cos(lambda)*cos(2.*phis)+0.5*sin(2.*delta)*
			sin(lambda)*sin(2.*phis));
		*m2=-m0*(cos(delta)*cos(lambda)*sin(phis)-cos(2.*delta)*
			sin(lambda)*cos(phis));
		*m3=0.0;
	}
}

/******************************************************************************

		Subroutine to expand interpolated layers and update
				input parameters

******************************************************************************/
void parameter_interpolation (int nlayers, int *intlayers, int *nintlayers,
	float *intlayth, float *cl, float *ql, float *ct, float *qt, float *rho, 
	float *t) 
/******************************************************************************
Input:
nlayers		number of total reflecting layers
nlint		number of times layer interpolation is required
intlayers	array[nlint] of layers on top of which interpolation is required
nintlayers	array[nlint] of number of layers to interpolate each time
intlayth	array[nlint] of layer thicknesses to interpolate each time
cl			array[nlayers] of compressional velocities (Km/s)
ql			array[nlayers] of compressional q values
ct			array[nlayers] of shear velocities (Km/s)
qt			array[nlayers] of shear q values
rho			array[nlayers] of densities
t			array[nlayers] of absolute depths (Km)

Output:
			same arrays with interpolated layers
*******************************************************************************
Note:
******************************************************************************/
{
	int i=0,ii=0,j;			/* loop counters */
	int nintlay;			/* number of layer to interpolate */
	float tintlay;			/* thickness at which to interpolate */
	float incrcl;			/* interpolation step for cl */
	float incrql;			/* interpolation step for ql */
	float incrct;			/* interpolation step for ct */
	float incrqt;			/* interpolation step for qt */
	float incrrho;			/* interpolation step for rho */
	float crust=0.0;		/* auxiliary variable */

	while (i<nlayers) {
	
		if (intlayers[ii] == i) { /* interpolation requested for this layer */

			/* get number of layers to interpolate */
			nintlay=nintlayers[ii];
			tintlay=intlayth[ii];

			/* check if thickness is negative */
			if (tintlay<0.0) {
				tintlay=-tintlay-crust;
			}
			crust +=tintlay;

			/* if required, adjust input parameters */
			if (ct[i+nintlay]==-1.0) 
				ct[i+nintlay]=cl[i+nintlay]/sqrt(3.0);
			if (qt[i+nintlay]==-1.0) 
				qt[i+nintlay]=ql[i+nintlay]/2.;
			if (rho[i+nintlay-1]==-1.0) 
				rho[i+nintlay]=(cl[i+nintlay]+1.5)/3.;

			/* compute the interpolation steps */
			incrcl=(cl[i+nintlay]-cl[i-1])/(nintlay+1);
			incrql=(ql[i+nintlay]-ql[i-1])/(nintlay+1);
			incrct=(ct[i+nintlay]-ct[i-1])/(nintlay+1);
			incrqt=(qt[i+nintlay]-qt[i-1])/(nintlay+1);
			incrrho=(rho[i+nintlay]-rho[i-1])/(nintlay+1);

			/* do the actual interpolation */
			for (j=0; j<nintlay; j++) {
				cl[j+i-1]=cl[i-1]+(j+1)*incrcl;
				ql[j+i-1]=ql[i-1]+(j+1)*incrql;
				ct[j+i-1]=ct[i-1]+(j+1)*incrct;
				qt[j+i-1]=qt[i-1]+(j+1)*incrqt;
				rho[j+i-1]=rho[i-1]+(j+1)*incrrho;
				t[j+i-1]=tintlay/nintlay;
			}

			/* update counter by number of interopolated layers */
			i +=nintlay;
			ii++;

		} else {		/* no layer interpolation requested */

			/* if required, adjust input parameters */	
			if (ct[i]==-1.0) 
				ct[i]=cl[i]/sqrt(3.0);
			if (qt[i]==-1.0)
				qt[i]=ql[i]/2.;
			if (rho[i]==-1.0)
				rho[i]=(cl[i]+1.5)/3.;
			if (t[i]<0.0)
				t[i]=-t[i]-crust;
			if (i<nlayers-1)
				crust +=t[i];
			else 
				t[i]=10000.0;	/* make last layer a subspace */
			i++;
		}
	}
}

/******************************************************************************

					Compute random velocity layers

******************************************************************************/
void random_velocity_layers (int *nlayers, int *lsource, int nrand_layers,
	float sdcl, float sdct, float layer, float zlayer, float *cl, float *ql, 
	float *ct, float *qt, float *rho, float *t)
/******************************************************************************
Input:
nlayers		pointer to number of reflecting layers
lsource		pointer to layer on top of which the source is located
sdcl		compressional wave velocity standar deviation
sdct		shear wave velocity standar deviation
layer		layer number to star computing random velocity layers
zlayer		layer thickness above which to insert the random layers
cl			array[nlayers] of compressional wave velocities
ql			array[nlayers] of compressional wave Q-values
ct			array[nlayers] of shear wave velocities
qt			array[nlayers] of shear wave Q-values
rho			array[nlayers] of densities
t			array[nlayers] of layer thicknesses

Output:	
			same arrays with the random velocity layers information
			included and nlayers and lsource updated to reflect the
			insertion of the random velocity layers
*******************************************************************************
Note:
This subroutine computes a "layer" number of layers with random velocities
with means cl(compressional) and ct(shear) and standard deviations sdcl and 
sdct. The layers are inserted after the layer thickness zlayer and numbered
"layer" and onwards
******************************************************************************/
{
	int i=0,il,j=0;		/* loop counters */
	int icnt;		/* count number of layers to insert */
	int ijk;
	int ksource=0;		/* index to update source layer number*/
	int nl=*nlayers;	/* current number of layers */
	int nlmax;
	float d1,d2,tlast,x,zl;			/* auxiliary varibles */
	float cls,cts,qls,qts,rhos;		/* auxiliary variables */
	float *cll,*ctt,*qll;			/* scratch arrays */
	float *qtt,*rhoo,*tt;			/* more scratch arrays */

	fprintf (stderr, "before rand: nlayers=%d ",nl);

	/* allocate working space */
	cll=alloc1float(nl);
	ctt=alloc1float(nl);
	qll=alloc1float(nl);
	qtt=alloc1float(nl);
	rhoo=alloc1float(nl);
	tt=alloc1float(nl);
	
	/* initialize variables */
	d1=2.0*sqrt(3.0)*(sdcl/100.0);
	d2=2.0*sqrt(3.0)*(sdct/100.0);

	/* save information from last layer */
	cls=cl[nl-1];
	cts=ct[nl-1];
	qls=ql[nl-1];
	qts=qt[nl-1];
	rhos=rho[nl-1];
	nlmax=nrand_layers+nl-layer+1;

	/* main loop to compute the random layers */
	for (il=layer-1; il<nl-1; il++) {

		if (t[il]<=zlayer) {
			icnt=1;		/* compute just one layer */
			zl=t[il];	/* with thickness t[il] */
		} else {
			tlast=fmod(t[il],zlayer);
			icnt=t[il]/zlayer;				/* make icnt layers with */
			zl=zlayer+tlast/(float)icnt;	/* thickness t[il]/zlayer */
		}

		if (il==*lsource-1) ksource=j;
		for (ijk=0; ijk<icnt; ijk++) {
			tt[i]=zl;					/* all layers, same thickness */
			x=franuni()-0.5;
			cll[i]=cl[il]*(1.0+d1*x);	/* random velocities */
			ctt[i]=ct[il]*(1.0+d2*x);
			qll[i]=ql[il];				/* same q's and rho */
			qtt[i]=qt[il];
			rhoo[i]=rho[il];
			
			/* if maximum number of layers reached, exit inner loop */
			if (i==nlmax) break; 

			/* update counters */
			i++;
			if (il<*lsource-1) j++;
		}

		/* if maximum number of layers reached, exit outer loop */
		if (i==nlmax) break; 	
	}

	/* update number of layers and source layer index */ 
	nl=i+layer-1;
	*lsource +=ksource-1;

	/* restore information for last layer */
	cl[nl-1]=cls;
	ql[nl-1]=qls;
	ct[nl-1]=cts;
	qt[nl-1]=qts;
	rho[nl-1]=rhos;

	/* main loop to insert the computed information for the random layers */
	for (il=layer-1, i=0; il<nl-1; il++, i++) {
		cl[il]=cll[i];
		ct[il]=ctt[i];
		rho[il]=rhoo[i];
		ql[il]=qll[i];
		qt[il]=qtt[i];
		t[il]=tt[i];
		if (il==*lsource-1) {
			cl[il]=cl[il-1];
			ct[il]=ct[il-1];
			ql[il]=ql[il-1];
			qt[il]=qt[il-1];
			rho[il]=rho[il-1];
		}
	}
	t[nl-1]=10000.0;	/* make last layer a subspace */

	/* update pointer for number of layers */
	*nlayers=nl;
	fprintf (stderr, "after rand: nlayers=%d\n",nl);

	/* free allocated space */
	free1float(cll);
	free1float(ctt);
	free1float(qll);
	free1float(qtt);
	free1float(rhoo);
	free1float(tt);
}


/******************************************************************************

		Subroutine to apply flattening earth approximation

******************************************************************************/
void apply_earth_flattening (int nlayers, float z0, float *cl, float *ct, 
	float *rho, float *t)
/******************************************************************************
Input:
nlayer		number of reflecting layers
z0			first layer depth
cl			array[nlayers] of compressional wave velocities
ct			array[nlayers] of shear wave velocities
rho			array[nlayers] of densities
t			array[nlayers] of layer thicknesses

Output:	
			same arrays with the correction applied
			(the thicksnesses array, t, is not modified)
*******************************************************************************
Note:
This subroutine applies the earth flattening correction after Chapman, 1973
******************************************************************************/
{
	int il;				/* loop counter */
	float y0,w0;		/* auxiliary variables */
	float z=z0;			/* layer depth */
	float *zz;			/* scratch array */

	/* allocate working space */
	zz=alloc1float(nlayers);

	/* create and array of absolute depths from the thicknesses */
	zz[0]=z;
	for (il=0; il<nlayers-1; il++) {
		zz[il+1]=z+t[il];
		z=zz[il+1];
	}

	/* apply the earth flattening correction */
	for (il=0; il<nlayers; il++) {

		/* compute the correction */
		y0=RSO/(RSO-zz[il]);
		zz[il]=RSO*log(y0);
		w0=RSO/(RSO-zz[il]);

		/* apply the correction */
		cl[il] *=w0;
		ct[il] *=w0;
		rho[il] /=w0;
	}

	/* update the thicknesses */
	for (il=1; il<nlayers; il++) t[il-1]=zz[il]-zz[il-1];

 	/* make last layer a half space*/
	t[nlayers-1]=10000.0;

	/* free allocated space */
	free1float(zz);
}
