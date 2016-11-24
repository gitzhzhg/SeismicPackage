/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/* SUDIPDIVCOR: $Revision: 1.4 $ ; 					*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUDIPDIVCOR - Dip-dependent Divergence (spreading) correction	",
" 								",
"	sudipdivcor <stdin >stdout  [optional parms]		",
" 								",
" Required Parameters:						",
"	dxcdp	distance between sucessive cdps	in meters	",
" 								",
" Optional Parameters:						",
"	np=50		number of slopes			",
"	tmig=0.0	times corresponding to rms velocities in vmig",
"	vmig=1500.0	rms velocities corresponding to times in tmig",
"	vfile=binary	(non-ascii) file containing velocities vmig(t) ",
"	conv=0		=1 to apply the conventional divergence correction",
"	trans=0		=1 to include transmission factors 	",
"	verbose=0	=1 for diagnostic print			",
" 								",
" Notes:								",
" The tmig, vmig arrays specify an rms velocity function of time.",
" Linear interpolation and constant extrapolation is used to determine",
" rms velocities at times not specified.  Values specified in tmig",
" must increase monotonically.					",
" 								",
" Alternatively, rms velocities may be stored in a binary file",
" containing one velocity for every time sample.  If vfile is	",
" specified, then the tmig and vmig arrays are ignored.		",
" The time of the first sample is assumed to be constant, and is",
" taken as the value of the first trace header field delrt. 	",
" 								",
" Whereas the conventional divergence correction (sudivcor) is	",
" valid only for horizontal reflectors, which have zero reflection",
" slope, the dip-dependent divergence correction is valid for any",
" reflector dip or reflection slope.  Only the conventional	",
" correction will be applied to the data if conv=1 is specified. ",
" Note that the conventional correction over-amplifies		",
" reflections from dipping beds					",
" 								",
" The transmission factor should be applied when the divergence ",
" corrected data is to be migrated with a reverse time migration ",
" based on the constant density wave equation.			",
"								",
" Trace header fields accessed:  ns, dt, delrt			",
" 								",
NULL};
/**************** end self doc**************************************/

/* Credits:
 *	CWP: Francesca Fazzari, May 1992
 */
 
/*  The hidden variable "norm" turns on and off the normalization of the
divergence correction by the correction at zero reflection slope and the
first time sampling interval. 	The dip-dependent divergence correction is
equivalent to the conventional correction (sudivcor) only when the
normalization is turned on (the default).  The dip-dependent divergence
correction is designed to correct for geometrical spreading along the
downgoing zero-offset raypath to the reflection point.   Conventional
correction, however, compensates for spreading along both the incident and
the reflected zero-offset raypath.  The conventional correction is equal to
twice the dip-dependent divergence correction for zero reflection slope.
When the dip-dependent divergence correction is normalized, this constant
scale factor of two cancels. 
	If the normalization is turned off and transmission is turned on;
zero-offset `susynlv' amplitudes after dip-dependent divergence correction
will equal exploding reflector `susynlv' amplitudes.
*/

void divcortable(int nt,int np,float dt,float tt[],float vt[],float **divcor,
	int trans, int norm);

void dipfilt(float k,float dpx, float dt, int np, int nw, int nt, float
		**div, complex *p,complex *q);
	
segy tr;

int main (int argc, char **argv)
{
	int nt,it,np,ntau,itau,nx,ix,nk,nkmax,ik,
		ntfft,nxfft,nv,trans,norm,conv,verbose;
	float dt,dx,dpx,k,dk,kscl,t,*tt,*vt,*tmig,*vmig,
		(*vsind)[4],**ptx,**divcor;
	complex **ptk;
	char *vfile="";
	FILE *hfp,*tfp;
	
	/* hook up getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = tr.dt/1000000.0;

	/* get parameters */
	if (!getparfloat("dxcdp",&dx)) err("dxcdp required");
	if (!getparint("np",&np)) np=50;
	if (!getparint("trans",&trans)) trans=0;
	if (!getparint("norm",&norm)) norm=1;
	if (!getparint("conv",&conv)) conv=0;
	if (!getparint("verbose",&verbose)) verbose=0;

	/* get velocity function */
	vt=ealloc1float(nt);
	tt=ealloc1float(nt);
	for (it=0; it<nt; it++)
		tt[it]=it*dt;
	if (!getparstring ("vfile",&vfile)){
		ntau = countparval("tmig");

		if (ntau==0) ntau=1;
		tmig = ealloc1float(ntau);

		if (!getparfloat("tmig",tmig)) tmig[0] = 0.0;

		nv = countparval("vmig");

		if (nv==0) nv=1;

		if (nv!=ntau) 
			err("number of tmig and vmig must be equal");

		vmig = ealloc1float(nv);

		if (!getparfloat("vmig",vmig)) vmig[0] = 1500.0;
		for (itau=1; itau<ntau; itau++)
			if (tmig[itau]<=tmig[itau-1])
			    err("tmig must increase monotonically");
		for (it=0,t=0.0; it<nt; ++it,t+=dt)
			intlin(ntau,tmig,vmig,vmig[0],vmig[ntau-1],
				1,&t,&vt[it]);
		if (ntau!=nt){
			vsind = (float (*)[4])ealloc1float(ntau*4);
			cmonot(ntau,tmig,vmig,vsind);
			intcub(0,ntau,tmig,vsind,nt,tt,vt);
		}
	} else{
		if (fread(vt,sizeof(float),nt,fopen(vfile,"r"))!=nt)
			err("Not %d velocities in file %s",nt,vfile);
	}

        checkpars();

	/* copy traces and headers to temporary files */
	tfp = tmpfile();
	hfp = tmpfile();
	nx = 0;
	do {
		nx++;
		fwrite(&tr,HDRBYTES,1,hfp);
		fwrite(tr.data,sizeof(float),nt,tfp);

	} while(gettr(&tr));
  	fseek(hfp,0L,SEEK_SET);
	fseek(tfp,0L,SEEK_SET);
	if (verbose) fprintf(stderr,"\t%d traces input\n",nx);

	/* determine wavenumber and frequency sampling */
	nxfft = npfar(nx);
	ntfft = npfa(nt);
	nk = nxfft/2+1;
	dx *= 0.001;
	dk = 2.0*PI/(nxfft*dx);

	/* allocate space for Fourier transform */
	ptk = ealloc2complex(nt,nk);
	ptx = ealloc1(nxfft,sizeof(float*));
	for (ix=0; ix<nxfft; ++ix)
		ptx[ix] = (float*)ptk[0]+ix*nt;

	/* allocate space for divergence correction */
	divcor=ealloc2float(nt,np);

	/* build table of divergence corrections */
	divcortable(nt,np,dt,tt,vt,divcor,trans,norm);	

	/* apply conventional correction if required */
	if (conv==1){
		for (ix=0; ix<nx; ++ix){
			efread(ptx[ix],sizeof(float),nt,tfp);
		for (it=0; it<nt; ++it)
			ptx[ix][it] *= divcor[0][it];
		}
	} else {
		/* read and apply fft scaling to traces */ 
		kscl = 1.0/nxfft;
		for (ix=0; ix<nx; ++ix) {
			efread(ptx[ix],sizeof(float),nt,tfp);
			for (it=0; it<nt; ++it)
				ptx[ix][it] *= kscl;
		}
		/* pad with zeros */
		for (ix=nx; ix<nxfft; ++ix)
			for (it=0; it<nt; ++it)
				ptx[ix][it] = 0.0;

		/* Fourier transform ptx(t,x) to ptk(t,k) */
		pfa2rc(-1,2,nt,nxfft,ptx[0],ptk[0]);
		if (verbose) fprintf(stderr,"\tFourier transform done\n");

		/* define relevant k range */
		nkmax = MIN(nk,NINT(PI/dt/vt[0]/dk));
		dpx = 1.0/(np-1)/vt[0];
		fprintf(stderr,
			"nkmax %d nk %d dk %f dpx %f \n",nkmax,nk,dk,dpx);

		/* special case k=0 */
		for (it=0; it<nt; it++){
			ptk[0][it].r *= divcor[0][it];
			ptk[0][it].i *= divcor[0][it];
		}
	
		/* loop over wavenumbers */
		for (ik=1,k=dk; ik<nkmax; ++ik,k+=dk){

			/* report */
			if (verbose && ik%(nkmax/10>0?nkmax/10:1)==0)
				fprintf(stderr,"\t%d of %d wavenumbers done\n",
						ik,nkmax);
		
			/* dip filter divergence correction */
			dipfilt(k,dpx,dt,np,ntfft,nt,divcor,ptk[ik],ptk[ik]);
		}

		/* Fourier transform p(t,k) to p(t,x) */
		pfa2cr(1,2,nt,nxfft,ptk[0],ptx[0]);
		if (verbose) 
			fprintf(stderr,"\tinverse Fourier transform done\n");
	} /* end else dipdivcor */

	/* output migrated traces with headers */
	for (ix=0; ix<nx; ++ix) {
		efread(&tr,HDRBYTES,1,hfp);
		memcpy((void *) tr.data,
			(const void *) ptx[ix], nt*sizeof(float));
		puttr(&tr);
	}
	
	return EXIT_SUCCESS;
}

void dipfilt(float k,float dpx, float dt, int np, int nw, int nt, float
**div,complex *p,complex *q)
/*********************************************************************
Jacubowicz filter to apply dip-dependent divergence correction
**********************************************************************
Input:
k		wavenumber
dpx		dip sampling interval
dt		time sampling interval
np		number of reflection slopes
nt		number of time samples
nw		number of frequency samples
div		amplitude table
p		array[nt] containing input p(t,k)

Output:
q		array[nt] containing divergence corrected output q(t,k)
*********************************************************************/
{
	int ip,iw,it,iwl,iwh;
	float dw,wny,pscl,fftscl,pmin,ph,pm,pl;
	complex *kq,*qq;

	/* allocate workspace */
	kq = ealloc1complex(nw);
	qq = ealloc1complex(nw);

	dw = 2.0*PI/(nw*dt);
	wny = PI/dt;
	pscl = 0.5;
	fftscl = 1.0/nw;
	pmin = k/wny;
	
	/* initialize qq */
	for (iw=0; iw<nw; iw++){
		qq[iw].r = 0.0;
		qq[iw].i = 0.0;	
	}

	for (ip=np-1; ip>=0; ip--){

		ph=dpx*(ip+1);
		pm=dpx*ip;
		pl=dpx*(ip-1);

		/*if (ip==np-1) ph=pm;*/

		/* define frequency range */
		iwl = k/ph/dw + 1;
		iwh = k/pl/dw;

		if (pl<1.01*pmin) iwh = (nw%2 ? (nw-1)/2 : nw/2-1);
		if (pm<1.01*pmin) iwh = (nw%2 ? nw/2 : nw/2-1);

		/* sum over frequency */
		if (iwh>=iwl){
			for (it=0; it<nt; it++){
				kq[it].r = p[it].r*div[ip][it];
				kq[it].i = p[it].i*div[ip][it];
			}
			for (it=nt; it<nw; it++){
				kq[it].r = 0.0;
				kq[it].i = 0.0;
			}
			pfacc(1,nw,kq);

			/* dip filter positive frequencies */
			for (iw=iwl; iw<=iwh; iw++){
				qq[iw].r += kq[iw].r*pscl;
				qq[iw].i += kq[iw].i*pscl;
			}

			/* dip filter negative frequencies */
			iwl=nw-iwl;
			iwh=nw-iwh;

			for (iw=iwh; iw<=iwl; iw++){
				qq[iw].r += kq[iw].r*pscl;
				qq[iw].i += kq[iw].i*pscl;
			}
		}
		if (pm<1.01*pmin) break;
	}

	/* Fourier transform w to t */
	pfacc(-1,nw,qq);
	for (it=0; it<nt; it++){
		q[it].r = qq[it].r*fftscl;
		q[it].i = qq[it].i*fftscl;
	}

	/* free workspace */
	free1complex(kq);
	free1complex(qq);
}

void divcortable(int nt,int np,float dt,float tt[],float vt[],float
**divcor,int trans, int norm)
/********************************************************************
divergence correction for arbitrary v(z) as a function A(px,t)
*********************************************************************
Input:
nt		number of time samples
np		number of reflection slopes
dt		time sampling interval
tt		array of times at which velocity is specified
vt 		array of velocities
divcor		divergence correction table,empty

Output:
divcor		divergence correction table
*********************************************************************/
{
	int it,ip,flag;
	float dpx,px,px2,vel,velold,gamma,gammaold,pz,tau,alpha,v0,
	denom,(*vind)[4],vel1,vel1old,vel2,vel2old,q,qold,p,pold,
	sigma,pz1;

	/* allocate space */
	vind=(float (*)[4])ealloc1float(nt*4);

	/* use v/2 to compensate for v(two-way t)*/
	for (it=0; it<nt; it++)
		vt[it] *= 0.0005;
		
	/* establish spline coefficients*/
	cmonot(nt,tt,vt,vind);

	/* clear divergence correction array */
	for (ip=0; ip<np; ip++)
		for (it=0; it<nt; it++)
			divcor[ip][it] = 0.0;
	
	v0 = vind[0][0];	
	dpx = 1.0/v0/(np-1);

	/* evaluate divergence correction */
	for (ip=0; ip<np; ip++){

		/* calculate px */
		px = ip*dpx;
	
		/* initialize variables */
		vel = v0;
		vel1 = vind[0][1];
		vel2 = vind[0][2];
		tau=0.0;
		px2= px*px;
		gamma = px2*(pow(vel1/vel,2.0) - vel2/vel);
		p=1.0/v0;
		q=0.0;
		sigma=0.0;
		if (ip==np-1) {pz=0.0; flag=0;}
		else {pz = sqrt(1/vel/vel - px2); flag=1;}

		/* ray tracing */
		for (it=1; it<nt; it++){

                	tau += vel*pz*dt;
			/*if (ip!=0) pz += -vel1/vel/vel*dt;*/

			if (tau>=0.0){
			velold = vel;
			vel1old = vel1;
			vel2old = vel2;
			intcub(0,nt,tt,vind,1,&tau,&vel);
			intcub(1,nt,tt,vind,1,&tau,&vel1);
			intcub(2,nt,tt,vind,1,&tau,&vel2);
			/*if (ip==0) pz=1.0/vel;*/
			if (flag==1) {
				/* pz1(it=1) from oldvel oldpz*/
				pz1 = pz-vel1old/velold/velold*dt; 
				/* pz(it=1) based on vel */
				pz = sqrt(1/vel/vel - px2); 
				if (pz1<0.0 || (px*vel>1.0)) {
					flag=0; pz = pz1;}
			}
				else pz += -vel1old/velold/velold*dt; 

			sigma += velold*velold*dt;

			/* Crank-Nicolson on p&q */
			gammaold = gamma;
			gamma = px2*(pow(vel1/vel,2.0) - vel2/vel);
			alpha = 1.0 - gamma*pow((dt/2*vel),2.0);
			qold = q;
			pold = p;
			q = pold*(dt/2/alpha)*(vel*vel + velold*velold) + qold/alpha*(1.0 + pow((dt*vel/2),2.0)*gammaold);
				p = qold*(dt/2/alpha)*(gamma + gammaold) + pold/alpha*(1.0 + pow((dt*velold/2),2.0)*gamma);
			
/*factor of 4 for vrms agreement */
/*correct zero-offset to exp. reflector; one-way w/transmission */

			if (trans==1){ 
				divcor[ip][it] =sqrt(2*sigma*ABS(q)/vel);
				} else {
				 	divcor[ip][it]=sqrt(2*sigma*ABS(q)/v0);
				}
			} else{ 
				vel = v0;
				sigma += vel*vel*dt;
				gammaold = gamma;
				gamma = 0.0;
				alpha = 1.0;
				qold = q;
				pold = p;
				q = pold*(dt*vel*vel) 
					+ qold*(1.0 
						+ pow((dt*vel/2),2.0)*gammaold);
				p = qold*dt/2*gammaold + pold;
				if (trans==1){ 
					divcor[ip][it]=sqrt(2*sigma*ABS(q)/vel);
				} else {
					divcor[ip][it]=sqrt(2*sigma*ABS(q)/v0);
				}
			}
		
		}/* end t*/
	} /* end p */

	/* normalize dip-dependent correction */
 	if (norm==1){
		denom=1.0/divcor[0][1];

		for (ip=np-1; ip>=0; ip--)
			for (it=1; it<nt; it++)
				divcor[ip][it] *= denom;
 	}
}
