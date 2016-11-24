/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUINVXZCO: $Revision: 1.6 $ ; $Date: 2011/11/16 22:14:43 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" SUINVXZCO - Seismic INVersion of Common Offset data for a smooth 	",
"             velocity function V(X,Z) plus a slowness perturbation vp(x,z)",
" 									",
"     suinvvxzco <infile >outfile [optional parameters] 		",
"									",
" Required Parameters:							",
" vfile                  file containing velocity array v[nx][nz]	",
" nx=                    number of x samples (2nd dimension) in velocity",
" nz=                    number of z samples (1st dimension) in velocity",
" nxm=			number of midpoints of input traces		",
"									",
" Optional Parameters:							",
" dt= or from header (dt) 	time sampling interval of input data	",
" offs= or from header (offset) 	source-receiver offset	 	",
" dxm= or from header (d2) 	sampling interval of midpoints 		",
" fxm=0		first midpoint in input trace				",
" nxd=5		skipped number of midpoints (see note)			",
" dx=50.0	x sampling interval of velocity				",
" fx=0.0	first x sample of velocity				",
" dz=50.0	z sampling interval of velocity				",
" nxb=nx/2	band centered at midpoints (see note)			",
" nxc=0		hozizontal range in which velocity is changed		",
" nzc=0		vertical range in which velocity is changed		",
" fxo=0.0	x-coordinate of first output trace 			",
" dxo=15.0	horizontal spacing of output trace 			",
" nxo=101	number of output traces 				",	
" fzo=0.0	z-coordinate of first point in output trace 		",
" dzo=15.0	vertical spacing of output trace 			",
" nzo=101	number of points in output trace			",	
" fmax=0.25/dt	Maximum frequency set for operator antialiasing		",
" ang=180	Maximum dip angle allowed in the image			",
" ls=0		=1 for line source; =0 for point source			",
" pert=0	=1 calculate time correction from v_p[nx][nz]		",
" vpfile	file containing slowness perturbation array v_p[nx][nz]	",
" verbose=1              =1 to print some useful information		",
" 									",
" Notes:								",
" Traveltime and amplitude are calculated by finite difference which	",
" is done only in one of every NXD midpoints; in the skipped midpoint, 	",
" interpolation is used to calculate traveltime and amplitude.		", 
" For each midpoint, traveltime and amplitude are calculated in the 	",
" horizontal range of (xm-nxb*dx, xm+nxb*dx). Velocity is changed by 	",
" constant extropolation in two upper trianglar corners whose width is 	",
" nxc*dx and height is nzc*dz.						",
"									",
" Eikonal equation will fail to solve if there is a polar turned ray.	",
" In this case, the program shows the related geometric information. 	",
" There are three ways to remove the turned ray: smoothing velocity, 	",
" reducing nxb, and increaing nxc and nzc (if the turned ray occurs  	",
" in the shallow areas). To prevent traveltime distortion from a over	",
" smoothed velocity, traveltime is corrected based on the slowness 	",
" perturbation.								",
"									",
" Offsets are signed - may be positive or negative. 			",
"									",
NULL};

/*
 *
 * Author:  Zhenyue Liu, 08/28/93,  Colorado School of Mines 
 *
 * Reference:
 * Bleistein, N., Cohen, J. K., and Hagin, F., 1987,
 *  Two-and-one-half dimensional Born inversion with an arbitrary reference
 *         Geophysics Vol. 52, no.1, 26-36.
 *
 */
/**************** end self doc ***********************************/
 
/*global varibles for detecting turned rays in solving eikonal equation */
int ierr=0;
float x_err, z_err, r_err;
int ia_err;

/* Prototypes for additional eikonal equation functions */
void delta_t (int na, float da, float r, float dr, 
	float uc[], float wc[], float sc[], float bc[],
	float un[], float wn[], float sn[], float bn[]);
void eiktam_pert (float xs, float zs, int nz, float dz, float fz, int nx, 
	float dx, float fx, float **vel, float **vel_p, int pert, 
	float **time, float **angle, float **sig, float **bet);


/* segy trace */
segy tr, tro;

int
main (int argc, char **argv)
{
	int 	ixm,nxm,nt,nxo,nzo,ixo,izo,nsamp,ls,pert,
		nx,nz,nxb,nxd,ixd,i,ix,iz,nx1,nx0,nz0,nxd1,nxc,nzc,
		verbose;
	float   temp,temp1,xo,zo,xi,zi,sx,sz,tsd,ampd,tgd,
		dx,dz,fx,ex,fx1,ex1,samp,smax,fmax,ang,cosa,
		dxm,dt,fxm,fxo,dxo,fzo,dzo,offs,xm,vs0,vg0,odx,odz,odt,
		xs,xg,ez,**outtrace=NULL,**vold=NULL, **vpold=NULL,
		**ts=NULL,**as=NULL,**sgs=NULL,**bas=NULL,
		**tg=NULL,**ag=NULL,**sgg=NULL,**bag=NULL,**amp=NULL,
		**v=NULL,**vp=NULL,**ts1=NULL,**tg1=NULL,**amp1=NULL;
	char *vfile="",*vpfile="";

	/* hook up getpar to handle the parameters */
	initargs(argc, argv);
	requestdoc(1);


	/* get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	if (!getparfloat("dt",&dt)) dt = tr.dt/1000000.0; 
	if (dt<0.0000001) err("dt must be positive!\n");
	if (!getparfloat("offs",&offs)) offs = tr.offset;
	if (!getparfloat("dxm",&dxm)) dxm = tr.d2;
	if  (dxm<0.0000001) err("dxm must be positive!\n");
	
	/* get required parameters */
	if (!getparint("nx",&nx)) err("must specify nx!\n");
	if (!getparint("nz",&nz)) err("must specify nz!\n");
	if (!getparint("nxm",&nxm)) err("must specify nxm!\n");
	if (!getparstring("vfile",&vfile)) err("must specify vfile!\n");
	
	/* get optional parameters */
	if (!getparfloat("fxm",&fxm)) fxm = 0.0;
	if (!getparint("nxd",&nxd)) nxd = 5;
	if (!getparfloat("dx",&dx)) dx = 50.;
	if (!getparfloat("fx",&fx)) fx = 0.0;
	if (!getparfloat("dz",&dz)) dz = 50.;
	if (!getparint("nxb",&nxb)) nxb = nx/2;
	if (!getparint("nxc",&nxc)) nxc = 0;
	nxc = MIN(nxc,nxb);
	if (!getparint("nzc",&nzc)) nzc = 0;
	nzc = MIN(nzc,nz);
	if (!getparfloat("fxo",&fxo)) fxo = 0.0;
	/* check the first output trace	*/
	if (fxo<fx )
		err("the first output trace is out of velocity model.\n");
	if (!getparfloat("dxo",&dxo)) dxo = 15.;
	if (!getparint("nxo",&nxo)) nxo = 101;
	if (!getparfloat("fzo",&fzo)) fzo = 0.;
	if (!getparfloat("dzo",&dzo)) dzo = 15.;
	if (!getparint("nzo",&nzo)) nzo = 101;
	if (!getparint("ls",&ls)) ls = 0;
	if (!getparint("pert",&pert)) pert = 0;
	if (!getparint("verbose",&verbose)) verbose = 1;
	if (!getparfloat("fmax",&fmax)) fmax = 0.25/dt;
	if (!getparfloat("ang",&ang)) ang = 180;
	cosa = cos(ang*PI/180);
	
	/* check the ranges of shots and receivers */
	ex = fx+(nx-1)*dx;
	ez = (nz-1)*dz;

 	for (ixm=0; ixm<nxm; ++ixm) {
		/* compute source and receiver coordinates */
		xs = fxm+ixm*dxm-0.5*offs;
		xg = xs+offs;
		if (fx>xs || ex<xs || fx>xg || ex<xg) 
		err("shot or receiver lie outside of specified (x,z) grid\n");
	} 
		
	odx = 1.0/dx;
	odz = 1.0/dz;
	odt = 1.0/dt;
	smax = 0.5/(dxm*fmax);
	
	/* allocate space */
	vold = ealloc2float(nz,nx);
	if(pert) vpold = ealloc2float(nz,nx);
	outtrace = ealloc2float(nzo,nxo);
	
	/* read velocities and slowness perturbation*/
	if(fread(vold[0],sizeof(float),nx*nz,fopen(vfile,"r"))!=nx*nz)
		err("cannot read %d velocities from file %s",nx*nz,vfile);
	if(pert) {
		/* read slowness perturbation*/
		if (!getparstring("vpfile",&vpfile)) 
			err("must specify vpfile!\n");
		if(fread(vpold[0],sizeof(float),nx*nz,fopen(vpfile,"r"))
			!=nx*nz)
		err("cannot read %d slowness perturbation from file %s"
			,nx*nz,vpfile);
		/* calculate 1/2 perturbation of slowness squares*/
		for (ix=0; ix<nx; ++ix)
			for (iz=0; iz<nz; ++iz)
				vpold[ix][iz] = vpold[ix][iz]/vold[ix][iz];
	}
 	
        checkpars();

	/* initialize output traces	*/
	for(ixo=0; ixo<nxo; ++ixo)
		for(izo=0; izo<nzo; ++izo)
			outtrace[ixo][izo]=0.0;
	
	/* allocate space for traveltimes and other quantities	*/ 
	nx1 = 1+2*nxb;
	ts = ealloc2float(nz,nx1);
	as = ealloc2float(nz,nx1);
	sgs = ealloc2float(nz,nx1);
	bas = ealloc2float(nz,nx1);
	tg = ealloc2float(nz,nx1);
	ag = ealloc2float(nz,nx1);
	bag = ealloc2float(nz,nx1);
	sgg = ealloc2float(nz,nx1);
	amp = ealloc2float(nz,nx1);
	v = ealloc2float(nz,nx1);
	if(pert) vp = ealloc2float(nz,nx1);
	
	if(nxd>1) {
		/* allocate space for interpolation */
		ts1 = ealloc2float(nz,nx1);
		tg1 = ealloc2float(nz,nx1);
		amp1 = ealloc2float(nz,nx1);
  	}
	
	if(ABS(offs)>2*nxb*dx) err("\t band NXB is too small!");

	/* save the skipping number */
	nxd1 = nxd;	
			    				
	/* compute the first z-sample of velocity model which corresponds
	   to fzo	*/
	nz0 = MAX(0,fzo/dz);
	    				
	/* loop over midpoints */
	for (ixm=0; ixm<nxm; ixm+=nxd1){
	    xm = fxm+ixm*dxm;
   	    xs = xm-0.5*offs;
	    xg = xs+offs;
	    /* set range for traveltimes' calculation */
	    fx1 = xm-nxb*dx;	
	    ex1 = MIN(ex+(nxd1-1)*dxm,xm+nxb*dx);
	    nx1 = 1+NINT((ex1-fx1)/dx);	
	    nx0 = (fx1-fx)/dx;
	    temp = (fx1-fx)/dx-nx0;
	    /* transpose velocity such that the first row is at fx1 */
	    for(ix=0;ix<nx1;++ix){
		if(ix<-nx0) 
			for(iz=0;iz<nz;++iz)
				v[ix][iz] = vold[0][iz];
		else if(ix+nx0>nx-2)
			for(iz=0;iz<nz;++iz)
				v[ix][iz]=vold[nx-1][iz];
		else	
			for(iz=0;iz<nz;++iz)
				v[ix][iz] = vold[ix+nx0][iz]*(1.0-temp)
					+temp*vold[ix+nx0+1][iz];
		}

	    if(pert){
	    	for(ix=0;ix<nx1;++ix){
		    if(ix<-nx0) 
			for(iz=0;iz<nz;++iz)
				vp[ix][iz] = vpold[0][iz];
		    else if(ix+nx0>nx-2) 
			for(iz=0;iz<nz;++iz)
				vp[ix][iz]=vpold[nx-1][iz];
		    else
			for(iz=0;iz<nz;++iz)
				vp[ix][iz] = vpold[ix+nx0][iz]*(1.0-temp)
					+temp*vpold[ix+nx0+1][iz];
		}
	    }

	    /* change velocity values within two trianglar upper corners to
 		avoid possible polar-turned rays	*/
	    for(iz=0; iz<nzc; ++iz)
		for(ix=iz*nxc/nzc; ix<nxc; ++ix){
			v[nxc-ix-1][iz] = v[nxc-iz*nxc/nzc][iz];
			v[nx1-nxc+ix][iz] = v[nx1-nxc+iz*nxc/nzc-1][iz];
		}
			
			
	    if(ixm==0 || nxd1==1){
		/* No interpolation for midpoints*/
			
		eiktam_pert(xs,0.,nz,dz,0.,nx1,dx,fx1,v,vp,pert,ts,as,sgs,bas);
		if(ierr){
			/* Ekonal equation suffers from turned rays*/
			fprintf(stderr,"\tEkonal equation fails to solve at x=%g, z=%g\n\tfrom shot xs=%g .\n",x_err+xs,z_err,xs);
			break;
		}
		eiktam_pert(xg,0.,nz,dz,0.,nx1,dx,fx1,v,vp,pert,tg,ag,sgg,bag);
		if(ierr){
			/* Ekonal equation suffers from turned rays*/
			fprintf(stderr,"\tEkonal equation fails to solve at x=%g, z=%g\n\tfrom receiver xg=%g .\n",x_err+xg,z_err,xg);
			break;
		}

		/* calculate velocities at shot and receiver 	*/
		temp = (xs-fx)/dx;
		ixd = temp;
		temp = temp-ixd;
		vs0 = (1-temp)*vold[ixd][0]+temp*vold[ixd+1][0];
		temp = (xg-fx)/dx;
		ixd = temp;
		temp = temp-ixd;
		vg0 = (1-temp)*vold[ixd][0]+temp*vold[ixd+1][0];

		/* calculate amplitude	*/
		for(ix=0; ix<nx1; ++ix){
		    amp[ix][nz0] = 0;
		    for(iz=nz0+1; iz<nz; ++iz){
			/* check operator aliasing */
			if(ABS(sin(bas[ix][iz])/vs0+sin(bag[ix][iz])/vg0)
				>=smax) amp[ix][iz] = 0.;
			/* dip filter for imaging */
			else if(cos((as[ix][iz]+ag[ix][iz])*0.5)<cosa)
				amp[ix][iz] = 0.;
			else {	
			    temp = sqrt(vs0*sgg[ix][iz]/(vg0*sgs[ix][iz]));
			    amp[ix][iz] = (cos(bas[ix][iz])*temp/vs0
				+cos(bag[ix][iz])/(temp*vg0))
				*ABS(cos((as[ix][iz]-ag[ix][iz])*0.5));
			    /* 2.5-D amplitude correction */
			    if(ls==0) amp[ix][iz]
				 *= sqrt(sgs[ix][iz]+sgg[ix][iz]);
			}
		    }
		}

		
		/* loop over output points */

		for (ixo=0,xo=fxo; ixo<nxo; ++ixo,xo+=dxo) 
		    for (izo=1,zo=fzo+dzo; izo<nzo; ++izo,zo+=dzo){ 
			
			/* check range of output */
			if(xo-fx1<0 || xo-ex1>=0 || zo-ez>=0)
				continue;
			/* determine sample indices */
			xi = (xo-fx1)*odx;
			ix = xi;
			zi = zo*odz;
			iz = zi;
			/* bilinear interpolation */
			sx = xi-ix;
			sz = zi-iz;
			tsd = (1.0-sz)*((1.0-sx)*ts[ix][iz] + 
						sx*ts[ix+1][iz]) +
					sz*((1.0-sx)*ts[ix][iz+1] +
						sx*ts[ix+1][iz+1]);
			tgd = (1.0-sz)*((1.0-sx)*tg[ix][iz] + 
						sx*tg[ix+1][iz]) +
					sz*((1.0-sx)*tg[ix][iz+1] +
						sx*tg[ix+1][iz+1]);
						
			samp = (tsd+tgd)*odt;
			nsamp = samp;
			if (nsamp>nt-2) continue;
			samp = samp-nsamp;
			ampd = (1.0-sz)*((1.0-sx)*amp[ix][iz] 
				+sx*amp[ix+1][iz])
				+sz*((1.0-sx)*amp[ix][iz+1]
				+sx*amp[ix+1][iz+1]);
			outtrace[ixo][izo] += ampd*(samp*tr.data[nsamp+1]
				+(1.0-samp)*tr.data[nsamp]);
		    }
			if(ixm<nxm-1) gettr(&tr);
	    }
	    else {
		/* Linear interpolation for midpoints */
			
	    	eiktam_pert(xs,0,nz,dz,0,nx1,dx,fx1,v,vp,pert,ts1,as,sgs,bas);
		if(ierr){
			/* Ekonal equation suffers from turned rays*/
			fprintf(stderr,"\tEkonal equation fails to solve at x=%g, z=%g\n\tfrom shot xs=%g .\n",x_err+xs,z_err,xs);
			break;
		}
		eiktam_pert(xg,0,nz,dz,0,nx1,dx,fx1,v,vp,pert,tg1,ag,sgg,bag);
		if(ierr){
			/* Ekonal equation suffers from turned rays*/
			fprintf(stderr,"\tEkonal equation fails to solve at x=%g, z=%g\n\tfrom receiver xg=%g .\n",x_err+xg,z_err,xg);
			break;
		}
		    
		/* calculate velocities at shot and receiver 	*/
		temp = (xs-fx)/dx;
		ixd = temp;
		temp = temp-ixd;
		vs0 = (1-temp)*vold[ixd][0]+temp*vold[ixd+1][0];
		temp = (xg-fx)/dx;
		ixd = temp;
		temp = temp-ixd;
		vg0 = (1-temp)*vold[ixd][0]+temp*vold[ixd+1][0];
		
		/* calculate amplitude	*/
		for(ix=0; ix<nx1; ++ix){
		    amp1[ix][nz0] = 0;
		    for(iz=nz0+1; iz<nz; ++iz){
			/* check operator aliasing */
			if(ABS(sin(bas[ix][iz])/vs0+sin(bag[ix][iz])/vg0)
				>=smax) amp1[ix][iz] = 0.;
			/* dip filter for imaging */
			else if(cos((as[ix][iz]+ag[ix][iz])*0.5)<cosa)
				amp1[ix][iz] = 0.;
			else {
			    temp = sqrt(vs0*sgg[ix][iz]/(vg0*sgs[ix][iz]));
			    amp1[ix][iz] = (cos(bas[ix][iz])*temp/vs0
				+cos(bag[ix][iz])/(temp*vg0))
				*ABS(cos((as[ix][iz]-ag[ix][iz])*0.5));
			    /* 2.5-D amplitude correction */
			    if(ls==0) amp1[ix][iz]
				 *= sqrt(sgs[ix][iz]+sgg[ix][iz]);
			}
		    }
		}
			
		/* interpolate quantities between two midpoints	*/			
		    xm -= nxd1*dxm;
		    for(i=1; i<=nxd1; ++i){
			xm += dxm;
			xs = xm-0.5*offs;
			xg = xs+offs;
			fx1 = xm-nxb*dx;	
			ex1 = MIN(ex+(nxd1-1)*dxm,xm+nxb*dx);
			nx1 = 1+NINT((ex1-fx1)/dx);	
			temp = nxd1-i;
			temp1 = 1.0/(nxd1-i+1);
			for(ix=0;ix<nx1;++ix)
			    for(iz=nz0;iz<nz;++iz){
			   	ts[ix][iz] = (temp*ts[ix][iz]
					+ts1[ix][iz])*temp1;
			   	tg[ix][iz] = (temp*tg[ix][iz]
					+tg1[ix][iz])*temp1;
			   	amp[ix][iz] = (temp*amp[ix][iz]
					+amp1[ix][iz])*temp1;
			}
				
			/* set the right boundary for output traces */
			ex1 = MIN(ex,xm+nxb*dx);
 		/* loop over output points */
		for (ixo=0,xo=fxo; ixo<nxo; ++ixo,xo+=dxo) 
		    for (izo=1,zo=fzo+dzo; izo<nzo; ++izo,zo+=dzo){ 
			/* check range of output */
			if(xo-fx1<0 || xo-ex1>=0 || zo-ez>=0)
				continue;
			/* determine sample indices */
			xi = (xo-fx1)*odx;
			ix = xi;
			zi = zo*odz;
			iz = zi;
			/* bilinear interpolation */
			sx = xi-ix;
			sz = zi-iz;
			tsd = (1.0-sz)*((1.0-sx)*ts[ix][iz] + 
						sx*ts[ix+1][iz]) +
					sz*((1.0-sx)*ts[ix][iz+1] +
						sx*ts[ix+1][iz+1]);
			tgd = (1.0-sz)*((1.0-sx)*tg[ix][iz] + 
						sx*tg[ix+1][iz]) +
					sz*((1.0-sx)*tg[ix][iz+1] +
						sx*tg[ix+1][iz+1]);
			
			samp = (tsd+tgd)*odt;
			nsamp = samp;
			if (nsamp>nt-2) continue;
			samp = samp-nsamp;
			ampd = (1.0-sz)*((1.0-sx)*amp[ix][iz] 
				+sx*amp[ix+1][iz])
				+sz*((1.0-sx)*amp[ix][iz+1]
				+sx*amp[ix+1][iz+1]);
			outtrace[ixo][izo] += ampd*(samp*tr.data[nsamp+1]
				+(1.0-samp)*tr.data[nsamp]);
		    }
			/* read input trace */
			if(ixm-nxd1+i<nxm-1) gettr(&tr);

		    }	
			
		}
		/* recount the skipping number for the last midpoint */
		if(ixm<nxm-1 && ixm>nxm-1-nxd1) nxd1 = nxm-1-ixm;

		if(verbose) fprintf(stderr,"\tfinish midpoint %i\n",ixm+1);
		if(ierr) break;
	}
	    
	/* write trace */
	temp = 4/sqrt(PI)*dxm;
	for (ixo=0; ixo<nxo; ++ixo){
		/* scale output traces */
 		for(izo=0; izo<nzo; ++izo)	
			outtrace[ixo][izo] *= temp; 
		/* make headers for output traces */
		tro.offset = offs;
		tro.tracl = tro.tracr = 1+ixo;
		tro.ns = nzo;
		tro.d1 = dzo;
		tro.ns = nzo;
		tro.f1 = fzo;
		tro.f2 = fxo;
		tro.d2 = dxo;
		tro.cdp = ixo;
		tro.trid = 200;
		/* copy migrated data to output trace */
		memcpy((void *) tro.data,
			(const void *) outtrace[ixo], nzo*sizeof(float));
		puttr(&tro); 
	}

	free2float(vold);
	free2float(ts);
	free2float(bas);
	free2float(sgs);
	free2float(as);
	free2float(v);
	free2float(tg);
	free2float(bag);
	free2float(sgg);
	free2float(ag);
	free2float(amp);
	if(pert) {
		free2float(vp);
		free2float(vpold);
	}
	if(nxd>1) {
		free2float(ts1);
		free2float(tg1);
		free2float(amp1);
  	}
	return(CWP_Exit());
}

void delta_t (int na, float da, float r, float dr, 
	float uc[], float wc[], float sc[], float bc[],
	float un[], float wn[], float sn[], float bn[])
/*****************************************************************************
difference equation extrapolation of "delta t" in polar coordinates
******************************************************************************
Input:
na		number of a samples
da		a sampling interval
r		current radial distance r
dr		radial distance to extrapolate
uc		array[na] of dt/dr at current r
un		array[na] of dt/dr at next r
wc		array[na] of dt/da at current r
wn		array[na] of dt/da at next r
sc		array[na] of dt/dv at current r

Output:
sn		array[na] of dt/dv at next r 
******************************************************************************

This function implements the Crank-Nicolson finite-difference method with
boundary conditions sn[1]=sn[0] and sn[na-1]=sn[na-2]. ******************************************************************************
Author:  Zhenyue Liu, Colorado School of Mines, 07/8/92
******************************************************************************/
{
	int i;
	float r1,*d,*b,*c,*e;
	
	/* allocate workspace */
	d = alloc1float(na-2);
	b = alloc1float(na-2);
	c = alloc1float(na-2);
	e = alloc1float(na-2);
	
	r1 = r+dr;
 	
	/* Crank-Nicolson */
 	for (i=0; i<na-2; ++i) {
		d[i] = (uc[i+1]+un[i+1])/(2.0*dr);
		e[i] = (wn[i+1]/(r1*r1)+wc[i+1]/(r*r))/(8.0*da);
		b[i] = (bc[i+1]+bn[i+1])*0.5-(sc[i+2]-sc[i])*e[i]
			+d[i]*sc[i+1];
		c[i] = -e[i];
	} 
	d[0] += c[0];
	d[na-3] += e[na-3]; 
	
	tripp(na-2,d,e,c,b);
	for(i=0;i<na-2; ++i) sn[i+1]=b[i];
	sn[0] = sn[1];
	sn[na-1] = sn[na-2];
	
	
	/* free workspace */
	free1float(d);
	free1float(c);
	free1float(e);
	free1float(b);
}

/* functions defined and used internally */
void eiktam_pert (float xs, float zs, int nz, float dz, float fz, int nx, 
	float dx, float fx, float **vel, float **vel_p, int pert,
	float **time, float **angle, float **sig, float **bet)
/*****************************************************************************
Compute traveltimes t(x,z) and  propagation angle a(x,z) via eikonal equation,
and sigma sig(x,z), incident angle bet(x,z) via Crank-Nicolson Method
******************************************************************************
Input:
xs		x coordinate of source (must be within x samples)
zs		z coordinate of source (must be within z samples)
nz		number of z samples
dz		z sampling interval
fz		first z sample
nx		number of x samples
dx		x sampling interval
fx		first x sample
vel		array[nx][nz] containing velocities
vel_p		array[nx][nz] containing velocity perturbations

Output:
time		array[nx][nz] containing first-arrival times
angle		array[nx][nz] containing propagation angles
sig  		array[nx][nz] containing sigmas
bet		array[nx][nz] containing betas
******************************************************************************
Notes:
The actual computation of times and sigmas is done in polar coordinates,
with bilinear interpolation used to map to/from rectangular coordinates.

This version includes a velocity perturbation.
******************************************************************************
Revisor:  Zhenyue Liu, Colorado School of Mines, 7/8/92
******************************************************************************/
{
	int ix,iz,ia,ir,na,nr;
	float ss,a,r,da,dr,fa,fr,ex,ez,ea,rmax,rmaxs,
		**s,**sp,**tp,**up,**wp,**ap,**time_p;

	/* shift coordinates so source is at (x=0,z=0) */
	fx -= xs;
	fz -= zs;
	ex = fx+(nx-1)*dx;
	ez = fz+(nz-1)*dz;
	
	/* determine polar coordinate sampling */
	rmaxs = fx*fx+fz*fz;
	rmaxs = MAX(rmaxs,fx*fx+ez*ez);
	rmaxs = MAX(rmaxs,ex*ex+ez*ez);
	rmaxs = MAX(rmaxs,ex*ex+fz*fz);
	rmax = sqrt(rmaxs);
	dr = MIN(ABS(dx),ABS(dz));
	nr = 1+NINT(rmax/dr);
	dr = rmax/(nr-1);
	fr = 0.0;
	if (fx==0.0 && fz==0.0) {
		fa = 0.0;  ea = PI/2.0;
	} else if (fx<0.0 && fz==0.0) {
		fa = -PI/2.0;  ea = PI/2.0;
	} else if (fx==0.0 && fz<0.0) {
		fa = 0.0;  ea = PI;
	} else {
		fa = -PI;  ea = PI;
	}
	da = dr/rmax;
	na = 1+NINT((ea-fa)/da);
	da = (ea-fa)/(na-1);
	if (fa==-PI && ea==PI)
		na = na-1;
	
	/* allocate space */
	s = alloc2float(nz,nx);
	time_p = alloc2float(nz,nx);
	sp = alloc2float(na,nr);
	tp = alloc2float(na,nr);
	up = alloc2float(na,nr);
	wp = alloc2float(na,nr);
	ap = alloc2float(na,nr);
	
	/* compute slownesses */
	for (ix=0; ix<nx; ++ix)
		for (iz=0; iz<nz; ++iz)
			s[ix][iz] = 1.0/vel[ix][iz];
	
	/* convert from rectangular to polar coordinates */
	recttopolar(nz,dz,fz,nx,dx,fx,s,na,da,fa,nr,dr,fr,sp);
	
	/* average the slownesses in source region */
	for (ir=0,ss=0.0; ir<2; ++ir)
		for (ia=0; ia<na; ++ia)
			ss += sp[ir][ia];
	ss /= 2*na;

	/* compute traveltimes and derivatives in source region */
	for (ir=0,r=0; ir<2; ++ir,r+=dr) {
		for (ia=0; ia<na; ++ia) {
			up[ir][ia] = ss;
			wp[ir][ia] = 0.0;
			tp[ir][ia] = r*ss;
		}
	}

	for (ir=1,r=dr; ir<nr-1; ++ir,r+=dr) {
		eikpex(na,da,r,dr,
			sp[ir],up[ir],wp[ir],tp[ir],
			sp[ir+1],up[ir+1],wp[ir+1],tp[ir+1]);
		if(ierr) {
			x_err = r_err*sin(ia_err*da+fa);
			z_err = r_err*cos(ia_err*da+fa);
			return;
		}
	}
	
	/* convert times from polar to rectangular coordinates */
	polartorect(na,da,fa,nr,dr,fr,tp,nz,dz,fz,nx,dx,fx,time);
	
	/* compute propagation angles in polar and convert */
	for (ia=0,a=fa; ia<na; ++ia,a+=da)
		ap[0][ia] = a;
	for (ir=1,r=fr+dr; ir<nr; ++ir,r+=dr)
		for (ia=0,a=fa; ia<na; ++ia,a+=da){
		    ap[ir][ia] = a+asin(wp[ir][ia]/(sp[ir][ia]*r));
		}
	polartorect(na,da,fa,nr,dr,fr,ap,nz,dz,fz,nx,dx,fx,angle);
	
	/* compute sigmas  for initial values */
	for (ir=0,r=0; ir<2; ++ir,r+=dr) 
		for (ia=0; ia<na; ++ia) tp[ir][ia] = r/ss;

	/* solve difference equation for remaining sigmas */
	for (ir=1,r=dr; ir<nr-1; ++ir,r+=dr) 
 		ray_theoretic_sigma(na,da,r,dr,up[ir],wp[ir],tp[ir],
			up[ir+1],wp[ir+1],tp[ir+1]);  
	polartorect(na,da,fa,nr,dr,fr,tp,nz,dz,fz,nx,dx,fx,sig);
	
	/* compute betas for initial values */
	for (ir=0; ir<2; ++ir) 
		for (ia=0,a=fa; ia<na; ++ia,a+=da) tp[ir][ia] = a;

	/* solve difference equation for remaining betas */
	for (ir=1,r=dr; ir<nr-1; ++ir,r+=dr) 
 		ray_theoretic_beta(na,da,r,dr,up[ir],wp[ir],tp[ir],
			up[ir+1],wp[ir+1],tp[ir+1]);  
	polartorect(na,da,fa,nr,dr,fr,tp,nz,dz,fz,nx,dx,fx,bet);
	
	if(pert){
	    /* compute time perturbation by Crank-Nicolson scheme */

	    recttopolar(nz,dz,fz,nx,dx,fx,vel_p,na,da,fa,nr,dr,fr,sp);
	    /* initial values */
	    for (ir=0,r=0; ir<2; ++ir,r+=dr) 
		for (ia=0; ia<na; ++ia) 
			tp[ir][ia] = r/ss*sp[ir][ia];
	    /* solve difference equation for remaining perturbed time */
	    for (ir=1,r=dr; ir<nr-1; ++ir,r+=dr) 
 		delta_t(na,da,r,dr,up[ir],wp[ir],tp[ir],sp[ir],
			up[ir+1],wp[ir+1],tp[ir+1],sp[ir+1]);  

	    polartorect(na,da,fa,nr,dr,fr,tp,nz,dz,fz,nx,dx,fx,time_p);
	    /* sum the total time */
	    for (ix=0; ix<nx; ++ix)
		for (iz=0; iz<nz; ++iz)
			time[ix][iz] += time_p[ix][iz];
	}

	/* free space */
	free2float(s);
	free2float(time_p);
	free2float(sp);
	free2float(tp);
	free2float(up);
	free2float(wp);
	free2float(ap);
}
