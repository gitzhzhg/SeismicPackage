/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* MAKEVEL: $Revision: 1.12 $ ; $Date: 2015/08/11 20:58:42 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" MAKEVEL - MAKE a VELocity function v(x,y,z)				",
" 									",
" makevel > outfile nx= nz= [optional parameters]			",
" 									",
" Required Parameters:							",
" nx=                    number of x samples (3rd dimension)		",
" nz=                    number of z samples (1st dimension)		",
" 									",
" Optional Parameters:							",
" ny=1                   number of y samples (2nd dimension)		",
" dx=1.0                 x sampling interval				",
" fx=0.0                 first x sample					",
" dy=1.0                 y sampling interval				",
" fy=0.0                 first y sample					",
" dz=1.0                 z sampling interval				",
" fz=0.0                 first z sample					",
" v000=2.0               velocity at (x=0,y=0,z=0)			",
" dvdx=0.0               velocity gradient with respect to x		",
" dvdy=0.0               velocity gradient with respect to y		",
" dvdz=0.0               velocity gradient with respect to z		",
" vlens=0.0              velocity perturbation in parabolic lens	",
" tlens=0.0              thickness of parabolic lens			",
" dlens=0.0              diameter of parabolic lens			",
" xlens=                 x coordinate of center of parabolic lens	",
" ylens=                 y coordinate of center of parabolic lens	",
" zlens=                 z coordinate of center of parabolic lens	",
" lambda=1.0             make lambda larger to sharpen edge of lens 	",
" vran=0.0		standard deviation of random perturbation	",
" vzfile=                file containing v(z) profile			",
" vzran=0.0              standard deviation of random perturbation to v(z)",
" vzc=0.0                v(z) chirp amplitude				",
" z1c=fz                 z at which to begin chirp			",
" z2c=fz+(nz-1)*dz       z at which to end chirp			",
" l1c=dz                 wavelength at beginning of chirp		",
" l2c=dz                 wavelength at end of chirp			",
" exc=1.0                exponent of chirp				",
NULL};
/**************** end self doc ********************************/

/*
 * Author: Dave Hale
 *
 */

int
main (int argc, char **argv)
{
	int nx;		/* number of points in inline direction */
	int ny;		/* number of points in xline direction */
	int nz;		/* number of points in the depth (fast) direction */

	int ix,iy,iz;   /* counters */

	float dx;	/* increment in inline direction */
	float dy;	/* increment in crossline direction */
	float dz;	/* increment in depth (fast) direction */
	float fx;	/* first inline value */
	float fy;	/* first crossline value */
	float fz;	/* first depth (fast) value */
	
	float x,y,z;	/* inline, xline, depth values where v000 defined */
	float v000;	/* background velocity */
	float dvdx;	/* rate of change of velocity in inline direction*/
	float dvdy;	/* rate of change of velocity in xline direction */
	float dvdz;	/* rate of change of velocity in depth direction */

	float xlens,ylens,zlens; /* location of the center of the lens */
	float lambda;	/* exponential scaling for lens */
	float dlens;	/* diameter of lens */
	float tlens;	/* thickness of lens */
	float vlens;	/* velocity perturbation of lens */
	float xn,ynn,zn; /* scaled lens parameters */

	/*float abot,bbot,atop,btop, */
	float vran,vzran, vzc,z1c,z2c,l1c,l2c,exc,ac,bc,vtemp,
		*v=NULL,*vz=NULL;
	/* float zbot,ztop;*/
	char *vzfile="";
	FILE *outfp=stdout,*vzfp=NULL;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);
	
	/* get required parameters */
	if (!getparint("nx",&nx)) err("must specify nx!\n");
	if (!getparint("nz",&nz)) err("must specify nz!\n");
	
	/* get optional parameters */
	if (!getparint("ny",&ny)) ny = 1;
	if (!getparfloat("dx",&dx)) dx = 1.0;
	if (!getparfloat("dy",&dy)) dy = 1.0;
	if (!getparfloat("dz",&dz)) dz = 1.0;
	if (!getparfloat("fx",&fx)) fx = 0.0;
	if (!getparfloat("fy",&fy)) fy = 0.0;
	if (!getparfloat("fz",&fz)) fz = 0.0;
	if (!getparfloat("v000",&v000)) v000 = 2.0;
	if (!getparfloat("dvdx",&dvdx)) dvdx = 0.0;
	if (!getparfloat("dvdy",&dvdy)) dvdy = 0.0;
	if (!getparfloat("dvdz",&dvdz)) dvdz = 0.0;
	if (!getparfloat("xlens",&xlens)) xlens = fx;
	if (!getparfloat("ylens",&ylens)) ylens = fy;
	if (!getparfloat("zlens",&zlens)) zlens = fz;
	if (!getparfloat("vlens",&vlens)) vlens = 0.0;
	if (!getparfloat("dlens",&dlens)) dlens = 1.0;
	if (!getparfloat("tlens",&tlens)) tlens = 1.0;
	if (!getparfloat("lambda",&lambda)) lambda = 1.0;
	if (!getparfloat("vran",&vran)) vran = 0.0;
	getparstring("vzfile",&vzfile);
	if (!getparfloat("vzran",&vzran)) vzran = 0.0;
	if (!getparfloat("vzc",&vzc)) vzc = 0.0;
	if (!getparfloat("z1c",&z1c)) z1c = fz;
	if (!getparfloat("z2c",&z2c)) z2c = fz+(nz-1)*dz;
	if (!getparfloat("l1c",&l1c)) l1c = dz;
	if (!getparfloat("l2c",&l2c)) l2c = dz;
	if (!getparfloat("exc",&exc)) exc = 1.0;
	
        checkpars();

	/* compute lens constants */
/*	abot = zlens-tlens/2.0;
	bbot = 2.0*tlens/(dlens*dlens);
	atop = zlens+tlens/2;
	btop = -2.0*tlens/(dlens*dlens);
*/
	
	/* compute chirp constants */
	bc = PI/(z2c-z1c)*(1.0/l2c-1.0/l1c);
	ac = 2.0*PI/l1c - 2.0*bc*z1c;
	
	/* allocate space */
	v = alloc1float(nz);
	vz = alloc1float(nz);
	
	/* if specified, read v(z) profile; otherwise, zero v(z) profile */
	if (vzfile[0]!='\0') {
		if ((vzfp=fopen(vzfile,"r"))==NULL)
			err("error opening vzfile=%s",vzfile);
		if (fread(vz,sizeof(float),nz,vzfp)!=nz)
			err("error reading vzfile=%s",vzfile);
		fclose(vzfp);
	} else {
		for (iz=0; iz<nz; ++iz)
			vz[iz] = 0.0;
	}

	/* random v(z) perturbation */
	for (iz=0; iz<nz; ++iz)
		vz[iz] += vzran*frannor();

	/* loop over x */
	for (ix=0,x=fx; ix<nx; ++ix,x+=dx) {
	
		/* loop over y */
		for (iy=0,y=fy; iy<ny; ++iy,y+=dy) {
		
			/* compute top and bottom of lens */
/*
			ztop = atop+btop*(pow(x-xlens,2)+pow(y-ylens,2));
			zbot = abot+bbot*(pow(x-xlens,2)+pow(y-ylens,2));
*/
			
			/* loop over z */
			for (iz=0,z=fz; iz<nz; ++iz,z+=dz) {
				
				/* v(z) profile */
				v[iz] = vz[iz];
				
				/* constant + constant gradient */
				v[iz] += v000+x*dvdx+y*dvdy+z*dvdz;
				
				/* lens */
				xn = 2.0*(x-xlens)/dlens;
				ynn = 2.0*(y-ylens)/dlens;
				zn = 2.0*(z-zlens)/tlens;
				v[iz] += vlens*exp(-lambda*xn*xn+ynn*ynn+zn*zn);
				/*
				if (z>zbot && z<ztop) v[iz] += vlens;
				*/

				/* chirp */
				if (z>z1c && z<z2c) {
					vtemp = sin((ac+bc*z)*z);
					if (vtemp<0.0)
						v[iz] -= vzc*pow(-vtemp,exc);
					else
						v[iz] += vzc*pow(vtemp,exc);
				}

				/* random perturbation */
				v[iz] += vran*frannor();
			}
			
			/* write velocity function */
			fwrite(v,sizeof(float),nz,outfp);
		}
	}
	
	/* free space before returning */
	free1float(v);
	free1float(vz);

	return(CWP_Exit());
}
