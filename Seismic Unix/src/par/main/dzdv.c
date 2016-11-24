/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* DZDV: $Revision: 1.6 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[]={
"									",
" DZDV - determine depth derivative with respect to the velocity	",  
"  parameter, dz/dv,  by ratios of migrated data with the primary 	",
"  amplitude and those with the extra amplitude				",
"									",
" dzdv <infile afile=afile dfile=dfile>outfile [parameters]		",
"									",
" Required Parameters:							",
" infile=	input of common image gathers with primary amplitude	",
" afile=	input of common image gathers with extra amplitude	",
" dfile=	output of imaged depths in common image gathers 	",
" outfile=	output of dz/dv at the imaged points			",
" nx= 	        number of migrated traces 				",
" nz=	        number of points in migrated traces 			",
" dx=		horizontal spacing of migrated trace 			",
" dz=	        vertical spacing of output trace 			",
" fx=	        x-coordinate of first migrated trace 			",
" fz=	        z-coordinate of first point in migrated trace 		",
" off0=         first offset in common image gathers 			",
" noff=	        number of offsets in common image gathers  		",
" doff=	        offset increment in common image gathers  		",
" cip=x1,z1,r1,..., cip=xn,zn,rn         description of input CIGS	",
"	x	x-value of a common image point				",
"	z	z-value of a common image point	at zero offset		",
"	r	r-parameter in a common image gather			",
" 									",
" Optional Parameters:							",
" nxw, nzw=0		window widths along x- and z-directions in 	",
"			which points are contributed in solving dz/dv. 	",
"									",
"									",
" Notes:								",
" This program is used as part of the velocity analysis technique developed",
" by Zhenyue Liu, CWP:1995.						",
NULL};
/*
 * Author: CWP: Zhenyue Liu,  1995
 * 
 * Reference: 
 * Liu, Z. 1995, "Migration Velocity Analysis", Ph.D. Thesis, Colorado
 *      School of Mines, CWP report #168.
 * 
 */
/**************** end self doc ***********************************/

int
main (int argc, char **argv)
{
	int ix,nx,iz,nz,ncip,icdp,noff,ioff,jx,nxw,nzw,ixl,ixh,izl,izh;
	float fx,dx,fz,dz,fnum,fden,offs,doff,off0,z2,temp1;
	float ***g0, ***g;
	float *rat, *xcip, *zcip, *rcip, temp[3], *x, *z0, *z, *r;
	char *afile="", *dfile="";
	FILE *infp=stdin, *outfp=stdout, *afp, *dfp;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);
	
	/* get required parameters */
	if (!getparint("nx",&nx)) err("must specify nx!\n");
	if (!getparint("nz",&nz)) err("must specify nz!\n");
	if (!getparfloat("dx",&dx)) err("must specify dx!\n");
	if (!getparfloat("dz",&dz)) err("must specify dz!\n");
	if (!getparfloat("fx",&fx)) fx = 0;
	if (!getparfloat("fz",&fz)) fz = 0;
	if (!getparfloat("off0",&off0)) err("must specify off0!\n");
	if (!getparfloat("doff",&doff)) err("must specify doff!\n");
	if (!getparint("noff",&noff)) err("must specify noff!\n");
	
	/* get optional parameters */
	if (!getparstring("afile",&afile)) afile="afile";
	if (!getparstring("dfile",&dfile)) dfile="dfile";
	if (!getparint("nxw",&nxw)) nxw = 0;
	if(nxw>nx-1) nxw = nx-1;
	if (!getparint("nzw",&nzw)) nzw = 0;
	if(nzw>nz-1) nzw = nz-1;
 	
	ncip = countparname("cip");
	if (ncip<1) err("Number of CIGS must be greater 0!\n");

        checkpars();
	/* allocate space */
	xcip = alloc1float(ncip);
	zcip = alloc1float(ncip);
	rcip = alloc1float(ncip);
	g0 = alloc3float(nz,noff,nx);
	g = alloc3float(nz,noff,nx);
	x = alloc1float(nx);
	z0 = alloc1float(nx);
	r = alloc1float(nx);
	rat = alloc1float(noff);
	z = alloc1float(noff);
	

	/* read common image gathers with primary amplitude */
	if(fread(g0[0][0],sizeof(float),nx*nz*noff,infp)!=nx*nz*noff)
	    err("cannot read %d values from file %s\n",nx*nz*noff,infp);
	
 	afp = fopen(afile,"r");
	/* read common image gathers with extra amplitude */
	if(fread(g[0][0],sizeof(float),nx*nz*noff,afp)!=nx*nz*noff)
	    err("cannot read %d values from file %s\n",nx*nz*noff,afp);
	
	for(icdp=0; icdp<ncip; ++icdp){
		getnparfloat(icdp+1,"cip",temp);
		xcip[icdp] = temp[0];
		zcip[icdp] = temp[1];
		rcip[icdp] = temp[2];
	}

	dfp = fopen(dfile,"w");

	/* linear interpolate imaged depth and r-parameter between CIGS	*/
	for (ix=0; ix<nx; ++ix)
		x[ix] = fx+ix*dx;
	intlin (ncip,xcip,zcip,zcip[0],zcip[ncip-1],nx,x,z0);
	intlin (ncip,xcip,rcip,rcip[0],rcip[ncip-1],nx,x,r);
 	

	for(ix=0; ix<nx; ++ix){
		ixl = (ix<nxw)?0: ix-nxw;
		ixh = (ix>nx-1-nxw) ? nx-1: ix+nxw;
		z2 = z0[ix]*z0[ix];

		for(ioff=0; ioff<noff; ++ioff){
			offs = off0+ioff*doff;
			temp1 = z2+r[ix]*offs*offs;
			if(temp1<0) temp1 = 0.0;
			z[ioff] = sqrt(temp1);

			/* determine depth sample indices */
			iz = NINT((z[ioff]-fz)/dz);
			izl = (iz<nzw)?0: iz-nzw;
			if(izl>nz-1) izl = nz-1;
			izh = (iz>nz-1-nzw) ? nz-1: iz+nzw;
			if(izh<0) izh = 0;
			fden = fnum = 0.;
			/* Least squares estimation for dz/dv */
			for(jx=ixl; jx<=ixh; ++jx)
			    for(iz=izl; iz<=izh; ++iz){ 
				fden += g0[jx][ioff][iz]*g0[jx][ioff][iz];	
				fnum += g[jx][ioff][iz]*g0[jx][ioff][iz];
			}
			rat[ioff] = (fden==0.0)?0.0:fnum/fden;
		}
		fwrite(rat,sizeof(float),noff,outfp);
		fwrite(z,sizeof(float),noff,dfp);
	}
	return(CWP_Exit());
}

