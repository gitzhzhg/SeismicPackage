/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* WKBJ: $Revision: 1.10 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" WKBJ - Compute WKBJ ray theoretic parameters, via finite differencing	",
"									",
" wkbj <vfile >tfile nx= nz= xs= zs= [optional parameters]		",
"									",
" Required Parameters:							",
" <vfile	file containing velocities v[nx][nz]			",
" nx=		number of x samples (2nd dimension)			",
" nz=		number of z samples (1st dimension)			",
" xs=		x coordinate of source					",
" zs=		z coordinate of source					",
"									",
" Optional Parameters:							",
" dx=1.0		 x sampling interval				",
" fx=0.0		 first x sample					",
" dz=1.0		 z sampling interval				",
" fz=0.0		 first z sample					",
" sfile=sfile	file containing sigmas sg[nx][nz]			",
" bfile=bfile	file containing incident angles bet[nx][nz]		",
" afile=afile	file containing propagation angles a[nx][nz]		",
"									",
" Notes:								",
" Traveltimes, propagation angles, sigmas, and incident angles in WKBJ	",
" by finite differences  in polar coordinates. Traveltimes are calculated",
" by upwind scheme; sigmas and incident angles by a Crank-Nicolson scheme.",
"									",
NULL};
/* Credits:
 *	CWP: Zhenyue Liu, Dave Hale, pre 1992. 
 */
/**************** end self doc ********************************/

int
main(int argc, char **argv)
{
	int nx,nz;
	float fx,fz,dx,dz,xs,zs,ex,ez,**v,**t,**a,**sg,**bet;
	FILE *vfp=stdin,*tfp=stdout,*afp,*sfp,*bfp;
	char  *bfile="", *sfile="", *afile="";

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);
	
	/* get required parameters */
	if (!getparint("nx",&nx)) err("must specify nx!\n");
	if (!getparint("nz",&nz)) err("must specify nz!\n");
	if (!getparfloat("xs",&xs)) err("must specify xs!\n");
	if (!getparfloat("zs",&zs)) err("must specify zs!\n");
	
	/* get optional parameters */
	if (!getparfloat("dx",&dx)) dx = 1.0;
	if (!getparfloat("fx",&fx)) fx = 0.0;
	if (!getparfloat("dz",&dz)) dz = 1.0;
	if (!getparfloat("fz",&fz)) fz = 0.0;

	if (!getparstring("sfile",&sfile)) sfile = "sfile";
	if (!getparstring("bfile",&bfile)) bfile = "bfile";
	if (!getparstring("afile",&afile)) afile = "afile";
	
        checkpars();


	if ((sfp=fopen(sfile,"w"))==NULL)
		err("cannot open sfile=%s",sfile);

	if ((bfp=fopen(bfile,"w"))==NULL)
		err("cannot open bfile=%s",bfile);

	if ((afp=fopen(afile,"w"))==NULL)
		err("cannot open afile=%s",afile);

	/* ensure source is in grid */
	ex = fx+(nx-1)*dx;
	ez = fz+(nz-1)*dz;
	if (fx>xs || ex<xs || fz>zs || ez<zs) 
		err("source lies outside of specified (x,z) grid\n");
	
	/* allocate space */
	v = alloc2float(nz,nx);
	t = alloc2float(nz,nx);
	sg = alloc2float(nz,nx);
	a = alloc2float(nz,nx);
	bet = alloc2float(nz,nx);

	/* read velocities */
	fread(v[0],sizeof(float),nx*nz,vfp);

	/* compute times, angles, sigma, and betas */
	eiktam(xs,zs,nz,dz,fz,nx,dx,fx,v,t,a,sg,bet);
	
	/* write first-arrival times */
	fwrite(t[0],sizeof(float),nx*nz,tfp);

	/* write sigma */
	fwrite(sg[0],sizeof(float),nx*nz,sfp);
	
	/* write angle */
	fwrite(a[0],sizeof(float),nx*nz,afp);
	
	/* write beta */
	fwrite(bet[0],sizeof(float),nx*nz,bfp);

	/* close files */
	fclose(sfp);
	fclose(afp);
	fclose(bfp);
	

	/* free space */
	free2float(v);
	free2float(t);
	free2float(a);
	free2float(sg);
	free2float(bet);
	
	return(CWP_Exit());
}
