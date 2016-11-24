/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* STIFF2VEL	version 1.0 Date: 8/3 1999 				*/

#include "par.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" STIFF2VEL - Transforms 2D elastic stiffnesses to (vp,vs,epsilon,delta) ",
" 									",
" stiff2vel  nx=  nz=  [optional parameters]				",
" 									",
" Required parameters:							",
" nx=		 	 number of x samples (2nd dimension)		",
" nz=		 	 number of z samples (1st dimension)		",
" rho_file='rho.bin'     input file containing rho(x,z)			",
" c11_file='c11.bin'     input file containing c11(x,z)			",
" c13_file='c13.bin'     input file containing c13(x,z)			",
" c33_file='c33.bin'     input file containing c33(x,z)			",
" c44_file='c44.bin'     input file containing c44(x,z)			",
"									",
" Optional Parameters:							",
" vp_file='vp.bin'	 output file containing P-wave velocities	",
" vs_file='vs.bin'	 output file containing S-wave velocities	",
" rho_file='rho.bin'	 output file containing densities		",
" eps_file='eps.bin'	 output file containing Thomsen epsilon	       	",
" delta_file='delta.bin' output file containing Thomsen delta	       	",
" 									",
" Notes: 								",
" 1. All quantities in MKS units					",
" 2. Isotropy implied by c11(x,z)=c33(x,z)=0 			",
" 3. Vertical symmetry axis is assumed.					",
" 									",
NULL};

/*
 *  Coded:
 *  Aramco: Chris Liner 9/25/2005 
 *          (based on vel2stiff.c)
 */
/**************** end self doc *******************************************/

int
main(int argc, char **argv)
{
	int nx,nz,ix,iz,verbose;
	float tmp;
	float **c11,**c13,**c33,**c44,**vp,**vs,**rho,**eps, **delta;

	/* input files */
	char *c11_file, *c13_file, *c33_file, *c44_file;
	FILE *c11fp, *c13fp, *c33fp, *c44fp;

	/* output files */
	char *vp_file,*vs_file,*rho_file,*eps_file,*delta_file;
	FILE *vpfp,*vsfp,*rhofp,*epsfp,*deltafp;

	/* hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* get required parameters */
	MUSTGETPARINT("nx",  &nx );
	MUSTGETPARINT("nz",  &nz );

	/* get parameters */
	if (!getparstring("rho_file", &rho_file))       rho_file="rho.bin";
	if (!getparstring("c11_file", &c11_file))   	c11_file="c11.bin";
	if (!getparstring("c13_file", &c13_file))   	c13_file="c13.bin";
	if (!getparstring("c33_file", &c33_file))   	c33_file="c33.bin";
	if (!getparstring("c44_file", &c44_file))   	c44_file="c44.bin";
	if (!getparstring("vp_file", &vp_file))       	vp_file="vp.bin";
	if (!getparstring("vs_file", &vs_file))       	vs_file="vs.bin";
	if (!getparstring("eps_file", &eps_file))     	eps_file="eps.bin";
	if (!getparstring("delta_file", &delta_file)) 	delta_file="delta.bin";
	if (!getparint("verbose", &verbose))        	verbose = 1;
	

        checkpars();

	/* allocate space */
	rho	= alloc2float(nz,nx);
	c11	= alloc2float(nz,nx);
	c13	= alloc2float(nz,nx);
	c33	= alloc2float(nz,nx);
	c44	= alloc2float(nz,nx);
	vp	= alloc2float(nz,nx);
	vs	= alloc2float(nz,nx);
	eps	= alloc2float(nz,nx);
	delta   = alloc2float(nz,nx);



	/* read mandatory input files */
	rhofp = efopen(rho_file,"r");
	if (efread(*rho, sizeof(float), nz*nx, rhofp)!=nz*nx)
	  err("error reading rho_file=%s!\n",rho_file);

	c11fp = efopen(c11_file,"r");
	if (efread(*c11, sizeof(float), nz*nx, c11fp)!=nz*nx)
	  err("error reading c11_file=%s!\n",c11_file);

	c13fp = efopen(c13_file,"r");
	if (efread(*c13, sizeof(float), nz*nx, c13fp)!=nz*nx)
	  err("error reading c13_file=%s!\n",c13_file);

	c33fp = efopen(c33_file,"r");
	if (efread(*c33, sizeof(float), nz*nx, c33fp)!=nz*nx)
	  err("error reading c33_file=%s!\n",c33_file);

	c44fp = efopen(c44_file,"r");
	if (efread(*c44, sizeof(float), nz*nx, c44fp)!=nz*nx)
	  err("error reading c44_file=%s!\n",c44_file);

	fclose(rhofp);
	fclose(c11fp);
	fclose(c13fp);
	fclose(c33fp);
	fclose(c44fp);


	/* open output file: */
	vpfp = fopen(vp_file,"w");
	vsfp = fopen(vs_file,"w");
	epsfp = fopen(eps_file,"w");
	deltafp = fopen(delta_file,"w");


	/* loop over gridpoints and do calculations */
	for(ix=0; ix<nx; ++ix){
	      for(iz=0; iz<nz; ++iz){
		vp[ix][iz] = sqrt(c33[ix][iz]/rho[ix][iz]);
		vs[ix][iz] = sqrt(c44[ix][iz]/rho[ix][iz]);
		eps[ix][iz] = (c11[ix][iz]-c33[ix][iz])/(2*c33[ix][iz]);
		tmp = (c13[ix][iz]+c44[ix][iz])*(c13[ix][iz]+c44[ix][iz]);
		tmp = tmp - (c33[ix][iz]-c44[ix][iz])*(c33[ix][iz]-c44[ix][iz]);
		delta[ix][iz] = tmp/(2*c33[ix][iz]*(c33[ix][iz]-c44[ix][iz]));
	      }
	}

	/* write the output files to disk */
	efwrite(*vp,sizeof(float),nz*nx,vpfp);
	efwrite(*vs,sizeof(float),nz*nx,vsfp);
	efwrite(*eps,sizeof(float),nz*nx,epsfp);
	efwrite(*delta,sizeof(float),nz*nx,deltafp);
		if(verbose){
	  warn("Output file for vp : %s ",vp_file);
	  warn("Output file for vs : %s ",vs_file);
	  warn("Output file for epsilon : %s ",eps_file);
	  warn("Output file for delta : %s ",delta_file);
	}


	/* free workspace */
	free2float(vp);
	free2float(vs);
	free2float(rho);
	free2float(eps);
	free2float(delta);
	free2float(c11);
	free2float(c13);
	free2float(c33);
	free2float(c44);

	return(CWP_Exit());	
}
