/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* VEL2STIFF	version 1.0 Date: 8/3 1999				*/

#include "par.h"
#include "anisotropy.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" VEL2STIFF - Transforms VELocities, densities, and Thomsen or Sayers   ",
"		parameters to elastic STIFFnesses 			",
" 									",
" vel2stiff  [Required parameters] [Optional Parameters] > stdout	",
" 									",
" Required parameters:							",
" vpfile=	file with P-wave velocities				",
" vsfile=	file with S-wave velocities				",
" rhofile=	file with densities					",
"									",
" Optional Parameters:							",
" epsfile=	file with Thomsen/Sayers epsilon			",
" deltafile=	file with Thomsen/Sayers delta			 	",
" gammafile=	file with Thomsen/Sayers gamma			 	",
" phi_file=	angle of axis of symmetry from vertical (radians)	",
"									",
" c11_file=c11_file     output filename for c11 values                  ",
" c13_file=c13_file     output filename for c13 values                  ",
" c15_file=c15_file     output filename for c15 values                  ",
" c33_file=c33_file     output filename for c33 values                  ",
" c35_file=c35_file     output filename for c35 values                  ",
" c44_file=c44_file     output filename for c44 values                  ",
" c55_file=c55_file     output filename for c55 values                  ",
" c66_file=c66_file     output filename for c66 values                  ",
" 									",
" paramtype=1  (1) Thomsen parameters, (0) Sayers parameters(see below) ",
" 									",
" nx=101	number of x samples 2nd (slow) dimension		",
" nz=101	number of z samples 1st (fast) dimension		",
" 									",
" Notes: 								",
" Transforms velocities, density and Thomsen/Sayers parameters		",
" epsilon, delta, and gamma into elastic stiffness coefficients.	",
" 									",
" If only P-wave, S-wave velocities and density is given as input,	",
" the model is assumed to be isotropic.					",
" 									",
" If files containing Thomsen/Sayers parameters are given, the model	",
" will be assumed to have VTI symmetry.		 			",
" 									",
" All input files  vpfile, vsfile, rhofile etc. are assumed to consist  ",
" only of C style binary floating point numbers representing the        ",
" corresponding  material values of vp, vs, rho etc. Similarly, the output",
" files consist of the coresponding stiffnesses as C style binary floats. ",
" If the output files are to be used as input for a modeling program,   ",
" such as suea2df, then further, the contents are assumed be arrays of  ",
" floating point numbers of the form of   Array[n2][n1], where the fast ",
" dimension, dimension 1, represents depth.                             ",

NULL};

/*
 *  Author:
 *  CWP: Sverre Brandsberg-Dahl 1999
 *
 *  Extended:
 *  CWP: Stig-Kyrre Foss 2001
 *  - to include the option to use the parameters by Sayers (1995) 
 *  instead of the Thomsen parameters
 *
 * Technical reference:
 * Sayers, C. M.: Simplified anisotropy parameters for transversely 
 * isotropic sedimentary rocks. Geophysics 1995, pages 1933-1935.
 *
 */
/**************** end self doc *******************************************/

Stiff2D spar;           /* stiffness tensor                     */

int
main(int argc, char **argv)
{
	int nx;			/* size of 2nd (slow) dimension */
	int nz;			/* size of 1st (fast) dimension */
	int ix,iz;		/* counters			*/
	int verbose;		/* verbose flag			*/
	int paramtype;		/* =1 Thomsen, =0 Sayers params	*/

        /* .. stiffness values */
        float **c11=NULL;       /* stiffness c11                */
        float **c13=NULL;       /* stiffness c13                */
        float **c33=NULL;       /* stiffness c33                */
        float **c44=NULL;       /* stiffness c44                */
        float **c66=NULL;       /* stiffness c66                */
        float **c15=NULL;       /* stiffness c15                */
        float **c35=NULL;       /* stiffness c15                */
        float **c55=NULL;       /* stiffness c15                */

	float **vp=NULL;	/* P-wave speed			*/	
	float **vs=NULL;	/* S-wave speed			*/
	float **rho=NULL;	/* density			*/

	float **eps=NULL;	/* Thomson/Sayers epsilon	*/ 
	float **delta=NULL;	/* Thomson/Sayers delta		*/
	float **gamma=NULL;	/* Thomsen/Sayers gamma		*/	
	double **phi=NULL;	/* angles of axis of symmetry	*/	


	/* Input files */
	char *vpfile="";	/* P-wavespeed filename */
	FILE *vpfp=NULL;	/* ... its file pointer */
	char *vsfile="";	/* S-wavespeed filename */
	FILE *vsfp=NULL;	/* ... its file pointer */
	char *rhofile="";	/* density filename	*/
	FILE *rhofp=NULL;	/* ... its file pointer */
	char *epsfile="";	/* epsilon filename 	*/
	FILE *epsfp=NULL;	/* ... its file pointer */
	char *deltafile="";	/* delta filename	*/
	FILE *deltafp=NULL;	/* ... its file pointer */
	char *gammafile="";	/* gamma filename	*/
	FILE *gammafp=NULL;	/* ... its file pointer */
	char *phifile="";	/* phi filename	*/
	FILE *phifp=NULL;	/* ... its file pointer */


        /* ... output files */
        char *c11_file;         /* c11 filename         */
        FILE *c11fp=NULL;       /* ... its file pointer */
        char *c13_file;         /* c13 filename         */
        FILE *c13fp=NULL;       /* ... its file pointer */
        char *c33_file;         /* c33 filename         */
        FILE *c33fp=NULL;       /* ... its file pointer */
        char *c44_file;         /* c44 filename         */
        FILE *c44fp=NULL;       /* ... its file pointer */
        char *c66_file;         /* c66 filename         */
        FILE *c66fp=NULL;       /* ... its file pointer */
        char *c15_file;         /* c15 filename         */
        FILE *c15fp=NULL;       /* ... its file pointer */
        char *c35_file;         /* c35 filename         */
        FILE *c35fp=NULL;       /* ... its file pointer */
        char *c55_file;         /* c55 filename         */
        FILE *c55fp=NULL;       /* ... its file pointer */


	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);


	/* Get required parameters */
	MUSTGETPARSTRING("vpfile",  &vpfile );
	MUSTGETPARSTRING("vsfile",  &vsfile );
	MUSTGETPARSTRING("rhofile", &rhofile);


	/* Get parameters */
	if (!getparint("nx",	&nx))	nx = 101;
	if (!getparint("nz",	&nz))	nz = 101;
	if (!getparint("verbose", &verbose)) verbose = 1;
	if (!getparint("paramtype",&paramtype)) paramtype = 1;
	getparstring("epsfile",  &epsfile);
	getparstring("deltafile",&deltafile);
	getparstring("gammafile",&gammafile);
	getparstring("phifile",&phifile);
	

	/* Allocate space */
	vp = alloc2float(nz,nx);
	vs = alloc2float(nz,nx);
	rho = alloc2float(nz,nx);
	eps = alloc2float(nz,nx);
	delta = alloc2float(nz,nx);
	gamma = alloc2float(nz,nx);
	phi = alloc2double(nz,nx);

	/* Output arrays */
        c11 = ealloc2float(nz,nx);
        c13 = ealloc2float(nz,nx);
        c33 = ealloc2float(nz,nx);
        c44 = ealloc2float(nz,nx);
        c66 = ealloc2float(nz,nx);
        c15 = ealloc2float(nz,nx);
        c35 = ealloc2float(nz,nx);
        c55 = ealloc2float(nz,nx);

	/* Read mandatory input files */
	vpfp = efopen(vpfile,"r");
	if (efread(*vp, sizeof(float), nz*nx, vpfp)!=nz*nx)
	  err("error reading vpfile=%s!\n",vpfile);

	vsfp = efopen(vsfile,"r");
	if (efread(*vs, sizeof(float), nz*nx, vsfp)!=nz*nx)
	  err("error reading vsfile=%s!\n",vsfile);

	rhofp = efopen(rhofile,"r");
	if (efread(*rho, sizeof(float), nz*nx, rhofp)!=nz*nx)
	  err("error reading rhofile=%s!\n",rhofile);

	fclose(vpfp);
	fclose(vsfp);
	fclose(rhofp);


	/* Optional files if anisotropic model */
	/* See if there is an input file for epsilon */
	if (*epsfile!='\0') {
	  if((epsfp=fopen(epsfile,"r"))==NULL)
		err("cannot open epsfile=%s",epsfile);
	  if (fread(eps[0],sizeof(float),nx*nz,epsfp)!=nx*nz)
		err("error reading epsilonfile=%s",epsfile);
	  fclose(epsfp);
	} else {
		if(verbose)
		  warn("No input file for epsilon, setting epsilon to zero!!!");

		/* set epsilon to zero */
		memset((void *) eps[0], 0 , nx*nz*FSIZE);
	}


	/* see if there is an input file for delta */
	if (*deltafile!='\0') {
	  if((deltafp=fopen(deltafile,"r"))==NULL)
		err("cannot open deltafile=%s",deltafile);
	  if (fread(delta[0],sizeof(float),nx*nz,deltafp)!=nx*nz)
		err("error reading deltafile=%s",deltafile);
	  fclose(deltafp);
	} else {
	  if(verbose) warn("No input file for delta, setting delta to zero!!!");

		/* Setting delta to zero */
		memset((void *) delta[0], 0 , nx*nz*FSIZE);
	}


	/* see if there is an input file for gamma */
	if (*gammafile!='\0') {
	  if((gammafp=fopen(gammafile,"r"))==NULL)
		err("cannot open gammafile=%s",gammafile);
	  if (fread(gamma[0],sizeof(float),nx*nz,gammafp)!=nx*nz)
		err("error reading gammafile=%s",gammafile);
	  fclose(gammafp);
	} else {
	  if(verbose) warn("No input file for gamma, setting gamma to zero!!!");
		/* Setting gamma to zero */
		memset((void *) gamma[0], 0 , nx*nz*FSIZE);
	}

	/* see if there is an input file for phi */
	if (*phifile!='\0') {
	  if((phifp=fopen(phifile,"r"))==NULL)
		err("cannot open phifile=%s",phifile);
	  if (fread(phi[0],sizeof(float),nx*nz,phifp)!=nx*nz)
		err("error reading phifile=%s",phifile);
	  fclose(phifp);
	} else {
	  if(verbose) warn("No input file for phi, setting phi to zero!!!");
		/* Setting phi to zero */
		memset((void *) phi[0], 0 , nx*nz*DSIZE);
	}

        /* Open output files */
        if( !getparstring("c11_file",&c11_file)) c11_file = "c11_file";
        c11fp = efopen(c11_file,"w");
        if( !getparstring("c13_file",&c13_file)) c13_file = "c13_file";
        c13fp = efopen(c13_file,"w");
        if( !getparstring("c33_file",&c33_file)) c33_file = "c33_file";
        c33fp = efopen(c33_file,"w");
        if( !getparstring("c44_file",&c44_file)) c44_file = "c44_file";
        c44fp = efopen(c44_file,"w");
        if( !getparstring("c66_file",&c66_file)) c66_file = "c66_file";
        c66fp = efopen(c66_file,"w");
        if( !getparstring("c15_file",&c15_file)) c15_file = "c15_file";
        c15fp = efopen(c15_file,"w");
        if( !getparstring("c35_file",&c35_file)) c35_file = "c35_file";
        c35fp = efopen(c35_file,"w");
        if( !getparstring("c55_file",&c55_file)) c55_file = "c55_file";
        c55fp = efopen(c55_file,"w");


        checkpars();
	/* Loop over gridpoints and do calculations */
	if (paramtype==1) {
		for(ix=0; ix<nx; ++ix){
			float b=0.0,c=0.0;	/* temporary variables */ 

			for(iz=0; iz<nz; ++iz){
				c33[ix][iz] = vp[ix][iz]*vp[ix][iz]*rho[ix][iz];
				c44[ix][iz] = vs[ix][iz]*vs[ix][iz]*rho[ix][iz];
				c11[ix][iz] = c33[ix][iz]*(1+2*eps[ix][iz]);
				c66[ix][iz] = c44[ix][iz]*(1+2*gamma[ix][iz]);
				c = c33[ix][iz]*(2*c44[ix][iz]*(1+delta[ix][iz])
					- c33[ix][iz]*(1+2*delta[ix][iz]));
				b = 2*c44[ix][iz];
				c13[ix][iz] = (-b+sqrt(b*b-4*c))/2;
                                c15[ix][iz] = 0.0;
                                c35[ix][iz] = 0.0;
                                c55[ix][iz] = c44[ix][iz];

			}

			for(iz=0; iz<nz; ++iz) { /* compute rotation */

				spar.a1111 = (double) c11[ix][iz];
                               	spar.a1133 = (double) c13[ix][iz];
                               	spar.a3333 = (double) c33[ix][iz];
                               	spar.a2323 = (double) c44[ix][iz];
                               	spar.a1212 = (double) c66[ix][iz];
                               	spar.a1113 = (double) c15[ix][iz];
                               	spar.a3313 = (double) c35[ix][iz];
                               	spar.a1313 = (double) c55[ix][iz];


                               	rottens2D(&spar,phi[ix][iz]);


                                c11[ix][iz] = (float) spar.a1111;
                                c13[ix][iz] = (float) spar.a1133;
                                c33[ix][iz] = (float) spar.a3333;
                                c44[ix][iz] = (float) spar.a2323;
                                c66[ix][iz] = (float) spar.a1212;
                                c15[ix][iz] = (float) spar.a1113;
                                c35[ix][iz] = (float) spar.a3313;
                                c55[ix][iz] = (float) spar.a1313;

                	}



		}


	} else {
		for(ix=0; ix<nx; ++ix){
			for(iz=0; iz<nz; ++iz){
				c33[ix][iz] = vp[ix][iz]*vp[ix][iz]*rho[ix][iz];
				c44[ix][iz] = vs[ix][iz]*vs[ix][iz]*rho[ix][iz];
				c11[ix][iz] = c33[ix][iz]*(1+2*eps[ix][iz]);
				c66[ix][iz] = c44[ix][iz]*(1+2*gamma[ix][iz]);
				c13[ix][iz] = (delta[ix][iz]+1)*c33[ix][iz]
							-2*c44[ix][iz];
                                c15[ix][iz] = 0.0;
                                c35[ix][iz] = 0.0;
                                c55[ix][iz] = c44[ix][iz];

			}

			for(iz=0; iz<nz; ++iz) { /* compute rotation */

                               	spar.a1111 = (double) c11[ix][iz];
                               	spar.a1133 = (double) c13[ix][iz];
                               	spar.a3333 = (double) c33[ix][iz];
                               	spar.a2323 = (double) c44[ix][iz];
                               	spar.a1212 = (double) c66[ix][iz];
                               	spar.a1113 = (double) c15[ix][iz];
                               	spar.a3313 = (double) c35[ix][iz];
                               	spar.a1313 = (double) c55[ix][iz];

                               	rottens2D(&spar,phi[ix][iz]);

				c11[ix][iz] = (float) spar.a1111;
                                c13[ix][iz] = (float) spar.a1133;
                                c33[ix][iz] = (float) spar.a3333;
                                c44[ix][iz] = (float) spar.a2323;
                                c66[ix][iz] = (float) spar.a1212;
                                c15[ix][iz] = (float) spar.a1113;
                                c35[ix][iz] = (float) spar.a3313;
                                c55[ix][iz] = (float) spar.a1313;
                	}


		}
	}

	/* Write the output files to disk */
        efwrite(*c11,sizeof(float),nz*nx,c11fp);
        efwrite(*c13,sizeof(float),nz*nx,c13fp);
        efwrite(*c33,sizeof(float),nz*nx,c33fp);
        efwrite(*c44,sizeof(float),nz*nx,c44fp);
        efwrite(*c66,sizeof(float),nz*nx,c66fp);
        efwrite(*c15,sizeof(float),nz*nx,c15fp);
        efwrite(*c35,sizeof(float),nz*nx,c35fp);
        efwrite(*c55,sizeof(float),nz*nx,c55fp);

	if(verbose){
		  warn("Output file for c11 parameter plane : %s ",c11_file);
	  	  warn("Output file for c13 parameter plane : %s ",c13_file);
	          warn("Output file for c33 parameter plane : %s ",c33_file);
	          warn("Output file for c44 parameter plane : %s ",c44_file);
	          warn("Output file for c66 parameter plane : %s ",c66_file);
	}


	/* Free workspace */
	free2float(vp);
	free2float(vs);
	free2float(rho);
	free2float(eps);
	free2float(delta);
	free2float(gamma);
	free2float(c11);
	free2float(c13);
	free2float(c33);
	free2float(c44);
	free2float(c66);

	return(CWP_Exit());	
}
