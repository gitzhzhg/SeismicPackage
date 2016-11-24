/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

/* ELATRIUNI: $Test Release: 1.2 $ ; $Date: 2011/11/21 17:00:43 $	*/

#include "par.h"
#include "tri.h"
#include "elastic.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" ELATRIUNI - convert TRIangulated ELAstic models to UNIformly sampled models",
"									",
"  elatriuni <modelfile nx= nz= [optional parameters]			",
"									",
"Required Parameters:							",
"nx                     number of x samples				",
"nz                     number of z samples				",
"									",
"Optional Parameters:							",
"dx=1.0                 x sampling interval    			    	",
"dz=1.0                 z sampling interval				",
"fx=0.0                 first x sampled					",
"fz=0.0                 first z sampled					",
"a1111file=a1111.bin    bin-file to store a1111 components 		",
"a3333file=a3333.bin    bin-file to store a3333 components 		",
"a1133file=a1133.bin    bin-file to store a1133 components 		",
"a1313file=a1313.bin    bin-file to store a1313 components 		",
"a1113file=a1113.bin    bin-file to store a1113 components 		",
"a3313file=a3313.bin    bin-file to store a3313 components 		",
"a1212file=a1212.bin    bin-file to store a1212 components 		",
"a1223file=a1212.bin    bin-file to store a1223 components 		",
"a2323file=a2323.bin    bin-file to store a2323 components 		",
"rhofile=rho.bin        bin-file to store rho components 		",
"									",
NULL};

/*
 * AUTHORS:  Andreas Rueger, Colorado School of Mines, 01/02/95
		  Dave Hale, Colorado School of Mines, 04/23/91
 	       	
 */

/**************** end self doc ***********************************/


/* the main program */
int main (int argc, char **argv)
{
	int nx,nz,ix,iz;
	float dx,fx,dz,fz,x,z,xmin,xmax,zmin,zmax;
	float **a1111,**a3333,**a1133,**a1313,**a1113;
	float **a1212,**a2323,**a1223,**rho;
	float **a3313;
	Tri *t;
	TriAttributes *ta;
	Model *m;
	char *a1111file, *a3333file, *a1133file;
	char *a1313file, *a1113file, *a3313file;
	char *a1212file, *a1223file, *a2323file;
	char *rhofile;
	FILE *a1111fp=NULL, *a3333fp=NULL, *a1133fp=NULL;
	FILE *a1313fp=NULL, *a1113fp=NULL, *a3313fp=NULL;
	FILE *a1212fp=NULL, *a1223fp=NULL, *a2323fp=NULL;
	FILE *rhofp=NULL;
	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);
	
	/* get required parameters */
	if (!getparint("nx",&nx)) err("must specify nx!");
	if (!getparint("nz",&nz)) err("must specify nz!");
	
	/* get optional parameters */
	if (!getparfloat("dx",&dx)) dx = 1.0;
	if (!getparfloat("dz",&dz)) dz = 1.0;
	if (!getparfloat("fx",&fx)) fx = 0.0;
	if (!getparfloat("fz",&fz)) fz = 0.0;
	
	if(getparstring("a1111file",&a1111file))
		 	a1111fp = efopen(a1111file,"w");
	else a1111fp = efopen("a1111.bin","w");

	if(getparstring("a3333file",&a3333file))
		 	a3333fp = efopen(a3333file,"w");
	else a3333fp = efopen("a3333.bin","w");

	if(getparstring("a1133file",&a1133file))
		 	a1133fp = efopen(a1133file,"w");
	else a1133fp = efopen("a1133.bin","w");

	if(getparstring("a1313file",&a1313file))
		 	a1313fp = efopen(a1313file,"w");
	else a1313fp = efopen("a1313.bin","w");

	if(getparstring("a1113file",&a1113file))
		 	a1113fp = efopen(a1113file,"w");
	else a1113fp = efopen("a1113.bin","w");

	if(getparstring("a3313file",&a3313file))
		 	a3313fp = efopen(a3313file,"w");
	else a3313fp = efopen("a3313.bin","w");

	if(getparstring("a1212file",&a1212file))
		 	a1212fp = efopen(a1212file,"w");
	else a1212fp = efopen("a1212.bin","w");

	if(getparstring("a2323file",&a2323file))
		 	a2323fp = efopen(a2323file,"w");
	else a2323fp = efopen("a2323.bin","w");

	if(getparstring("a1223file",&a1223file))
		 	a1223fp = efopen(a1223file,"w");
	else a1223fp = efopen("a1223.bin","w");

	if(getparstring("rhofile",&rhofile))
		 	rhofp = efopen(rhofile,"w");
	else rhofp = efopen("rho.bin","w");

        checkpars();


	/* read input triangulated sloth model */
	m = readModel(stdin);
	
	/* determine min and max x and z coordinates */
	xmin = m->ymin;
	xmax = m->ymax;
	zmin = m->xmin;
	zmax = m->xmax;
	
	/* allocate space for uniformly sampled stiffnesses */
	a1111 = ealloc2float(nz,nx);
	a3333 = ealloc2float(nz,nx);
	a1133 = ealloc2float(nz,nx);
	a1313 = ealloc2float(nz,nx);
	a1113 = ealloc2float(nz,nx);
	a3313 = ealloc2float(nz,nx);
	a1212 = ealloc2float(nz,nx);
	a1223 = ealloc2float(nz,nx);
	a2323 = ealloc2float(nz,nx);
	rho = ealloc2float(nz,nx);

	/* loop over all samples */
	for (ix=0,x=fx,t=NULL; ix<nx; ++ix,x+=dx) {
		if (x<xmin || x>xmax)
			err("x=%g must be between xmin=%g and xmax=%g",
				x,xmin,xmax);
		for (iz=0,z=fz; iz<nz; ++iz,z+=dz) {
			if (z<zmin || z>zmax)
				err("z=%g must be between zmin=%g and zmax=%g",
					z,zmin,zmax);
			t = insideTriInModel(m,t,z,x);
			ta = (TriAttributes*)t->fa;
			a1111[ix][iz] = ta->a1111;
			a3333[ix][iz] = ta->a3333;
			a1133[ix][iz] = ta->a1133;
			a1313[ix][iz] = ta->a1313;
			a1113[ix][iz] = ta->a1113;
			a3313[ix][iz] = ta->a3313;
			a1212[ix][iz] = ta->a1212;
			a2323[ix][iz] = ta->a2323;
			a1223[ix][iz] = ta->a1223;
			rho[ix][iz] = ta->rho;

		}
	}
	
	/* write uniformly sampled sloth */
	fwrite(a1111[0],sizeof(float),nz*nx,a1111fp);
	fwrite(a3333[0],sizeof(float),nz*nx,a3333fp);
	fwrite(a1133[0],sizeof(float),nz*nx,a1133fp);
	fwrite(a1313[0],sizeof(float),nz*nx,a1313fp);
	fwrite(a1113[0],sizeof(float),nz*nx,a1113fp);
	fwrite(a3313[0],sizeof(float),nz*nx,a3313fp);
	fwrite(a1212[0],sizeof(float),nz*nx,a1212fp);
	fwrite(a2323[0],sizeof(float),nz*nx,a2323fp);
	fwrite(a1223[0],sizeof(float),nz*nx,a1223fp);
	fwrite(rho[0],sizeof(float),nz*nx,rhofp);

	return 1;
}
