/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUHROT: $Revision: 1.7 $ ; $Date: 2011/11/16 22:58:31 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation *****************************/
char *sdoc[] = {
"									",
" SUHROT - Horizontal ROTation of three-component data			",
"									",
" suhrot <stdin >stdout [optional parameters]				",
"									",
" Required parameters:							",
" none									",
"									",
" Optional parameters:							",
" angle=rad	unit of angles, choose \"rad\", \"deg\", or \"gon\"	",
" inv=0		1 = inverse rotation (counter-clockwise)		",
" verbose=0	1 = echo angle for each 3-C station			",
"									",
" a=...		array of user-supplied rotation angles			",
" x=0.0,...	array of corresponding header value(s)			",
" key=tracf	header word defining 3-C station (\"x\")		",
"									",
" ... or input angles from files:					",
" n=0		 number of x and a values in input files		",
" xfile=...   file containing the x values as specified by the		",
" 				\"key\" parameter			",
" afile=...   file containing the a values				",
"									",
" Notes:								",
" Three adjacent traces are considered as one three-component		",
" dataset.								",
" By default, the data will be rotated from the Z-North-East (Z,N,E)	",
" coordinate system into Z-Radial-Transverse (Z,R,T).			",
"									",
"	If one of the parameters \"a=\" or \"afile=\" is set, the data	",
"	are rotated by these user-supplied angles. Specified x values	",
"	must be monotonically increasing or decreasing, and afile and	",
"	xfile are files of binary (C-style) floats.			",
"									",
NULL};

/* 
 * Author: Nils Maercklin,
 *		 Geophysics, Kiel University, Germany, 1999.
 *
 *
 * Trace header fields accessed: ns, sx, sy, gx, gy, key=keyword
 * Trace header fields modified: trid
 * 
 */
/**************** end self doc *******************************************/

#define HROT_TVERT TVERT
#define HROT_TINLIN TINLIN
#define HROT_TXLIN TXLIN
#define HROT_ROTVERT ROTVERT
#define HROT_TRADIAL TRADIAL
#define HROT_TTRANS TTRANS

segy tr;

int
main(int argc, char **argv)
{
	FILE *headerfp;	  /* temporary file for trace headers (one 3C station) */

	cwp_String xfile="";	/* file containing positions by key	*/
	FILE *xfilep=NULL;	/* ... and its file pointer 		*/
	cwp_String afile="";	/* file containing times		*/
	FILE *afilep=NULL;	/* ... and its file pointer		*/
	char *key=NULL;		/* header key word from segy.h		*/
	char *type=NULL;	/* ... its type				*/
	int index;		/* ... its index			*/
	Value val;		/* ... its value			*/
	float fval;		/* ... its value cast to float		*/

	int i;
	int verbose;		/* flag for additional information	*/
	int user;		/* flag, 0 = calculate phi from header, */
				/* 1 = user supplied phi		*/
	int inv;		/* flag, 0 = "normal" rotation,		*/
				/* inverse rotation			*/
	int icomp=0;		/* component identifier (0,1,2)		*/
	int nstat=0;		/* number of 3-C stations		*/
	int nt;			/* number of samples in time direction	*/
	int nxv, nav;		/* number of values in arrays x and a	*/
	char *angle=NULL;	/* unit used for angles theta and phi	*/
	float *x=NULL,*a=NULL;	/* arrays for user supplied phi (a) at	*/
				/* station x				*/
	float fangle=0.0;	/* unit conversion factor applied to	*/
				/* angle phi				*/
	float dx,dy;	/* horizontal coordinates relative to origin (sx,sy) */
	float phi=0.0;	/* rotation angle (horizontal azimuth) */
	float **indata;   /* three-component data: 0 = Z, 1 = N, 2 = E */
	float **outdata;  /* output data: 0 = radial direction, */
				/* 1 = transverse */
	
	/* initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* get info from first trace */
	if(!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
			
	/* get parameters */
	if (!getparstring("angle", &angle)) angle="rad";
	if (!getparstring("key", &key)) key="tracf";
	if (!getparint("inv",&inv)) inv=0;	
	if (!getparint("verbose",&verbose)) verbose=0;

	/* get unit conversion factor for angles */
	if (STREQ(angle, "rad")) fangle=1.0;
	else if (STREQ(angle, "deg")) fangle=180.0/PI;
	else if (STREQ(angle, "gon")) fangle=200.0/PI;
	else err("unknown angle=%s", angle);


	/* get user-supplied angles */
	user=1;
	if ((nav=countparval("a"))) {
		if (!(nxv = countparval("x"))) {
			nxv=1;
			x=ealloc1float(1); x[0]=0.0;
		} else {
			x=ealloc1float(nxv); getparfloat("x", x);
		}
		if (nav != nxv) 
			err("number of values in \"x=\" and \"a=\" must be equal");
			
		a=ealloc1float(nav); getparfloat("a", a);
	} else if (getparstring("afile", &afile)) {
		MUSTGETPARSTRING("xfile",&xfile);
		MUSTGETPARINT("n",&nxv);
		nav = nxv;
		a = ealloc1float(nav);
		x = ealloc1float(nxv);

		if(!(afilep=fopen(afile,"r"))) 
			err("cannot open afile=%s",afile);
		if (fread(a,sizeof(float),nav,afilep) != nav) 
			err("error reading afile=%s",afile);

		if(!(xfilep=fopen(xfile,"r"))) 
			err("cannot open xfile=%s",xfile);
		if (fread(x,sizeof(float),nxv,xfilep) != nxv) 
			err("error reading xfile=%s",xfile);

		efclose(afilep);
		efclose(xfilep);
	} else {
	/* or calculate angles from gx,gy,sx,sy */
		user=0;
	}
	
        checkpars();

	/* convert user-supplied angles to radians */
	if (fangle != 1.0 && nav!=0 ) {
		for (i=0;i<nxv; i++) a[i] /= fangle;
	}

	/* get key types and indices */
	type = hdtype(key);
	index = getindex(key);

	/* open temporary file for trace headers */
	headerfp = etmpfile();
		
	/* allocate space */
	indata = ealloc2float(nt,3);
	outdata = ealloc2float(nt,2);
				
	/* loop over traces */
	do {
		/* read data */
		efwrite(&tr, HDRBYTES, 1, headerfp);		
		memcpy((void *) indata[icomp], (const void *) tr.data, nt*FSIZE);
		
		/* get value of key, convert to float, */
		/* and get interpolated phi */
		if (user && icomp==0) {
			gethval(&tr, index, &val);
			fval = vtof(type,val);
			intlin(nxv,x,a,a[0],a[nxv-1],1,&fval,&phi); 
		}
		
		++icomp;
		
		/* process 3-component dataset */
		if (icomp==3) {
			erewind(headerfp);
			icomp = 0;
			++nstat;
				
			/* get coordinates and rotation angle, */
			/* if not user-supplied */
			if (!user) {
				dx = (float) tr.gx - (float) tr.sx;
				dy = (float) tr.gy - (float) tr.sy;
				if (dy) {
					phi = atan2( dx, dy);
					if (phi<0.0)  /* 0 <= phi < 2*PI */
						phi += 2.0*PI;
				} else {
					phi = (dx>0.0) ? 0.5*PI : 1.5*PI;
				}
				if (!(dy && dx)) phi = 0.0;
			}
			
			/* multiply phi with -1, if rotation is inverse */
			if (inv) phi *= -1.0;
			
			/* diagnostic print */
			if (verbose) 
				warn("angle %g %s at station %d",
						phi*fangle, angle, nstat);
				
			/* loop over samples (perform rotation)		   */
			/* The minus sign (-) is introduced here to get   */
			/* clockwise rotation.				*/
			for (i=0;i<nt;i++) {
				outdata[0][i] = cos(-phi)*indata[1][i] + sin(-phi)*indata[2][i];
				outdata[1][i] = - sin(-phi)*indata[1][i] + cos(-phi)*indata[2][i];
			}
				
			/* write data to stdout (untouched Z-component first) */
			efread(&tr, 1, HDRBYTES, headerfp);
			memcpy((void *) tr.data, (const void *) indata[0], nt*FSIZE);
			tr.trid = HROT_ROTVERT;
			puttr(&tr);
		
			for (i=0;i<2;i++) {
				efread(&tr, 1, HDRBYTES, headerfp);
				memcpy((void *) tr.data, (const void *) outdata[i], nt*FSIZE);
				if (i==0) tr.trid = HROT_TRADIAL;
				else if (i==1) tr.trid = HROT_TTRANS;
				puttr(&tr);
			}
			erewind(headerfp);

		} /* end of three-component processing */
			
	} while (gettr(&tr));
	
	efclose(headerfp);
	
	return(CWP_Exit());
}

/* END OF FILE */
