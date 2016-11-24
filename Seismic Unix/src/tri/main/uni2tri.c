/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* UNI2TRI: $Revision: 1.7 $ ; $Date: 2011/11/21 16:56:25 $	*/

#include "par.h"
#include "triP.h"
#include "tri.h"
#include "sloth.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" UNI2TRI - convert UNIformly sampled model to a TRIangulated model	",
"									",
" uni2tri <slothfile >modelfile n2= n1= [optional parameters]		",
"									",
" Required Parameters:							",
" n1=                    number of samples in first (fast) dimension	",
" n2=                    number of samples in second dimension		",
"									",
" Optional Parameters:							",
" d1=1.0                 sampling interval in dimension 1		",
" d2=1.0                 sampling interval in dimension 2		",
" f1=0.0                 first value in dimension 1			",
" f2=0.0                 first value in dimesion 2			",
" ifile=                 triangulated model file used as initial model	",
" errmax=                maximum sloth error (see notes below)		",
" verbose=1              =0 for silence					",
"                        =1 to report maximum error at each stage to stderr",
"                        =2 to also write the normalized error to efile	",
" efile=emax.dat         filename for error file (for verbose=2)	",
" mm=0			output every mm-th intermediate model (0 for none)",
" mfile=intmodel        intermediate models written to intmodel%d	",
" method=3              =1 add 1 vertex at maximum error		",
"                       =2 add vertex to every triangle that exceeds errmax",
"                       =3 method 2, but avoid closely spaced vertices	",
" tol=10                closeness criterion for (in samples)		",
" sfill=                 x, z, x0, z0, s00, dsdx, dsdz to fill a region	",
"									",
" Notes:								",
" Triangles are constructed until the maximum error is			",
" not greater than the user-specified errmax.  The default errmax	",
" is 1% of the maximum value in the sampled input file.			",
"									",
" After the uniform values have been triangulated, the optional sfill	",
" parameters are used to fill closed regions bounded by fixed edges.	",
" Let (x,z) denote any point inside a closed region.  Values inside	",
" this region is determined by s(x,z) = s00+(x-x0)*dsdx+(z-z0)*dsdz.	",
" The (x,z) component of the sfill parameter is used to identify a	",
" closed region.							",
"									",
" The uniformly sampled quantity is assumed to be sloth=1/v^2.		",
"									",
NULL};
/*
 *
 * AUTHOR:  Craig Artley, Colorado School of Mines, 03/31/94
 * NOTE:  After a program outlined by Dave Hale, 12/27/90.
 *
 */
/**************** end self doc ***********************************/

/* prototypes for functions defined and used internally */
static Model *makeMod1 (float errmax, 
	int n2, float d2, float f2, int n1, float d1, float f1, float **s,
	float smax, char *efile, int mm, char *mfile, int verbose);
static Model *makeMod2 (float errmax, 
	int n2, float d2, float f2, int n1, float d1, float f1, float **s,
	float smax, char *efile, int mm, char *mfile, int verbose);
static Model *makeMod3 (float errmax, 
	int n2, float d2, float f2, int n1, float d1, float f1, float **s,
	float smax, char *efile, int mm, char *mfile, int tol, int verbose);
static void makeSlothForTri (Tri *t, float *s00, float *dsdx, float *dsdz);
static void maxErrorInTri (Tri *t,
	int n2, float d2, float f2, int n1, float d1, float f1, float **s,
	float *emax, int *ixmax, int *izmax);
static void setSlothOfVertex (Vertex *v,
	int n2, float d2, float f2, int n1, float d1, float f1, float **s);
static void writeIntermediateModel (Model *m, int im, char *mfile);
static void fillsloth (Model *m, int nr, float **sr);
static void setsloth (Tri *t, float s00, float dsdx, float dsdz);

/* the main program */
int main (int argc, char **argv)
{
	int n2,n1,ix,iz,mm,method,nr,ir,tol,verbose;
	float d2,f2,d1,f1,errmax,smax,**s,**sr;
	Model *m=NULL;
	char *efile="emax.dat",*mfile="intmodel";
	FILE *outfp=stdout;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	/* get required parameters */
	if (!getparint("n2",&n2)) err("must specify n2!");
	if (!getparint("n1",&n1)) err("must specify n1!");

	/* read input uniformly sampled sloth */
	s = ealloc2float(n1,n2);
	if (fread(s[0],sizeof(float),n1*n2,stdin)!=n1*n2)
		err("error reading input file!");

	/* determine maximum sloth input */
	for (ix=0,smax=s[0][0]; ix<n2; ++ix)
		for (iz=0; iz<n1; ++iz)
			if (s[ix][iz]>smax) smax = s[ix][iz];

	/* get optional parameters */
	if (!getparfloat("d2",&d2)) d2 = 1.0;
	if (!getparfloat("d1",&d1)) d1 = 1.0;
	if (!getparfloat("f2",&f2)) f2 = 0.0;
	if (!getparfloat("f1",&f1)) f1 = 0.0;
	if (!getparfloat("errmax",&errmax)) errmax = 0.01*smax;
	getparstring("efile",&efile);
	if (!getparint("mm",&mm)) mm = 0;  mm = (mm>0)?mm:0;
	getparstring("mfile",&mfile);
	if (!getparint("method",&method)) method = 3;
	if (!getparint("tol",&tol)) tol = 10;

	if (!getparint("verbose",&verbose)) verbose = 1;

	/* get parameters for sloth fill */
	nr = countparname("sfill");
	sr = ealloc2float(7,nr);
	for (ir=0; ir<nr; ++ir)
		if (getnparfloat(ir+1,"sfill",sr[ir])!=7)
			err("7 values must be specified in sfill parameter");

	if (verbose) 
		fprintf(stderr,
			"n2=%d d2=%g f2=%g\n" 
			"n1=%d d1=%g f1=%g\n"
			"errmax=%g\n",
			n2,d2,f2,n1,d1,f1,errmax);

	/* make model */
	if (method==1)
		m = makeMod1(errmax,n2,d2,f2,n1,d1,f1,s,smax,
			efile,mm,mfile,verbose);
	else if (method==2)
		m = makeMod2(errmax,n2,d2,f2,n1,d1,f1,s,smax,
			efile,mm,mfile,verbose);
	else if (method==3)
		m = makeMod3(errmax,n2,d2,f2,n1,d1,f1,s,smax,
			efile,mm,mfile,tol,verbose);
	else
		err("Unknown method=%d!\n",method);

	/* fill regions with sloth */
	fillsloth (m,nr,sr);

	/* write model */
	writeModel(m,outfp);

	return EXIT_SUCCESS;
}

/* build model corresponding to uniformly sampled sloth */
static Model *makeMod1 (float errmax, 
	int n2, float d2, float f2, int n1, float d1, float f1, float **s,
	float smax, char *efile, int mm, char *mfile, int verbose)
{
	Model *m;
	Face *f;
	FaceAttributes *fa;
	Vertex *v;
	VertexAttributes *va;
	int ix,iz,ixmax=0,izmax=0,nf,im;
	float xmin,xmax,zmin,zmax,
		emax,e,errfrac;
	char *ifile="";
	FILE *efp=NULL,*ifp;

	/* min and max x and z coordinates */
	xmin = f2;
	zmin = f1;
	xmax = f2+(n2-1)*d2;
	zmax = f1+(n1-1)*d1;

	/* read initial model for interfaces */
	if (getparstring("ifile",&ifile)) {
		ifp = efopen(ifile,"r");
		m = readModel(ifp);
		efclose(ifp);

		/* check that model is compatible with sampled sloth */
		if (ABS(xmin-m->ymin)>0.01*d2)
			err("xmin!=m->ymin %g!=%g",xmin,m->ymin);
		if (ABS(xmax-m->ymax)>0.01*d2)
			err("xmax!=m->ymax %g!=%g",xmax,m->ymax);
		if (ABS(zmin-m->xmin)>0.01*d1)
			err("zmin!=m->xmin %g!=%g",zmin,m->xmin);
		if (ABS(zmax-m->xmax)>0.01*d1)
			err("zmax!=m->xmax %g!=%g",zmax,m->xmax);
/*		if (xmin!=m->ymin) err("xmin!=m->ymin %g!=%g",xmin,m->ymin);
		if (xmax!=m->ymax) err("xmax!=m->ymax %g!=%g",xmax,m->ymax);
		if (zmin!=m->xmin) err("zmin!=m->xmin %g!=%g",zmin,m->xmin);
		if (zmax!=m->xmax) err("zmax!=m->xmax %g!=%g",zmax,m->xmax);
*/
		/* reset epsilon */
/*		m->eps = 0.01*MIN(d2,d1);
*/
/*		m->eps *= 0.001;
*/
		/* loop over faces */
		f = m->f;
		do {
			/* free face attributes */
			free1(f->fa);  f->fa = NULL;

			/* set sloth at each vertex of face */
			setSlothOfVertex(f->eu->vu->v,
				n2,d2,f2,n1,d1,f1,s);
			setSlothOfVertex(f->eu->euCW->vu->v,
				n2,d2,f2,n1,d1,f1,s);
			setSlothOfVertex(f->eu->euCCW->vu->v,
				n2,d2,f2,n1,d1,f1,s);

			/* next face */
			f = f->fNext;
		} while (f!=m->f);

	} else {
		/* initialize model */
		m = makeModel(zmin,xmin,zmax,xmax);
		m->sfa = sizeof(FaceAttributes);
		m->eps = 0.01*MIN(d2,d1);
		v = nearestVertexInModel(m,NULL,zmin,xmin);
		va = (VertexAttributes*) ealloc1(1,sizeof(VertexAttributes));
		va->s = s[0][0];
		v->va = va;
		v = nearestVertexInModel(m,NULL,zmax,xmin);
		va = (VertexAttributes*) ealloc1(1,sizeof(VertexAttributes));
		va->s = s[0][n1-1];
		v->va = va;
		v = nearestVertexInModel(m,NULL,zmin,xmax);
		va = (VertexAttributes*) ealloc1(1,sizeof(VertexAttributes));
		va->s = s[n2-1][0];
		v->va = va;
		v = nearestVertexInModel(m,NULL,zmax,xmax);
		va = (VertexAttributes*) ealloc1(1,sizeof(VertexAttributes));
		va->s = s[n2-1][n1-1];
		v->va = va;
	}

	if (verbose==2)
		if ((efp=fopen(efile,"w"))==NULL)
			err("Could not open efile=%s!\n",efile);

	/* label output columns */
	if (verbose) {
		fprintf(stderr,"iterations faces  "
			"max errror    norm error\n");
		fprintf(stderr,"--------   -----  "
			"----------    ----------\n");
	}

	/* count iterations */
	im = 0;

	/* loop until error is small enough */
	do {
		/* initialize maximum error */
		emax = 0.0;

		/* loop over faces */
		f = m->f;  nf = 0;
		do {
			/* determine maximum error in triangle */
			maxErrorInTri(f,n2,d2,f2,n1,d1,f1,s,&e,&ix,&iz);

			/* if error exceeds maximum */
			if (e>=emax) {
				emax = e;
				ixmax = ix;
				izmax = iz;
			}

			/* next face */
			f = f->fNext;  ++nf;

		} while (f!=m->f);

		/* compute and report maximum error */
		errfrac = emax/smax;
		if (verbose)
			fprintf(stderr," %5d     %5d   % .6g     % .6g\n",
				im,nf,emax,errfrac);
		if (verbose==2) {
			fwrite(&errfrac,sizeof(float),1,efp);
			if (im%50==0) efflush(efp);
		}

		/* if requested, write intermediate model */
		if ((mm>0) ? im%mm==0 : 0)
			writeIntermediateModel(m,im,mfile);

		/* if error is too big, add another vertex */
		if (emax>errmax) {
			v = addVertexToModel(m,f1+izmax*d1,f2+ixmax*d2);
			if (v==NULL) err("This can't happen!");
			va = (VertexAttributes*)
				ealloc1(1,sizeof(VertexAttributes));
			va->s = s[ixmax][izmax];
			v->va = va;
		}

		/* increment iteration counter */
		++im;

	} while (emax>errmax);

	if (verbose==2) fclose(efp);

	/* final loop over faces */
	f = m->f;
	do {
		/* allocate face attributes */
		fa = (FaceAttributes*) ealloc1(1,sizeof(FaceAttributes));
		f->fa = fa;

		/* make sloth for triangle */
		makeSlothForTri(f,&fa->s00,&fa->dsdx,&fa->dsdz);

		/* set density and Q for triangle */
		fa->dens = FLT_MAX;
		fa->qfac = FLT_MAX;

		/* next face */
		f = f->fNext;

	} while (f!=m->f);

	/* return model */
	return m;
}

/* build model corresponding to uniformly sampled sloth */
static Model *makeMod2 (float errmax, 
	int n2, float d2, float f2, int n1, float d1, float f1, float **s,
	float smax, char *efile, int mm, char *mfile, int verbose)
{
	Model *m;
	Face *f;
	FaceAttributes *fa;
	Vertex *v;
	VertexAttributes *va;
	int *ix,*iz,jf,nf,im,iv,nfalloc;
	float xmin,xmax,zmin,zmax,
		emax,*e,errfrac;
	char *ifile="";
	FILE *efp=NULL,*ifp;

	/* min and max x and z coordinates */
	xmin = f2;
	zmin = f1;
	xmax = f2+(n2-1)*d2;
	zmax = f1+(n1-1)*d1;

	/* read initial model for interfaces */
	if (getparstring("ifile",&ifile)) {
		ifp = efopen(ifile,"r");
		m = readModel(ifp);
		efclose(ifp);

		/* check that model is compatible with sampled sloth */
		if (ABS(xmin-m->ymin)>0.01*d2)
			err("xmin!=m->ymin %g!=%g",xmin,m->ymin);
		if (ABS(xmax-m->ymax)>0.01*d2)
			err("xmax!=m->ymax %g!=%g",xmax,m->ymax);
		if (ABS(zmin-m->xmin)>0.01*d1)
			err("zmin!=m->xmin %g!=%g",zmin,m->xmin);
		if (ABS(zmax-m->xmax)>0.01*d1)
			err("zmax!=m->xmax %g!=%g",zmax,m->xmax);
/*		if (xmin!=m->ymin) err("xmin!=m->ymin %g!=%g",xmin,m->ymin);
		if (xmax!=m->ymax) err("xmax!=m->ymax %g!=%g",xmax,m->ymax);
		if (zmin!=m->xmin) err("zmin!=m->xmin %g!=%g",zmin,m->xmin);
		if (zmax!=m->xmax) err("zmax!=m->xmax %g!=%g",zmax,m->xmax);
*/

		/* reset epsilon */
/*		m->eps = 0.01*MIN(d2,d1);
*//*		m->eps *= 0.001;
*/
		/* loop over faces */
		f = m->f;
		do {
			/* free face attributes */
			free1(f->fa);  f->fa = NULL;

			/* set sloth at each vertex of face */
			setSlothOfVertex(f->eu->vu->v,
				n2,d2,f2,n1,d1,f1,s);
			setSlothOfVertex(f->eu->euCW->vu->v,
				n2,d2,f2,n1,d1,f1,s);
			setSlothOfVertex(f->eu->euCCW->vu->v,
				n2,d2,f2,n1,d1,f1,s);

			/* next face */
			f = f->fNext;
		} while (f!=m->f);

	} else {
		/* initialize model */
		m = makeModel(zmin,xmin,zmax,xmax);
		m->sfa = sizeof(FaceAttributes);
		m->eps = 0.01*MIN(d2,d1);
		v = nearestVertexInModel(m,NULL,zmin,xmin);
		va = (VertexAttributes*) ealloc1(1,sizeof(VertexAttributes));
		va->s = s[0][0];
		v->va = va;
		v = nearestVertexInModel(m,NULL,zmax,xmin);
		va = (VertexAttributes*) ealloc1(1,sizeof(VertexAttributes));
		va->s = s[0][n1-1];
		v->va = va;
		v = nearestVertexInModel(m,NULL,zmin,xmax);
		va = (VertexAttributes*) ealloc1(1,sizeof(VertexAttributes));
		va->s = s[n2-1][0];
		v->va = va;
		v = nearestVertexInModel(m,NULL,zmax,xmax);
		va = (VertexAttributes*) ealloc1(1,sizeof(VertexAttributes));
		va->s = s[n2-1][n1-1];
		v->va = va;
	}

	if (verbose==2)
		if ((efp=fopen(efile,"w"))==NULL)
			err("Could not open efile=%s!\n",efile);

	/* label output columns */
	if (verbose) {
		fprintf(stderr,"iterations faces  "
			"max errror    norm error\n");
		fprintf(stderr,"--------   -----  "
			"----------    ----------\n");
	}

	/* allocate space */
	nfalloc = 1024;
	e = ealloc1float(nfalloc);
	ix = ealloc1int(nfalloc);
	iz = ealloc1int(nfalloc);

	/* count iterations */
	im = 0;

	/* loop until error is small enough */
	do {
		/* initialize maximum error */
		emax = 0.0;

		/* loop over faces */
		f = m->f;  jf = 0;
		do {
			/* determine maximum error in triangle */
			maxErrorInTri(f,n2,d2,f2,n1,d1,f1,s,
				&e[jf],&ix[jf],&iz[jf]);

			/* save maximum error */
			if (e[jf]>=emax) emax = e[jf];

			/* next face */
			f = f->fNext;  ++jf;

		} while (f!=m->f);

		/* number of faces */
		nf = jf;

		/* compute and report maximum error */
		errfrac = emax/smax;
		if (verbose)
			fprintf(stderr," %5d     %5d   % .6g     % .6g\n",
				im,nf,emax,errfrac);
		if (verbose==2) {
			fwrite(&errfrac,sizeof(float),1,efp);
			if (im%50==0) fflush(efp);
		}

		/* if requested, write intermediate model */
		if ((mm>0) ? im%mm==0 : 0)
			writeIntermediateModel(m,im,mfile);

		/* loop over points of maximum error */
		for (jf=0,iv=0; jf<nf; ++jf) {

			/* if error is too big, add another vertex */
			if (e[jf]>errmax) {
				v = addVertexToModel(m,
					f1+iz[jf]*d1,f2+ix[jf]*d2);
				if (v==NULL) continue;
				va = (VertexAttributes*)
					ealloc1(1,sizeof(VertexAttributes));
				va->s = s[ix[jf]][iz[jf]];
				v->va = va;
				++iv;
			}
		}

		/* Make sure at least one vertex has been added. */
		/* This should never be a problem. */
		if (iv==0 && emax>errmax) err("added 0 vertices!");

		/* Just added iv vertices at this stage, */
		/* increasing face count by at most 2*iv. */
		/* Allocate more space for vertices if necessary */
		if (nf+2*iv>nfalloc) {
			nfalloc *= 2;
			free1float(e);  e = ealloc1float(nfalloc);
			free1int(ix);  ix = ealloc1int(nfalloc);
			free1int(iz);  iz = ealloc1int(nfalloc);
		}

		/* increment iteration counter */
		++im;

	} while (emax>errmax);

	if (verbose==2) fclose(efp);

	/* final loop over faces */
	f = m->f;
	do {
		/* allocate face attributes */
		fa = (FaceAttributes*) ealloc1(1,sizeof(FaceAttributes));
		f->fa = fa;

		/* make sloth for triangle */
		makeSlothForTri(f,&fa->s00,&fa->dsdx,&fa->dsdz);

		/* set density and Q for triangle */
		fa->dens = FLT_MAX;
		fa->qfac = FLT_MAX;

		/* next face */
		f = f->fNext;

	} while (f!=m->f);

	/* return model */
	return m;
}

/* build model corresponding to uniformly sampled sloth */
static Model *makeMod3 (float errmax, 
	int n2, float d2, float f2, int n1, float d1, float f1, float **s,
	float smax, char *efile, int mm, char *mfile, int tol, int verbose)
{
	Model *m;
	Face *f;
	FaceAttributes *fa;
	Vertex *v;
	VertexAttributes *va;
	int *ix,*iz,*ind2,jf,kf,nf,dix,diz,im,iv,nfalloc,skip,itemp,nv;
	float xmin,xmax,zmin,zmax,
		emax,*e,errfrac;

	int *used;

	char *ifile="";
	FILE *efp=NULL,*ifp;

	/* min and max x and z coordinates */
	xmin = f2;
	zmin = f1;
	xmax = f2+(n2-1)*d2;
	zmax = f1+(n1-1)*d1;

	/* read initial model for interfaces */
	if (getparstring("ifile",&ifile)) {
		ifp = efopen(ifile,"r");
		m = readModel(ifp);
		efclose(ifp);

		/* check that model is compatible with sampled sloth */
		if (ABS(xmin-m->ymin)>0.01*d2)
			err("xmin!=m->ymin %g!=%g",xmin,m->ymin);
		if (ABS(xmax-m->ymax)>0.01*d2)
			err("xmax!=m->ymax %g!=%g",xmax,m->ymax);
		if (ABS(zmin-m->xmin)>0.01*d1)
			err("zmin!=m->xmin %g!=%g",zmin,m->xmin);
		if (ABS(zmax-m->xmax)>0.01*d1)
			err("zmax!=m->xmax %g!=%g",zmax,m->xmax);
/*		if (xmin!=m->ymin) err("xmin!=m->ymin %g!=%g",xmin,m->ymin);
		if (xmax!=m->ymax) err("xmax!=m->ymax %g!=%g",xmax,m->ymax);
		if (zmin!=m->xmin) err("zmin!=m->xmin %g!=%g",zmin,m->xmin);
		if (zmax!=m->xmax) err("zmax!=m->xmax %g!=%g",zmax,m->xmax);
*/

		/* reset epsilon */
/*		m->eps = 0.01*MIN(d2,d1);
*//*		m->eps *= 0.001;
*/
		/* loop over faces */
		f = m->f;
		do {
			/* free face attributes */
			free1(f->fa);  f->fa = NULL;

			/* set sloth at each vertex of face */
			setSlothOfVertex(f->eu->vu->v,
				n2,d2,f2,n1,d1,f1,s);
			setSlothOfVertex(f->eu->euCW->vu->v,
				n2,d2,f2,n1,d1,f1,s);
			setSlothOfVertex(f->eu->euCCW->vu->v,
				n2,d2,f2,n1,d1,f1,s);

			/* next face */
			f = f->fNext;
		} while (f!=m->f);

	} else {
		/* initialize model */
		m = makeModel(zmin,xmin,zmax,xmax);
		m->sfa = sizeof(FaceAttributes);
		m->eps = 0.01*MIN(d2,d1);
		v = nearestVertexInModel(m,NULL,zmin,xmin);
		va = (VertexAttributes*) ealloc1(1,sizeof(VertexAttributes));
		va->s = s[0][0];
		v->va = va;
		v = nearestVertexInModel(m,NULL,zmax,xmin);
		va = (VertexAttributes*) ealloc1(1,sizeof(VertexAttributes));
		va->s = s[0][n1-1];
		v->va = va;
		v = nearestVertexInModel(m,NULL,zmin,xmax);
		va = (VertexAttributes*) ealloc1(1,sizeof(VertexAttributes));
		va->s = s[n2-1][0];
		v->va = va;
		v = nearestVertexInModel(m,NULL,zmax,xmax);
		va = (VertexAttributes*) ealloc1(1,sizeof(VertexAttributes));
		va->s = s[n2-1][n1-1];
		v->va = va;
	}

        checkpars();

	/* check model */
/*	fprintf(stderr,"Checking model\n");
	checkModel(m);
*/
	if (verbose==2)
		if ((efp=fopen(efile,"w"))==NULL)
			err("Could not open efile=%s!\n",efile);

	/* label output columns */
	if (verbose) {
		fprintf(stderr,"iterations faces  "
			"max errror    norm error\n");
		fprintf(stderr,"--------   -----  "
			"----------    ----------\n");
	}

	/* allocate space */
	nfalloc = 1024;
	e = ealloc1float(nfalloc);
	ix = ealloc1int(nfalloc);
	iz = ealloc1int(nfalloc);
	ind2 = ealloc1int(nfalloc);
	used = ealloc1int(nfalloc);

	/* count iterations */
	im = 0; nv = 0;

	/* loop until error is small enough */
	do {
		/* initialize maximum error */
		emax = 0.0;

		/* loop over faces */
		f = m->f;  jf = 0;
		do {
			/* determine maximum error in triangle */
			maxErrorInTri(f,n2,d2,f2,n1,d1,f1,s,
				&e[jf],&ix[jf],&iz[jf]);

			/* save maximum error */
			if (e[jf]>=emax) emax = e[jf];

			/* next face */
			f = f->fNext;  ++jf;

		} while (f!=m->f);

		/* number of faces */
		nf = jf;

		/* compute and report maximum error */
		errfrac = emax/smax;
		if (verbose)
			fprintf(stderr," %5d     %5d   % .6g     % .6g\n",
				im,nf,emax,errfrac);
		if (verbose==2) {
			fwrite(&errfrac,sizeof(float),1,efp);
			if (im%50==0) fflush(efp);
		}

		/* if requested, write intermediate model */
		if ((mm>0) ? im%mm==0 : 0)
			writeIntermediateModel(m,im,mfile);

		/* sort errors from largest to smallest */
		for (jf=0; jf<nf; ++jf) {
			ind2[jf] = jf;
			used[jf] = 0;
		}
		qkisort(nf,e,ind2);
		for (jf=0,kf=nf-1; jf<nf/2; ++jf,--kf) {
			itemp = ind2[jf];
			ind2[jf] = ind2[kf];
			ind2[kf] = itemp;
		}

		/* loop over points of maximum error */
		for (jf=0,iv=0; jf<nf; ++jf) {

			/* if error is too big, add another vertex */
			if (e[ind2[jf]]>errmax) {

				/* check that this vertex is not too close */
				/* to others previously added at this stage */
				for (kf=0,skip=0; kf<jf; ++kf) {
/*					if (!used[kf]) continue;
*/					dix = ix[ind2[kf]] - ix[ind2[jf]];
					diz = iz[ind2[kf]] - iz[ind2[jf]];
					if (ABS(dix)<=tol && ABS(diz)<=tol) {
						skip = 1;
						break;
					}
				}
				if (skip) continue;

				/* do not add if vertex is on a fixed edge */
/*				z = f1+iz[ind2[jf]]*d1;
				x = f2+ix[ind2[jf]]*d2;
				ne = nearestEdgeInModel(m,NULL,z,x);
				if (distanceToEdge(ne,z,x)<m->eps && ne->fixed)
							continue;
*/
				/* add vertex */
				v = addVertexToModel(m,
					f1+iz[ind2[jf]]*d1,
					f2+ix[ind2[jf]]*d2);
				if (v==NULL) continue;

				/* set vertex attributes */
				va = (VertexAttributes*)
					ealloc1(1,sizeof(VertexAttributes));
				va->s = s[ix[ind2[jf]]][iz[ind2[jf]]];
				v->va = va;

				/* mark vertex as used, increment counter */
				used[jf] = 1;
				++iv; ++nv;
			}
		}

		/* Make sure at least one vertex has been added. */
		/* This should never be a problem. */
		if (iv==0 && emax>errmax) err("added 0 vertices!");

		/* Just added iv vertices at this stage, */
		/* increasing face count by at most 2*iv. */
		/* Allocate more space for vertices if necessary */
		if (nf+2*iv>nfalloc) {
			nfalloc *= 2;
			free1float(e);  e = ealloc1float(nfalloc);
			free1int(ix);  ix = ealloc1int(nfalloc);
			free1int(iz);  iz = ealloc1int(nfalloc);
			free1int(ind2);  ind2 = ealloc1int(nfalloc);
			free1int(used);  used = ealloc1int(nfalloc);
		}

		/* increment iteration counter */
		++im;

	} while (emax>errmax);

	if (verbose==2) fclose(efp);
	if (verbose) fprintf(stderr,"added %d vertices\b",nv);

	/* final loop over faces */
	f = m->f;
	do {
		/* allocate face attributes */
		fa = (FaceAttributes*)ealloc1(1,sizeof(FaceAttributes));
		f->fa = fa;

		/* make sloth for triangle */
		makeSlothForTri(f,&fa->s00,&fa->dsdx,&fa->dsdz);

		/* set density and Q for triangle */
		fa->dens = FLT_MAX;
		fa->qfac = FLT_MAX;

		/* next face */
		f = f->fNext;

	} while (f!=m->f);

#ifdef COMMENT
	/* verify that density and Q are set in all faces */
	f = m->f;
	do {
		fa = f->fa;

		fprintf(stderr,"fa->dens=%.25g   fa->qfac=%.25g\n",
			fa->dens,fa->qfac);

		if (fa->dens!=FLT_MAX)
			err("density not set in face!\n");
		if (fa->qfac!=FLT_MAX)
			err("Q not set in face!\n");

		f = f->fNext;

	} while (f!=m->f);
#endif

	/* return model */
	return m;
}

/* make sloth function for triangle */
static void makeSlothForTri (Tri *t, float *s00, float *dsdx, float *dsdz)
/*
* Author:  Dave Hale
* Modified:  Craig Artley
*	Fixed bug when computing s00 for non1ero m->xmin and m->ymin
*/
{
	VertexAttributes *va;
	float x1,z1,x2,z2,x3,z3,s1,s2,s3,
		x2mx1,z2mz1,s2ms1,x3mx1,z3mz1,s3ms1,
		a,b,det;

	x1 = t->eu->vu->v->y;
	z1 = t->eu->vu->v->x;
	va = t->eu->vu->v->va;
	s1 = va->s;
	x2 = t->eu->euCW->vu->v->y;
	z2 = t->eu->euCW->vu->v->x;
	va = t->eu->euCW->vu->v->va;
	s2 = va->s;
	x3 = t->eu->euCCW->vu->v->y;
	z3 = t->eu->euCCW->vu->v->x;
	va = t->eu->euCCW->vu->v->va;
	s3 = va->s;
	x2mx1 = x2-x1;  z2mz1 = z2-z1;  s2ms1 = s2-s1;
	x3mx1 = x3-x1;  z3mz1 = z3-z1;  s3ms1 = s3-s1;
	det = z3mz1*x2mx1-z2mz1*x3mx1;
	a = z3mz1*s2ms1-z2mz1*s3ms1;
	b = s3ms1*x2mx1-s2ms1*x3mx1;
	*dsdx = a/det;
	*dsdz = b/det;
	*s00 = s2-(*dsdx)*x2-(*dsdz)*z2;
}

#define SWAPVERTEX(I,J) { \
	float temp=x##I; x##I=x##J; x##J=temp; \
	temp=z##I; z##I=z##J; z##J=temp; \
}

/* determine sample with maximum error in triangle */
static void maxErrorInTri (Tri *t,
	int n2, float d2, float f2, int n1, float d1, float f1, float **s,
	float *emax, int *ixmax, int *izmax)
/*
* Author:  Dave Hale
* Modified:  Craig Artley
*	Fixed bugs arising when handling triangles with vertical edges.
*/
{
	int ixlo=n1,ixhi=n2,izlo,izhi,ix,iz; /* dummy usage */
	float s00,dsdx,dsdz,x1,z1,x2,z2,x3,z3,slope21,slope31,slope32,
		zlo,zhi,d1lo,d1hi,x,z,e;

	/* sloth function for tri */
	makeSlothForTri(t,&s00,&dsdx,&dsdz);

	/* vertices */
	x1 = t->eu->vu->v->y;
	z1 = t->eu->vu->v->x;
	x2 = t->eu->euCW->vu->v->y;
	z2 = t->eu->euCW->vu->v->x;
	x3 = t->eu->euCCW->vu->v->y;
	z3 = t->eu->euCCW->vu->v->x;

	/* sort vertices by increasing x */
	if (x1>x2) SWAPVERTEX(1,2);
	if (x1>x3) SWAPVERTEX(1,3);
	if (x2>x3) SWAPVERTEX(2,3);

	/* slopes of edges */		
	slope21 = (z2-z1)/(x2!=x1?x2-x1:1e-10*d2);
	slope31 = (z3-z1)/(x3!=x1?x3-x1:1e-10*d2);
	slope32 = (z3-z2)/(x3!=x2?x3-x2:1e-10*d2);

	/* x limits and initial x */
	ixlo = (x1-f2)/d2;
	ixhi = (x3-f2)/d2;
	if (f2+ixlo*d2<x1) ixlo++;

	/* initial z limits */
	zlo = z1+(f2+ixlo*d2-x1)*MIN(slope21,slope31);
	zhi = z1+(f2+ixlo*d2-x1)*MAX(slope21,slope31);
	d1lo = d2*MIN(slope21,slope31);
	d1hi = d2*MAX(slope21,slope31);

	/* adjust z limits if the 1-2 edge is vertical */
	if (x1==x2 && z1<z2) {
		zhi = z2+(f2+ixlo*d2-x2)*slope32;
		d1hi = d2*slope32;
	}
	if (x1==x2 && z1>z2) {
		zlo = z2+(f2+ixlo*d2-x2)*slope32;
		d1lo = d2*slope32;
	}

	/* initial error */
	*emax = 0.0;

	/* loop over sampled x coordinates */
	for (ix=ixlo; ix<=ixhi; ++ix) {

		/* sampled x coordinate */
		x = f2+ix*d2;

		/* if right of vertex 2, update z limits */
		if (x>x2) {
			if (slope21<slope31) {
				zlo = z2+(x-x2)*slope32;
				d1lo = d2*slope32;
			} else {
				zhi = z2+(x-x2)*slope32;
				d1hi = d2*slope32;
			}
		}

		/* z sample limits */
		izlo = (zlo-f1)/d1;
		if (f1+izlo*d1<zlo) izlo++;
		izhi = (zhi-f1)/d1;

		/* loop over samples within limits */
		for (iz=izlo; iz<=izhi; ++iz) {

			/* sampled z coordinate */
			z = f1+iz*d1;

			/* update error */
			e = s00+x*dsdx+z*dsdz-s[ix][iz];
			e = ABS(e);
			if (e>=*emax) {
				*emax = e;
				*ixmax = ix;
				*izmax = iz;
			}
		}

		/* increment z limits */
		zlo += d1lo;
		zhi += d1hi;
	}
}

static void writeIntermediateModel (Model *m, int im, char *mfile)
/*
* Author:  Craig Artley
*/
{
	Face *f;
	FaceAttributes *fa;
	FILE *mfp;
	static char *mexfile=NULL;

	/* loop over faces */
	f = m->f;
	do {
		/* make sloth for triangle */
		fa = (FaceAttributes*)ealloc1(1,sizeof(FaceAttributes));
		f->fa = fa;
		makeSlothForTri(f,&fa->s00,&fa->dsdx,&fa->dsdz);

		/* next face */
		f = f->fNext;

	} while (f!=m->f);

	/* allocate space for filename plus model number extension, */
	/* allowing space for null terminator and possible overflow */
	if (mexfile==NULL)
		mexfile = ealloc1(strlen(mfile)+10,sizeof(char));
	sprintf(mexfile,"%s%.4d",mfile,im);

	/* open file, write model, close file */
	mfp = efopen(mexfile,"w");
	writeModel(m,mfp);
	efclose(mfp);

	/* loop over faces */
	f = m->f;
	do {
		/* free face atributes */
		free1(f->fa);  f->fa = NULL;

		/* next face */
		f = f->fNext;

	} while (f!=m->f);
}

static void setSlothOfVertex (Vertex *v,
	int n2, float d2, float f2, int n1, float d1, float f1, float **s)
/*
* Author:  Craig Artley
*/
{
	int ix,iz;
	float x,z,xi,zi,fracx,fracz;
	VertexAttributes *va;

	/* find grid coordinates closest to vertex */
	x = v->y;		z = v->x;
	xi = (x-f2)/d2;		zi = (z-f1)/d1;
	ix = xi;		iz = zi;
	fracx = xi-ix;		fracz = zi-iz;

	/* adjust coordinates at edges of grid */
	if (ix<0) {
		ix = 0;
		fracx = 0.0;
	}
	if (ix>n2-2) {
		ix = n2-2;
		fracx = 1.0;
	}

	if (iz<0) {
		iz = 0;
		fracz = 0.0;
	}
	if (iz>n1-2) {
		iz = n1-2;
		fracz = 1.0;
	}

	/* allocate space for vertex attributes if necessary */
	if (v->va==NULL)
		v->va = va = (VertexAttributes*)
			ealloc1(1,sizeof(VertexAttributes));
	else
		va = v->va;

	/* compute sloth via bilinear interpolation */
	va->s = (1.0-fracx)*((1.0-fracz)*s[ix][iz]+fracz*s[ix][iz+1])
		+fracx*((1.0-fracz)*s[ix+1][iz]+fracz*s[ix+1][iz+1]);
}

static void fillsloth (Model *m, int nr, float **sr)
/* fill regions bounded by fixed edges with sloths */
{
	int ir;
	float x,z,x0,z0,s00,dsdx,dsdz;
	Tri *t;

	/* loop over regions for which sloth function is specified */
	for (ir=0; ir<nr; ++ir) {

		/* determine parameters of sloth function */
		x = sr[ir][0];  z = sr[ir][1];
		x0 = sr[ir][2];  z0 = sr[ir][3];
		s00 = sr[ir][4];  dsdx = sr[ir][5];  dsdz = sr[ir][6];

		/* adjust v0 for x0 and z0 */
		s00 -= x0*dsdx+z0*dsdz;

		/* determine triangle containing point (x,z) */
		t = insideTriInModel(m,NULL,z,x);

		/* flood triangles in region */
		setsloth(t,s00,dsdx,dsdz);
	}
}

static void setsloth (Tri *t, float s00, float dsdx, float dsdz)
/* recursively set sloth functions in triangles */
{
	EdgeUse *eu;
	FaceAttributes *fa;

	/* if sloth already set, then return */
	if ((fa=t->fa)!=NULL)
		if (fa->s00==s00 && fa->dsdx==dsdx && fa->dsdz==dsdz)
			return;

	/* if necessary, allocate space for attributes */
	if (fa==NULL)
		t->fa = fa = (FaceAttributes*)
			ealloc1(1,sizeof(FaceAttributes));

	/* set attributes */
	fa->s00 = s00;
	fa->dsdx = dsdx;
	fa->dsdz = dsdz;

	/* for each edge not fixed, set attributes in opposite triangle */
	eu = t->eu;
	do {
		if (!eu->e->fixed) setsloth(eu->euMate->f,s00,dsdx,dsdz);
		eu = eu->euCW;
	} while (eu!=t->eu);
}
