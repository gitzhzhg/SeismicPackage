/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* TRIRAY: $Test Release: 1.1 $ ; $Date: 2011/11/21 16:56:25 $	*/

#include "par.h"
#include "Triangles/tri.h"
#include "Triangles/sloth.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"										",
" TRIRAY - dynamic RAY tracing for a TRIangulated sloth model		",
"										",
"  triray <modelfile >rayends [optional parameters]			",
"									",
" Optional Parameters:							",
" xs=(max-min)/2 x coordinate of source (default is halfway across model)",
" zs=min	 z coordinate of source (default is at top of model)	",
" nangle=101	number of takeoff angles				",
" fangle=-45	first takeoff angle (in degrees)			",
" langle=45	last takeoff angle (in degrees)				",
" rayfile=	file of ray x,z coordinates of ray-edge intersections	",
" nxz=101	number of (x,z) in optional rayfile (see notes below)	",
" wavefile=	file of ray x,z coordinates uniformly sampled in time	",
" nt=101	number of (x,z) in optional wavefile (see notes below)	",
" infofile=	ASCII-file to store useful information 		",
" fresnelfile=  used if you want to plot the fresnel volumes.		",
"		default is <fresnelfile.bin> 				",
" outparfile=	contains parameters for the plotting software.		",
"		default is <outpar> 					",
" krecord=	if specified, only rays incident at interface with index",
"		krecord are displayed and stored			",
" prim=	   =1, only single-reflected rays are plotted			",
"		=0, only direct hits are displayed  			",
" ffreq=-1	FresnelVolume frequency 				",
" refseq=1,0,0  index of reflector followed by sequence of reflection (1)",
"		transmission(0) or ray stops(-1).			",
"		The default rayend is at the model boundary.		",
"		NOTE:refseq must be defined for each reflector		",
" NOTES:								",
" The rayends file contains ray parameters for the locations at which	",
" the rays terminate.  							",
"									",
" The rayfile is useful for making plots of ray paths.			",
" nxz should be larger than twice the number of triangles intersected	",
" by the rays.								",
"									",
" The wavefile is useful for making plots of wavefronts.		",
" The time sampling interval in the wavefile is tmax/(nt-1),		",
" where tmax is the maximum time for all rays.				",
"									",
" The infofile is useful for collecting information along the		",
" individual rays. The fresnelfile contains data used to plot 		",
" the Fresnel volumes. The outparfile stores information used 		",
" for the plotting software.						",
"									",
NULL};

/*
 * AUTHOR:  Dave Hale, Colorado School of Mines, 02/16/91
 * MODIFIED:  Andreas Rueger, Colorado School of Mines, 08/12/93
 *	Modifications include: functions writeFresnel, checkIfSourceIsOnEdge;
 *		options refseq=, krecord=, prim=, infofile=;
 *		computation of reflection/transmission losses, attenuation.
 */
/**************** end self doc ***********************************/


#define TAPER 0.9

typedef struct RSStruct {
	float sigma,x,z,px,pz,t;
	float q1,p1,q2,p2;
	int kmah,nref;
	EdgeUse *eu;
	float atten;
	float ampli,ampliphase;
	Face *f;
	struct RSStruct *rsNext;
} RayStep;

typedef struct RCStruct {
	int k;
	int nhits;
	int *hitseq;
} RefCheck;

/* prototypes for functions defined and used internally */
void shootRays (Model *m, float xs, float zs,
	int nangle, float dangle, float fangle,
	RefCheck *rc, int kk,
	RayStep *rsp[], RayEnd re[], FILE *infofp);
void writeRays (Model *m,FILE *rayfp, int nxz, int nray, RayStep *rs[], RayEnd  
	re[], int krecord, int prim, FILE *infofp, FILE *outparfp);
void writeFresnel (Model *m,FILE *rayfp,FILE *infofp, int nxz, int nray,
	RayStep *rs[], RayEnd re[], int krecord, float freq, int prim,
	FILE *fresnelfp, FILE *outparfp);
void writeWaves (FILE *wavefp, int nt, int nray, RayStep *rs[]);
static RayStep* traceRayInTri (RayStep *rs);
static RayStep* traceRayAcrossEdge (RayStep *rs,FILE *infofp);
static RayStep* reflectRayFromEdge (RayStep *rs,FILE *infofp);
static void evaluateDynamic (float sigma, float px, float pz, float dsdx,
	float dsdz, float *q1, float *p1, float *q2, float *p2, int *kmah);
static void evaluateDynamic2 (float sigma, float px, float pz, float dsdx,
	float dsdz, float *q1, float *p1, float *q2,float *p2,
	float s0, float s1, float s2); 
int checkIfSourceOnEdge(Face *tris, float zs, float xs);

/* the main program */
int main (int argc, char **argv)
{
	int i,ii,nangle,nxz,nt,kk,nseq,krecord,prim;
	int **hitseqa=NULL,*nrefseq=NULL,ibuf=2;
	float xs,zs,fangle,langle,dangle,ffreq;
	char *rayfile,*wavefile,*infofile,
		*fresnelfile="fresnelfile.bin",*outparfile="outpar";
	Model *m;
	FILE *rayfp=NULL,*wavefp=NULL,*infofp=NULL;
	FILE *outparfp=NULL,*fresnelfp=NULL;

	RayStep **rsp;
	RayEnd *re;	
	RefCheck *rc;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);
	
	/* read model */
	m = readModel(stdin);

	/* get optional parameters */
	if (!getparfloat("xs",&xs)) xs = 0.5*(m->ymin+m->ymax);
	if (!getparfloat("zs",&zs)) zs = m->xmin;
	if (!getparint("nangle",&nangle)) nangle = 101;
	if (!getparfloat("fangle",&fangle)) fangle = -45.0;
	if (!getparfloat("langle",&langle)) langle = 45.0;
	if (!getparint("nxz",&nxz)) nxz = 101;
	if (!getparint("nt",&nt)) nt = 101;
	if (!getparint("krecord",&krecord)) krecord = INT_MAX;
	if (!getparint("prim",&prim)) prim = INT_MAX;
	if (!getparfloat("ffreq",&ffreq)) ffreq = -1;

	/* open files as necessary */
	if (getparstring("rayfile",&rayfile)) rayfp = efopen(rayfile,"w");
	if (getparstring("wavefile",&wavefile)) wavefp = efopen(wavefile,"w");
	if (getparstring("infofile",&infofile)) infofp = efopen(infofile,"w");
	getparstring("fresnelfile",&fresnelfile);
	if (ffreq>0) fresnelfp = efopen(fresnelfile,"w");
	getparstring("outparfile",&outparfile);
	outparfp = efopen(outparfile,"w");

	/* number of reflection sequences */
	kk = countparname("refseq");

	/* error checks */
	if(prim!=INT_MAX && krecord==INT_MAX)
		err("krecord= must be specified with prim=\n");
	if(prim!=INT_MAX && kk==0)
		err("must define target reflector(s) via refseq= \n");

	/* allocate space for raychecks */
	rc = (RefCheck*)ealloc1(kk,sizeof(RefCheck));

	/* allocate space for hitseq */
	if (kk>0) {
		hitseqa = (int**)ealloc1(kk,sizeof(int*));
		nrefseq = ealloc1int(kk);
	}

	/* determine reflection sequences */
	for (i=0; i<kk; ++i) {
		nseq = countnparval(i+1,"refseq");
		nrefseq[i] = nseq;
		hitseqa[i] = ealloc1int(nrefseq[i]+ibuf);
		getnparint(i+1,"refseq",hitseqa[i]);
		rc[i].k = hitseqa[i][0];
		rc[i].nhits = 0;
		rc[i].hitseq = hitseqa[i]+1;
	}

	/* print reflection sequence information if requested */
	if (infofp!=NULL) {
		fprintf(infofp,
			"THIS FILE CONTAINS RAY TRACING INFORMATION\n\n"
			"*********************************************\n"
			"defined reflection/transmission sequences:\n"
			"*********************************************\n");
		for (i=0; i<kk; ++i) {
			fprintf(infofp,"interface %d:",rc[i].k);
			for(ii=0; ii<nrefseq[i]-1; ++ii)
				fprintf(infofp," %d%s",
					rc[i].hitseq[ii],
					(ii<nrefseq[i]-2)?",":"");
			fprintf(infofp,"\n");
		}
		fprintf(infofp,"Interfaces without defined "
				"refseq are transmitting.\n");
	}
	
	/* convert angles to radians and determine angle increment */
	fangle *= PI/180.0;
	langle *= PI/180.0;
	dangle = (nangle>1) ? (langle-fangle)/(nangle-1) : 0.0;

	/* allocate space for ray step lists and ray ends */
	rsp = (RayStep**)ealloc1(nangle,sizeof(RayStep*));
	re = (RayEnd*)ealloc1(nangle,sizeof(RayEnd));

	/* shoot rays */
	shootRays(m,xs,zs,nangle,dangle,fangle,rc,kk,rsp,re,infofp);
	
	/* if Fresnel Volumes are requested */
	if (fresnelfp!=NULL && rayfp!=NULL) {

		/* write Fresnel Volumes and associated rays */
		writeFresnel(m,rayfp,infofp,nxz,nangle,rsp,re,krecord,
			ffreq,prim,fresnelfp,outparfp);

	} else if (rayfp!=NULL) {

		/* if requested, write rays to rayfile */
		writeRays(m,rayfp,nxz,nangle,rsp,re,krecord,prim,
			infofp,outparfp);
	}

	/* if requested, write waves to wavefile */
	if (wavefp!=NULL) writeWaves(wavefp,nt,nangle,rsp);

	/* write ray ends */
	if (efwrite(re,sizeof(RayEnd),nangle,stdout)!=nangle)
		err("Error writing ray ends to stdout!\n");

	return EXIT_SUCCESS;
}

void shootRays (Model *m, float xs, float zs,
	int nangle, float dangle, float fangle,
	RefCheck *rc, int kk,
	RayStep *rsp[], RayEnd re[], FILE *infofp)
/*****************************************************************************
Shoot rays via dynamic ray tracing for a sloth model
******************************************************************************
Input:
m		trianglulated sloth model
xs		horizontal coordinate of source
zs		vertical coordinate (depth) of source
nangle		number of ray takeoff angles
dangle		increment in ray takeoff angle
fangle		first ray takeoff angle
rc		pointer to reflector structure
kk		number of reflectors

Output:
rsp		array[nangle] of pointers to linked list of RaySteps
re		array[nangle] of RayEnds
******************************************************************************
Notes:
Rays are traced from triangle to triangle.
Whenever a ray crosses a triangle edge, two RaySteps are added
to the linked list of RaySteps, one for each side of the edge.
The RaySteps are useful if ray parameters along the entire raypath
are required.  The RayEnds are useful if parameters at the ray
endpoint are required.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 02/28/91
Modified: Andreas Rueger, Colorado School of Mines, 08/12/93
******************************************************************************/
{
	int iangle,refl,stop,i,temp,test;
	float angle,sb,zmax,xmax,eps;
	Tri *tris;
	FaceAttributes *fa;
	EdgeAttributes *ea;
	RayStep *rs,*rslast;

	/* determine triangle in model containing source location */
	tris = insideTriInModel(m,NULL,zs,xs);
	
	/* if source sits on edge, move it by eps */
	if((test=checkIfSourceOnEdge(tris,zs,xs) != 0)) {
		zmax = m->xmax;
		eps = m->eps;
		if (test==2) eps = 5*eps;
		zs = (zs<zmax-eps) ? zs+eps : zs-eps;

		/* if we are still on an edge, move x value */
		if(checkIfSourceOnEdge(tris,zs,xs) != 0) {
			xmax = m->ymax;
			xs = (xs<xmax-eps) ? xs+eps : xs-eps;
		}

		/* find new triangle */
		tris = insideTriInModel(m,NULL,zs,xs);
	}

	/* determine sloth at source location (beginning of ray) */
	fa = tris->fa;
	sb = fa->s00+fa->dsdx*xs+fa->dsdz*zs;

	/* loop over takeoff angles */
	for (iangle=0,angle=fangle; iangle<nangle; ++iangle,angle+=dangle) {

		/* initialize linked list of ray steps */
		rs = rsp[iangle] = (RayStep*)ealloc1(1,sizeof(RayStep));
		rs->sigma = 0.0;
		rs->x = xs;
		rs->z = zs;
		rs->px = sin(angle)*sqrt(sb);
		rs->pz = cos(angle)*sqrt(sb);
		rs->q1 = 1.0;
		rs->p1 = 0.0;
		rs->q2 = 0.0;
		rs->p2 = 1.0;
		rs->t = 0.0;
		rs->kmah = 0;
		rs->nref = 0;
		rs->eu = NULL;
		rs->f = tris;
		rs->ampli = 1;
		rs->ampliphase = 0;
		rs->atten = 0;
		rs->rsNext = NULL;

		if (infofp!=NULL)
			fprintf(infofp,"\n"
				"****************************************\n"
				"RAY WITH TAKEOFF ANGLE: %g (degrees)\n"
				"****************************************\n",
				angle*180.0/PI);

		/* trace ray */
		do {
			/* trace ray in triangle */
			rs = traceRayInTri(rs);

			/* remember last ray step */
			rslast = rs;

			/* edge attributes */
			ea = rs->eu->e->ea;

			if (ea==NULL) {

				/* null edges are transmitting */
				refl = stop = temp = 0;

			} else {

				/* check if edge is reflecting or stopping */
				for (i=0,temp=ea->k,refl=stop=0; i<kk; ++i) {
					if (temp==rc[i].k) {
						if (rc[i].hitseq[0]==1)
							refl = 1;
						if (rc[i].hitseq[0]==-1)
							stop = 1;
						++(rc[i].nhits);
						++(rc[i].hitseq);
						break;
					}
				}
			}

			if (infofp!=NULL)
				fprintf(infofp,"^^^\n"
					"Interaction with interface %d at "
					"(x=%g,z=%g).\n",
					temp,rslast->x,rslast->z);

			/* if ray at stopping edge, stop */
			if (stop) {
				if (infofp!=NULL)
					fprintf(infofp,"Hit stopping edge.  "
							"Ray stopped.\n");
				break;
			}

			/* else if ray at reflecting edge, reflect */
			else if (refl)
				rs = reflectRayFromEdge(rs,infofp);

			/* else trace ray across edge */
			else
				rs = traceRayAcrossEdge(rs,infofp);
			
		} while (rs!=NULL);
		
		/* reinitialize RayChecks for new ray */
		for (i=0; i<kk; ++i) {
			rc[i].hitseq = rc[i].hitseq-rc[i].nhits;
			rc[i].nhits = 0;
		}

		/* save ray end parameters */
		fa = rslast->f->fa;
		re[iangle].sigma = rslast->sigma;
		re[iangle].x = rslast->x;
		re[iangle].z = rslast->z;
		re[iangle].px = rslast->px;
		re[iangle].pz = rslast->pz;
		re[iangle].t = rslast->t;
		re[iangle].q1 = rslast->q1;
		re[iangle].p1 = rslast->p1;
		re[iangle].q2 = rslast->q2;
		re[iangle].p2 = rslast->p2;
		re[iangle].kmah = rslast->kmah;
		re[iangle].nref = rslast->nref;
		re[iangle].sb = sb;
		re[iangle].se = fa->s00+fa->dsdx*rslast->x+fa->dsdz*rslast->z;
		re[iangle].dsdxe = fa->dsdx;
		re[iangle].dsdze = fa->dsdz;
		re[iangle].ab = angle;
		re[iangle].kend = temp;
		re[iangle].ampli = rslast->ampli;
		re[iangle].ampliphase = rslast->ampliphase;
		re[iangle].atten = rslast->atten;
		re[iangle].dangle = dangle;

		/* optional output of rayend information */
		if (infofp!=NULL) {
			fprintf(infofp,
				"---------------------------------------"
				"------------\n\n"
				"The following information is stored at "
				"the rayend:\n\n");
			fprintf(infofp,
				"Ray stops at (%g,%g) at interface %d\n",
				re[iangle].x,re[iangle].z,re[iangle].kend);
			fprintf(infofp,
				"Takeoff angle=%g   Emergence angle=%g\n",
				re[iangle].ab*180./PI,-asin(re[iangle].px
				*1./sqrt(re[iangle].se))*180./PI);
			fprintf(infofp,"Takeoff velocity=%g   "
				"Emergence velocity=%g\n",
				1./sqrt(sb),1./sqrt(re[iangle].se));
			fprintf(infofp,"Slowness components px=%g   pz=%g\n",
				re[iangle].px,re[iangle].pz);
			fprintf(infofp,"Traveltime=%g   Sigma=%g\n",
				re[iangle].t,re[iangle].sigma);
			fprintf(infofp,"kmah index=%d   "
				"Number of reflections=%d\n",
				re[iangle].kmah,re[iangle].nref);
			fprintf(infofp,"Ray propagator q1=%g   q2=%g\n",
				re[iangle].q1,re[iangle].q2);
			fprintf(infofp,"Ray propagator p1=%g   p2=%g\n",
				re[iangle].p1,re[iangle].p2);
			fprintf(infofp,"Attenuation factor=%g   "
				"Angle increment=%g\n",
				re[iangle].atten,dangle*180./PI);
			fprintf(infofp,"Refl/Transm Amplitude=%g   Phase=%g\n",
				re[iangle].ampli,re[iangle].ampliphase); 
			fprintf(infofp,
				"---------------------------------------"
				"------------\n\n");
		}
	}
}

void writeRays (Model *m, FILE *rayfp, int nxz, int nray, RayStep *rs[],
	RayEnd re[], int krecord, int prim, FILE *infofp, FILE *outparfp)
/* for each ray, write x,z pairs */
{
	int iray,nrs,irs,nxzw,ns,is,icount;
	float sigmamax=0.,sigma1,sigma2,x,z,px,pz,dsdx,dsdz,
		ds,xs,zs,dsigma;
	RayStep *rsi;
	FaceAttributes *fa;
	
	m += 0; /* dummy */

	/* initialize counter */
	icount = 0;

	/* loop over rays */
	for (iray=0; iray<nray; ++iray) {

		if (krecord==INT_MAX || (krecord==re[iray].kend
			&& (prim==INT_MAX || prim==re[iray].nref))) {

			/* count rays */
			++icount;

			/* count ray steps while determining maximum sigma */
			for (nrs=0,rsi=rs[iray]; rsi!=NULL;
				++nrs,rsi=rsi->rsNext)
				sigmamax = rsi->sigma;

			/* initialize number of (x,z) written for this ray */
			nxzw = 0;

			/* loop over ray steps */
			for (irs=0,rsi=rs[iray]; irs<nrs;
				++irs,rsi=rsi->rsNext) {

				/* if sufficient number of (x,z) */
				/* have been written */
				if (nxzw>=nxz) break;

				/* ray parameters at this step */
				x = rsi->x;
				z = rsi->z;
				px = rsi->px;
				pz = rsi->pz;
				fa = rsi->f->fa;
				dsdx = fa->dsdx;
				dsdz = fa->dsdz;

				/* sigma at this step and next step */
				sigma1 = rsi->sigma;
				sigma2 = (irs<nrs-1)?rsi->rsNext->sigma:sigma1;

				/* number of sigma between this */
				/* step and next */
				ns = 1+(sigma2-sigma1)*(nxz-1)/(2.0*sigmamax);

				/* increment in sigma */
				ds = (sigma2-sigma1)/ns;

				/* loop over sigma */
				for (is=0,dsigma=0.0; is<ns; ++is,dsigma+=ds) {

					/* compute and write x and z */
					xs = x+dsigma*(px+dsigma*0.25*dsdx);
					zs = z+dsigma*(pz+dsigma*0.25*dsdz);
					if (efwrite(&zs,sizeof(float),1,rayfp)
						!=1)
						err("error writing rays!\n");
					if (efwrite(&xs,sizeof(float),1,rayfp)
						!=1)
						err("error writing rays!\n");


					/* if sufficient number written, */
					/* break */
					if (++nxzw>=nxz) break;
				}
			}

			/* if necessary, repeat last (x,z) */
			while (nxzw<nxz) {
				if (efwrite(&zs,sizeof(float),1,rayfp)!=1)
					err("error writing rays!\n");
				if (efwrite(&xs,sizeof(float),1,rayfp)!=1)
					err("error writing rays!\n");
				++nxzw;
			}

		} else {

			/* manipulate rayend information */
			re[iray].kend = -1;

		}

	} /* end loop over rays */

	if (infofp!=NULL)
		fprintf(infofp,"\nWrote %d rays to ray file.\n",icount);

	/* write number of rays to parameter file */
	fprintf(outparfp,"%d\n",icount);
}

void writeWaves (FILE *wavefp, int nt, int nray, RayStep *rs[])
/* for each ray, write nt z(t),x(t) pairs uniformly sampled in time t */
{
	int iray,it;
	float tmax,dt,t1,t2,x1,z1,px1,pz1,sigma1,sigma2,s00,dsdx,dsdz,
		ax,bx,az,bz,at,bt,ct,ti,sigmah,sigmam,tm,
		dsigma,sigma,t,x,z,xstart,zstart;
	RayStep *rsi,*rs1,*rs2;
	FaceAttributes *fa;

	/* determine maximum time for all rays */
	for (iray=0,tmax=0.0; iray<nray; ++iray)
		for (rsi=rs[iray]; rsi!=NULL; rsi=rsi->rsNext)
			tmax = MAX(tmax,rsi->t);

	/* determine time sampling interval */
	dt = tmax/((nt>1)?nt-1:1);
	fprintf(stderr,"wavefront time increment = %g\n",dt);

	/* loop over rays */
	for (iray=0; iray<nray; ++iray) {

		/* initialize */
		rs1 = rs[iray];
		rs2 = rs1->rsNext;
		t1 = rs1->t;
		t2 = rs2->t;
		x1 = rs1->x;
		z1 = rs1->z;
		px1 = rs1->px;
		pz1 = rs1->pz;
		sigma1 = rs1->sigma;
		sigma2 = rs2->sigma;
		fa = rs1->f->fa;
		s00 = fa->s00;
		dsdx = fa->dsdx;
		dsdz = fa->dsdz;
		ax = px1;
		bx = 0.25*dsdx;
		az = pz1;
		bz = 0.25*dsdz;
		at = s00+dsdx*x1+dsdz*z1;
		bt = 0.5*(dsdx*px1+dsdz*pz1);
		ct = 0.0833333*(dsdx*dsdx+dsdz*dsdz);
		sigma = sigma1;
		t = t1;

		/* remember start x,z */
		xstart = x1;
		zstart = z1;

		/* loop over times */
		for (it=0,ti=0.0; it<nt; ++it,ti+=dt) {

			/* if necessary, go to next ray step */
			if (t2<ti) {
				do {
					rs1 = rs2;
					rs2 = rs1->rsNext;
					if (rs2==NULL) break;
				} while (rs2->t<ti);
				if (rs2==NULL) break;
				t1 = rs1->t;
				t2 = rs2->t;
				x1 = rs1->x;
				z1 = rs1->z;
				px1 = rs1->px;
				pz1 = rs1->pz;
				sigma1 = rs1->sigma;
				sigma2 = rs2->sigma;
				fa = rs1->f->fa;
				s00 = fa->s00;
				dsdx = fa->dsdx;
				dsdz = fa->dsdz;
				ax = px1;
				bx = 0.25*dsdx;
				az = pz1;
				bz = 0.25*dsdz;
				at = s00+dsdx*x1+dsdz*z1;
				bt = 0.5*(dsdx*px1+dsdz*pz1);
				ct = 0.0833333*(dsdx*dsdx+dsdz*dsdz);
				sigma = sigma1;
				t = t1;
			}

			/* determine dsigma for this time by bisection */
			sigmah = sigma2;
			while (t+0.01*dt<ti) {
				sigmam = 0.5*(sigma+sigmah);
				dsigma = sigmam-sigma1;
				tm = t1+dsigma*(at+dsigma*(bt+dsigma*ct));
				if (tm<ti) {
					t = tm;
					sigma = sigmam;
				} else {
					sigmah = sigmam;
				}
			}
			dsigma = sigma-sigma1;

			/* compute x and z */
			x = x1+dsigma*(ax+dsigma*bx);
			z = z1+dsigma*(az+dsigma*bz);

			/* write x and z */
			if (efwrite(&z,sizeof(float),1,wavefp)!=1)
				err("error writing waves!\n");
			if (efwrite(&x,sizeof(float),1,wavefp)!=1)
				err("error writing waves!\n");
		}

		/* finish writing x and z */
		while (it<nt) {
			if (efwrite(&zstart,sizeof(float),1,wavefp)!=1)
				err("error writing waves!\n");
			if (efwrite(&xstart,sizeof(float),1,wavefp)!=1)
				err("error writing waves!\n");
			++it;
		}
	}
}

static RayStep* traceRayInTri (RayStep *rs)
/* Trace ray in triangle.  Return pointer to new RayStep. */
{
	int kmah,nref;
	float x,z,px,pz,t,q1,p1,q2,p2,ddt,
		s00,dsdx,dsdz,xa,za,xb,zb,dx,dz,a,b,c,
		ds,q,sigma,dsigma,dsigma1,dsigma2,small,frac;
	float ampli,ampliphase,atten,qfac;
	EdgeUse *eu,*eut,*eusmall,*euzero=NULL;
	Face *f;
	FaceAttributes *fa;
	
	/* get input parameters */
	sigma =	rs->sigma;
	x = rs->x;
	z = rs->z;
	px = rs->px;
	pz = rs->pz;
	t = rs->t;
	q1 = rs->q1;
	p1 = rs->p1;
	q2 = rs->q2;
	p2 = rs->p2;
	kmah = rs->kmah;
	nref = rs->nref;
	eu = rs->eu;
	f = rs->f;
	ampli = rs->ampli;
	ampliphase = rs->ampliphase;
	atten = rs->atten;

	/* determine sloth function */
	fa = f->fa;
	s00 = fa->s00;
	dsdx = fa->dsdx;
	dsdz = fa->dsdz;

	/* determine Q-factor */
	qfac = fa->qfac;

	/* determine edge intersection with smallest positive dsigma */
	eusmall = NULL;
	small = FLT_MAX;
	eut = f->eu;
	do {
		/* edge endpoints */
		xa = eut->vu->v->y;  za = eut->vu->v->x;
		xb = eut->euCW->vu->v->y;  zb = eut->euCW->vu->v->x;

		/* coefficients b and c of equation to be solved for dsigma */
		dx = xb-xa;  dz = zb-za;
		b = dx*pz-dz*px;
		c = (eut==eu) ? 0.0 : dx*(z-za)-dz*(x-xa); 

		/* if sloth constant, solve linear equation */
		if (dsdx==0.0 && dsdz==0.0) {
			if (b!=0.0)
				dsigma1 = -c/b;
			else
				dsigma1 = FLT_MAX;
			dsigma2 = FLT_MAX;

		/* else, if sloth not constant, solve quadratic equation */
		} else {
			a = 0.25*(dx*dsdz-dz*dsdx);
			ds = b*b-4.0*a*c;
			if (ds<0.0) {
				dsigma1 = dsigma2 = FLT_MAX;
			} else {
				q = -0.5*((b<0.0)?b-sqrt(ds):b+sqrt(ds));
				if (a!=0.0)
					dsigma1 = q/a;
				else
					dsigma1 = FLT_MAX;
				if (q!=0.0)
					dsigma2 = c/q;
				else
					dsigma2 = FLT_MAX;
			}
		}
		
		/* remember edge with smallest positive dsigma */
		if (0.0<dsigma1 && dsigma1<small) {
			small = dsigma1;
			eusmall = eut;
		}
		if (0.0<dsigma2 && dsigma2<small) {
			small = dsigma2;
			eusmall = eut;
		}

		/* remember edge for which dsigma is zero */
		if (dsigma1==0.0) euzero = eut;
		if (dsigma2==0.0) euzero = eut;

		/* next edge use */
		eut = eut->euCW;

	} while (eut!=f->eu);

	/* ray exits at edge with smallest positive dsigma */
	if (eusmall!=NULL) {
		dsigma = small;
		eu = eusmall;

	/* but if no dsigma>0, choose the edge we are up against */
	} else {
		dsigma = 0.0;
		eu = euzero;
	}

	/* update dynamic ray parameters */
	evaluateDynamic(dsigma,px,pz,dsdx,dsdz,&q1,&p1,&q2,&p2,&kmah);	

	/* update ray parameters */
	sigma += dsigma;
	ddt = dsigma*(s00+dsdx*x+dsdz*z
		+dsigma*(0.5*(dsdx*px+dsdz*pz)
		+dsigma*(0.0833333*(dsdx*dsdx+dsdz*dsdz))));
	t += ddt;
	x += dsigma*(px+0.25*dsdx*dsigma);
	z += dsigma*(pz+0.25*dsdz*dsigma);
	px += 0.5*dsdx*dsigma;
	pz += 0.5*dsdz*dsigma;

	/* don't let ray exit too close to a vertex */
	xa = eu->vu->v->y;  dx = eu->euCW->vu->v->y-xa;
	za = eu->vu->v->x;  dz = eu->euCW->vu->v->x-za;
	frac = (ABS(dx)>ABS(dz))?(x-xa)/dx:(z-za)/dz;
	if (frac<0.0001) {
		x = xa+0.0001*dx;
		z = za+0.0001*dz;
	} else if (frac>0.9999) {
		x = xa+0.9999*dx;
		z = za+0.9999*dz;
	}

	/* compute contribution due to attenuation */
	if (qfac!=FLT_MAX)
		atten += ddt/qfac;
	else
		atten = 0;

	/* return new raystep */
	rs->rsNext = (RayStep*)ealloc1(1,sizeof(RayStep));
	rs = rs->rsNext;
	rs->sigma = sigma;
	rs->x = x;
	rs->z = z;
	rs->px = px;
	rs->pz = pz;
	rs->t = t;
	rs->q1 = q1;
	rs->p1 = p1;
	rs->q2 = q2;
	rs->p2 = p2;
	rs->kmah = kmah;
	rs->nref = nref;
	rs->eu = eu;
	rs->f = f;
	rs->rsNext = NULL;
	rs->atten = atten;
	rs->ampli = ampli;
	rs->ampliphase = ampliphase;

	return rs;
}

static RayStep* traceRayAcrossEdge (RayStep *rs, FILE *infofp)
/********************************************************************
 traceRayAcrossEdge - Trace ray across edge.
*********************************************************************
Input:
rs		Pointer to a ray step
infofp		info file pointer
Returns:
If successful, return pointer to new RayStep.
If unsuccessful, return NULL.

*********************************************************************
Notes:
 Failure to trace across an edge is because:
(1) the ray was incident with angle greater than the critical angle, or
(2) the ray is incident at a boundary edge
*********************************************************************
Author: Andreas Rueger, 1993
*********************************************************************/
{
	int kmah,nref;
	float sigma,x,z,px,pz,t,q1,p1,q2,p2,temp1,temp2,
		s00,s1,ds1dx,ds1dz,s2,ds2dx,ds2dz,
		px1,pz1,px1r,pz1r,px2r,pz2r,pz2rs,
		c1ov1,c2ov2,oc2s,g,cterm,iterm,
		fac1,fac2,dv1dl,dv2dl,dv1dm,dv2dm,
		scale,gx,gz,hx,h_z,frac,dx,dz,taper;
	float ampli,atten,dens1,dens2,ampliphase,coeff;
	EdgeUse *eu,*eum;
	EdgeUseAttributes *eua,*euma;
	Face *f;
	FaceAttributes *fa;

	/* get input parameters */
	sigma = rs->sigma;
	x = rs->x;
	z = rs->z;
	px = rs->px;
	pz = rs->pz;
	t = rs->t;
	q1 = rs->q1;
	p1 = rs->p1;
	q2 = rs->q2;
	p2 = rs->p2;
	kmah = rs->kmah;
	nref = rs->nref;
	eu = rs->eu;
	f = rs->f;
	atten = rs->atten;
	ampli = rs->ampli;
	ampliphase = rs->ampliphase;

	/* check for boundary */
	if (eu->euMate->f==NULL) {
		if (infofp!=NULL)
			fprintf(infofp,"Transmitted outside model.  "
					"Ray stopped.\n");
		return NULL;
	}

	/* determine sloth on this side of edge */
	fa = f->fa;
	s00 = fa->s00;
	ds1dx = fa->dsdx;
	ds1dz = fa->dsdz;
	s1 = s00+ds1dx*x+ds1dz*z;

	/* determine density on this side */
	dens1 = fa->dens;

	/* determine sloth on other side of edge */
	eum = eu->euMate;
	f = eum->f;
	fa = f->fa;
	s00 = fa->s00;
	ds2dx = fa->dsdx;
	ds2dz = fa->dsdz;
	s2 = s00+ds2dx*x+ds2dz*z;

	/* determine density on other side of the edge */
	dens2 = fa->dens;

	/* edge vector */
	dx = eum->vu->v->y-eu->vu->v->y;
	dz = eum->vu->v->x-eu->vu->v->x;

	/* fractional distance along edge */
	frac = (ABS(dx)>ABS(dz)) ? (x-eu->vu->v->y)/dx : (z-eu->vu->v->x)/dz;

	/* here is speeding up potential */

	/* linearly interpolate unit vector g tangent to edge */
	eua = eu->eua;
	euma = eum->eua;
	if (eua!=NULL && euma!=NULL) {
		gx = frac*euma->tx-(1.0-frac)*eua->tx;
		gz = frac*euma->tz-(1.0-frac)*eua->tz;
	} else {
		gx = -dx;
		gz = -dz;
	}
	scale = 1.0/sqrt(gx*gx+gz*gz);
	gx *= scale;
	gz *= scale;

	/* unit vector h normal to edge */
	hx = -gz;
	h_z = gx;

	/* remember ray parameters on this side */
	px1 = px;
	pz1 = pz;

	/* rotated ray parameters on this side */
	px1r = px*h_z-pz*hx;
	pz1r = px*hx+pz*h_z;

	/* rotated ray parameters on other side */
	px2r = px1r;
	pz2rs = s2-px2r*px2r;

	/* post-critical incidence*/
	if (pz2rs<=0.0) {
		if (infofp!=NULL)
			fprintf(infofp,"Post-critical transmission.  "
					"Ray stopped.\n");
		return NULL;
	}

	pz2r = sqrt(pz2rs);

	/* ray parameters on other side */
	px = px2r*h_z+pz2r*hx;
	pz = pz2r*h_z-px2r*hx;

	/* curvature term */
	c1ov1 = pz1r;
	c2ov2 = pz2r;
	oc2s = s2/pz2rs;
	if (eua!=NULL && euma!=NULL) {
		g = frac*euma->c-(1.0-frac)*eua->c;
		cterm = g*oc2s*(c1ov1-c2ov2);
	} else {
		cterm = 0.0;
	}

	/* update dynamic ray parameters */
	scale = (pz2r*sqrt(s1))/(pz1r*sqrt(s2));
	q1 = q1*scale;
	p1 = p1/scale+cterm*q1;
	q2 = q2*scale;
	p2 = p2/scale+cterm*q2;

	/* velocity derivatives tangent and normal to ray */
	fac1 = -0.5/(s1*s1);
	fac2 = -0.5/(s2*s2);
	dv1dl = fac1*(px1*ds1dx+pz1*ds1dz);
	dv2dl = fac2*(px*ds2dx+pz*ds2dz);
	dv1dm = fac1*(pz1*ds1dx-px1*ds1dz);
	dv2dm = fac2*(pz*ds2dx-px*ds2dz);

	/* inhomogeneity term */
	iterm = -px1r*oc2s
		*(2.0*(dv1dm*c1ov1-dv2dm*c2ov2)+px1r*(dv1dl-dv2dl));

	/* update dynamic ray parameters */
	p1 += iterm*q1;
	p2 += iterm*q2;

	/* transmission effects on amplitudes */
	if (ABS(s1-s2)>0.001 || dens1!=dens2) {
		if (infofp!=NULL)
			fprintf(infofp,"Transmitted WITH "
					"influence on amplitude.\n");
		if (dens1==FLT_MAX)
			dens1 = dens2 = 1.0;
		temp1 = dens2/dens1;
		temp2 = pz2r/pz1r;
		coeff = 1.0+(temp1-temp2)/(temp1+temp2);

		/* check if too close to critical */
		taper = (px1r*px1r)/s2;
		taper = (taper>TAPER) ? cos((taper-TAPER)*5.0*PI) : 1.0;

		/* complete amplitude coeff */
		ampli *= coeff*sqrt(pz2r/pz1r)*taper;

		if (infofp!=NULL) {
	   		fprintf(infofp,
				"  --incident side: v1=%g  dens1=%g  "
				"incidence angle=%g\n",
				1/sqrt(s1),dens1,acos(pz1r/sqrt(s1))*180/PI);
			fprintf(infofp,
				"  --opposite side: v2=%g  dens2=%g  "
				"refracted angle=%g\n",
				1/sqrt(s2),dens2,acos(pz2r/sqrt(s2))*180/PI);
			fprintf(infofp,
				"  --transmission coeff: %g\n",coeff);
			fprintf(infofp,
				"  --critical transm. taper: %g\n",taper);
			fprintf(infofp,
				"  --total ampli. coeff: %g\n",ampli);
		}

		/* grazing incidence*/
		if (pz1r*pz1r<=0.008*s1) {
			if (infofp!=NULL)
				fprintf(infofp,"Grazing incidence.  "
						"Ray stopped.\n");
			return NULL;
		}

	} else if (infofp!=NULL) {
		fprintf(infofp,"Transmitted WITHOUT influencing amplitude.\n");
	}

	/* return new raystep */
	rs->rsNext = (RayStep*)ealloc1(1,sizeof(RayStep));
	rs = rs->rsNext;
	rs->sigma = sigma;
	rs->x = x;
	rs->z = z;
	rs->px = px;
	rs->pz = pz;
	rs->t = t;
	rs->q1 = q1;
	rs->p1 = p1;
	rs->q2 = q2;
	rs->p2 = p2;
	rs->kmah = kmah;
	rs->nref = nref;
	rs->eu = eum;
	rs->f = f;
	rs->rsNext = NULL; 
	rs->ampli = ampli;
	rs->ampliphase = ampliphase;
	rs->atten = atten;

	return rs;
}

static RayStep* reflectRayFromEdge (RayStep *rs, FILE *infofp)
/* Reflect ray from edge and  return pointer to new RayStep. */
{
	int kmah,nref;
	float sigma,x,z,px,pz,t,q1,p1,q2,p2,
		s00,dsdx,dsdz,s,coeff,
		px1,pz1,pxr,pzr,
		c1ov1,c2ov2,oc2s,g,cterm,iterm,
		dv1dl,dv2dl,dv1dm,dv2dm,
		scale,gx,gz,hx,h_z,frac,dx,dz;
	float atten,ampli,ampliphase,dens1,dens2,s2,
		ds2dx,ds2dz,temp1,temp2;
	EdgeUse *eu,*eum;
	EdgeUseAttributes *eua,*euma;
	Face *f,*fn;
	FaceAttributes *fa;

	/* get input parameters */
	sigma = rs->sigma;
	x = rs->x;
	z = rs->z;
	px = rs->px;
	pz = rs->pz;
	t = rs->t;
	q1 = rs->q1;
	p1 = rs->p1;
	q2 = rs->q2;
	p2 = rs->p2;
	kmah = rs->kmah;
	nref = rs->nref;
	eu = rs->eu;
	f = rs->f;
	ampli = rs->ampli;
	ampliphase = rs->ampliphase;
	atten = rs->atten;

	/* determine sloth on incident side of edge */
	fa = f->fa;
	s00 = fa->s00;
	dsdx = fa->dsdx;
	dsdz = fa->dsdz;
	s = s00+dsdx*x+dsdz*z;

	/* determine dens on incident side of edge */
	dens1 = fa->dens;
	
	/* edge vector */
	eum = eu->euMate;

	/* determine sloth on other side of edge */
	fn = eum->f;
	fa = fn->fa;
	s00 = fa->s00;
	ds2dx = fa->dsdx;
	ds2dz = fa->dsdz;
	s2 = s00+ds2dx*x+ds2dz*z;

	/* determine density on other side of the edge */
	dens2 = fa->dens;

	if (dens1==FLT_MAX) dens1 = 1.0;
	if (dens2==FLT_MAX) dens2 = 1.0;

	dx = eum->vu->v->y-eu->vu->v->y;
	dz = eum->vu->v->x-eu->vu->v->x;

	/* fractional distance along edge */
	frac = (ABS(dx)>ABS(dz)) ? (x-eu->vu->v->y)/dx : (z-eu->vu->v->x)/dz;

	/* linearly interpolate unit vector g tangent to edge */
	eua = eu->eua;
	euma = eum->eua;
	gx = frac*euma->tx-(1.0-frac)*eua->tx;
	gz = frac*euma->tz-(1.0-frac)*eua->tz;
	scale = 1.0/sqrt(gx*gx+gz*gz);
	gx *= scale;
	gz *= scale;

	/* unit vector h normal to edge */
	hx = -gz;
	h_z = gx;

	/* remember incident ray parameters */
	px1 = px;
	pz1 = pz;

	/* rotated incident ray parameters */
	pxr = px*h_z-pz*hx;
	pzr = px*hx+pz*h_z;

	/* rotated reflected ray parameters */
	pxr = pxr;
	pzr = -pzr;

	/* reflected ray parameters */
	px = pxr*h_z+pzr*hx;
	pz = pzr*h_z-pxr*hx;

	/* curvature term */
	c1ov1 = c2ov2 = -pzr;
	oc2s = s/(pzr*pzr);
	g = frac*euma->c-(1.0-frac)*eua->c;
	cterm = g*oc2s*(c1ov1+c2ov2);

	/* update dynamic ray parameters */
	scale = -c2ov2/c1ov1;
	q1 = q1*scale;
	p1 = p1/scale+cterm*q1;
	q2 = q2*scale;
	p2 = p2/scale+cterm*q2;

	/* if sloth not constant */
	if (dsdx!=0.0 || dsdz!=0.0) {

		/* velocity derivatives tangent and normal to ray */
		scale = -0.5/(s*s);
		dv1dl = scale*(px1*dsdx+pz1*dsdz);
		dv2dl = scale*(px*dsdx+pz*dsdz);
		dv1dm = scale*(pz1*dsdx-px1*dsdz);
		dv2dm = scale*(pz*dsdx-px*dsdz);

		/* inhomogeneity term */
		iterm = -pxr*oc2s
			*(2.0*(dv1dm*c1ov1+dv2dm*c2ov2)+pxr*(dv1dl-dv2dl));

		/* update dynamic ray parameters */
		p1 += iterm*q1;
		p2 += iterm*q2;
	}

	/* increment number of reflections */
	++nref;

	if (s2-pxr*pxr>=0) {
		/* pre-critical reflection */
		if (infofp!=NULL)
			fprintf(infofp,
				"Reflected pre-critically at %g degrees.\n",
				acos(-pzr/sqrt(s))*180/PI);
		temp1 = -pzr*dens2/dens1/sqrt(s);
		temp2 = sqrt(s2/s-(pxr*pxr/s));
		coeff = (temp1-temp2)/(temp1+temp2);
		ampli *= coeff;  
	} else {
		/* post-critical reflection */
		coeff = 1;
		ampliphase -= 2*atan(-sqrt(pxr*pxr-s2)*dens1/(dens2*pzr));
		if (infofp!=NULL)
			fprintf(infofp,
				"Reflected post-critically at %g degrees.\n",
				acos(-pzr/sqrt(s))*180/PI);
	}

	if (infofp!=NULL) {
		fprintf(infofp,"  --incident side: v1=%g  dens1=%g\n",
			1/sqrt(s),dens1);
	   	fprintf(infofp,"  --opposite side: v2=%g  dens2=%g\n",
			1/sqrt(s2),dens2);
		fprintf(infofp,"  --reflection coeff: %g\n",coeff);
		fprintf(infofp,"  --total ampl coeff: %g\n",ampli);
		fprintf(infofp,"  --total phase shift: %g\n",ampliphase);
	}

	/* return new raystep */
	rs->rsNext = (RayStep*)ealloc1(1,sizeof(RayStep));
	rs = rs->rsNext;
	rs->sigma = sigma;
	rs->x = x;
	rs->z = z;
	rs->px = px;
	rs->pz = pz;
	rs->t = t;
	rs->q1 = q1;
	rs->p1 = p1;
	rs->q2 = q2;
	rs->p2 = p2;
	rs->kmah = kmah;
	rs->nref = nref;
	rs->eu = eu;
	rs->f = f;
	rs->rsNext = NULL;
	rs->ampli = ampli;
	rs->ampliphase = ampliphase;
	rs->atten = atten;

	return rs;
}

static void evaluateDynamic (float sigma, 
	float px, float pz, float dsdx, float dsdz,
	float *q1, float *p1, float *q2, float *p2, int *kmah)
/* evaluate dynamic ray parameters */
{
	double s0,s1,s2,ss,scale,a,b,c,d,e,
		q1i,p1i,q2i,p2i,q1o,p1o,q2o,p2o;

	/* get input dynamic ray parameters */
	q1i = *q1;  p1i = *p1;  q2i = *q2;  p2i = *p2;

	/* trivial case:  constant sloth or ray parallel to sloth gradient */
	if (pz*dsdx==px*dsdz) {
		q1o = q1i+p1i*sigma;
		q2o = q2i+p2i*sigma;
		p2o = p2i;
		p1o = p1i;

	/* else, general case */
	} else {

		/* constants */
		s0 = px*px+pz*pz;
		s1 = px*dsdx+pz*dsdz;
		s2 = 0.25*(dsdx*dsdx+dsdz*dsdz);
		ss = (s2*sigma+s1)*sigma+s0;
		scale = 1.0/sqrt(s0*ss);
		a = s0+0.5*s1*sigma;
		b = (0.25*s1*s1/s0-s2)*sigma;
		c = s0+s1*sigma;
		d = 0.5*s1+2.0*b;
		b *= sigma;
		e = (0.5*s1+s2*sigma)/ss;

		/* update q1 and q2 */
		q1o = scale*(a*(q1i+p1i*sigma)+b*q1i);
		q2o = scale*(a*(q2i+p2i*sigma)+b*q2i);

		/* update p1 and p2 */
		p1o = scale*(c*p1i+d*q1i)-e*q1o;
		p2o = scale*(c*p2i+d*q2i)-e*q2o;
	}

	/* update kmah index */
	if (q2i*q2o>=0.0 && p2i*p2o<0.0 && q2i*p2i<0.0)
		*kmah += 2;
	else if (q2o==0.0 || q2i*q2o<0.0
			|| (q2i==0.0 && p2i*p2o<0.0 && q2o*p2o>0.0))
		*kmah += 1;

	/* return updated dynamic ray parameters */
	*q1 = q1o;  *p1 = p1o;  *q2 = q2o;  *p2 = p2o;
}

static void evaluateDynamic2 (float sigma, 
	float px, float pz, float dsdx, float dsdz,
	float *q1, float *p1, float *q2, float *p2,
	float s0, float s1, float s2)
/* evaluate dynamic ray parameters at each point of the ray */
{
	double ss,scale,a,b,c,d,e,
		q1i,p1i,q2i,p2i,q1o,p1o,q2o,p2o;
	
	/* get input dynamic ray parameters */
	q1i = *q1;  p1i = *p1;  q2i = *q2;  p2i = *p2;
		
	/* trivial case:  constant sloth or ray parallel to sloth gradient */
	if (pz*dsdx==px*dsdz) {
		q1o = q1i+p1i*sigma;
		q2o = q2i+p2i*sigma;
		p2o = p2i;
		p1o = p1i;

	/* else, general case */
	} else {

		/* constants */
		ss = (s2*sigma+s1)*sigma+s0;
		scale = 1.0/sqrt(s0*ss);
		a = s0+0.5*s1*sigma;
		b = (0.25*s1*s1/s0-s2)*sigma;
		c = s0+s1*sigma;
		d = 0.5*s1+2.0*b;
		b *= sigma;
		e = (0.5*s1+s2*sigma)/ss;

		/* update q1 and q2 */
		q1o = scale*(a*(q1i+p1i*sigma)+b*q1i);
		q2o = scale*(a*(q2i+p2i*sigma)+b*q2i);

		/* update p1 and p2 */
		p1o = scale*(c*p1i+d*q1i)-e*q1o;
		p2o = scale*(c*p2i+d*q2i)-e*q2o;
	}
	
	/* return updated dynamic ray parameters */
	*q1 = q1o;  *p1 = p1o;  *q2 = q2o;  *p2 = p2o;
}		

int checkIfSourceOnEdge (Face *tris, float zs, float xs)
{
	float x1,x2,x3,z1,z2,z3,m12,m13,m23,b12,b13,b23,eps;
	EdgeUse *eu;

	eu = tris->eu;
	eps = tris->m->eps;

	/* get vertices */
	x1 = eu->vu->v->y;
	z1 = eu->vu->v->x;
	x2 = eu->euCW->vu->v->y;
	z2 = eu->euCW->vu->v->x;
	x3 = eu->euCCW->vu->v->y;
	z3 = eu->euCCW->vu->v->x;

	/* source is sitting on vertex */
	if ((xs-x1)*(xs-x1)+(zs-z1)*(zs-z1)<eps
		|| (xs-x2)*(xs-x2)+(zs-z2)*(zs-z2)<eps
		|| (xs-x3)*(xs-x3)+(zs-z3)*(zs-z3)<eps) return 2;

	/* check special cases and compute slope of edges */
	if (ABS(x1-x2)<0.1*eps) {
		if (ABS(xs-x1)<0.1*eps) {
			return 1;
		} else {
			m12 = 0.01*FLT_MAX;
		}
	} else {
		m12 = (z1-z2)/(x1-x2);
	}
	if (ABS(x1-x3)<0.1*eps) {
		if (ABS(xs-x1)<0.1*eps) {
			return 1;
		} else {
			m13 = 0.01*FLT_MAX;
		}
	} else {
		m13 = (z1-z3)/(x1-x3);
	}
	if (ABS(x2-x3)<0.1*eps) {
		if (ABS(xs-x2)<0.1*eps) {
			return 1;
		} else {
			m23 = 0.01*FLT_MAX;
		}
	} else {
		m23 = (z2-z3)/(x2-x3);
	}
	b12 = z1-m12*x1;
	b13 = z1-m13*x1;
	b23 = z2-m23*x2;

	/* source on edge? */
	if (ABS(zs-m12*xs-b12)<0.1*eps
		|| ABS(zs-m13*xs-b13)<0.1*eps
		|| ABS(zs-m23*xs-b23)<0.1*eps) return 1;

	return 0;
}

void writeFresnel (Model *m, FILE *rayfp, FILE *infofp, int nxz, int nray,
	RayStep *rs[], RayEnd re[], int krecord, float freq, int prim,
	FILE *fresnelfp, FILE *outparfp)
/* for each ray, write x,z pairs and FresnelVolume */
{
	int iray,nrs,irs,nxzw,ns,is,icount;
	float sigmamax=0.0,sigma1,sigma2,x,z,px,pz,dsdx,dsdz,dummy,
		ds,xs,zs,dsigma,mfa,mfb,scale;
	float q1end=0.0,q2end=0.0,q1,q2,p1,p2,s0,s1,s2,q1i,q2i,p1i,p2i;
	float xfresnel1,xfresnel2,zfresnel1,zfresnel2;
	float *xa,*za,*r;
	RayStep *rsi;
	FaceAttributes *fa;

	m += 0; /* dummy */

	/* allocate space for auxiliary arrays */
	xa = ealloc1float(nxz);
	za = ealloc1float(nxz);
	r = ealloc1float(nxz);

	/* initialize counter */
	icount = 0;

	/* loop over rays */
	for (iray=0; iray<nray; ++iray) {

		if (krecord==INT_MAX || (krecord==re[iray].kend
			&& (prim==INT_MAX || prim==re[iray].nref))) { 

			/* count rays */
			++icount;

			/* count ray steps while determining maximum sigma */
			/* and the spreading values at the ray ends */
			for (nrs=0,rsi=rs[iray]; rsi!=NULL;
				++nrs,rsi=rsi->rsNext) {
				sigmamax = rsi->sigma;
				q1end = rsi->q1;
				q2end = rsi->q2;
			}

			/* initialize number of (x,z) written for this ray */
			nxzw = 0;

			/* loop over ray steps */
			for (irs=0,rsi=rs[iray]; irs<nrs;
				++irs,rsi=rsi->rsNext) {

				/* if sufficient number of (x,z) */
				/* have been written */
				if (nxzw>=nxz) break;

				/* ray parameters at this step */
				x = rsi->x;
				z = rsi->z;
				px = rsi->px;
				pz = rsi->pz;
				fa = rsi->f->fa;
				dsdx = fa->dsdx;
				dsdz = fa->dsdz;

				/* constants for nontrivial case */
				s0 = px*px+pz*pz;
				s1 = px*dsdx+pz*dsdz;
				s2 = 0.25*(dsdx*dsdx+dsdz*dsdz);

				/* sigma at this step and next step */
				sigma1 = rsi->sigma;
				sigma2 = (irs<nrs-1)?rsi->rsNext->sigma:sigma1;

				/* q1, q2, p1, p2 at this step */
				q1i = rsi->q1; 
				q2i = rsi->q2;
				p1i = rsi->p1; 
				p2i = rsi->p2;

				/* number of sigma between this */
				/* step and next */
				ns = 1+(sigma2-sigma1)*(nxz-1)/(2.0*sigmamax);

				/* increment in sigma */
				ds = (sigma2-sigma1)/ns;

				/* loop over sigma */
				for (is=0,dsigma=0.0; is<ns; ++is,dsigma+=ds) {

					/* compute and write x and z */
					xs = x+dsigma*(px+dsigma*0.25*dsdx);
					zs = z+dsigma*(pz+dsigma*0.25*dsdz);
					xa[nxzw] = xs;
					za[nxzw] = zs;
					if (efwrite(&zs,sizeof(float),1,rayfp)
						!=1) err("error writing "
						"ray file!\n");
					if (efwrite(&xs,sizeof(float),1,rayfp)
						!=1) err("error writing "
						"ray file!\n");

					q1 = q1i;
					q2 = q2i;
					p1 = p1i;
					p2 = p2i;

					/* compute and write p1,p2,q1,q2 */
					evaluateDynamic2(dsigma,px,pz,
						dsdx,dsdz,&q1,&p1,&q2,&p2,
						s0,s1,s2);

					/* compute m(Of,A) */
					if (q2==0)
						mfa = FLT_MAX;
					else
						mfa = p2/q2;

					/* compute m(Of,B) */
					mfb = -q1*q2end+q2*q1end;
					if (mfb==0)
						mfb = FLT_MAX;
					else
						mfb = (p2*q1end-p1*q2end)/mfb;

					/* compute fresnel radius */
					dummy = ABS(1.0/((mfa-mfb)*freq));
					r[nxzw] = sqrt(dummy);

					/* if sufficient number */
					/* written, break */
					if (++nxzw>=nxz) break;
				}
			}

			/* if necessary, repeat last (x,z) and r */
			while (nxzw<nxz) {
				xa[nxzw] = xs;
				za[nxzw] = zs;
				r[nxzw] = 0;
				if (efwrite(&zs,sizeof(float),1,rayfp)!=1)
					err("error writing ray file!\n");
				if (efwrite(&xs,sizeof(float),1,rayfp)!=1)
					err("error writing ray file!\n");
				++nxzw;
			}

			/* compute fresnel points */
			if (infofp!=NULL)
				fprintf(infofp,"Fresnel points:\n");
			for (is=0; is<nxz; ++is) {
				if (is==0 || is==nxz-1) {
					xfresnel1 = xfresnel2 = xa[is];
					zfresnel1 = zfresnel2 = za[is];
				} else {
					scale = sqrt((za[is-1]-za[is+1])
						*(za[is-1]-za[is+1])
						+(xa[is+1]-xa[is-1])
						*(xa[is+1]-xa[is-1]));
					if (scale==0 || is==0 || is==nxz-1) {
						xfresnel1 = xfresnel2 = xa[is];
						zfresnel1 = zfresnel2 = za[is];
					} else {
						dummy = 1.0/scale
							*(za[is-1]-za[is+1])
							*r[is];
						xfresnel1 = xa[is]+dummy;
						xfresnel2 = xa[is]-dummy;
						dummy = 1.0/scale
							*(xa[is+1]-xa[is-1])
							*r[is];
						zfresnel1 = za[is]+dummy;
						zfresnel2 = za[is]-dummy;
					}
				}
				if (infofp!=NULL)
					fprintf(infofp,
						"x1=%g  z1=%g  "
						"x2=%g  z2=%g  r=%g\n",
						xfresnel1,zfresnel1,
						xfresnel2,zfresnel2,r[is]);
				if (efwrite(&zfresnel1,sizeof(float),1,
					fresnelfp)!=1)
					err("error writing Fresnel file!\n");
				if (efwrite(&xfresnel1,sizeof(float),1,
					fresnelfp)!=1)
					err("error writing Fresnel file!\n");
				if (efwrite(&zfresnel2,sizeof(float),1,
					fresnelfp)!=1)
					err("error writing Fresnel file!\n");
				if (efwrite(&xfresnel2,sizeof(float),1,
					fresnelfp)!=1)
					err("error writing Fresnel file!\n");

			} /* end loop over fresnel points */

		} else {
			/* do not write rays and fresnel volumes; */
			/* manipulate rayend information */
			re[iray].kend = -1;
		}

	} /* close loop over rays */

	if (infofp!=NULL)
		fprintf(infofp,"\nWrote %d rays to ray file.\n",icount);

	/* write number of rays to parameter file */
	fprintf(outparfp,"%d\n",icount);

	/* free space */
	free1float(xa);
	free1float(za);
	free1float(r);
}
