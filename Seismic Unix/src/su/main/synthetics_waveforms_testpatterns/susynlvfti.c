/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSYLVTI: $Revision: 1.8 $ ; $Date: 2015/06/02 20:15:23 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUSYNLVFTI - SYNthetic seismograms for Linear Velocity function in a  ",
"              Factorized Transversely Isotropic medium			",
" 									",
" susynlvfti >outfile [optional parameters]				",
" 									",
" Optional Parameters:							",
" nt=101		number of time samples				",
" dt=0.04		time sampling interval (sec)			",
" ft=0.0		first time (sec)				",
" kilounits=1            input length units are km or kilo-feet         ",
"                        =0 for m or ft                                 ",
"                        Note: Output (sx,gx,offset) are always m or ft ",
" nxo=1			number of source-receiver offsets		",
" dxo=0.05		offset sampling interval (kilounits)		",
" fxo=0.0		first offset (kilounits, see notes below)	",
" xo=fxo,fxo+dxo,...    array of offsets (use only for non-uniform offsets)",
" nxm=101		number of midpoints (see notes below)		",
" dxm=0.05		midpoint sampling interval (kilounits)		",
" fxm=0.0		first midpoint (kilounits)			",
" nxs=101		number of shotpoints (see notes below)		",
" dxs=0.05		shotpoint sampling interval (kilounits)		",
" fxs=0.0		first shotpoint (kilounits)			",
" x0=0.0		distance x at which v00 is specified		",
" z0=0.0		depth z at which v00 is specified		",
" v00=2.0		velocity at x0,z0 (kilounits/sec)		",
" dvdx=0.0		derivative of velocity with distance x (dv/dx)	",
" dvdz=0.0		derivative of velocity with depth z (dv/dz)	",
" fpeak=0.2/dt		peak frequency of symmetric Ricker wavelet (Hz)	",
" ref=1:1,2;4,2		reflector(s):  \"amplitude:x1,z1;x2,z2;x3,z3;...\"",
" smooth=0		=1 for smooth (piecewise cubic spline) reflectors",
" er=0			=1 for exploding reflector amplitudes		",
" ls=0			=1 for line source; default is point source	",
" ob=0			=1 to include obliquity factors			",
" tmin=10.0*dt		minimum time of interest (sec)			",
" ndpfz=5		number of diffractors per Fresnel zone		",
" verbose=1		=1 to print some useful information		",
" 									",
" For transversely isotropic media:					",
" angxs=0.0		angle of symmetry axis with the vertical (degrees)",
" define the media using either						",
" a=1.0		corresponding to the ratio of elastic coef.(c1111/c3333)",
" f=0.4		corresponding to the ratio of elastic coef. (c1133/c3333)",
" l=0.3		corresponding to the ratio of elastic coef. (c1313/c3333)",
" Alternately use Tompson\'s parameters:				",
" delta=0	Thomsen's 1986 defined parameter			",
" epsilon=0	Thomsen's 1986 defined parameter			",
" ntries=40	number of iterations in Snell's law and offset searches ",
" epsx=.001	lateral offset tolerance				",
" epst=.0001	reflection time tolerance				",
" nitmax=12	max number of iterations in travel time integrations	",
" 									",
" Notes:								",
" 									",
" Offsets are signed - may be positive or negative.  Receiver locations	",
" are computed by adding the signed offset to the source location.	",
" 									",
" Specify either midpoint sampling or shotpoint sampling, but not both.	",
" If neither is specified, the default is the midpoint sampling above.	",
" 									",
" More than one ref (reflector) may be specified.  When obliquity factors",
" are included, then only the left side of each reflector (as the x,z	",
" reflector coordinates are traversed) is reflecting.  For example, if x",
" coordinates increase, then the top side of a reflector is reflecting.	",
" Note that reflectors are encoded as quoted strings, with an optional	",
" reflector amplitude: preceding the x,z coordinates of each reflector.	",
" Default amplitude is 1.0 if amplitude: part of the string is omitted.	",
" 									",
" Concerning the choice of delta and epsilon. The difference between delta", 
" and epsilon should not exceed one. A possible break down of the program",
" is the result. This is caused primarly by the break down in the two point", 
" ray-tracing. Also keep the values of delta and epsilon between 2 and -2.",
NULL};
/**************** end self doc ***********************************/

/*
 * Author:  Dave Hale, 09/17/91,  Colorado School of Mines
 * Upgrade to Transversely Isotropic medium: T. Alkhallfah, 4/15/93
 *
 */


/* these structures are defined in par.h -- this is documentation only
 *
 * typedef struct ReflectorSegmentStruct {
 *	float x;	( x coordinate of segment midpoint )
 *	float z;	( z coordinate of segment midpoint )
 *	float s;	( x component of unit-normal-vector )
 *	float c;	( z component of unit-normal-vector )
 * } ReflectorSegment;
 * typedef struct ReflectorStruct {
 *	int ns;			( number of reflector segments )
 *	float ds;		( segment length )
 *	float a;		( amplitude of reflector )
 *	ReflectorSegment *rs;	( array[ns] of reflector segments )
 * } Reflector;
 * typedef struct WaveletStruct {
 *	int lw;			( length of wavelet )
 *	int iw;			( index of first wavelet sample )
 *	float *wv;		( wavelet sample values )
 * } Wavelet;
 *
 */

/* parameters for half-derivative filter */
#define LHD 20
#define NHD 1+2*LHD

/* prototypes for functions defined and used internally */
static void makeone (float v00, float dvdx, float dvdz,
	int ls, int er, int ob, Wavelet *w, int trans,
	int nitmax, float epst, int zeroff,
	int ntries, float epsx, float angxs,
	float xs, float zs, float xg, float zg,
	float a, float f, float l,
	int nr, Reflector *r, int nt, float dt, float ft, float *trace);
static void raylvt (float v00, float dvdx, float dvdz,
	float x0, float z0, float x, float z,
	float a, float f, float l, int ntries,
	float nitmax, float epst, float epsx, float angxs,
	float *c, float *s, float *t, float *q);

/* segy trace */
segy tr;

int
main (int argc, char **argv)
{
	int nr,er,ob,ir,ixz,ls,smooth,ndpfz,ns,
		ixo,ixsm,nxo,nxs,nxm,nt,nxsm,
		shots,midpoints,verbose,tracl,zeroff,
		*nxz,kilounits;
	int nitmax,ntries,trans;
	float x0,z0,v00,dvdx,dvdz,vmin,tmin,tminr,
		x,z,v,t,dsmax,fpeak,
		dxs,dxm,dxo,dt,fxs,fxm,fxo,ft,dxsm,
		xs,zs,xg,zg,angxs,torad,delta,epsilon,
		*xo,*ar,**xr,**zr;
	float epst,epsx,a,f,l;
	Reflector *r;
	Wavelet *w;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	/* get parameters */
	if (!getparint("nt",&nt)) nt = 101;
	if (!getparfloat("dt",&dt)) dt = 0.04;
	if (!getparfloat("ft",&ft)) ft = 0.0;
        if (!getparint("kilounits",&kilounits)) kilounits = 1;
	if ((nxo=countparval("xo"))!=0) {
		xo = ealloc1float(nxo);
		getparfloat("xo",xo);
	} else {
		if (!getparint("nxo",&nxo)) nxo = 1;
		if (!getparfloat("dxo",&dxo)) dxo = 0.05;
		if (!getparfloat("fxo",&fxo)) fxo = 0.0;
		xo = ealloc1float(nxo);
		for (ixo=0; ixo<nxo; ++ixo)
			xo[ixo] = fxo+ixo*dxo;
	}
	shots = (getparint("nxs",&nxs) || 
		getparfloat("dxs",&dxs) || 
		getparfloat("fxs",&fxs));
	midpoints = (getparint("nxm",&nxm) || 
		getparfloat("dxm",&dxm) || 
		getparfloat("fxm",&fxm)); 
	if (shots && midpoints)
		err("cannot specify both shot and midpoint sampling!\n");
	if (shots) {
		if (!getparint("nxs",&nxs)) nxs = 101;
		if (!getparfloat("dxs",&dxs)) dxs = 0.05;
		if (!getparfloat("fxs",&fxs)) fxs = 0.0;
		nxsm = nxs;
		dxsm = dxs;
	} else {
		midpoints = 1;
		if (!getparint("nxm",&nxm)) nxm = 101;
		if (!getparfloat("dxm",&dxm)) dxm = 0.05;
		if (!getparfloat("fxm",&fxm)) fxm = 0.0;
		nxsm = nxm;
		dxsm = dxm;
	}
	if (!getparint("nxm",&nxm)) nxm = 101;
	if (!getparfloat("dxm",&dxm)) dxm = 0.05;
	if (!getparfloat("fxm",&fxm)) fxm = 0.0;
	if (!getparfloat("x0",&x0)) x0 = 0.0;
	if (!getparfloat("z0",&z0)) z0 = 0.0;
	if (!getparfloat("v00",&v00)) v00 = 2.0;
	if (!getparfloat("dvdx",&dvdx)) dvdx = 0.0;
	if (!getparfloat("dvdz",&dvdz)) dvdz = 0.0;
	if (!getparfloat("fpeak",&fpeak)) fpeak = 0.2/dt;
	if (!getparint("ls",&ls)) ls = 0;
	if (!getparint("er",&er)) er = 0;
	if (!getparint("ob",&ob)) ob = 0;
	if (!getparfloat("tmin",&tmin)) tmin = 10.0*dt;
	if (!getparint("ndpfz",&ndpfz)) ndpfz = 5;
	if (!getparint("smooth",&smooth)) smooth = 0;
	if (!getparint("verbose",&verbose)) verbose = 1;
	decodeReflectors(&nr,&ar,&nxz,&xr,&zr);
	if (!smooth) breakReflectors(&nr,&ar,&nxz,&xr,&zr);

	/********************************/
	/* get optional parameters relating to anisotropy*/
	if (!getparfloat("angxs",&angxs))     angxs   = 0.;
	if (!getparfloat("a",&a))	 	a     = 1.0;
	if (!getparfloat("f",&f))	 	f     = 0.4;
	if (!getparfloat("l",&l))		l     = 0.3;
	if (!getparfloat("delta",&delta))	delta     = 0.0;
	if (!getparfloat("epsilon",&epsilon))  epsilon     = 0.0;
	/********************************/
	/* for computing incidence and reflection angles and velocities */
	if (!getparint("ntries",&ntries))	ntries   = 20;
	if (!getparfloat("epsx",&epsx)) epsx  = .001;
	if (!getparfloat("epst",&epst)) epst  = .0001;

	if (!getparint("nitmax",&nitmax))  nitmax  = 12;
	/********************************/

	trans=0;
	if(a != 1 || (f+2*l) != 1) trans=1;
	if(delta != 0 || epsilon != 0){
		a=1+2*epsilon;
		f=sqrt(2*delta*(1-l)+(1-l)*(1-l))-l;
		trans=1;
	}

	/*determine wether it's zero-offset*/
	zeroff = 0;
	if((nxo==1) && (fxo==0.0)) zeroff = 1;

	/* convert velocity v(x0,z0) to v(0,0) */
	v00 -= dvdx*x0+dvdz*z0;
	
	/*transform to radians*/
	torad = PI/180;
	/* determine minimum velocity and minimum reflection time */
	for (ir=0,vmin=FLT_MAX,tminr=FLT_MAX; ir<nr; ++ir) {
		for (ixz=0; ixz<nxz[ir]; ++ixz) {
			x = xr[ir][ixz];
			z = zr[ir][ixz];
			v = v00+dvdx*x+dvdz*z;
			if (v<vmin) vmin = v;
			t = 2.0*z/v;
			if (t<tminr) tminr = t;
		}
	}

	/* determine maximum reflector segment length */
	tmin = MAX(tmin,MAX(ft,dt));
	dsmax = vmin/(2*ndpfz)*sqrt(tmin/fpeak);
 	
	/* make reflectors */
	makeref(dsmax,nr,ar,nxz,xr,zr,&r);

	/* count reflector segments */
	for (ir=0,ns=0; ir<nr; ++ir)
		ns += r[ir].ns;

	/* make wavelet */
	makericker(fpeak,dt,&w);
	
	/* if requested, print information */
	if (verbose) {
		fprintf(stderr,"\nSYNLVXZ:\n");
		fprintf(stderr,
			"Minimum possible reflection time (assuming sources\n"
			"and receivers are at the surface Z=0) is %g s.\n"
			"You may want to adjust the \"minimum time of \n"
			"interest\" parameter.\n",tminr);
		fprintf(stderr,
			"Total number of small reflecting\n"
			"segments is %d.\n",ns);
		fprintf(stderr,"\n");
	}
	

	/* set constant segy trace header parameters */
	memset( (void *) &tr, 0, sizeof(segy));
	tr.trid = 1;
	tr.counit = 1;
	tr.ns = nt;
	tr.dt = 1.0e6*dt;
	tr.delrt = 1.0e3*ft;
	
	/* loop over shots or midpoints */
	for (ixsm=0,tracl=0; ixsm<nxsm; ++ixsm) {
	
		/* loop over offsets */
		for (ixo=0; ixo<nxo; ++ixo) {
		
			/* compute source and receiver coordinates */
			if (shots)
				xs = fxs+ixsm*dxs;
			else
				xs = fxm+ixsm*dxm-0.5*xo[ixo];
			zs = 0.0;
			xg = xs+xo[ixo];
			zg = 0.0;
			
			/* set segy trace header parameters */
			tr.tracl = tr.tracr = ++tracl;
			if (shots) {
				tr.fldr = 1+ixsm;
				tr.tracf = 1+ixo;
                                tr.d2 = dxo;
                                tr.f2 = fxo;

			} else {
				tr.cdp = 1+ixsm;
				tr.cdpt = 1+ixo;
                                tr.d2 = dxm;
                                tr.f2 = fxm;
			}

			if (kilounits==1) {
 		  	    tr.offset = NINT(1000.0*(dxsm>0.0?xo[ixo]:-xo[ixo]));
			    tr.sx = NINT(1000.0*xs);
			    tr.gx = NINT(1000.0*xg);
                        } else {
                            tr.offset = NINT((dxsm>0.0?xo[ixo]:-xo[ixo]));
                            tr.sx = NINT(xs);
                            tr.gx = NINT(xg);
                        }

				
			/* make one trace */
			makeone(v00,dvdx,dvdz,
				ls,er,ob,w,trans,
				nitmax,epst,zeroff,
				ntries,epsx,angxs*torad,
				xs,zs,xg,zg,
				a,f,l,
				nr,r,nt,dt,ft,tr.data);
			
			/* write trace */
			puttr(&tr);
		}
	}
	return(CWP_Exit());
}


static void makeone (float v00, float dvdx, float dvdz, 
	int ls, int er, int ob, Wavelet *w, int trans,
	int nitmax, float epst, int zeroff,
	int ntries, float epsx, float angxs,
	float xs, float zs, float xg, float zg,
	float a, float f, float l,
	int nr, Reflector *r, int nt, float dt, float ft, float *trace)
/*****************************************************************************
Make one synthetic seismogram for linear velocity v(x,z) = v00+dvdx*x+dvdz*z
******************************************************************************
Input:
v00		velocity v at (x=0,z=0)
dvdx		derivative dv/dx of velocity v with respect to x
dvdz		derivative dv/dz of velocity v with respect to z
ls		=1 for line source amplitudes; =0 for point source
er		=1 for exploding, =0 for normal reflector amplitudes
ob		=1 to include cos obliquity factors; =0 to omit
w		wavelet to convolve with trace
xs		x coordinate of source
zs		z coordinate of source
xg		x coordinate of receiver group
zg		z coordinate of receiver group
nr		number of reflectors
r		array[nr] of reflectors
nt		number of time samples
dt		time sampling interval
ft		first time sample

Output:
trace		array[nt] containing synthetic seismogram
*****************************************************************************/
{
	int it,ir,is,ns;
	float ar,ds,xd,zd,cd,sd,vs,vg,vd,cs,ss,ts,qs,cg,sg,tg,qg,
		ci,cr,time,amp,*temp;
	ReflectorSegment *rs;
	int lhd=LHD,nhd=NHD;
	static float hd[NHD];
	static int madehd=0;
	
	/* if half-derivative filter not yet made, make it */
	if (!madehd) {
		mkhdiff(dt,lhd,hd);
		madehd = 1;
	}

	/* zero trace */
	for (it=0; it<nt; ++it)
		trace[it] = 0.0;
	
	/* velocities at source and receiver */
	vs = v00+dvdx*xs+dvdz*zs;
	vg = v00+dvdx*xg+dvdz*zg;

	/* loop over reflectors */
	for (ir=0; ir<nr; ++ir) {

		/* amplitude, number of segments, segment length */
		ar = r[ir].a;
		ns = r[ir].ns;
		ds = r[ir].ds;
		rs = r[ir].rs;
	
		/* loop over diffracting segments */
		for (is=0; is<ns; ++is) {
		
			/* diffractor midpoint, unit-normal, and length */
			xd = rs[is].x;
			zd = rs[is].z;
			cd = rs[is].c;
			sd = rs[is].s;
			
			/* velocity at diffractor */
			vd = v00+dvdx*xd+dvdz*zd;
			
			/*if shotpoint and geophones (prestack) */
			if(zeroff==0) {

			  /* if transversely isotropic */
			  if(trans) {

				/* ray from shot to diffractor */
				raylvt(v00,dvdx,dvdz,xs,zs,xd,zd,a,f,l,ntries,
				nitmax,epst,epsx,angxs,&cs,&ss,&ts,&qs);

				/* ray from receiver to diffractor */
				raylvt(v00,dvdx,dvdz,xg,zg,xd,zd,a,f,l,ntries,
				nitmax,epst,epsx,angxs,&cg,&sg,&tg,&qg);
			  }
			  else {

				/* ray from shot to diffractor */
				raylv2(v00,dvdx,dvdz,xs,zs,xd,zd,&cs,&ss,&ts,&qs);

				/* ray from receiver to diffractor */
				raylv2(v00,dvdx,dvdz,xg,zg,xd,zd,&cg,&sg,&tg,&qg);

			  }
			} else {

			  if(trans) {

				/* ray  at one CMP */
				raylvt(v00,dvdx,dvdz,xs,zs,xd,zd,a,f,l,ntries,
				nitmax,epst,epsx,angxs,&cs,&ss,&ts,&qs);
				
				cg  = cs;
				sg  = ss;
				tg  = ts;
				qg  = qs;
			  }
			  else {

				/* ray at one CMP */
				raylv2(v00,dvdx,dvdz,xs,zs,xd,zd,&cs,&ss,&ts,&qs);

				cg  = cs;
				sg  = ss;
				tg  = ts;
				qg  = qs;
			  }
			}

			/* cosines of incidence and reflection angles */
			if (ob) {
				ci = cd*cs+sd*ss;
				cr = cd*cg+sd*sg;
			} else {
				ci = 1.0;
				cr = 1.0;
			}

			/* if either cosine is negative, skip diffractor */
			if (ci<0.0 || cr<0.0) continue;

			/* two-way time and amplitude */
			time = ts+tg;
			if (er) {
				amp = sqrt(vg*vd/qg);
			} else {
				if (ls)
					amp = sqrt((vs*vd*vd*vg)/(qs*qg));
				else
					amp = sqrt((vs*vd*vd*vg)/
						(qs*qg*(qs+qg)));
			}
			amp *= (ci+cr)*ar*ds;
				
			/* add sinc wavelet to trace */
			addsinc(time,amp,nt,dt,ft,trace);
		}
	}
	
	/* allocate workspace */
	temp = ealloc1float(nt);
	
	/* apply half-derivative filter to trace */
	convolve_cwp(nhd,-lhd,hd,nt,0,trace,nt,0,temp);

	/* convolve wavelet with trace */
	convolve_cwp(w->lw,w->iw,w->wv,nt,0,temp,nt,0,trace);
	
	/* free workspace */
	free1float(temp);
}


static void raylvt (float v00, float dvdx, float dvdz,
	float x0, float z0, float x, float z,
	float a, float f, float l, int ntries,
	float nitmax, float epst, float epsx, float angxs,
	float *c, float *s, float *t, float *q)
/*****************************************************************************
Trace ray between two points, for linear velocity v(x,z) = v00+dvdx*x+dvdz*z
in a transversely isotropic medium
Reference:
Alkhalifah, T., 1993, Efficient synthetic seismograms for transversely isotropic media with constant velocity gradient:CWP project review. 
******************************************************************************
Input:
v00	     velocity at (x=0,z=0)
dvdx	    derivative dv/dx of velocity with respect to x
dvdz	    derivative dv/dz of velocity with respect to z
x0		x coordinate at beginning of ray
z0		z coordinate at beginning of ray
x		x coordinate at end of ray
z		z coordinate at end of ray
ntries	  number of iterations in Snell's law search
epsx	    lateral offset tolerance
a,f,l	   ratios of velocities for the FAI transversely isotropic medium
angxs		angle of symmetry axis of anisotriopy

Output:
c		cosine of propagation angle at end of ray
s		sine of propagation angle at end of ray
t		time along raypath
q		integral of velocity along raypath
*****************************************************************************/
{

	double px0,v0,aa,alfa,beta=0.0,vcp=0.0,oa;
	double v,c1,c2,f1,fx,cr,sr;
	double r,or,vcpi=0.0,vcps=0.0,p11,so,co=0.0,ci=0.0,cz2,
			dso,co1,so1=0.0,ss=0.0,cs=0.0;
	double si2,co2,sc,sx=0.0,cx=0.0,so3,diff,so4,diff1=0.0,sz2;
	double w1,w2,w3,px2,pz2,betaa,betab,alpha,pz0,betat,xx,dvcp,bbb,soo;
	double umax,time=0.0,fctn1,fctn2,fctnu,temp,mx,sm=0.0,vg,gang=0.0;
	double rt,timenew=0.0,tnm,u,sum,del,si=0.0,vrat,zz,uv,cw=0.0,sw=0.0,
			uv0,px,pz;
	int j,count,it=0,nit,sig,zneg,err,tfturn,sign;

	/* if (x,z) same as (x0,z0) */
	if ((x==x0) && (z==z0)) {
		*c = 1.0;
		*s = 0.0;
		*t = 0.0;
		*q = 0.0;
		return;
	}

	/*defining variables variables*/
	c1 = 1 + l;
	c2 = a + l;
	w1	= a-l;
	w2     = 1-l;
	w3     = (f+l)*(f+l);

	/*if necessary, rotate coordinates until the symmetry axis is in z direction*/
	if (angxs != 0.) {
		sx    = sin(angxs);
		cx    = cos(angxs);
		co2  = (cx)*(cx);
		si2  = (sx)*(sx);
		alfa = c2*si2+c1*co2;
		beta = sqrt(pow(w1*si2-w2*co2,2)+4*si2*co2*w3);
		vrat  = sqrt(.5*(alfa+beta));
		temp  = cx*x0 - sx*z0;
		z0    = cx*z0 + sx*x0;
		x0    = temp;
		temp  = cx*x - sx*z;
		z     = cx*z + sx*x;
		x     = temp;
		temp  = cx*dvdx - sx*dvdz;
		dvdz  = (cx*dvdz + sx*dvdx)/vrat;
		dvdx  = temp/vrat;
		v00   = v00/vrat;
	}

	 /* if constant velocity */
	if ((dvdx==0.0) && (dvdz==0.0)) {
		x  -= x0;
		z  -= z0;
		r   = sqrt(x*x+z*z);
		or  = 1.0/r;
		*s  = x*or;
		so  = *s;
		dso = .1;
		so4 =0;
		
		/*secant search for takeoff ray angle*/
		for(j=0; j<ntries; ++j) {
			if(so>1)
				so = .5*(1+so1);
			if(so<-1)
				so = .5*(-1+so1);
			so4 = so;
			co   = sqrt(1-so*so);
			co2  = (co)*(co);
			si2  = (so)*(so);
			sc   = (so)*(co);
			alfa = c2*si2+c1*co2;
			beta = sqrt(pow(w1*si2-w2*co2,2)+4*si2*co2*w3);
			vcp  = sqrt(.5*(alfa+beta));
			dvcp = (.5*(a+l)*sc-.5*(1+l)*sc+.25*(2*((a-l)*si2-(1-l)*co2)*(
				(a-l)*sc+(1-l)*sc)+4*sc*co2*(l+f)*(l+f)-4*
				si2*sc*(l+f)*(l+f))/beta)/vcp;
			gang = atan(dvcp/vcp);
			diff = z*tan(gang+asin(so))-x;
			if(j ==0) {
				if(ABS(diff) < epsx){
					err=0;
					break;
				}
				so1 = so;
				so  = so + dso;
				diff1  = diff;
			} else {
				count += 1;
				so3 = so;
				if(ABS(diff)<epsx ) {
					err  = 0;
					break;
				}
				so  = (diff1*so3-diff*so1)/(diff1-diff);
				diff1  = diff;
				so1 = so3;
			}
			/*fprintf(stderr,"diff=%f so=%f count=%d/n",diff,so,count);*/
			}
		*s = sin(asin(*s)+gang);
		*c = sqrt(1-(*s)*(*s));
		vg   = vcp/cos(gang);
		*t = r/(v00*vg);
		*q = r*v00*vg;

		/* if coordinates were rotated, unrotate cosine and sine */
		if (angxs != 0.) {
			temp  = cx*(*s) + sx*(*c);
			*c    = cx*(*c) - sx*(*s);
			*s    = temp;
		}
		return;
	}

	/*define distances before axes rotation*/
	xx = x-x0;
	zz = z-z0;

	/*if not rotated*/
	cr  = 1.0;
	sr  = 0.0;

	/* if necessary, rotate coordinates so that v(x,z) = v0+a*(z-z0) */
	if (dvdx!=0.0 || dvdz<0) {
		aa  = sqrt(dvdx*dvdx+dvdz*dvdz);
		oa  = 1.0/aa;
		cr  = dvdz*oa;
		sr  = dvdx*oa;
		temp = cr*x0-sr*z0;
		z0 = cr*z0+sr*x0;
		x0 = temp;
		temp  = cr*x-sr*z;
		z  = cr*z+sr*x;
		x  = temp;
	} else {
		aa = dvdz;
	}

	/*velocities and distances after possible rotation of coordinates*/
	v0  = v00 + aa*z0;
	v   = v00 + aa*z;
	x  -= x0;
	z  -= z0;
	z0  = v0/aa;
	
	/* defining regions depending on wether we needed the equation reversed */
	if(((x<0.) && (aa>0.)) || ((x>0.) && (aa<0.))) 
		sig=-1;
	else
		sig=1;
	zneg=1;

	

	/* if raypath is parallel to velocity gradient */
	if (x*x<0.00001*z*z) {
		so = sr;
		dso= .1;
		so4=0;

		/*secant search for takeoff ray angle*/
		for(j=0; j<ntries; ++j) {
			if(ABS(so)>1){
				so = .5*(sig*sm+so4);
			}
			so4 = so;
			co   = sqrt(1-so*so);
			co2  = (co)*(co);
			si2  = (so)*(so);
			sc   = (so)*(co);
			alfa = c2*si2+c1*co2;
			beta = sqrt(pow(w1*si2-w2*co2,2)+4*si2*co2*w3);
			vcp  = sqrt(.5*(alfa+beta));
			dvcp = (.5*(a+l)*sc-.5*(1+l)*sc+.25*(2*((a-l)*si2-(1-l)*co2)*(
				(a-l)*sc+(1-l)*sc)+4*sc*co2*(l+f)*(l+f)-4*
				si2*sc*(l+f)*(l+f))/beta)/vcp;
			gang = atan(dvcp/vcp);
			diff = zz*tan(gang+asin(so))-xx;
			if(j ==0) {
				if(ABS(diff) < epsx){
					err=0;
					break;
				}
				so1 = so;
				so  = so + dso;
				diff1  = diff;
			} else {
				count += 1;
				so3 = so;
				if(ABS(diff)<epsx ) {
					err  = 0;
					break;
				}
				so  = (diff1*so3-diff*so1)/(diff1-diff);
				diff1  = diff;
				so1 = so3;
			}
			}
		/* if ray is going down */
		if (z>0.0) {
			*s = sin(asin(sr)-gang);
			*c = sqrt(1-(*s)*(*s));
			vg   = vcp/cos(gang);
			*t = log(v/v0)/(aa*vg);
			*q = 0.5*z*(v+v0)*vg;

		/* else, if ray is going up */
		} else {
			*s =  sin(asin(sr)-gang);
			*c = -sqrt(1-(*s)*(*s));
			vg   = vcp/cos(gang);
			*t = -log(v/v0)/(aa*vg);
			*q = -0.5*z*(v+v0)*vg;
		}

	/* else raypath is circular with finite radius */
	} else {


		/*calculating the center for a circular raypath for isotropic*/
		z0   = v0/aa;
		z   += z0;
		x0   = -(x*x+z*z-z0*z0)/(2.0*x);
		rt    = sqrt(x0*x0+z0*z0);
		f1    = 1/(aa*aa*z0*z0/(rt*rt*v0*v0));
		so    = sig*z0/rt;
		soo   = so;

		/*intial trial parameters*/
		count = 1;
		err  = 1;
		dso   = .1;
		so4   = 0.;
		sm    = 1;

		/*for search effeciency*/
		if(dvdz*cx<0) {
			zneg = -1;
		}
	
		/* looping over ntries to find the spatial root through Secant method*/
		for(j=0; j<ntries; ++j) {
			if(ABS(so)>sm){
				so = .5*(sig*sm+so4);
			}
			if(sig*so<0)
				so = .5*(so4+0.);
			so4  = so;
			co   = zneg*sqrt(1-so*so);
			sw   = cr*so+sr*co;
			cw   = cr*co-sr*so;
			sz2  = sw*sw;
			cz2  = cw*cw;
			alfa = c2*sz2+c1*cz2;
			bbb  = w1*sz2-w2*cz2;
			beta = sqrt(bbb*bbb+4*sz2*cz2*w3);
			vcpi = sqrt(.5*(alfa+beta));
			if((z0-x*so/co)==0) {
				ss=sig;
			} else {
				p11  = z*so/(z0*co-x*so);
				ss   = sig*sqrt(p11*p11/(1+p11*p11));
			}
			/*if beyond the ray turning point*/
			if((z0-x*so/co)<0) tfturn=-1;
			else tfturn=1;
			cs   = zneg*tfturn*sqrt(1-ss*ss);
			si   = cr*ss+sr*cs;
			ci   = cr*cs-sr*ss;
			sz2  = si*si;
			cz2  = ci*ci;
			alfa = c2*sz2+c1*cz2;
			bbb  = w1*sz2-w2*cz2;
			beta = sqrt(bbb*bbb+4*sz2*cz2*w3);
			vcps = sqrt(.5*(alfa+beta));
			diff = z/ss - z0*(vcpi/vcps)/so;
			if(j ==0) {
				if(ABS(diff) < epsx){
					err=0;
					break;
				}
				co1 = co;
				so1 = so;
				so  = so + dso*tfturn*sig;
				diff1  = diff;
			} else {
				count += 1;
				so3 = so;
				if(ABS(diff)<epsx ) {
					err  = 0;
					break;
				}
				if(j==(ntries-1)){
					zneg *= -1;
					j     = -1;
					so    = soo;
					if(err == 3){
						err = 1;
						break;
					}
					err = 3;
				}else{
					so  = (diff1*so3-diff*so1)/(diff1-diff);
					diff1  = diff;
					so1 = so3;
				}
			}
			} 
		/*ignoring unfound rays*/
		if(err){ 
			*t=0;
			*q=10000000;
			*s=0;
			*c=1;
		}else{
			fx  = -z*cs/ss;
			f1  = vcps*vcps*(fx*fx+z*z);

			/* signed radius of raypath; if ray going clockwise, r > 0 */
			if (x<0.0){
				r = sqrt(fx*fx+z*z);
			}else {
				r = -sqrt(fx*fx+z*z);
			}

			/*initial ray parameters for traveltime calculations*/
			uv0  = 1/(vcpi*v0);
			uv   = 1/(vcps*v);
			px0  = sw*uv0;
			pz0  = cw*uv0;
			px   = si*uv;
			pz   = ci*uv;

			/*calculating the time*/
			if(dvdx==0) umax = (pz0-pz)/dvdz;
			else umax = (px0-px)/dvdx;

	    		for(nit=0; nit<nitmax; nit++)   {
				if(nit==0)   {
					u     =  0.;
						px2   = px0*px0;
					pz2   = pz0*pz0;
					alpha = c2*px2 + c1*pz2;
					betaa = (w1*px2 - w2*pz2)*(w1*px2 - w2*pz2);
					betab = 4*w3*px2*pz2;
					betat = sqrt(betaa+betab);
					fctn1 = 1/sqrt(.5*(alpha+betat));
					/*fctn1 = v0;*/
					u     = umax;
					px2   = (px0-dvdx*u)*(px0-dvdx*u);
					pz2   = (pz0-dvdz*u)*(pz0-dvdz*u);
					alpha = c2*px2 + c1*pz2;
					betaa = (w1*px2 - w2*pz2)*(w1*px2 - w2*pz2);
					betab = 4*w3*px2*pz2;
					betat = sqrt(betaa+betab);
					fctn2 = 1/sqrt(.5*(alpha+betat));
					/*fctn2 = v;*/

		    		timenew = 0.5*umax*(fctn2+fctn1);
		    		it	= 1;
				}
				else   {
		    			tnm = 1./it;
		    			del = umax*tnm;
		    			u   = 0.5*del;

		    			sum = 0.;
		    			for(j=0; j<it; j++)   {
						px2   = (px0-dvdx*u)*(px0-dvdx*u);
						pz2   = (pz0-dvdz*u)*(pz0-dvdz*u);
						alpha = c2*px2 + c1*pz2;
						betaa = (w1*px2 - w2*pz2)*(w1*px2 - w2*pz2);
						betab = 4*w3*px2*pz2;
						betat = sqrt(betaa+betab);
						fctnu = 1/sqrt(.5*(alpha+betat));
						sum  += fctnu;
						u    += del;
		    			}
		    			timenew = 0.5*(timenew+umax*sum*tnm);
		    			it	= 2*it;
				}
				if(ABS(timenew-time)<epst)   {
		    			time = ABS(timenew);
		    			break;
				}
				time = timenew;
	    		}
			*t = ABS(time);

			/*calculating the geometrical spreading*/
			sign = umax/ABS(umax);
			*s   = si*sign;
			*c   = ci*sign;
			mx   = sqrt(f1)*((cw-ci)*cr+(sw-si)*sr);
			co2  = ci*ci;
			si2  = si*si;
			sc   = si*ci;
			dvcp = (.5*c2*sc-.5*c1*sc+.25*(2*(w1*si2-w2*co2)*(
				w1*sc+w2*sc)+4*sc*co2*w3-4*
				si2*sc*w3)/beta)/vcps; 
			*q   = ABS(aa*cos(atan(dvcp/vcps))*r*mx);
		}
	}

	/* if coordinates were rotated, unrotate cosine and sine */
	if (angxs != 0.) {
		temp  = cx*(*s) + sx*(*c);
		*c    = cx*(*c) - sx*(*s);
		*s    = temp;
	}
}
