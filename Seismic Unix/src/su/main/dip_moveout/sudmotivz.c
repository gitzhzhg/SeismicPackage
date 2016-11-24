/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUDMOTIVZ: $$Revision: 1.4 $ ; $Date: 2011/11/16 17:51:02 $		*/


#include "su.h"
#include "segy.h"
#include "header.h"


/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUDMOTIVZ - DMO for Transeversely Isotropic V(Z) media for common-offset",
"            gathers							",
"									",
" sudmotivz <stdin >stdout cdpmin= cdpmax= dxcdp= noffmix= [...]	",
"									",
" Required Parameters:							",
" cdpmin         minimum cdp (integer number) for which to apply DMO	",
" cdpmax         maximum cdp (integer number) for which to apply DMO	",
" dxcdp          distance between adjacent cdp bins (m)			",
" noffmix        number of offsets to mix (see notes)			",
"									",
" Optional Parameters:							",
" vnfile=        binary (non-ascii) file containing NMO interval	",
"                  velocities (m/s)					",
" vfile=         binary (non-ascii) file containing interval velocities	(m/s)",
" etafile=       binary (non-ascii) file containing eta interval values (m/s)",
" tdmo=0.0       times corresponding to interval velocities in vdmo (s)	",
" vndmo=1500.0   NMO interval velocities corresponding to times in tdmo (m/s)",
" vdmo=vndmo    interval velocities corresponding to times in tdmo (m/s)",
" etadmo=1500.0  eta interval values corresponding to times in tdmo (m/s)",
" fmax=0.5/dt    maximum frequency in input traces (Hz)			",
" smute=1.5      stretch mute used for NMO correction			",
" speed=1.0      twist this knob for speed (and aliasing)		",
" verbose=0      =1 for diagnostic print				",
"									",
" Notes:								",
" Input traces should be sorted into common-offset gathers.  One common-",
" offset gather ends and another begins when the offset field of the trace",
" headers changes.							",
"									",
" The cdp field of the input trace headers must be the cdp bin NUMBER, NOT",
" the cdp location expressed in units of meters or feet.		",
"									",
" The number of offsets to mix (noffmix) should typically equal the ratio of",
" the shotpoint spacing to the cdp spacing.  This choice ensures that every",
" cdp will be represented in each offset mix.  Traces in each mix will	",
" contribute through DMO to other traces in adjacent cdps within that mix.",
"									",
" vnfile, vfile and etafile should contain the regularly sampled interval",
" values of NMO velocity, velocity and eta respectivily as a		",
" function of time.  If, for example, vfile is not supplied, the interval",
" velocity function is defined by linear interpolation of the values in the",
" tdmo and vdmo arrays.  The times in tdmo must be monotonically increasing.",
" If vfile or vdmo are not given it will be equated to vnfile or vndmo. ",
"									",
" For each offset, the minimum time to process is determined using the	",
" smute parameter.  The DMO correction is not computed for samples that	",
" have experienced greater stretch during NMO.				",
"									",
" Trace header fields accessed:  nt, dt, delrt, offset, cdp.		",
NULL};
/**************** end self doc ********************************/

/* Credits:
 *	CWP: Tariq Alkhalifah, Craig Artley
 *
 * Technical Reference:
 *
 *	Alkhalifah, T., 1994,
 *	Transformation to zero offset in transversely isotropic media,
 *	Center for Wave Phenomena, CWP-162.
 *
 *	Alkhalifah, T., Tsvankin, Ilya, 1994,
 *	Velocity analysis for transversely isotropic media,
 *	published at Center for Wave Phenomena report CWP-145.
 *
 *	Artley, C., 1992,
 *	Dip-moveout processing for depth-variable velocity,
 *	M.S. Thesis, Colorado School of Mines;
 *	also published at Center for Wave Phenomena report CWP-115.
 *	
 */

#define NPTAB 401
#define NITER 10
#define TOL 1.0e-4

/* function prototypes for functions defined below */
void dmooff (float **x, float **a, float **s, float **tau,
	 int nttab, float dt, float ft,
	int nptab, float dptab, float fp, float lp,
	int nx, float dx, float offset, int nt,
	float *vrms, float fmax, float smute, float speed, int verbose,
	float **q);
void dmomap (float **x, float **a, float **tau, float **s, int nt, float dt, float ft,
	int nptab, float dptab, int np, float dp2, float fp, float lp,
	int itn, float tn, float h, float *vrms,
	int verbose, float **t0table);
void dmoapply (int nx, float dx, int nt, float dt, int np, float dp2,
	float h, float **tntable, int *it0min, int *nt0,
	float tmin, float fmax, float **qx);
void jakub1k (float k, float h, int np, float dp2, int nt, float dt, float ft,
	float **tntable, int *it0min, int *nt0, float fmax, complex *qq);
float tmute(float *vrms, int nt, float dt, float ft, float h, float smute);
void getX (float tsg, float h, float p0, float v, float *vars);
void getFJ (float **x, float **a, float **tau, float **s,
	int nt, int np, float dt, float dp, float tsg, float h, float p0,
	float *vars, float *eqns, float **jacob);
void intder (float **f, int nx, float dx, float fx,
	int ny, float dy, float fy, float x, float y,
	float *fxy, float *dfdx, float *dfdy);
void rayvt (float p, int nt, float dt,
	float a1111[], float a3333[], float a1313[], float a1133[],
	float tau[], float x[], float a[], float s[]);

segy tr,tro;

int
main(int argc, char **argv)
{
	int nt;		/* number of time samples per trace */
	float dt;	/* time sampling interval */
	float ft;	/* time of first sample */
	int it;		/* time sample index */
	int cdpmin;	/* minimum cdp to process */
	int cdpmax;	/* maximum cdp to process */
	float dx;	/* cdp sampling interval */
	int nx;	        /* number of cdps to process */
	int nxfft;	/* number of cdps after zero padding for fft */
	int nxpad;	/* minimum number of cdps for zero padding */
	int ix;		/* cdp index, starting with ix=0 */
	int noffmix;	/* number of offsets to mix */
	float *tdmo;	/* times at which rms velocities are specified */
	float *vdmo,*etadmo,*vndmo;/* rms velocities at times specified in tdmo */
	int ntdmo;	/* number tnmo values specified */
	int itdmo;	/* index into tnmo array */
	int nvdmo,netadmo,nvndmo;	/* number vnmo values specified */
	float fmax;	/* maximum frequency */
	float *vt,*etat,*vnt;	/* uniformly sampled v(t) */
	float *vrms;	/* uniformly sampled vrms(t) */
	float **q;	/* traces for one offset - common-offset gather */
	float **qmix;	/* DMO-corrected and mixed traces to be output */
	float offset;	/* source-receiver offset of current trace */
	float oldoffset;/* offset of previous trace */
	int noff;	/* number of offsets processed in current mix */
	int ntrace;	/* number of traces processed in current mix */
	int itrace;	/* trace index */
	int gottrace;	/* non-zero if an input trace was read */
	int done;	/* non-zero if done */
	int verbose;	/* =1 for diagnostic print */
	char *vfile=""; /* name of interval velocity file */
	char *vnfile="";/* name of interval NMO velocity file */
	char *etafile="";/* name of interval eta velocity file */
	FILE *hfp;	/* file pointer for temporary header file */
	int nttab;	/* number of times in ray tables */
	int nptab=NPTAB;/* number of ray parameters in ray tables */
	int ip;		/* ray parameter counter */
	float smute;	/* NMO stretch mute */
	float t;	/* time */
	float sum;	/* sum accumulator */
	float lt;	/* last time */
	float lttab;	/* last time in ray tables */
	float p;	/* ray parameter */
	float fp;	/* first ray parameter */
	float lp;	/* last ray parameter */
	float dptab;	/* ray parameter sampling interval in ray tables */
	float speed;	/* speed knob --- twist for speed and aliasing */
	float **x;	/* table [nptab][nttab] of horizontal distance ray */
	float **tau;	/* table [nptab][nttab] of time depth of ray */
	float **a;	/* table [nptab][nttab] of propagation angle of ray */
	float *a1111,*a3333,*a1313,*a1133,delta,vnt2,vt2,**s;

	/* hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = tr.dt/1000000.0;
	ft = tr.delrt/1000.0;
	if (ft!=0.0) err("ft=%f --- nonzero ft not yet implemented!");
	offset = tr.offset;

	/* get parameters */
	if (!getparint("cdpmin",&cdpmin)) err("must specify cdpmin");
	if (!getparint("cdpmax",&cdpmax)) err("must specify cdpmax");
	if (cdpmin>cdpmax) err("cdpmin must not be greater than cdpmax");
	if (!getparfloat("dxcdp",&dx)) err("must specify dxcdp");
	if (!getparint("noffmix",&noffmix)) err("must specify noffmix");
	if (!getparfloat("fmax",&fmax)) fmax = 0.5/dt;
	if (!getparint("verbose",&verbose)) verbose = 0;
	if (!getparfloat("speed",&speed)) speed = 1.0;
	if (!getparfloat("smute",&smute)) smute = 1.5;

	/* allocate space for velocity arrays */
	nttab = 2*nt-1;
        vt = ealloc1float(nttab);
	vnt = ealloc1float(nttab);
	etat = ealloc1float(nttab);
	a1111 = ealloc1float(nttab);
	a3333 = ealloc1float(nttab);
	a1313 = ealloc1float(nttab);
	a1133 = ealloc1float(nttab);
	vrms = ealloc1float(nttab);

        /* determine NMO velocity function vnm(t) */
        if (!getparstring("vnfile",&vnfile)) {
                ntdmo = countparval("tdmo");
                if (ntdmo==0) ntdmo = 1;
                tdmo = ealloc1float(ntdmo);
                if (!getparfloat("tdmo",tdmo)) tdmo[0] = 0.0;
                nvndmo = countparval("vndmo");
                if (nvndmo==0) nvndmo = 1;
                if (nvndmo!=ntdmo) err("number of tdmo and vndmo must be equal");
                vndmo = ealloc1float(nvndmo);
                if (!getparfloat("vndmo",vndmo)) vndmo[0] = 1500.0;
                for (itdmo=1; itdmo<ntdmo; ++itdmo)
                        if (tdmo[itdmo]<=tdmo[itdmo-1])
                                err("tdmo must increase monotonically");
                for (it=0,t=0.0; it<nt; ++it,t+=dt)
                        intlin(ntdmo,tdmo,vndmo,vndmo[0],vndmo[ntdmo-1],
                                1,&t,&vnt[it]);
        } else {
                if (fread(vnt,sizeof(float),nt,fopen(vnfile,"r"))!=nt)
                        err("cannot read %d velocities from file %s",nt,vnfile);
        }

	/* constant extrapolation of interval velocities */
	for (it=nt; it<nttab; ++it)
		vnt[it] = vnt[nt-1];

	/* determine eta function eta(t) */
        if (!getparstring("etafile",&etafile)) {
                ntdmo = countparval("tdmo");
                if (ntdmo==0) ntdmo = 1;
                tdmo = ealloc1float(ntdmo);
                if (!getparfloat("tdmo",tdmo)) tdmo[0] = 0.0;
                netadmo = countparval("etadmo");
                if (netadmo==0) netadmo = 1;
                if (netadmo!=ntdmo) err("number of tdmo and etadmo must be equal");
                etadmo = ealloc1float(netadmo);
                if (!getparfloat("etadmo",etadmo)) etadmo[0] = 0.0;
                for (itdmo=1; itdmo<ntdmo; ++itdmo)
                        if (tdmo[itdmo]<=tdmo[itdmo-1])
                                err("tdmo must increase monotonically");
                for (it=0,t=0.0; it<nt; ++it,t+=dt)
                        intlin(ntdmo,tdmo,etadmo,etadmo[0],etadmo[ntdmo-1],
                                1,&t,&etat[it]);
        } else {
                if (fread(etat,sizeof(float),nt,fopen(etafile,"r"))!=nt)
                        err("cannot read %d velocities from file %s",nt,etafile);
        }

	/* constant extrapolation of interval velocities */
	for (it=nt; it<nttab; ++it)
		etat[it] = etat[nt-1];

	/* determine velocity function v(t) */
        if (!getparstring("vfile",&vfile)) {
                ntdmo = countparval("tdmo");
                if (ntdmo==0) ntdmo = 1;
                tdmo = ealloc1float(ntdmo);
                if (!getparfloat("tdmo",tdmo)) tdmo[0] = 0.0;
                nvdmo = countparval("vdmo");
                if (nvdmo!=ntdmo){ 
			for (it=0; it<nt; ++it)
				vt[it] = vnt[it];
		} else {
                	vdmo = ealloc1float(nvdmo);
                	if (!getparfloat("vdmo",vdmo)) vdmo[0] = vnt[0];
                	for (itdmo=1; itdmo<ntdmo; ++itdmo)
                        	if (tdmo[itdmo]<=tdmo[itdmo-1])
                                	err("tdmo must increase monotonically");
                	for (it=0,t=0.0; it<nt; ++it,t+=dt)
                        	intlin(ntdmo,tdmo,vdmo,vdmo[0],vdmo[ntdmo-1],
                                	1,&t,&vt[it]);
		}
        } else {
                if (fread(vt,sizeof(float),nt,fopen(vfile,"r"))!=nt)
                        err("cannot read %d velocities from file %s",nt,vfile);
        }
        checkpars();

	/* constant extrapolation of interval velocities */
	for (it=nt; it<nttab; ++it)
		vt[it] = vt[nt-1];

	/* compute vrms */
	vrms[0] = vnt[0];
	for (it=1,sum=0.0; it<nttab; ++it) {
		sum += vnt[it-1]*vnt[it-1];
			vrms[it] = sqrt(sum/it);
	}
        
	/*convert eta and NMO velocity values to elastic coeffecients*/
	for(it=0; it<nttab; ++it){
		vnt2   =vnt[it]*vnt[it];
		vt2    =vt[it]*vt[it];
		a1111[it]=vnt2*(1+2*etat[it]);
		a3333[it]=vt2;
		a1313[it]=0.25*vt2;
		delta= 0.5*(vnt2/vt2-1);
		a1133[it]=sqrt(2*delta*a3333[it]*
			(a3333[it]-a1313[it])+(a3333[it]-
			a1313[it])*(a3333[it]-a1313[it]))-a1313[it];
	}



	/* compute half-velocities */
        for (it=0; it<nttab; ++it){
                a1111[it] *= 0.25;
		a3333[it] *= 0.25;
		a1313[it] *= 0.25;
		a1133[it] *= 0.25;
		vt[it]    *= 0.5;
	}

	/* compute extreme values */
	ft = 0.0;
	lt = ft+(nt-1)*dt;
	lttab = ft+(nttab-1)*dt;
	fp = 0.0;
	lp = 1.0/sqrt(a1111[0]);

	/* compute p sampling for ray tables */
	dptab = lp/(nptab-1);

	/* allocate space for ray tables */
	tau = ealloc2float(nttab,nptab);
	x = ealloc2float(nttab,nptab);
	a = ealloc2float(nttab,nptab);
	s = ealloc2float(nttab,nptab);

	/* compute ray tables tau(p,t), x(p,t), a(p,t) */
	if (verbose) fprintf(stderr,"Computing ray tables...\n");
	for (ip=0,p=fp; ip<nptab; ++ip,p+=dptab)
		rayvt(p,nttab,dt,a1111,a3333,a1313,
			a1133,tau[ip],x[ip],a[ip],s[ip]);
		
	if (verbose) fprintf(stderr,"Ray tables ready...\n");
	free1float(vt);
	free1float(vnt);
	free1float(etat);
	free1float(a1111);
	free1float(a3333);
	free1float(a1133);
	free1float(a1313);
	

	/* determine number of cdps to process */
	nx = cdpmax-cdpmin+1;
	
	/* allocate and zero common-offset gather q(t,x) */
	nxpad = 0.5*ABS(offset/dx);
	nxfft = npfar(nx+nxpad);
	q = ealloc2float(nt,nxfft+2);
	for (ix=0; ix<nxfft; ++ix)
		for (it=0; it<nt; ++it)
			q[ix][it] = 0.0;

	/* allocate and zero offset mix accumulator qmix(t,x) */
	qmix = ealloc2float(nt,nx);
	for (ix=0; ix<nx; ++ix)
		for (it=0; it<nt; ++it)
			qmix[ix][it] = 0.0;
		
	/* open temporary file for headers */
	hfp = tmpfile();
	
	/* initialize */
	oldoffset = offset;
	gottrace = 1;
	done = 0;
	ntrace = 0;
	noff = 0;

	/* loop over traces */
	do {
		/* if got a trace, determine offset */
		if (gottrace) offset = tr.offset;
		
		/* if an offset is complete */
		if ((gottrace && offset!=oldoffset) || !gottrace) {

			/* do dmo for old common-offset gather */
			dmooff(x,a,tau,s,nttab,dt,ft,nptab,dptab,fp,lp,
				nx,dx,oldoffset,nt,vrms,fmax,smute,speed,
				verbose,q);
			
			/* add dmo-corrected traces to mix */
			for (ix=0; ix<nx; ++ix)
				for (it=0; it<nt; ++it)
					qmix[ix][it] += q[ix][it];
			
			/* count offsets in mix */
			++noff;
							
			/* free space for old common-offset gather */
			free2float(q);
			
			/* if beginning a new offset */
			if (offset!=oldoffset) {
				
				/* allocate space for new offset */
				nxpad = 0.5*ABS(offset/dx);
				nxfft = npfar(nx+nxpad);
				q = ealloc2float(nt,nxfft+2);
				for (ix=0; ix<nxfft; ++ix)
					for (it=0; it<nt; ++it)
						q[ix][it] = 0.0;
			}
		}
		
		/* if a mix of offsets is complete */
		if (noff==noffmix || !gottrace) {
			
			/* rewind trace header file */
			efseeko(hfp,(off_t) 0,SEEK_SET);
			
			/* loop over all output traces */
			for (itrace=0; itrace<ntrace; ++itrace) {
			
				/* read trace header and determine cdp index */
				efread(&tro,HDRBYTES,1,hfp);
				
				/* get dmo-corrected data */
				memcpy((void *) tro.data,
					(const void *) qmix[tro.cdp-cdpmin],
					nt*sizeof(float));
				
				/* write output trace */
				puttr(&tro);
			}
			
			/* report */
			if (verbose) 
				fprintf(stderr,"\tCompleted mix of "
					"%d offsets with %d traces\n",
					noff,ntrace);
			
			/* if no more traces, break */
			if (!gottrace) break;
			
			/* rewind trace header file */
			efseeko(hfp,(off_t) 0,SEEK_SET);
			
			/* reset number of offsets and traces in mix */
			noff = 0;
			ntrace = 0;
			
			/* zero offset mix accumulator */
			for (ix=0; ix<nx; ++ix)
				for (it=0; it<nt; ++it)
					qmix[ix][it] = 0.0;
		}
			
		/* if cdp is within range to process */
		if (tr.cdp>=cdpmin && tr.cdp<=cdpmax) {
	
			/* save trace header and update number of traces */
			efwrite(&tr,HDRBYTES,1,hfp);
			++ntrace;

			/* remember offset */
			oldoffset = offset;

			/* get trace samples */
			memcpy((void *) q[tr.cdp-cdpmin],
				(const void *) tr.data, nt*sizeof(float));
		}

		/* get next trace (if there is one) */
		if (!gettr(&tr)) gottrace = 0;
		
	} while (!done);

	return(CWP_Exit());
}

void dmooff (float **x, float **a, float **tau, float **s, int nttab, float dt, float ft,
	int nptab, float dptab, float fp, float lp,
	int nx, float dx, float offset, int nt,
	float *vrms, float fmax, float smute, float speed, int verbose,
	float **q)
/***************************************************************************
Compute and apply DMO correction to one common offset section.
****************************************************************************
Input:
x	array[nptab][nttab] of horizontal position of ray 
a	array[nptab][nttab] of propagation angle along ray
tau	array[nptab][nttab] of time depth of ray
s	array[nptab][nttab] of slowness of ray
nttab	number of times in ray tables
dt	time sampling interval
ft	first time (must be zero for now)
nptab	number of ray parameters in tables
dptab	ray parameter sampling interval in tables
fp	first ray parameter
lp	last ray parameter
nx	number of traces in common offset section
dx	trace sampling interval
offset	source-receiver full offset
nt	number of time samples
vrms	array[nttab] of rms velocity as function of two-way vertical time
fmax	maximum frequency to process
smute	NMO stretch mute.  Samples experiencing greater NMO stretch are muted
speed	knob to increase speed and aliasing by coarsening the slope sampling
verbose	flag for diagnostic info
q	array[nx][nt] of input common offset section

Output:
q	array[nx][nt] of output common offset section (DMO corrected)
****************************************************************************
Author: Craig Artley and Tariq Alkhalifah, Colorado School of Mines, 94
****************************************************************************/
{
	int it,ntn,itn,itnmin,itnmax,np,npdmo,ip,it0max,*it0min,*nt0;
	float h,lt,tn,ftn,dtn,dp2,tmin,**t0table,**tntable;

	/* compute minimum time based on stretch mute */
	tmin = tmute(vrms,nt,dt,ft,0.5*offset,smute);
	
	/* compute p sampling required to avoid aliasing */
	lt = (nt-1)*dt;
	if (tmin>=lt)
		err("tmin=%f >= lt=%f:  no data survive the mute!\n",tmin,lt);

	dtn = 10.0*dt;
	ftn = tmin;
	ntn = NINT((lt-ftn)/dtn)+1;
	dp2 = tmin/(fmax*0.25*offset*offset);
	dp2 *= speed;
	np = NINT(lp*lp/dp2)+1;

	/* allocate space for dmo mapping tables */
	it0min = ealloc1int(np);
	nt0 = ealloc1int(np);
	t0table = ealloc2float(ntn,np);
	tntable = ealloc2float(nt,np);

	/* zero the tables */
	for (ip=0; ip<np; ++ip)
		for (itn=0; itn<ntn; ++itn)
			t0table[ip][itn] = 0.0;
	for (ip=0; ip<np; ++ip)
		for (it=0; it<nt; ++it)
			tntable[ip][it] = 0.0;
	
	/* compute (unsigned) half-offset */
	h = ABS(offset/2.0);

	/* loop over nmo times tn */
	for (itn=0,tn=ftn; itn<ntn; ++itn,tn+=dtn) {

		/* compute dmo mapping t0(p) for this tn */
		dmomap(x,a,tau,s,nttab,dt,ft,nptab,dptab,np,dp2,fp,lp,
			itn,tn,h,vrms,verbose,t0table);
	}

	/* inverse interpolation to get tn(t0,p) */
	for (ip=0; ip<np; ++ip) {
		for (itn=0; itn<ntn; ++itn)
			if (t0table[ip][itn]>0.0) break;
		itnmin = itn;
		
		it0min[ip] = NINT(t0table[ip][itnmin]/dt);
		if (it0min[ip]>=nt) it0min[ip] = nt-1;

		for (itn=ntn-1; itn>=0; --itn)
			if (t0table[ip][itn]>0.0) break;
		itnmax = itn;
		if (itnmax <= itnmin) break;

		it0max = NINT(t0table[ip][itnmax]/dt);
		if (it0max>=nt) it0max = nt-1;

		nt0[ip] = it0max-it0min[ip]+1;

		/* check that table is monotonically increasing */
		for (itn=itnmin+1; itn<itnmax; ++itn)
			if (t0table[ip][itn]<=t0table[ip][itn-1] && verbose)
				fprintf(stderr,
					"t0table not monotonically "
					"increasing---extrapolating\n");

		yxtoxy(itnmax-itnmin+1,dtn,ftn+itnmin*dtn,t0table[ip]+itnmin,
			nt0[ip],dt,it0min[ip]*dt,
			ftn+itnmin*dtn,ftn+itnmax*dtn,tntable[ip]);

		/* extend tntable by linear extrapolation */
		for (it=nt0[ip]; it<nt; ++it) {
			tntable[ip][it] = 2.0*tntable[ip][it-1]
				-tntable[ip][it-2];
		}
		nt0[ip] = nt-it0min[ip];
	}

	/* reset the number of ray parameters for dmo */
	npdmo = MIN(ip,np);

	/* free space */
	free2float(t0table);

	/* dmo by dip decomposition */
	dmoapply(nx,dx,nt,dt,npdmo,dp2,h,tntable,it0min,nt0,tmin,fmax,q);

	/* free space */
	free1int(it0min);
	free1int(nt0);
	free2float(tntable);

}

void dmomap (float **x, float **a, float **tau, float **s, int nt, float dt, float ft,
	int nptab, float dptab, int np, float dp2, float fp, float lp,
	int itn, float tn, float h, float *vrms,
	int verbose, float **t0table)
/***************************************************************************
Compute DMO correction for one NMO time, tn, using Newton's method to solve
system of equations.
****************************************************************************
Input:
x	array[nptab][nt] of horizontal position of ray 
a	array[nptab][nt] of propagation angle along ray
tau	array[nptab][nt] of time depth of ray
s	array[nptab][nttab] of slowness of ray
nt	number of times in ray tables
dt	time sampling interval
ft	first time (must be zero for now)
nptab	number of ray parameters in ray tables
dptab	ray parameter sampling interval in ray tables
np	number of ray parameters for which to find DMO mapping
dp2	sampling interval in (ray parameter)^2 in DMO mapping table
fp	first ray parameter
lp	last ray parameter
itn	index of NMO time for which to compute DMO mapping
tn	NMO time for which to compute DMO mapping
h	source-receiver half offset
vrms	array[nt] of rms velocity as function of two-way vertical time
verbose	flag for diagnostic info (does nothing here)

Output:
t0table	array[np][ntn] containing DMO mapping t0(tn,p0)
	One call to this function fills in the itn'th row of this table.
****************************************************************************
Author: Craig Artley and Tariq Alkhalifah, Colorado School of Mines, 94
****************************************************************************/
{
	int ip,i,neqns=5,niter=NITER,iter,*ipvt;
	float p2,vtn,tsg,p0,tautp,dtaudt,dtaudp,error,rcond,
		lt,tol=TOL,*vars,*eqns,*work,t0,x0,**jacob;

	/* compute last time in ray tables */
	lt = ft+(nt-1)*dt;

	/* allocate space for system of equations */
	vars = ealloc1float(neqns);		/* variables X */
	eqns = ealloc1float(neqns);		/* F(X) */
	jacob = ealloc2float(neqns,neqns);	/* Jacobian dF_i/dx_j */
	work = ealloc1float(neqns);		/* work array for sgeco() */
	ipvt = ealloc1int(neqns);	/* pivot index array for sgeco() */

	/* calculate tsg from tn via Dix approximation */
	vtn = vrms[(int)(tn/dt)];
	tsg = sqrt(tn*tn+4*h*h/(vtn*vtn));

	/* get initial values for parameters */
	getX(tsg,h,fp,vtn,vars);

	for (ip=0,p2=fp; ip<np; ++ip,p2+=dp2) {

		/* compute p0 */
		p0 = sqrt(p2);

		/* use Newton's method to improve the guess */
		for (iter=0; iter<niter; ++iter) {
	
			/* calculate F(X) and J(X) */
			getFJ(x,a,tau,s,nt,nptab,dt,dptab,tsg,h,p0,
				vars,eqns,jacob);

			/* compute total error */
			for (i=0,error=0.0; i<neqns; ++i)
				error += ABS(eqns[i]);

			/* if iteration has converged, break */
			if (error<=tol) break;

			/* solve system */
			sgeco(jacob,neqns,ipvt,&rcond,work);
			sgesl(jacob,neqns,ipvt,eqns,0);

			/* update X */
			if (rcond!=0.0) {
				for (i=0; i<neqns; ++i)
					vars[i] -= eqns[i];
			} else {
				/* system is ill-conditioned, break */
				break;
			}

			/* clip ps and pg values to pmax if necessary */
			if (ABS(vars[1])>lp) {
				vars[1] = SGN(vars[1])*lp;
			}
			if (ABS(vars[2])>lp) {
				vars[2] = SGN(vars[2])*lp;
			}

			/* clip tg to 0<tg<tsg */
			if (vars[3]<0.0) {
				vars[3] = 0.0;
			}
			if (vars[3]>tsg) {
				vars[3] = tsg;
			}

			/* clip t0 values to 0<t0<lt */
			if (vars[4]<0.0) {
				vars[4] = 0.0;
			}
			if (vars[4]>lt) {
				vars[4] = lt;
			}

		}

		/* vars[] now contains the solution to the system */

		/* compute depth of reflection point */
		intder(tau,nt,dt,ft,nptab,dptab,fp,vars[4],ABS(p0),
			&tautp,&dtaudt,&dtaudp);

		/* if reflector is at surface, break */
		if (tautp<dt) {
			break;
		}

		/* if system is ill-conditioned, break */
		if (rcond==0.0) {
			break;
		}

		/* now have t0(x0) for this tsg, p0, h -- store it */
		x0 = vars[0];
		t0 = vars[4];
		t0table[ip][itn] = t0+p0*x0;

	}

	/* free space */
	free1float(vars);
	free1float(eqns);
	free2float(jacob);
	free1float(work);
	free1int(ipvt);
}

void dmoapply (int nx, float dx, int nt, float dt, int np, float dp2,
	float h, float **tntable, int *it0min, int *nt0,
	float tmin, float fmax, float **qx)
/*****************************************************************************
Apply DMO correction to common offset section using previously computed
mapping tables.
******************************************************************************
Input:
nx		number of traces in common offset section
dx		trace sampling interval
nt		number of time samples per trace
dt		time sampling interval
np		number of ray parameters in DMO mapping tables
dp2		ray parameter (squared) sampling interval
h		source-receiver half-offset
tntable		array[np][nt0] of DMO mapping table tn(t0,p0)
it0min		array[np] of index of first entry in DMO mapping table 
nt0		array[np] of number of t0's in DMO mapping table
fmax		maximum frequency of data (frequencies above fmax are muted)
qx		array[nx][nt] of input NMO corrected data

Output:
qx		array[nx][nt] of output DMO corrected data
******************************************************************************
Author: Craig Artley and Tariq Alkhalifah, Colorado School of Mines, 94
******************************************************************************/
{
	int ix,nxfft,it,itmin,ik,nk;
	float k,dk,fk,ft,scale;
	complex **qk;

	/* determine wavenumber sampling (for real to complex FFT) */
	nxfft = npfar(nx);
	nk = nxfft/2+1;
	dk = 2.0*PI/(nxfft*dx);
	fk = 0.0;

	ft = 0.0;

	/* pointers to complex q(t,k) */
	qk = (complex**)ealloc1(nk,sizeof(complex*));
	for (ik=0; ik<nk; ++ik)
		qk[ik] = (complex*)qx[0]+ik*nt;
	
	/* Fourier transform x to k */
	pfa2rc(-1,2,nt,nxfft,qx[0],qk[0]);

	/* loop over wavenumber */
	for (ik=0,k=fk; ik<nk; ++ik,k+=dk) {

		/* apply DMO correction */
		jakub1k(k,h,np,dp2,nt,dt,ft,tntable,
			it0min,nt0,fmax,qk[ik]);
	}

	/* Fourier transform k to x (including FFT scaling and mute) */
	pfa2cr(1,2,nt,nxfft,qk[0],qx[0]);
	scale = 1.0/nxfft;
	itmin = MIN(nt,NINT(tmin/dt));
	for (ix=0; ix<nx; ++ix) {
		for (it=0; it<itmin; ++it)
			qx[ix][it] = 0.0;
		for (it=itmin; it<nt; ++it)
			qx[ix][it] *= scale;
	}

	/* free space */
	free1(qk);
}

void jakub1k (float k, float h, int np, float dp2, int nt, float dt, float ft,
	float **tntable, int *it0min, int *nt0, float fmax, complex *qq)
/*****************************************************************************
Jakubowicz's dip decomposition DMO for one wavenumber
******************************************************************************
Input:
k		wavenumber
h		half-offset
np		number of ray paramaters
dp2		sampling interval in (ray parameter)^2
nt		number of time samples
dt		time sampling interval
ft		first time sample
qq		complex array[nt] of input NMO corrected data 

Output:
qq		complex array[nt] of output DMO corrected data
******************************************************************************
Author: Craig Artley, Colorado School of Mines, 08/29/91
******************************************************************************/
{
	int ntfft,nw,it,iw,iwlm,iwhm,iwlp,iwhp,ip;
	float p,ph,pl,ph2,dw,fw,wl,wh,scale;
	complex czero=cmplx(0.0,0.0),*qn,*q0;

	/* determine frequency sampling */
	ntfft = npfa(nt);
	nw = ntfft;
	dw = 2.0*PI/(ntfft*dt);
	fw = -PI/dt;
	
	/* allocate workspace */
	qn = ealloc1complex(ntfft);
	q0 = ealloc1complex(ntfft);

	/* zero w0 domain accumulator */
	for (iw=0; iw<nw; ++iw) {
		qn[iw] = czero;
		q0[iw] = czero;
	}

	/* loop over zero-offset ray parameter */
	for (ip=0,p=pl=0.0,ph2=dp2; ip<np; ++ip,ph2+=dp2) {

		/* update ray parameter */
		ph = sqrt(ph2);

		/* determine limits of w0 for this wavenumber and slope */
		wl = k/ph;
		if (pl==0.0) {
			wh = 2.0*PI*fmax;
		} else {
			wh = MIN(k/pl,2.0*PI*fmax);
		}

		/* compute limits of sum for -p */
		iwlm = (int) ((-wl-fw)/dw);
		iwhm = (int) ((-wh-fw)/dw);
		if (iwhm<0) iwhm = 0;
		++iwhm;

		/* compute limits of sum for +p */
		iwlp = (int) ((wl-fw)/dw);
		iwhp = (int) ((wh-fw)/dw);
		if (iwhp>=nw) iwhp = nw-1;
		++iwlp;

		/* if this range of frequencies contributes to the sum */
		if ((iwhm<=iwlm) || (iwlp<=iwhp)) {
		
			/* interpolate, padding with zeros */
			for (it=0; it<it0min[ip]; ++it)
				qn[it] = czero;
			ints8c(nt,dt,ft,qq,czero,czero,nt0[ip],tntable[ip],
				qn+it0min[ip]);
			for (it=it0min[ip]+nt0[ip]; it<ntfft; ++it)
				qn[it] = czero;

			/* Fourier transform t to w, with w centered */
			for (it=1; it<ntfft; it+=2) {
				qn[it].r = -qn[it].r;
				qn[it].i = -qn[it].i;
			}
			pfacc(1,ntfft,qn);

			/* accumulate into q0 for slopes near -p */
			for (iw=iwhm; iw<=iwlm; ++iw) {
				q0[iw].r += qn[iw].r;
				q0[iw].i += qn[iw].i;
			}

			/* accumulate into q0 for slopes near +p */
			for (iw=iwlp; iw<=iwhp; ++iw) {
				q0[iw].r += qn[iw].r;
				q0[iw].i += qn[iw].i;
			}
		}

		/* roll ray parameters */
		pl = p;
		p = ph;
	}

	/* Fourier transform w to t, with w centered */
	/* (including FFT scaling and division by 2 for plane filling) */
	pfacc(-1,ntfft,q0);
	scale = 0.5/ntfft;
	for (it=0; it<nt; it+=2) {
		qq[it].r = q0[it].r*scale;
		qq[it].i = q0[it].i*scale;
	}
	for (it=1; it<nt; it+=2) {
		qq[it].r = -q0[it].r*scale;
		qq[it].i = -q0[it].i*scale;
	}

	/* free workspace */
	free1complex(qn);	
	free1complex(q0);	
}
	
float tmute (float *vrms, int nt, float dt, float ft, float h, float smute)
/***************************************************************************
Returns MUTE Time for given rms velocity function and half-offset.
****************************************************************************
Input:
vrms	array[nt] of rms velocity as a function of NMO time
nt	number of times
dt	time sampling interval
ft	first time
h	source-receiver half-offset
smute	maximum allowable NMO stretch t/tn [(100+stretch_percentage)/100]

Return:
	gretest time that is subject to given NMO stretch mute
****************************************************************************
Author: Craig Artley, Colorado School of Mines, 09/28/91
****************************************************************************/
{
	int it;
	float tn,*t;

	/* allocate space */
	t = ealloc1float(nt);

	for (it=0,tn=ft; it<nt; ++it,tn+=dt)
		t[it] = sqrt(tn*tn+4.0*h*h/(vrms[it]*vrms[it]));

	/* find greatest nmo time that is subject to stretch mute */
	for (it=nt-1; it>0; --it)
		if (dt/(t[it]-t[it-1])>smute) break;

	/* free space */
	free1float(t);

	return ft+it*dt;
}

void getX (float tsg, float h, float p0, float v, float *vars)
/***************************************************************************
GET trial solution X to system of equations assuming constant velocity.
****************************************************************************
Input:
tsg	recording time (from source to receiver)
h	source-receiver half-offset
p0	ray parameter of zero-offset ray
v	constant velocity

Output:
vars	array[5] of variables that satisfies the system for const. v
****************************************************************************
Author: Craig Artley, Colorado School of Mines, 09/21/91
****************************************************************************/
{
	float x,z,a2,b2,h2,x0,ps,pg,tg,t0,sine,tangent;

	/* compute constants that determine prestack migration ellipse */
	h2 = h*h;
	a2 = v*v*tsg*tsg/4.0;
	b2 = MAX(a2-h2,0.0);

	/* compute sine and tangent of propagation angle */
	sine = 0.5*p0*v;
	tangent = sine/sqrt(MAX(1.0-sine*sine,0.0));
	
	/* find reflection point (x,z) on prestack migration ellipse */
	x = tangent*sqrt(a2/(b2+tangent*tangent));
	z = sqrt(b2*MAX(1.0-x*x/a2,0.0));
	
	/* find zero-offset location */
	x0 = h2/a2*x;

	/* find times along receiver and zero-offset rays */
	tg = sqrt((h-x)*(h-x)+z*z)/v;
	t0 = sqrt((x0-x)*(x0-x)+z*z)*2.0/v;

	/* find ray parameters for source and receiver rays */
	ps = (x+h)/(v*v*(tsg-tg));
	pg = (x-h)/(v*v*tg);

	/* load trial solution into the variables array */
	vars[0] = x0;
	vars[1] = ps;
	vars[2] = pg;	
	vars[3] = tg;
	vars[4] = t0;
}

void getFJ (float **x, float **a, float **tau, float **s, int nt, int np,
	float dt, float dp, float tsg, float h, float p0,
	float *vars, float *eqns, float **jacob)
/***************************************************************************
Compute equation array and Jacobian matrix for system of equations for
a given set of parameters.
****************************************************************************
Input:
x	array[np][nt] of horizontal location of tabled rays
a	array[np][nt] of propagation angle of tabled rays
tau	array[np][nt] of travel-time depth of tabled rays
s	array[np][nt] of slownesses of tabled rays
nt	number of times in ray tables
np	number of ray parameters in ray tables
dt	time sampling interval in tables
dp	ray parameter sampling interval in tables
tsg	recording time (from source to receiver)
h	source-receiver half offset
p0	zero-offset ray parameter
vars	array[5] of variables of system of equations

Output:
eqns	array[5] of right-hand-sides of equations
jacob	pointer to array[5][5] of Jacobian partial derivatives
****************************************************************************
Author: Craig Artley and Tariq Alkhalifah, Colorado School of Mines, 94
****************************************************************************/
{
	int i,j,neqns=5;
	float x0,ps,pg,ts,tg,t0,ft=0.0,fp=0.0,ftp,dfdt,dfdp,sign,f0tp,
		df0dt,df0dp,stp,dsdt,dsdp,signp0;

	/* extract parameters from variables array */
	x0 = vars[0];
	ps = vars[1];
	pg = vars[2];
	tg = vars[3];
	t0 = vars[4];
	ts = MAX(tsg-tg,0.0);

	/* zero out F and J */
	for (i=0; i<neqns; ++i)
		eqns[i] = 0.0;
	for (j=0; j<neqns; ++j)
		for (i=0; i<neqns; ++i)
			jacob[j][i] = 0.0;

	/* insert constant terms into equations */
	eqns[0] += 2.0*h;
	eqns[1] += h-x0;
	jacob[0][1] -= 1.0;

	/* get x(ps,2*ts) and partial derivatives, load into equations */
	intder(x,nt,dt,ft,np,dp,fp,2.0*ts,ABS(ps),&ftp,&dfdt,&dfdp);
	sign = SGN(ps);
	eqns[0] -= sign*ftp;
	jacob[1][0] -= dfdp;
	jacob[3][0] += 2.0*sign*dfdt;

	/* get x(pg,2*tg) and partial derivatives, load into equations  */
	intder(x,nt,dt,ft,np,dp,fp,2.0*tg,ABS(pg),&ftp,&dfdt,&dfdp);
	sign = SGN(pg);
	eqns[0] += sign*ftp;
	eqns[1] += sign*ftp;
	jacob[2][0] += dfdp;
	jacob[2][1] += dfdp;
	jacob[3][0] += 2.0*sign*dfdt;
	jacob[3][1] += 2.0*sign*dfdt;

	/* get x(p0,t0) and partial derivatives, load into equations */
	intder(x,nt,dt,ft,np,dp,fp,t0,ABS(p0),&ftp,&dfdt,&dfdp);
	sign = SGN(p0);
	eqns[1] -= sign*ftp;
	jacob[4][1] -= sign*dfdt;

	/* get tau(ps,2*ts) and partial derivatives, load into equations */
	intder(tau,nt,dt,ft,np,dp,fp,2.0*ts,ABS(ps),&ftp,&dfdt,&dfdp);
	sign = SGN(ps);
	eqns[2] -= ftp;
	jacob[1][2] -= sign*dfdp;
	jacob[3][2] += 2.0*dfdt;

	/* get tau(pg,2*tg) and partial derivatives, load into equations */
	intder(tau,nt,dt,ft,np,dp,fp,2.0*tg,ABS(pg),&ftp,&dfdt,&dfdp);
	sign = SGN(pg);
	eqns[2] += ftp;
	eqns[3] += ftp;
	jacob[2][2] += sign*dfdp;
	jacob[2][3] += sign*dfdp;
	jacob[3][2] += 2.0*dfdt;
	jacob[3][3] += 2.0*dfdt;

	/* get tau(p0,t0) and partial derivatives, load into equations */
	intder(tau,nt,dt,ft,np,dp,fp,t0,ABS(p0),&ftp,&dfdt,&dfdp);
	sign = SGN(p0);
	eqns[3] -= ftp;
	jacob[4][3] -= dfdt;

	/* get a(ps,2*ts) and partial derivatives, load into equations */
	intder(a,nt,dt,ft,np,dp,fp,2.0*ts,ABS(ps),&ftp,&dfdt,&dfdp);
	intder(a,nt,dt,ft,np,dp,fp,t0,ABS(p0),&f0tp,&df0dt,&df0dp);
	intder(s,nt,dt,ft,np,dp,fp,2.0*ts,ABS(ps),&stp,&dsdt,&dsdp);
	sign = SGN(ps);
	signp0 = SGN(p0);
	eqns[4] -= stp*sin(sign*ftp-signp0*f0tp);
	jacob[1][4] += -sign*dsdp*sin(sign*ftp-signp0*f0tp)-
			stp*cos(sign*ftp-signp0*f0tp)*dfdp;
	jacob[3][4] += 2.0*(dsdt*sin(sign*ftp-signp0*f0tp)+
			sign*stp*cos(sign*ftp-signp0*f0tp)*dfdt);
	jacob[4][4] +=  2.*signp0*stp*cos(sign*ftp-signp0*f0tp)*df0dt;

	/* get a(pg,2*tg) and partial derivatives, load into equations */
	intder(a,nt,dt,ft,np,dp,fp,2.0*tg,ABS(pg),&ftp,&dfdt,&dfdp);
	intder(s,nt,dt,ft,np,dp,fp,2.0*tg,ABS(pg),&stp,&dsdt,&dsdp);
	sign = SGN(pg);
	eqns[4] -= stp*sin(sign*ftp-signp0*f0tp);
	jacob[2][4] += -sign*dsdp*sin(sign*ftp-signp0*f0tp)-
			stp*cos(sign*ftp-signp0*f0tp)*dfdp;
	jacob[3][4] -= 2.0*(dsdt*sin(sign*ftp-signp0*f0tp)+
			sign*stp*cos(sign*ftp-signp0*f0tp)*dfdt);
	jacob[4][4] +=  2.*signp0*stp*cos(sign*ftp-signp0*f0tp)*df0dt;
}

void intder (float **f, int nx, float dx, float fx,
	int ny, float dy, float fy, float x, float y,
	float *fxy, float *dfdx, float *dfdy)
/***************************************************************************
Finds f(x,y), df/dx, df/dy given function values on a mesh f[iy][ix],
via linear interpolation and finite differencing.
****************************************************************************
Input:
f	array[ny][nx] of sampled function values
nx	number of samples in x direction (fast dimension)
dx	x sampling interval
fx	first sample in x direction
ny	number of samples in y direction (slow dimension)
dy	y sampling interval
fy	first sample in y direction
x	x value at which to evaluate functions
y	y value at which to evaluate functions

Output:
fxy	interpolated function value f(x,y)
dfdx	partial derivative df/dx evaluated at (x,y)
dfdy	partial derivative df/dy evaluated at (x,y)
****************************************************************************
Note: Constant extrapolation of function and zero derivatives for points
	outside the table.  This is desirable, but somewhat awkward.
****************************************************************************
Author: Craig Artley, Colorado School of Mines, 08/31/91
****************************************************************************/
{
	int	ix0,ix1,iy0,iy1;
	float	x0i,y0i,fracx,fracy,lx,ly,f00,f10,f01,f11;

	/* compute x and y extent of table */
	lx = fx+(nx-1)*dx;
	ly = fy+(ny-1)*dy;

	if (x<=fx) {
		ix0 = ix1 = 0;
		fracx = 0.0;
	} else if (x>=lx) {
		ix0 = ix1 = nx-1;
		fracx = 1.0;
	} else {
		x0i = (x-fx)/dx;
		ix0 = x0i;
		ix1 = ix0+1;
		fracx = x0i-ix0;
	}

	if (y<=fy) {
		iy0 = iy1 = 0;
		fracy = 0;
	} else if (y>=ly) {
		iy0 = iy1 = ny-1;
		fracy = 1.0;
	} else {
		y0i = (y-fy)/dy;
		iy0 = y0i;
		iy1 = iy0+1;
		fracy = y0i-iy0;
	}

	/* get tabled function values nearest (x,y) */
	f00 = f[iy0][ix0];	f01 = f[iy1][ix0];
	f10 = f[iy0][ix1];	f11 = f[iy1][ix1];

	/* linear interpolation to find f(x,y) */
	*fxy = (1-fracx)*(1-fracy)*f00 + (1-fracx)*fracy*f01
		+ fracx*(1-fracy)*f10 + fracx*fracy*f11;

	/* interpolate and finite difference to find partial derivatives */
	*dfdx = ((1-fracy)*(f10-f00) + fracy*(f11-f01))/dx;
	*dfdy = ((1-fracx)*(f01-f00) + fracx*(f11-f10))/dy;
}

void rayvt (float p, int nt, float dt,
	float a1111[], float a3333[], float a1313[], float a1133[],
	float tau[], float x[], float a[], float s[]) 
/*****************************************************************************
trace ray for the given elastic coeffecient as a function of verical time, 
tau=ONE-way vertical time
******************************************************************************
Input:
p		ray parameter
nt		number of times (including t=0.0)
dt		time sampling interval
a1111		array[nt] of elastic coeffecient a1111 (a1111 = a1111(tau))
a3333		array[nt] of elastic coeffecient a3333 (a3333 = a3333(tau))
a1313		array[nt] of elastic coeffecient a1313 (a1313 = a1313(tau))
a1133		array[nt] of elastic coeffecient a1133 (a1133 = a1133(tau))

Output:
tau		array[nt] of vertical times tau(t)
x		array[nt] of horizontal distances x(t)
a		array[nt] of propagation angles a(t)
s  		array[nt] of slownesses s(t)
******************************************************************************
Notes:
Ray tracing begins at tau=x=t=0.0.  If the ray turns and reaches tau=0.0
at some time index it, then tau[it+1:nt-1] = tau[it], x[it+1:nt-1] = x[it],
and a[it+1:nt-1] = a[it].  In other words, constant extrapolation is used
to fill the remaining values in the output arrays.
******************************************************************************
Author:  Tariq Alkhalifah, Colorado School of Mines, 94
******************************************************************************/
{
	int it,jt,itau,turn;
	float at,taut,xt,taui,frac;
	float a1111t,a3333t,a1313t,a1133t;
	float f1,f2,f3,f4,f5,f6,eps,pz,pxz,px2,pz2,alpha,beta,gamma,det;
	float rad,signbeta,q,vp,gamma11,gamma33,gamma13,den,g11,g33,g13;
	float fac,dx,dz,dtau,sp;

	/* initialize ray tracing parameters */
	a1111t = a1111[0];
	a3333t = a3333[0];
	a1313t = a1313[0];
	a1133t = a1133[0];

	f1 = a1111t+a1313t;
	f2 = a3333t+a1313t;
	f3 = a1111t-a1313t;
	f4 = a3333t-a1313t;
	f5 = 2.*(a1133t+a1313t)*(a1133t+a1313t);
	f6 = 2;


	eps   = .00001;
	px2   = p*p;
	alpha = f2*f2-f4*f4;
	beta  = 2*((f1*f2+f3*f4-f5)*px2-f2*f6);
	gamma = f6*f6-(2.*f1*f6-(f1*f1-f3*f3)*px2)*px2;
	det   = beta*beta-4.*alpha*gamma;

	if (det<0) return;
	rad = sqrt(det);
	if(ABS(beta)>eps)   signbeta = ABS(beta)/beta;
	else                signbeta = 1.;
	q    = -.5*(beta+signbeta*rad);
	pz2  = gamma/q;
	if(pz2<0) return;
	sp   = sqrt(px2+pz2);
	vp   = 1/sp;
	pz   = sqrt(pz2);
	pxz  = p*pz;

	gamma11 = a1111t*px2+ a1313t*pz2;
	gamma33 = a3333t*pz2 + a1313t*px2;
	gamma13 = (a1133t+a1313t)*pxz;
	den     = 1/(gamma11+gamma33-2);
	g11     = (gamma33-1)*den;
	g33     = (gamma11-1)*den;
	g13     = -gamma13*den;
	fac=1/sqrt(a3333t);

	dx =  a1111t*p*g11+(a1133t+a1313t)*pz*g13+a1313t*g33*p;
	dz = a3333t*pz*g33+(a1133t+a1313t)*p*g13+a1313t*g11*pz;
	dtau =  MIN(1.0,dz*fac);

	at = acos(MIN(1.0,pz*vp));

	taut = xt = 0.0;
	tau[0] = taut;
	x[0] = xt;
	a[0] = at;
	s[0] = sp;
	turn=1;

	/* loop over times t */
	for (it=1; it<nt; ++it) {
		
		/* update vertical time */
		tau[it] = tau[it-1]+dt*dtau;

		/* if ray has returned to surface, break */
		if (tau[it]<=0.0) break;

		/* update horizontal distance */
		x[it] = x[it-1]+dx*dt;

		/* update velocity and derivative */
		if (it<nt-1) {
			taui = tau[it]/dt;
			itau = taui;
			frac = taui-itau;
			a1111t = (1.0-frac)*a1111[itau] + frac*a1111[itau+1];
			a3333t = (1.0-frac)*a3333[itau] + frac*a3333[itau+1];
			a1313t = (1.0-frac)*a1313[itau] + frac*a1313[itau+1];
			a1133t = (1.0-frac)*a1133[itau] + frac*a1133[itau+1];
		}

		f1 = a1111t+a1313t;
		f2 = a3333t+a1313t;
		f3 = a1111t-a1313t;
		f4 = a3333t-a1313t;
		f5 = 2.*(a1133t+a1313t)*(a1133t+a1313t);

		alpha = f2*f2-f4*f4;
		beta  = 2*((f1*f2+f3*f4-f5)*px2-f2*f6);
		gamma = f6*f6-(2.*f1*f6-(f1*f1-f3*f3)*px2)*px2;
		det   = beta*beta-4.*alpha*gamma;

		
		if (det<0){
		fprintf(stderr,"det=%f\n",det);
		return;
		}
		rad = sqrt(det);
		if(ABS(beta)>eps)   signbeta = ABS(beta)/beta;
		else                signbeta = 1.;
		q    = -.5*(beta+signbeta*rad);
		pz2  = gamma/q;
		if(pz2<0)
			turn *=-1;
		pz2 = ABS(pz2);
		pz = turn*sqrt(pz2);

		sp   = sqrt(px2+pz2);
		vp   = 1/sp;
		
		pxz  = p*pz;
		gamma11 = a1111t*px2+ a1313t*pz2;
		gamma33 = a3333t*pz2 + a1313t*px2;
		gamma13 = (a1133t+a1313t)*pxz;
		den     = 1/(gamma11+gamma33-2);
		g11     = (gamma33-1)*den;
		g33     = (gamma11-1)*den;
		g13     = -gamma13*den;
		fac=1/sqrt(a3333t);

		dx =  a1111t*p*g11+(a1133t+a1313t)*pz*g13+a1313t*g33*p;
		dz = a3333t*pz*g33+(a1133t+a1313t)*p*g13+a1313t*g11*pz;
		dtau =  dz*fac;

		at = acos(MIN(1.0,pz*vp));
		a[it] = at;
		s[it] = sp;
	}

	/* if ray returned to surface, extrapolate with surface values */
	for (jt=it; jt<nt; ++jt) {
		tau[jt] = tau[jt-1];
		x[jt] = x[jt-1];
		a[jt] = a[jt-1];
		s[jt] = s[jt-1];
	}
}

