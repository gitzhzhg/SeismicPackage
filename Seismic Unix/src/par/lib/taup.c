/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "taup.h"

/*********************** self documentation **********************/
/******************************************************************************
TAUP - Functions to perform forward and inverse taup transforms (radon or
	slant stacks) in t-x, F-K or F-X domains

fwd_FK_sstack		Performs forward taup tranaform in F-K domain, via
			8-point sinc interpolator. Only linear transform is
			possible. Very fast for taup transform of many traces.

fwd_tx_sstack		Performs forward taup transform in t-x domain.
			Only linear transform is implemented, but it is
			straight forward to implement other curves.

forward_p_transform	Performs forward tau-p transform in F-X domain. Uses
			Beylkin's approach. Not very fast but can compute
			linear parabolic or time-independent hyperbolic
			transform. Space coordinate does not need to be 
			uniformly sampled.

inv_FK_sstack		Performs inverse taup transform in F-K domain, via
			8-point sinc interpolation.

inv_tx_sstack		Performs inverse taup transform in t-x domain.

inverse_p_transform	Performs inverse taup transform in F-X domain (Beylkin's
			approach).

rho_filter		Computes rho filter in frequency domain for inverse
			t-x domain transform.

ga_xinterpolate        interpolate input data in space by placing a requested
			number of traces between each pair of input traces
*******************************************************************************
Function Prototypes:
void fwd_FK_sstack (float dt, int nt, int nx, float xmin, float dx, int np,
	float pmin, float dp, float fmin, float **traces, float **out_traces);

void fwd_tx_sstack (float dt, int nt, int nx, float xmin, float dx, int np,
	float pmin, float dp, float **traces, float **out_traces);

void forward_p_transform(int nx, int nt, float dt, float pmax, float pmin,
        float dp, float depthref, float f1, float f2, float freq1, float freq2,
        int lagc, int lent, int lenx, int xopt, int ninterp, int nwin,
        float prewhite, float interoff, float offref, int igopt, float dx,
        float fx, float pmula, float pmulb, float **in_traces,
        float **out_traces);

void inverse_p_transform(int nx, int nt, float dt, float pmax, float pmin,
	float dp, float depthref, float f1, float f2, float interoff,
	float offref, int igopt, float dx, float fx, float **in_traces,
	float **out_traces);

void inv_FK_sstack (float dt, int nt, int nx, float xmin, float dx, int np,
	 float pmin, float dp, float fmin, float **traces, float **out_traces); 

void inv_tx_sstack (float dt, int nt, int nx, int npoints, float xmin, 
	float dx, int np, float pmin, float dp, float **traces,
	float **out_traces); 

void rho_filter (int npoints, int nt, float dt, float *rho);

void ga_xinterpolate(float **in_traces, float **out_traces, int ninterp,
                int nt, int nx, float freq1, float freq2, int lagc,
                int lent, int lenx, int xopt, float dt, int iopt);

void runav(int n,int len,float *a,float *b);
*******************************************************************************
fwd_FK_sstack:
Input:
dt              time sampling interval
nt              number of time samples
nx              number of horizontal samples (traces)
np              number of slopes
pmin            minimum slope for tau-p transform
dp		slope sampling interval
fmin            minimum frequency of interest
traces          2-D array of input traces in t-x domain

Output:
traces          2-D array of output traces in tau-p domain
*******************************************************************************
Credits:
	Gabriel Alvarez (1994). Based on subroutine ..... in CWP program
	migbzo.c by Dave Hale.
*******************************************************************************
fwd_tx_sstack:
Input:
dt              time sampling interval
nt              number of time samples
nx              number of horizontal samples (traces)
np              number of slopes
pmin            minimum slope for tau-p transform
dp		slope sampling interval
traces          2-D array of input traces in t-x domain

Output:
out_traces      2-D array of output traces in tau-p domain
*******************************************************************************
Credits:
	Gabriel Alvarez (1994). 
*******************************************************************************
inv_FK_sstack:
Input:
dt              time sampling interval
nt              number of time samples
nx              number of horizontal samples
np              number of slopes
pmin            minimum slope for inverse tau-p transform
dp		slope sampling interval
fmin            minimum frequency of interest
traces          2-D array of input traces in tau-p domain

Output:
out_traces      2-D array of output traces in t-x domain
*******************************************************************************
Credits:
	Gabriel Alvarez (1994).  Based on subroutine ..... in CWP program
	migbzo.c by Dave Hale.
*******************************************************************************
inv_tx_sstack:
Input:
dt              time sampling interval
nt              number of time samples
nx              number of horizontal samples
np              number of slopes
pmin            minimum slope for inverse tau-p transform
dp		slope sampling interval
traces          2-D array of input traces in tau-p domain

Output:
out_traces      2-D array of output traces in t-x domain
*******************************************************************************
Credits:
	Gabriel Alvarez (1994).
*******************************************************************************
rho_filter:
Input:
npoints         number of point for the rho filter
nt              number of time samples
dt              time sampling interval

Output:
rho             1-D array of filter points
*******************************************************************************
Credits:
	Gabriel Alvarez (1994).
*******************************************************************************
forward_p_transform:
Input:
nx              number of input traces
nt              number of intput time samples
dt              sample rate in seconds
dx		offset sampling interval (distance between traces) (m).
fx		first offset (meters)
igopt           =1 parabolic transform g(x)=offset**2
                =2 Foster/Mosher pseudo hyperbolic transform
                        g(x)=sqrt(depth**2+offset**2)
                =3 linear tau-p g(x)=offset
                =4 abs linear taup g(x)=abs(offset)
offref          reference maximum offset to which maximum and minimum moveout
                times are associated
interoff        intercept offset to which tau-p times are associated
                (usually zero)
pmax            maximum moveout in ms on reference offset
pmin            minimum moveout in ms on reference offset
dp              moveout sampling interval (ms/m)
depthref        reference depth for Foster/Mosher hyperbolic transform
f1              high-end  frequency before taper off
f2              high-end frequency
prewhite        prewhitening factor in percent (usually between 0.01 and 0.1)
nwin            number of windows to use through the mute zone

Parameters with good suggested values:
freq1           low-end frequency for picking (usually 3 Hz)
freq1           high-end frequency for picking (usually 20 Hz)
lagc            length of AGC operator for picking (usually 400 ms)
lent            length of time smoother in samples for picking (usually 5)
lenx            length of space smoother in samples for picking (usually 1)
xopt            =1 use differences for spatial derivatives (works with
                irregular spacing)
                =0 use FFT derivative for spatial derivatives (more accurate
                but requires regular spacing and at least 16 input traces),
                will switch to differences automatically is this is not met
in_traces       2-D array of input t-x traces

Output:
out_traces      2-D array[np][nt] of output tau-p traces

Notes:
offsets are computed internally as offset[ix]=fx+ix*dx
************************************************************************
Credits:
	Adapted by Gabriel Alvarez (1995) from suradon.c written by John
        Anderson (1993)
************************************************************************
inverse_p_transform:
Input:
nx              number of output traces
nt              number of output time samples
dt              time sampling interval (seconds)
dx              spatial sampling interval (meters)
fx		first offset (meters)
igopt           =1 parabolic trransform g(x)=offset**2
                =2 Foster/Mosher pseudo hyperbolic transform
                        g(x)=sqrt(depth**2+offset**2)
                =3 linear tau-p g(x)=offset
                =4 abs linear taup g(x)=abs(offset)
		=5 new pseudo-hyperbolic ransform
			g(x)=1/ref_vel*sqrt((ref_time*ref_vel)**2+offset**2)
offref          reference maximum offset to which maximum and minimum moveout
                times are associated
interoff        intercept offset to which tau-p times are associated
                (usually zero)
pmax            maximum moveout in ms on reference offset
pmin            minimum moveout in ms on reference offset
dp              moveout sampling interval (ms/m)
depthref        reference depth for Foster/Mosher hyperbolic transform
f1              high-end  frequency before taper off (hz)
f2              high-end frequency (hz)
in_traces       2-D array[np][nt] of input taup traces

Output:
out_traces      2-D array[nx][nt] of output t-x traces
**************************************************************************
Credits:
	Adapted by Gabriel Alvarez (1995) from suradon.c written by John
        Anderson (1993)
**************************************************************************
ga_xinterpolate
Input:
int ninterp             number of traces to interpolate between each input trace
int nt                  number of time samples
int nx                  number of input traces
float freq1             low-end frequency for picking (good default: 3 Hz)
float freq2             high-end frequency for picking (good default: 20 Hz)
int lagc                length of AGC operator for picking(good default: 400 ms)
int lent                length of time smoother in samples for picker
                        (good default: 5 samples)
int lenx                length of space smoother in samples for picker
                        (good default: 1 sample)
int xopt                1 = use differences for spatial derivative
                            (works with irregular spacing)
                        0 = use FFT derivative for spatial derivatives
                            (more accurate but requires regular spacing and
                            at least 16 input tracs--will switch to differences
                            automatically if have less than 16 input traces)
float dt                sample rate in sec
int iopt		0 = interpolate: output 1+(nx-1)*(1+ninterp) traces
                           with ninterp traces between each pair of input traces
			1 = compute low-pass model: output nx traces
                            on original trace locations -- This is typically
                            used for Quality Control if the interpolator
                            is failing for any reason
			2 = compute dip picks in units of samples/trace: 
                            output nx traces on original trace locations
in_traces       	2-D array of input traces

Output:
out_traces      	2-D array of interpolated tau-p traces


Notes:
This routine outputs 'ninterp' interpolated traces between each pair of
input traces.  The values for lagc, freq1, and freq2 are only used for
event tracking. The output data will be full bandwidth with no agc.  The
suggested default parameters typically will do a satisfactory job of
interpolation for dips up to about 12 ms/trace.  Using a larger value for
freq2 causes the algorithm to do a better job on the shallow dips, but to
fail on the steep dips.  Only one dip is assumed at each time sample between
each pair of input traces.  The original input traces are passed through
this routine without modification.

The key assumption used here is that the low frequency data are unaliased
and can be used for event tracking.  Those dip picks are used to interpolate
the original full-bandwidth data, giving some measure of interpolation
at higher frequencies which otherwise would be aliased.  Using iopt equal
to 1 allows you to visually check whether the low-pass picking model is aliased.
If you can't visually pick dips correctly on the low-pass picking
model, this computer routine will fail.

The place this code is most likely to fail is on the first breaks.
************************************************************************
Credits:
	Adapted by Gabriel Alvarez (1995) from suradon.c written by John
        Anderson (1993)
************************************************************************
Notes:
Other subroutines, used internally, might be of interest:
gofx		computes offsets for linear, parabolic or hyperbolic transforms
freqweight	computes frequency dependent weigths
compute_r	computes top row of a Hermitian Toeplitz matrix
compute_rhs	computes hermitian matrix times data vector 
ctoep		complex hermitian Toeplitz solver
ctoephcg	Hestenes and Stiefel conjugate gradient algorithm especialized
		for solving Hermitian Toeplitz systems
rcdot		computes real part of a complex dot product where the first
		vector is the one complex conjugated
runav		computes a boxcar running average filter 


More documentation about these subroutines on their headings, below.
******************************************************************************/
/**************** end self doc ********************************/




/******************************************************************************

        Compute global forward slant stack (tau-p transform) via
				FK  transform

******************************************************************************/
void fwd_FK_sstack (float dt, int nt, int nx, float xmin, float dx, int np,
	float pmin, float dp, float fmin, float **traces, float **out_traces)
/******************************************************************************
Input:
dt              time sampling interval
nt              number of time samples
nx              number of horizontal samples (traces)
xmin		minimum horizontal offset (ignored)
np              number of slopes
pmin            minimum slope for tau-p transform
dp		slope sampling interval
fmin            minimum frequency of interest (ignored)
traces          2-D array of input traces in t-x domain

Output:
traces          2-D array of output traces in tau-p domain
******************************************************************************/
{
        int it,itau,ix,iw,ik,ip;/* loop counters */
        float fx,fw;		/* first frequency */
        float fp;            	/* first slope */
        float dw,dk;            /* frequency and wavenumber sampling interval */
        int nw,nk;              /* number of frequencies and wavenumbers */
        int ntpad;              /* number of time samples to pad */
        int ntfft;              /* number of padded time samples */
	int ntau;		/*  */
	/* int lk; */		/* last wavenumber */
	int nxfft;		/* number of x samples to pad */
	float fftscl;		/* scale factor for FFT */
        float w,p,k;            /* frequency, slope and wavenumber */
        float phase;            /* phase angle */
        float xshift;           /* shift to account for non-centered x-axis */
        float c,s;              /* auxiliary variables */
        float xmax;             /* maximum horizontal value */
        float fk;          	/* first wavenumber */
        float pmax;             /* maximum slope */
        int lwrap;              /* samples required to make k periodic */
        float fka;              /* first wavenumber with k periodic */
	float temp;		/* auxiliary variable */
        int nka;                /* number of wavenumbers with k perioic */
        float *tr_fft;          /* padded trace for FFT */
        complex **ctr;          /* F-K transformed trace */
        complex **ctr_p;        /*  */
        complex *tr_k;          /* K-transformed trace  */
        complex *tr_ka;         /*  */
        complex *tr_x;          /* W-transformed input tracescaled  by dx */
        complex *hp;            /* slant stacked single trace */
        float *kp;		/* K-transformed slant stacked single trace */
	complex czero;		/* complex number zero */

        /* compute slope sampling interval */
        pmax = pmin+(np-1)*dp;
	czero = cmplx(0.0*fmin,0.0*xmin);

	/* determine lengths and scale factors for FFT's */
	fx=xmin; /* Changed by Bee Bednar 12/25/06 */
	fp=pmin+0.5*(pmax-pmin-(np-1)*dp);
	pmax = (dp<0.0)?fp:fp+(np-1)*dp;
        xmax = (dx<0.0)?fx:fx+(nx-1)*dx;
	ntau = nt;
	ntpad = ABS(pmax*xmax)/dt;
	ntfft = npfar(MAX(nt+ntpad,ntau));
	fftscl = 1.0/ntfft;
	lwrap = 8;      /* samples required to make k periodic */
	nxfft = npfa(2*(nx+lwrap));

	/* determine frequency and wavenumber sampling */
	nw = ntfft/2+1;
	dw = 2.0*PI/(ntfft*dt);
	fw = 0.0;
	nk = nxfft;
	dk = 2.0*PI/(nxfft*dx);
	fk = -PI/dx;
	/* lk = PI/dx; */
	fka = fk-lwrap*dk;
	nka = lwrap+nk+lwrap;

	/* allocate working space for FFT's */
	tr_fft = alloc1float(ntfft);
	ctr = alloc2complex(nw,nk);
	ctr_p = alloc2complex(nw,np);
	tr_ka = alloc1complex(nka);
	tr_x = tr_k=tr_ka+lwrap;        /* pointers to tr_ka[lwrap] */
	hp = alloc1complex(np);
	kp = alloc1float(np);

	/* loop over traces */
	for (ix=0; ix<nx; ix++) {

		/* pad time axis with zeros */
		for (it=0; it<nt; it++)
		tr_fft[it]=traces[ix][it];
		for (it=nt; it<ntfft; it++)
		tr_fft[it]=0.0;

		/* Fourier transform time to frequency */
		pfarc(1,ntfft,tr_fft,ctr[ix]);
	}

	/* loop over w */
	for (iw=0, w=fw; iw<nw; ++iw, w+=dw) {

	/* scale tr_x by x sampling interval */
	for (ix=0; ix<nx; ix++) {
		tr_x[ix].r = ctr[ix][iw].r*dx*fftscl;
		tr_x[ix].i = ctr[ix][iw].i*dx*fftscl;
	}

	/* pad tr_x with zeros */
	for (ix=nx; ix<nxfft; ix++)
		tr_x[ix].r = tr_x[ix].i = 0.0;

		/* negate every other sample for k-axis centered */
		for (ix=1; ix<nx; ix+=2) {
			tr_x[ix].r = -tr_x[ix].r;
			tr_x[ix].i = -tr_x[ix].i;
		}

		/* Fourier transform tr_x to tr_k */
		pfacc (-1, nxfft, tr_x);
				
		/* wrap-around tr_k to avoid interpol end effects */
		for (ik=0; ik<lwrap; ik++)
			tr_ka[ik] = tr_k[ik+nk-lwrap];
		for (ik=lwrap+nk; ik<lwrap+nk+lwrap; ik++)
			tr_ka[ik] = tr_k[ik-lwrap-nk];

		/* phase shift to account for non-centered x-axis */
		xshift = 0.5*(nx-1)*dx;
		for (ik=0, k=fka; ik<nka; ik++, k+=dk) {
			phase = k*xshift;
			c = cos(phase);
			s = sin(phase);
			temp = tr_ka[ik].r*c-tr_ka[ik].i*s;
			tr_ka[ik].i = tr_ka[ik].r*s+tr_ka[ik].i*c;
			tr_ka[ik].r=temp;
		}

		/* compute k values at which to interpolate tr_k */
		for (ip=0, p=fp; ip<np; ip++, p+=dp) {
			kp[ip] = w*p;

			/* if outside Nyq bounds do not interpolate 
			if (kp[ip]<fk && kp[ip]<lk)
				kp[ip] = fk-1000.0*dk;
			else if (kp[ip]>fk && kp[ip]>lk)
				kp[ip] = lk+1000.0*dk; */
		}

		/* 8-point sinc interpolation of tr_k to obtain h(p) */
		ints8c (nka, dk, fka, tr_ka, czero, czero, np, kp, hp);

		/* phase shift to account for non-centered x-axis */
		xshift = -fx - 0.5*(nx-1)*dx;
		for (ip=0; ip<np; ip++) {
			phase = kp[ip]*xshift;
			c = cos(phase);
			s = sin(phase);
			ctr_p[ip][iw].r = hp[ip].r*c-hp[ip].i*s;
			ctr_p[ip][iw].i = hp[ip].r*s+hp[ip].i*c;
		}
	}

	/* loop over p */
	for (ip=0; ip<np; ip++) {

		/* Fourier transform frequency to time */
		pfacr(-1, ntfft, ctr_p[ip], tr_fft);

		/* copy to output array */
		for (itau=0; itau<ntau; itau++)
		out_traces[ip][itau] = tr_fft[itau];
	}

	/* clean up */
	free1float(tr_fft);
	free2complex(ctr);
	free2complex(ctr_p);
	free1complex(tr_ka);
	free1complex(hp);
	free1float(kp);
}

/******************************************************************************

	Subroutine to compute a forward slant stack (taup transform)
				in t-x domain

******************************************************************************/
void fwd_tx_sstack (float dt, int nt, int nx, float xmin, float dx, int np,
	float pmin, float dp, float **traces, float **out_traces)
/******************************************************************************
Input:
dt              time sampling interval
nt              number of time samples
nx              number of horizontal samples (traces)
np              number of slopes
pmin            minimum slope for tau-p transform
dp		slope sampling interval
traces          2-D array of input traces in t-x domain

Output:
traces          2-D array of output traces in tau-p domain

Credits:
written by Gabriel Alvarez, CWP
modified by Bjoern E. Rommel, IKU, Petroleumsforskning
******************************************************************************/
{
	int ip,ix,it;		/* loop counters */
	float x,p;		/* auxilairy variables */
	int fit,lit;		/* first and last time samples */
	int id;                 /* auxiliary variables */
	float dfrac,delay;	/* more auxiliary variables */

	/* loop over slopes */
	for (ip=0, p=pmin; ip<np; p=pmin+(++ip)*dp) {

		/* initialize output array */
		for (it=0; it<nt; it++)
			out_traces[ip][it]=0.0;

		/* loop over traces */
		for (ix=0, x=xmin; ix<nx; x=xmin+(++ix)*dx) {

			/* compute two point interpolator */
			delay=p*x/dt;
			if (delay>=0) {
				id = (int)delay;
				fit = id+1;
				lit = nt-1;
			} else {
				id = (int)delay-1;
				fit = 1;
				lit = nt+id;
			}	
			dfrac = delay-id;

			/* compute the actual slant stack */
			for (it=fit; it<lit; it++) {
				out_traces[ip][it-id]+=fabs(dx)*
				    (traces[ix][it]+
				     dfrac*(traces[ix][it+1]-traces[ix][it]));
			}
		}
        }
}

/******************************************************************************

        Compute inverse global slant stack in FK domain

******************************************************************************/
void inv_FK_sstack (float dt, int nt, int nx, float xmin, float dx, int np,
	float pmin, float dp, float fmin, float **traces, float **out_traces) 
/******************************************************************************
Input Parameters:
dt              time sampling interval
nt              number of time samples
nx              number of horizontal samples
xmin		minimum horizontal distance (ignored)
np              number of slopes
pmin            minimum slope for inverse tau-p transform
dp		slope sampling interval
fmin            minimum frequency of interest (ignored)
traces          2-D array of input traces in tau-p domain

Output Parameters:
out_traces      2-D array of output traces in t-x domain
******************************************************************************/

{
        int ix,ip,itau,iw,ik,it;/* loop counters */
        float fw;               /* first frequency */
        float dw,dk;            /* frequency and wavenumber sampling interval */
        int nw,nk;              /* number of frequencies and wavenumbers */
        int ntaufft;            /* number of padded time samples */
        int nkfft;              /* number of padded wavenumber samples */
	int nkmax;
	int ntau=nt;
	float pmax;
	float dtau=dt;		/* time intercept sampling ingerval */
        float fp=0.,fk;            /* first slope and wavenumber */
	float w,k;		/* angular frequency, slope and wavenumber */
	float fftscl;  		/* scale factor for FFT */
        float *pk;		/* k-interpolated w trace */
        float *tr_fft;          /* padded trace for FFT */
	complex *tr_p;
        complex **ctr;          /* */
        complex **ctr_x;        /* */
        complex *hk;            /* slant stacked single trace */
	complex czero;		/* complex zero number */

        /* added by Bee Bednar 12/25/2006*/
        float c,s;
        float phase;
        float temp;
        float xshift;

        /* compute slope sampling interval */
	czero = cmplx(0.0*fmin,0.0*xmin);
	pmax=pmin+(np-1)*dp;

	/* determine lengths and scale factors for FFT's */
	fp=pmin+0.5*(pmax-pmin-(np-1)*dp);
	pmax=fp+(np-1)*dp;	
	ntaufft = npfar(2*ntau); 
	fftscl = 1.0/ntaufft;
	nkfft = npfa(4*np);

	/* determine frequency and wavenumber sampling */
	nw = ntaufft/2+1;
	dw = 2.0*PI/(ntaufft*dtau);
	fw = fk = 0.0;
	dk = 2.0*PI/(nkfft*dx);
	nkmax = PI*pmax/(dt*dk) + 1;

	/* allocate working space for FFT's */
	tr_fft = alloc1float(ntaufft);
	ctr = alloc2complex(nw,np);
	ctr_x = alloc2complex(nw,nx);
	tr_p = alloc1complex(np); 

/* change suggested by Andreas Klaedtke */
/* replace */
/*	hk = alloc1complex(nkmax); */
/* with */
	hk = alloc1complex(nkmax < nkfft ? nkfft : nkmax);
/* */
	/* loop over traces in tau-p domain */
	for (ip=0; ip<np; ip++) {

		/* pad tau axis with zeros */
		for (itau=0; itau<ntau; itau++)
		tr_fft[itau]=traces[ip][itau];
		for (itau=ntau; itau<ntaufft; itau++)
			tr_fft[itau]=0.0;

		/* Fourier transform tau to frequency */
		pfarc(1,ntaufft,tr_fft,ctr[ip]);
	}

	/* loop over w */
        xshift = xmin;
	for (iw=0, w=fw; iw<nw; ++iw, w+=dw) {

	/* scale ctr by p sampling interval */
	for (ip=0; ip<np; ip++)
		tr_p[ip] = crmul(ctr[ip][iw],dp*fftscl);

		/* compute number of k's for interpolation */
		nk = pmax*w/dk + 1;
               	pk = alloc1float(nk);

		/* compute p values at which to interpolate tr_p */
		for (ik=0, k=fk; ik<nk; ik++, k+=dk) {
			if (w==0.0) pk[ik]=0.0;  
			else pk[ik] = k/w;
		}
	
		/* interpolate tr_pa to obtain tr_k */ 
		ints8c (np, dp, fp, tr_p, czero, czero, nk, pk, hk); 

                /* shift back to proper x axis */
                for (ik = 0 ;ik < nk; ik++) {
                    phase = pk[ik]*w*xshift;
                    c = cos(phase);
                    s = sin(phase);
                    temp = hk[ik].r*c - hk[ik].i*s;
                    hk[ik].i = hk[ik].r*s + hk[ik].i*c;
                    hk[ik].r = temp;
                }
                 
	
		/* free temporary space */
		free1float(pk);	

		/*pad hk with zeros  */
		for (ik=nk; ik<nkfft; ik++) 
			hk[ik]=czero;

		/* inverse Fourier transform from k to x */
		pfacc (1, nkfft, hk);

		/* copy 1-D interpolated array to 2-D array */
		for (ix=0; ix<nx; ix++)
			ctr_x[ix][iw]=hk[ix];
	}

	/* inverse Fourier transform from frequency to time */
	for (ix=0; ix<nx; ix++) {
		pfacr(-1,ntaufft,ctr_x[ix],tr_fft);
		for (it=0; it<nt; it++)
			out_traces[ix][it]=tr_fft[it];	
	} 	

	/* clean up */
	free1float(tr_fft);
	free1complex(hk); 
	free1complex(tr_p);
	free2complex(ctr);
	free2complex(ctr_x);
}

/******************************************************************************

	Subroutine to compute an inverse slant stack (taup transform)
				in t-x domain

******************************************************************************/
void inv_tx_sstack (float dt, int nt, int nx, int npoints, float xmin, 
	float dx, int np, float pmin, float dp, float **traces,
	float **out_traces) 
/******************************************************************************
Input Parameters:
dt              time sampling interval
nt              number of time samples
nx              number of horizontal samples
np              number of slopes
pmin            minimum slope for inverse tau-p transform
dp		slope sampling interval
traces          2-D array of input traces in tau-p domain

Output Parameters:
out_traces      2-D array of output traces in t-x domain
******************************************************************************/
{
	int ip;			/* loop counter */
	int np2=npoints/2;	/* half number of points in rho filter */
	float *rho;		/* auxiliary array for rho filter */
	float **rho_traces;	/* aux array for traces convolved with rho */

	/* allocate space */
	rho_traces = alloc2float(nt,np);
	rho = alloc1float(npoints);

	/* compute rho filter */
	rho_filter (npoints, nt, dt, rho);

       	/* convolve input traces with rho filter */
        for (ip=0; ip<np; ip++)
       	        convolve_cwp(nt,-np2,traces[ip],npoints,0,rho,nt,0,rho_traces[ip]);

	/* inverse transform == forward transform with negative slopes */
	fwd_tx_sstack (dt, nt, np, -pmin, -dp, nx, xmin, dx, rho_traces,
		out_traces);

	/* clean up */
	free2float(rho_traces);
	free1float(rho);
}

/******************************************************************************

        Subroutine to compute the rho filter in frequenccy
         domain for the time domain inverse slant stack

******************************************************************************/
void rho_filter (int npoints, int nt, float dt, float *rho)
/******************************************************************************
Input Parameters:
npoints         number of point for the rho filter
nt              number of time samples
dt              time sampling interval

Output Parameters:
rho             1-D array of filter points

Credits:
written by Gabriel Alvarez, CWP
corrected by Bjoern E. Rommel, IKU, Petroleumsforskning
******************************************************************************/
{
	const int maxnpfa = 720720;
				/* maximum number that npfa can handle */
                                /* (see function npfa) */
        int it,iff; 		/* loop counters */
        int nfh,nfhp;  		/* half number of frequency coefficients */
	int ntfft;		/* time samples to pad */
	int nph=npoints/2;
        float f, df;            /* frequency and frequency sampling interval */
        complex *cx;            /* array of frequencies */

	/* oddness of npoints */
	if (2 * nph == npoints)   err ("npoints must be odd!\n");

	/* compare filter length with number of time samples */
	if (nph > nt)   
		err ("filter length larger than number of time samples!");

	/* compute padding factor */
	if (nt > maxnpfa)   
		err ("number of time samples too large for npfa!");
	ntfft = npfa(nt);

	/* allocate working space */
	cx = alloc1complex(ntfft);

	/* define constants */
	nfh = ntfft/2;
	nfhp = nfh+1;
	df = 1.0/(2*dt*nfh);

	/* compute filter coefficients */
	cx[0] = cmplx (0.0, 0.0);
	for (iff = 1, f = df; iff < nfhp; f = ++iff * df)
		cx[iff] = cx[ntfft-iff] = cmplx (f, 0.0);
  
	/* inverse Fourier transform from f to t */
	pfacc(-1,ntfft,cx);

	/* normalized output filter coefficients */
	rho[nph] = 1.0;
	for (it = 0; it < nph; it++)
		rho[nph-it-1] = rho[nph+it+1] = cx[it+1].r / cx[0].r;
  
	/* clean up */
	free1complex(cx);
}

/******************************************************************************
        Compute global forward slant stack in F-X domain
                        via radon transform

******************************************************************************/
void forward_p_transform(int nx, int nt, float dt, float pmax, float pmin,
        float dp, float depthref, float f1, float f2, float freq1, float freq2,
	int lagc, int lent, int lenx, int xopt, int ninterp, int nwin,
	float prewhite, float interoff, float offref, int igopt, float dx,
	float fx, float pmula, float pmulb, float **in_traces,
	float **out_traces)
/******************************************************************************
Input:
nx              number of input traces
nt              number of intput time samples
dt              sample rate in seconds
dx		offset sampling interval (distance between traces)
fx		first offset 
igopt           =1 parabolic trransform g(x)=offset**2
                =2 Foster/Mosher pseudo hyperbolic transform
                        g(x)=sqrt(depth**2+offset**2)
                =3 linear tau-p g(x)=offset
                =4 abs linear taup g(x)=abs(offset)
offref          reference maximum offset to which maximum and minimum moveout
                times are associated
interoff        intercept offset to which tau-p times are associated
                (usually zero)
pmax            maximum moveout in ms on reference offset
pmin            minimum moveout in ms on reference offset
dp              moveout sampling interval (ms/m)
depthref        reference depth for Foster/Mosher hyperbolic transform
f1              high-end  frequency before taper off
f2              high-end frequency
prewhite        prewhitening factor in percent (usually between 0.01 and 0.1)
nwin            number of windows to use through the mute zone (ignored)

Parameters with good suggested values:
freq1           low-end frequency for picking (usually 3 Hz)
freq1           high-end frequency for picking (usually 20 Hz)
lagc            length of AGC operator for picking (usually 400 ms)
lent            length of time smoother in samples for picking (usually 5)
lenx            length of space smoother in samples for picking (usually 1)
xopt            =1 use differences for spatial derivatives (works with
                irregular spacing)
                =0 use FFT derivative for spatial derivatives (more accurate
                but requires regular spacing and at least 16 input traces
                will switch to differences automatically is this is not met)
in_traces       2-D array[ntfft] of input t-x traces

Output:
out_traces      2-D array of output tau-p traces

offsets are not read from the headers but computed as offset[ix]=fx+ix*dx
*******************************************************************************
Credits:
	Adapted by Gabriel Alvarez (1995) from suradon.c written by
	John Anderson (1993)
******************************************************************************/
{
        int ix,ip,it,iw;                /* loop counters */
        int dummy; 	                /* dummy variable */
        int ntfft;                      /* length of time fft  */
        int nw;                         /* number of frequencies */
        int nxinterp;                   /* number of traces with interpol */
        long nmax;                       /* maximum number of traces */
        int np;                         /* number of sopes for taup transform*/
	int ipa,ipb;
	int ltaper;
        float w;                        /* frequency */
        float dw,df;                    /* frequency sampling intervals */
        float wa;                       /* some sort of frequencies ??? */
        float fac;                      /* scaling factor */
        float d,rsum;			/* auxiliary variables */
        float *rtr;			/* more auxiliary vectors */
        float *rt;			/* vector to hold trace */
        float *g,*offset; 		/* arrays of offsets */
        float *xin;  	                /* auxiliaty arrays */
        complex czero;                  /* complex zero number */
        complex *crt,*ccrt,*r,*rhs;     /* auxiliaty complex arrays */
        complex *wrk1,*wrk2,*wrk3,*wrk4;/* more complex arrays */
        float **interpolated_tr;        /* array of interpolated traces */
        char *fname;                    /* character pointer to file name */
        VND *vndresult;                 /* 2-D array of output (result) data */
        VND *vndinterp;                 /* 2-D array of input (interpol) data */

        /* define variables */
        ntfft=npfar(nt);
        fac = 1000.*gofx(igopt,offref,interoff,depthref);
        pmin /=fac;
        pmax /=fac;
        dp /=fac;
	pmula /=fac;
	pmulb /=fac;
	ipa = (pmula-pmin)/dp;
	ipb = (pmulb-pmin)/dp;
	if (ipa<0) ipa=0;
	ltaper=7;
	np =1+(pmax-pmin)/dp;
	nxinterp = (1+ninterp)*(nx-1)+1;
	nmax = MAX(nxinterp, np);
	nmax = MAX(nmax, ntfft+4);

	fprintf(stderr,"computing forward radon transform: pmin=%g pmax=%g"
		" dp=%g np=%d\n",pmin,pmax,dp,np);

	/* allocate 1-D VND arrays */
	offset =(float *)VNDemalloc(nx*sizeof(float),"fwdslant_taup:offset");
	rtr = (float *)VNDemalloc(nmax*sizeof(float),"fwdslant_taup:rtr");
	xin = (float *)VNDemalloc(nx*sizeof(float),"fwdslant_taup:trace");
	g = (float *)VNDemalloc(nxinterp*sizeof(float),"fwdslant_taup:gg");

	/* allocate 2-D VND arrays */
	fname = VNDtempname("radontem");
	vndinterp = V2Dop(2,2000000,sizeof(float),fname,nt,nxinterp);
	VNDfree(fname,"fwdslant_taup:fname 1");	
	fname = VNDtempname("radontmp");
	nmax = MAX(nxinterp,np);		/* max number of horiz samples*/
	vndresult = V2Dop(2,1000000,sizeof(float),fname,ntfft+2,nmax);
	VNDfree(fname,"fwdslant_taup:fname 2");

	/* compute offsets and offset function g(x) */
	for (ix=0; ix<nx; ix++) {

		/* compute offsets */
		offset[ix] = fx+ix*dx;	
	}

	/* interpolate traces */
	interpolated_tr=alloc2float(nt, nxinterp);
	ga_xinterpolate (in_traces, interpolated_tr, ninterp, nt, nx, freq1,
		freq2, lagc, lent, lenx, xopt, dt, 0);
	d = 1.0/(1+ninterp);

	/* copy interpolated traces to a VND array */
	for (ix=0; ix<nxinterp; ix++) {
		rtr[ix] = ix*d;	
		V2Dw0(vndinterp,ix,(char *)interpolated_tr[ix],1010);
	}
	free2float(interpolated_tr);
	
	/* interpolate offsets */
	for (ix=0; ix<nx; ix++)
		xin[ix] = ix;
	intlin (nx, xin, offset, offset[0], offset[nx-1], nxinterp, rtr, g);

	/* compute offset function depending on the type of transform */
	for (ix=0; ix<nxinterp; ix++) {
		g[ix] = gofx(igopt, g[ix], interoff, depthref);
	}
	
	/* define some variables */
	fac=1./ntfft;
	nw=1+ntfft/2;
	df=1./(ntfft*dt);
	dw=2.*PI*df;
	nmax=MAX(vndresult->N[0],vndresult->N[1]);
	czero.r=czero.i=0.;

	/* allocate working 1D complex space */
	crt=(complex *)VNDemalloc(nmax*sizeof(complex),
		"forward_transform:crt");
	ccrt=(complex *)VNDemalloc(MAX(2*np,vndresult->N[1])*
		sizeof(complex),"forward_transform:ccrt");
	r=(complex *)VNDemalloc(np*sizeof(complex),
		"forward_p_transform:r");
	rhs=(complex *)VNDemalloc(np*sizeof(complex),
		"forward_p_transform:rhs");
	wrk1=(complex *)VNDemalloc(np*sizeof(complex),
		"forward_p_transform:wrk1");
	wrk2=(complex *)VNDemalloc(np*sizeof(complex),
		"forward_p_transform:wrk2");
	wrk3=(complex *)VNDemalloc(np*sizeof(complex),
		"forward_p_transform:wrk3");
	wrk4=(complex *)VNDemalloc(np*sizeof(complex),
		"forward_p_transform:wrk4");

	/* pointers to complex arrays */
	rt=(float *)crt;

	/* do forward time to frequency fft */
	for(ix=0;ix<nxinterp;ix++) {
		V2Dr0(vndinterp,ix,(char *)rt,201);	/* read input data */
		for(it=0;it<nt;it++) rt[it]*=fac;	/* apply fft scale */
		for(it=nt;it<ntfft;it++) rt[it]=0.;	/* zero padding */
		pfarc(1,ntfft,rt,crt);
		V2Dw0(vndresult,ix,(char *)crt,202);
	}

	VNDr2c(vndresult); 		/* real to complex conversion */

	/* do radon transform, frequency by frequency, 
	for multiple spatial windows */

	for(iw=0;iw<nw;iw++) {
		wa=freqweight(iw,df,f1,f2);	/* compute frequency weights */
		if(wa>0.) {
		    	w=iw*dw;
		    	V2Dr1(vndresult,iw,(char *)crt,203);
		    	if(wa<1.)
         	       	   for(ix=0;ix<nxinterp;ix++) crt[ix]=crmul(crt[ix],wa);

			/* compute right hand side vector B+ and top row of
				Toeplitz matrix */
			compute_rhs(w,nxinterp,g,crt,np,pmin,dp,rhs);
			compute_r(w,nxinterp,g,np,dp,r);
			r[0].r *= (1.+prewhite);	/* apply prewhitening */
			for(rsum=0.,ip=1;ip<np;ip++) 
				rsum +=sqrt(r[ip].r*r[ip].r + r[ip].i*r[ip].i);
			rsum=rsum/r[0].r;  

			if (rsum>1.+np/5) { 

				/* conjugate gradient solution of Toeplitz eqn*/
				dummy=ctoephcg(np/7,np,r,&ccrt[0],rhs,
					wrk1,wrk2,wrk3,wrk4);
			} else {

				/* complex Hermitian solver of Toeplitz eqns */
				dummy=ctoep(np,r,&ccrt[0],rhs,wrk1,wrk2);
			}

			dummy=nwin|dummy;
		} else {
		    for(ip=0;ip<np;ip++) ccrt[ip]=czero;		
		}
		V2Dw1(vndresult,iw,(char *)ccrt,204);
	}

	/* do fourier transform from frequency to tau */
	for(ip=0; ip<np; ip++) {
		V2Dr0(vndresult,ip,(char *)crt,205);
		pfacr(-1,ntfft,crt,rt);

		/* do tau-p mute */	
		taupmute (ip,ipa,ipb,ntfft,ltaper,rt);	
	
		/* copy output data */
		for (it=0; it<nt; it++)
			out_traces[ip][it]=rt[it];
	}

	/* free allocated space */
	VNDcl(vndresult,1);
	VNDcl(vndinterp,1);
	VNDfree(crt,"forward_p_transform: crt");
	VNDfree(ccrt,"forward_p_transform: ccrt");
	VNDfree(r,"forward_p_transform: r");
	VNDfree(rhs,"forward_p_transform: rhs");
	VNDfree(wrk1,"forward_p_transform: wrk1");
	VNDfree(wrk2,"forward_p_transform: wrk2");
	VNDfree(wrk3,"forward_p_transform: wrk3");
	VNDfree(wrk4,"forward_p_transform: wrk4");	
	VNDfree(offset,"fwdslant_taup: offset");
	VNDfree(rtr,"fwdslant_taup: rtr");
	VNDfree(xin,"fwdslant_taup: xin");
	VNDfree(g,"fwdslant_taup: g");
	return;
}

/******************************************************************************

        Compute global inverse slant stack in frequency domain

******************************************************************************/
void inverse_p_transform(int nx, int nt, float dt, float pmax, float pmin,
	float dp, float depthref, float f1, float f2, float interoff,
	float offref, int igopt, float dx, float fx, float **in_traces,
	float **out_traces)
/******************************************************************************
Input:
nx              number of input traces
nt              number of intput time samples
dt              time sampling interval (seconds)
dx              spatial sampling interval (meters)
fx		first offset (meters)
igopt           =1 parabolic trransform g(x)=offset**2
                =2 Foster/Mosher pseudo hyperbolic transform
                        g(x)=sqrt(depth**2+offset**2)
                =3 linear tau-p g(x)=offset
                =4 abs linear taup g(x)=abs(offset)
offref          reference maximum offset to which maximum and minimum moveout
                times are associated
interoff        intercept offset to which tau-p times are associated
                (usually zero)
pmax            maximum moveout in ms on reference offset
pmin            minimum moveout in ms on reference offset
dp              moveout sampling interval (ms/m)
depthref        reference depth for Foster/Mosher hyperbolic transform
f1              high-end  frequency before taper off (hz)
f2              high-end frequency (hz)
in_traces       2-D array[np][ntfft]  of input taup traces

Output:
out_traces      2-D array[nx][nt] of output t-x traces

*******************************************************************************
Credits:
	Adapted by Gabriel Alvarez (1995) from suradon.c written by John
        Anderson (1993)
******************************************************************************/
{
	int ix,ip,it,iw;		/* loop counters */
	int ntfft;			/* length of time fft  */
	int nw;				/* number of frequencies */
	int np;				/* number of sopes for taup transform*/
	size_t nmax;			/* maximum space to allocate */
	float rsum,isum;		/* real and imaginary part of a sum */
	float w;			/* frequency */
	float p;			/* slope */
	float dw,df;			/* frequency sampling intervals */
	float wa;			/* frequency weight */
	float fac;			/* fft scaling factor */
	float dr,di,tr,ti;		/* r and im parts of aux complex num*/
	float *rt;			/* auxiliary vector to store a trace */
	float *g;			/* originally input arrays */
	float *offset;			/* array of offsets */
	complex czero;			/* complex zero number */
	complex *crt;			/*complex array for FFT's */
	complex *ctemp;			/* auxiliary complex array */
	char *fname;			/* character pointer to file name */
	VND *vnda;			/* 2-D VND scratch array */

	/* define variables */
	ntfft = npfar(nt);
	fac = 1000.*gofx(igopt,offref,interoff,depthref);
	pmin /= fac;
	pmax /= fac;
	dp /= fac;
	np = 1+(pmax-pmin)/dp;
	nmax = MAX(nx,np);
	nmax = MAX(nmax,ntfft+4);

	fprintf(stderr, "computing inverse radon transform: pmin=%g pmax=%g"
		" dp=%g np=%d\n",pmin,pmax,dp,np);

	/* allocate 1-D arrays */
	offset  = (float *)VNDemalloc(nx*sizeof(float),"invslant:offset");
	g 	= (float *)VNDemalloc(nx*sizeof(float),"invslant:g");

	/* allocate VND 2-D space */
	fname = VNDtempname("radontemp");
	vnda = V2Dop(2,1000000,sizeof(float),fname,ntfft+2,(long) nmax); 
	VNDfree(fname,"invslant:fname 1");

	/* compute offsets */
	for (ix=0; ix<nx; ix++) {

		/* compute offsets */
		offset[ix] = fx+ix*dx;	

		/* get g(x) (offset values) depending on type of transform */
		g[ix] = gofx (igopt, offset[ix], interoff, depthref);
	}
	/* copy input traces to ntfft VND array */
	for (ip=0; ip<np; ip++) {
		V2Dw0(vnda,ip,(char *)in_traces[ip],1002);
	} 
		
	nw=1+ntfft/2;
	df=1./(ntfft*dt);
	dw=2.*PI*df;
	czero.r=czero.i=0.;
	nmax=MAX(vnda->N[0],2*vnda->N[1])*vnda->NumBytesPerNode;
	nmax=MAX(nmax,(nx*sizeof(complex)));

	/* allocate additional VND space */
	crt=(complex *)VNDemalloc(nmax,
		"inverse_p_transform:crt");
	rt=(float *)crt;
	ctemp=(complex *)VNDemalloc(np*sizeof(complex),
		"inverse_p_transform:ctemp");

	fac=1./ntfft;
	/* compute forward time (tau) to frequency Fourier transform */
	for(ip=0;ip<np;ip++) {

		V2Dr0(vnda,ip,(char *)rt,301);
		for (it=nt; it<ntfft; it++) rt[it]=0.0;
		for(it=0;it<ntfft;it++) rt[it]*=fac;
		pfarc(1,ntfft,rt,crt);
		V2Dw0(vnda,ip,(char *)crt,302);
	}
	VNDr2c(vnda);

	fac=1./np;
	for(iw=0;iw<nw;iw++) {
		wa=freqweight(iw,df,f1,f2);
		if(wa>0.) {
			w=iw*dw;
			V2Dr1(vnda,iw,(char *)crt,303);
			if(wa<1.) {
				for(ip=0;ip<np;ip++) crt[ip]=crmul(crt[ip],wa);
			}
			for(ip=0;ip<np;ip++) ctemp[ip]=crt[ip];
			for(ix=0;ix<nx;ix++) {
			    rsum = isum = 0.;
			    for(ip=0;ip<np;ip++) {
				p = pmin + ip*dp;
				tr = cos(w*p*g[ix]);
				ti = sin(w*p*g[ix]);
				dr = ctemp[ip].r;
				di = ctemp[ip].i;
				rsum += tr*dr - ti*di;
				isum += tr*di + ti*dr;
			    }
			    crt[ix].r   = fac*rsum;
			    crt[ix].i   = fac*isum;
			}
		}else{
			for(ix=0;ix<nx;ix++) crt[ix]=czero;
		}
		V2Dw1(vnda,iw,(char *)crt,304);
	}
	for(ix=0;ix<nx;ix++) {
		V2Dr0(vnda,ix,(char *)crt,305);
		pfacr(-1,ntfft,crt,rt);

		/* copy output data */
		for (it=0; it<nt; it++)
			out_traces[ix][it]=rt[it];
	}

	/* free allocated space */
	VNDcl(vnda,1);
	VNDfree(crt,"inverse_p_transform: crt");
	VNDfree(ctemp,"inverse_p_transform: ctemp");
	VNDfree(offset,"inverse_p_tarnsform: offset");
	VNDfree(g,"inverse_p_transform: g");
	return;
}

float gofx(int igopt, float offset, float intercept_off, float refdepth)
/******************************************************************************
return g(x) for various options
*******************************************************************************
Function parameters:

int igopt		1 = parabolic transform
			2 = modified Foster/Mosher pseudo hyperbolic option
			3 = linear tau-p
			4 = linear tau-p using absolute value of 
				offset
			5 = original Foster/Mosher pseudo hyperbolic option
float offset		offset in m
float intercept_off	offset corresponding to intercept time
float refdepth		reference depth in m for igopt=2
*******************************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
******************************************************************************/
{
	offset=offset-intercept_off;
	if(igopt==1) {
		return(offset*offset);
	}
	if(igopt==2) {
		return(sqrt(refdepth*refdepth+offset*offset));
	}
	if(igopt==3) {
		return(offset);
	}
	if(igopt==4) {
		return(fabs(offset));
	}
	if(igopt==5) {
		return(sqrt(refdepth*refdepth+offset*offset)-refdepth);
	} else {
		return 0.0;
	}
}


float freqweight(int j, float df, float f1, float f2)
/******************************************************************************
return weight for each frequency
*******************************************************************************
Function parameters:

int j		freq index
float df	freq increment
float f1	taper off freq
float f2	freq beyond which all components are zero
*******************************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
*****************************************************************************/
{
	float w;
	float f=j*df;
	if(f<=f1) return (1.);
	if(f>=f2) return (0.);
	w = (f2-f)/(f2-f1);
	return (w);
}

void compute_r( float w, int nx, float *g, int np, float dp, complex *r)
/******************************************************************************
Compute the top row of the Hermitian Toeplitz Matrix
			+
		  R = B B

		  i w p g(x)
where B = (1/np) e	    for equal increments in p as

     +           -i w p g(x)
and B = (1/nx) e 

as used for the Discrete Radon Transform computation for
linear or parabolic tau-p.


		 nx-1	i w j dp g(x )
r[j] = 1/(nx*np) Sum	e	    k
		 k=0
						  2
g(x ) is initialized to  x  for linear tau-p or x   for the parabolic transform
   k		          k		         k
prior to calling this routine.  The use of g is intended to emphasize that the
spatial locations do not have to be equally spaced for either method.
In general, this routine can be called for g specified as any function
of spatial position only.  For a more general function of x, dp will
not correspond to an increment in slowness or slowness squared but
rather to a more general parameter.

*******************************************************************************
Function parameters:

float w	input as angular frequency component of interest
int   nx      number of spatial positions stored in g
float g[]     spatial function for this Radon Transform
int   np      number of slowness (or slowness squared) components
float dp      increment in slownes (or slowness squared)
float r[]     output vector of { real r0, imaginary r0, real r1, 
	      imaginary r1, ...}
*******************************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
******************************************************************************/
{
	int j,k;
	float rsum, isum, fac;
	fac= 1./(nx*np);
	for(j=0;j<np;j++) {
		rsum=0.;
		isum=0.;
		for(k=0;k<nx;k++) {
			rsum = rsum+cos( w*j*dp*g[k] );
			isum = isum+sin( w*j*dp*g[k] );	
		}
		r[j].r    = fac*rsum;
		r[j].i    = fac*isum;
	}
}

void compute_rhs( float w, int nx, float *g, complex *data, int np, 
		float pmin, float dp, complex *rhs)
/******************************************************************************
				     +
Compute the right-hand-side vector  B  data(x)

	+	    -i w p g(x)
where B   = (1/nx) e	        for equal increments in p as
used for the Discrete Radon Transform computation for
linear or parabolic tau-p.

Function parameters:

float w	input angular frequency of interest
int   nx	number of spatial positions ( defines length of g and data )
float g[]      spatial function corresponding to spatial locations of data
complex data[] data as a function of spatial position for a single
		angular frequency w as complex values 
int   np	number of output slownesses p (may be slowness squared
		or a more general function)
float pmin     starting value of output p
float dp	increment in output p
complex rhs[]  np complex values for the result 
*******************************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
******************************************************************************/
{
	int ip, ix;
	float p, rsum, isum, dr, di, tr, ti, fac;
	fac=1./nx;
	for(ip=0;ip<np;ip++) {
		p = pmin + ip*dp;
		rsum = isum = 0.;
		for(ix=0;ix<nx;ix++) {
			tr = cos(w*p*g[ix]);
			ti = -sin(w*p*g[ix]);
			dr = data[ix].r;
			di = data[ix].i;
			rsum += tr*dr - ti*di;
			isum += tr*di + ti*dr;
		}
		rhs[ip].r   = fac*rsum;
		rhs[ip].i   = fac*isum;
	}
}

int ctoep( int n, complex *r, complex *a, complex *b, complex *f, complex *g )
/*****************************************************************************	
Complex Hermitian Toeplitz Solver for

N-1
Sum  R	     A  = B      for i=0,1,2,...,N-1
j=0   (i-j)   j    i

where R is Hermitian Toeplitz and A and B are complex.  For
an example 4 x 4 system,  A returns as the solution of


   R0  R1  R2  R3	A0	     B0

     *
   R1  R0  R1  R2	A1	     B1
				=    
     *   *
   R2  R1  R0  R1	A2	     B2

     *   *   *
   R3  R2  R1  R0	A3	     B3

and


   R0  R1  R2  R3	F0	     1

     *
   R1  R0  R1  R2	F1	     0
				=    
     *   *
   R2  R1  R0  R1	F2	     0

     *   *   *
   R3  R2  R1  R0	F3	     0


*******************************************************************************	
where the function parameters are defined by

n     dimension of system
*r    provides the top row of the Hermitian Toeplitz matrix R 
*a    returns the complex solution vector A
*b    input as complex vector B (not changed during call)
*f    returns the complex spiking filter F
      (may be needed later for Simpson's sideways recursion
      if do search for optimum filter lag)
*g    work space of length n complex values

The function value returns as the number of successfully
computed complex filter coefficients (up to n) if successful or
0 if no coefficients could be computed.
*******************************************************************************	
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
******************************************************************************/

{
	float er, ei, vr, vi, cr, ci, vsq;
	int j;	  	/*  for the jth iteration, j=0,n-1 	*/
	int k;			/*  for the kth component, k=0,j-1 	*/
	int jmk;		/*  j-k 				*/
	if (r[0].r==0.) return 0;

	f[0].r = 1.0/r[0].r;
	f[0].i = 0.;
	a[0].r = b[0].r/r[0].r;
	a[0].i = b[0].i/r[0].r;
	vr=1.;
	vi=0.;
	vsq=1.;

	for(j=1;j<n;j++) {     	/* iteration loop for iteration j	*/
	/*  	Compute spiking filter that outputs {v,0,0,...} 
		for this iteration step j			*/
		f[j].r=0.;
		f[j].i=0.;
		er=ei=0.;
		for(k=0;k<j;k++) {
			jmk=j-k;
			er+=r[jmk].r*f[k].r+r[jmk].i*f[k].i;
			ei+=r[jmk].r*f[k].i-r[jmk].i*f[k].r;
		}
		cr  = (er*vr - ei*vi)/vsq;
		ci  = (er*vi + ei*vr)/vsq;
		vr  = vr - (cr*er+ci*ei);
		vi  = vi + (cr*ei-ci*er);
		vsq =  vr*vr + vi*vi;
		if (vsq <= 0.) break;
		for(k=0;k<=j;k++) {
			jmk=j-k;
			g[k].r = f[k].r - cr*f[jmk].r - ci*f[jmk].i;
			g[k].i = f[k].i + cr*f[jmk].i - ci*f[jmk].r;		
		}
		for(k=0;k<=j;k++) {
			f[k]=g[k];
		}

		/*  Compute shaping filter for this iteration */
		a[j].r=0.;
		a[j].i=0.;
		er=ei=0.;
		for(k=0;k<j;k++) {
			jmk=j-k;
			er+=r[jmk].r*a[k].r+r[jmk].i*a[k].i;
			ei+=r[jmk].r*a[k].i-r[jmk].i*a[k].r;
		}
		er  = er-b[j].r;
		ei  = ei-b[j].i;
		cr  = (er*vr - ei*vi)/vsq;
		ci  = (er*vi + ei*vr)/vsq;
		for(k=0;k<=j;k++) {
			jmk=j-k;
			a[k].r += - cr*f[jmk].r - ci*f[jmk].i;
			a[k].i += + cr*f[jmk].i - ci*f[jmk].r;		
		}	
	}

	/* Properly normalize the spiking filter so that R F = {1,0,0,...} */
	/* instead of {v,0,0,...}.  To be accurate, recompute vr,vi,vsq */ 
	vr=vi=0.;
	for(k=0;k<j;k++) {
		vr+=r[k].r*f[k].r-r[k].i*f[k].i;
		vi+=r[k].r*f[k].i+r[k].i*f[k].r;
	}

	vsq = vr*vr + vi*vi;

	/*  Compute (er,ei) = 1./(vr,vi)   */
	er = vr/vsq;
	ei = -vi/vsq;
	for(k=0;k<j;k++) {
		f[k].r = er*f[k].r - ei*f[k].i;
		f[k].i = er*f[k].i + ei*f[k].r;	
	}
	return (j);
}

int ctoephcg( int niter, int n, complex *a, complex *x, complex *y, 
	complex *s, complex *ss, complex *g, complex *rr)

/******************************************************************************

Hestenes and Stiefel conjugate gradient algorithm 
specialized for solving Hermitian Toeplitz
system.  a[] is input as a vector defining the only the
top row of A.  x[] is the solution vector returned.
y[] is input.  niter is the maximum number of conjugate 
gradient steps to compute.  The function returns as
the number of steps actually computed.  The other 
vectors provide workspace.

Complex Hermitian Toeplitz Solver for

N-1
Sum  A	     x  = y      for i=0,1,2,...,N-1
j=0   (i-j)   j    i

where A is Hermitian Toeplitz and x and y are complex.  For
an example 4 x 4 system,  x returns as the solution of


   A0  A1  A2  A3	x0	     y0

     *
   A1  A0  A1  A2	x1	     y1
				=    
     *   *
   A2  A1  A0  A1	x2	     y2

     *   *   *
   A3  A2  A1  A0	x3	     y3

*******************************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
******************************************************************************/
{
	int j, iter;
	complex czero;
	float alpha, beta, gamma, gammam, rsq, rp, test;
	float eps=1.0e-6;
	float rcdot(int n, complex *a, complex *b);
	rp   = rcdot(n,y,y);
	test = n*eps*eps*rp;
	czero.r=czero.i=0.;

	for(j=0;j<n;j++) {
		x[j]=czero;
		rr[j]=y[j];
	}
	htmul(n,a,rr,g);	   /*  adjoint matrix multiply */

	for(j=0;j<n;j++) s[j]=g[j];
	gammam=rcdot(n,g,g);

	for(iter=0;iter<niter;iter++) { /* forward matrix multiply  */
		htmul(n,a,s,ss);  
		alpha  = gammam/rcdot(n,ss,ss);
		for(j=0;j<n;j++) {
			x[j] =cadd(x[j],crmul(s[j],alpha));
			rr[j]=csub(rr[j],crmul(ss[j],alpha));
		}
		rsq = rcdot(n,rr,rr);
		if ( iter>0 && ( rsq==rp || rsq<test ) ) return(iter-1);
		rp = rsq;

		htmul(n,a,rr,g);   /*  adjoint matrix multiply  */
		gamma  = rcdot(n,g,g);
		if (gamma<eps) break;
		beta   = gamma/gammam;
		gammam = gamma;			

		for(j=0;j<n;j++) {
			s[j] =cadd(g[j],crmul(s[j],beta));
		}	
	}
return(iter);
}

void ga_xinterpolate(float **in_traces, float **out_traces, int ninterp, 
		int nt, int nx, float freq1, float freq2, int lagc, 
		int lent, int lenx, int xopt, float dt, int iopt)
/******************************************************************************
interpolate input data in space placing "ninterp" synthetic traces 
between each pair of original input traces
*******************************************************************************
Function parameters:
Input:
int ninterp		number of traces to interpolate between each input trace
int nt			number of time samples
int nx			number of input traces
float freq1		low-end frequency for picking (good default: 3 Hz)
float freq2		high-end frequency for picking (good default: 20 Hz)
int lagc		length of AGC operator for picking (good default:400 ms)
int lent		length of time smoother in samples for picker
                        (good default: 5 samples)
int lenx		length of space smoother in samples for picker
                        (good default: 1 sample)
int xopt		1 = use differences for spatial derivative
                            (works with irregular spacing)
                        0 = use FFT derivative for spatial derivatives
                            (more accurate but requires regular spacing and
                            at least 16 input tracs--will switch to differences
                            automatically if have less than 16 input traces)
float dt		sample rate in sec
int iopt		0 = interpolate: output 1+(nx-1)*(1+ninterp) traces
                           with ninterp traces between each pair of input traces
			1 = compute low-pass model: output nx traces
                            on original trace locations -- This is typically
                            used for Quality Control if the interpolator
                            is failing for any reason
			2 = compute dip picks in units of samples/trace: 
                            output nx traces on original trace locations
in_traces		2-D array of input data

Output:
out_traces		2-D array of interpolated traces


Notes:
This routine outputs 'ninterp' interpolated traces between each pair of 
input traces.  The values for lagc, freq1, and freq2 are only used for
event tracking. The output data will be full bandwidth with no agc.  The 
suggested default parameters typically will do a satisfactory job of 
interpolation for dips up to about 12 ms/trace.  Using a larger value for 
freq2 causes the algorithm to do a better job on the shallow dips, but to 
fail on the steep dips.  Only one dip is assumed at each time sample between 
each pair of input traces.  The original input traces are passed through
this routine without modification.

The key assumption used here is that the low frequency data are unaliased
and can be used for event tracking.  Those dip picks are used to interpolate
the original full-bandwidth data, giving some measure of interpolation
at higher frequencies which otherwise would be aliased.  Using iopt equal
to 1 allows you to visually check whether the low-pass picking model is aliased.
If you can't visually pick dips correctly on the low-pass picking 
model, this computer routine will fail.

The place this code is most likely to fail is on the first breaks.
*******************************************************************************
Credits:
Adapted by Gabriel Alvarez (1995) from suradon.c written by John 
	Anderson (1993)
******************************************************************************/
{
	int	ntfft,ntfftny,nxfft,nxfftny,j,k,ix,it,ixm;
	float	df,dff,wa,wb,dxx,eps=1.0e-30,f,fcl,fch;
	float 	*rt,*rrt,*a,*b,*p,*time,*aa,*bb,*save;
	complex	*crt,*ccrt;
	VND	*vnda,*vndb;
	char 	*fname;

	/* defensive programming */
	if(nx<2 || (iopt==0 && ninterp==0) ) {
		for (ix=0;ix<nx;ix++)
			for (it=0;it<nt;it++)
				out_traces[ix][it]=in_traces[ix][it];	
		return;
	}

	/* define useful variables */
	lent=1+2*(lent/2);
	lenx=1+2*(lenx/2);
	lagc=1 + lagc*0.001/dt;

	ntfft=npfar(nt);
	ntfftny=1+ntfft/2;
	nxfft=npfar(nx);
	nxfftny=1+nxfft/2;

	df=1./(ntfft*dt);

	/* allocate working VND space */
	crt = (complex *)VNDemalloc( MAX(ntfftny,nxfftny)*sizeof(complex),
		"ga_xinterpolate:allocating crt" );
	rt = (float *)crt;
	ccrt = (complex *)VNDemalloc( ntfftny*sizeof(complex),
		"ga_xinterpolate:allocating ccrt" );
	rrt = (float *)ccrt;
	a =  (float *)VNDemalloc( MAX(nx,nt)*sizeof(float),
		"ga_xinterpolate:allocating a" );
	b =  (float *)VNDemalloc( MAX(nx,nt)*sizeof(float),
		"ga_xinterpolate:allocating b" );
	p =  (float *)VNDemalloc( nt*sizeof(float),
		"ga_xinterpolate:allocating p" );
	time =  (float *)VNDemalloc( nt*sizeof(float),
		"ga_xinterpolate:allocating time" );
	aa =  (float *)VNDemalloc( MAX(nx,nt)*sizeof(float),
		"ga_xinterpolate:allocating aa" );
	bb =  (float *)VNDemalloc( MAX(nx,nt)*sizeof(float),
		"ga_xinterpolate:allocating bb" );

	fname = VNDtempname("ga_xinterpolate");
	vnda  = V2Dop(2,500000,sizeof(float),fname,nt,nx);
	VNDfree(fname,"ga_xinterpolate: fname");
	fname = VNDtempname("ga_xinterpolate");
	vndb  = V2Dop(2,500000,sizeof(float),fname,nt,nx);
	VNDfree(fname,"ga_xinterpolate: fname");

	/* loop computing filtered data for picking purposes in vnda */
	/* compute time derivative of filtered data in vndb */
	dff=2.*PI/ntfft;
	for(ix=0;ix<nx;ix++) {
		for(it=0;it<nt;it++) {
			rt[it]=in_traces[ix][it];
			a[it]=fabs(rt[it]);
		}
		runav(nt,lagc,a,b);
		runav(nt,lagc,b,a);
		for(it=0;it<nt;it++) rt[it]=rt[it]/(a[it]+eps);	
		for(it=nt;it<ntfft;it++) rt[it]=0.;
		pfarc(1,ntfft,rt,crt);
		for(it=0;it<ntfftny;it++){
			f=it*df;
			fcl=(f/freq1);
			fcl=fcl*fcl*fcl*fcl;
			fch=(f/freq2);
			fch=fch*fch*fch*fch;
			f=fcl/( (1.+fcl)*(1.+fch) );
			crt[it]=crmul(crt[it],f);
			ccrt[it]=cmul(crt[it],cmplx(0.,-it*dff));
		}
		pfacr(-1,ntfft,crt,rt); 
		V2Dw0(vnda,ix,(char *)rt,104);
		pfacr(-1,ntfft,ccrt,rrt); 
		V2Dw0(vndb,ix,(char *)rrt,105);
	} 

	if(iopt==1){
		for(ix=0;ix<nx;ix++){
			V2Dr0(vnda,ix,(char *)rt,104);
			for (it=0;it<nt;it++)
				out_traces[ix][it]=rt[it];
		}
		VNDcl(vnda,1);
		VNDcl(vndb,1);
		VNDfree(crt,"ga_xinterpolate: crt");
		VNDfree(ccrt,"ga_xinterpolate: ccrt");
		VNDfree(a,"ga_xinterpolate: a");
		VNDfree(b,"ga_xinterpolate: b");
		VNDfree(p,"ga_xinterpolate: p");
		VNDfree(time,"ga_xinterpolate: time");
		VNDfree(aa,"ga_xinterpolate: aa");
		VNDfree(bb,"ga_xinterpolate: bb");
		return;
	}

	/* loop computing spatial derivative of data for picking purposes*/
	nxfft=npfar(nx);
	nxfftny=1+nxfft/2;
	dxx=2.*PI/(nxfft*nxfft);
	if(nx<16) xopt=1;
	for(it=0;it<nt;it++) {
		V2Dr1(vnda,it,(char *)rt,106);
		if(xopt) {
			for(j=0;j<nx-1;j++) rt[j]=rt[j+1]-rt[j];
			rt[nx-1]=rt[nx-2];
		}else{
			for(j=nx;j<nxfft;j++) rt[j]=0.;
			pfarc(1,nxfft,rt,crt);
			for(j=0;j<nxfftny;j++){
				crt[j]=cmul(crt[j],cmplx(0.,-j*dxx));
			}
			pfacr(-1,nxfft,crt,rt); 
		}
		V2Dw1(vnda,it,(char *)rt,107);
	} 

	/* compute dot products and smooth over time */
	for(ix=0;ix<nx;ix++) {
		V2Dr0(vnda,ix,(char *)a,108);
		V2Dr0(vndb,ix,(char *)b,109);
		for(it=0;it<nt;it++) {
			aa[it]=a[it]*b[it];
			bb[it]=b[it]*b[it];
		}
		runav(nt,lent,aa,a);
		runav(nt,lent,a,aa);
		runav(nt,lent,bb,b);
		runav(nt,lent,b,bb);
		V2Dw0(vnda,ix,(char *)aa,110);
		V2Dw0(vndb,ix,(char *)bb,111);
	}

	/* smooth dot products in x */
	if(lenx>1){
	    for(it=0;it<nt;it++) {
		V2Dr1(vnda,it,(char *)a,112);
		V2Dr1(vndb,it,(char *)b,113);
		runav(nx,lenx,a,aa);
		runav(nx,lenx,aa,a);
		runav(nx,lenx,b,bb);
		runav(nx,lenx,bb,b);
		V2Dw1(vnda,it,(char *)a,114);
		V2Dw1(vndb,it,(char *)b,115);
	    }
	}

	/* loop computing p, interpolating, and outputting results */
	/* get first trace from input data */
	for (it=0; it<nt; it++)
		a[it]=in_traces[0][it];
	for(ix=1;ix<nx;ix++) {
		ixm=ix-1;
		V2Dr0(vnda,ixm,(char *)aa,117);
		V2Dr0(vndb,ixm,(char *)bb,118);
		for(it=0;it<nt;it++) {
			p[it] = - aa[it]/( bb[it] + eps );
		}
		
		/* get input traces one by one */
		for (it=0; it<nt; it++)
			b[it]=in_traces[ix][it];	
		if(iopt==2) {
	
			/* write to output array */
			for (it=0; it<nt; it++)
				out_traces[ixm][it]=p[it];
			/* don't output dip picks except on original traces */
		}else{
			/* write to output array */
			for (it=0; it<nt; it++)
				out_traces[ixm*(ninterp+1)][it]=a[it];
			for(k=0;k<ninterp;k++){
				wa=(1.+k)/(1+ninterp);
				wb=1.-wa;
				for(it=0;it<nt;it++) time[it] = it - p[it]*wa;		
				ints8r(nt,1.0,0.,a,0.0,0.0,nt,time,aa);
				for(it=0;it<nt;it++) time[it] = it + p[it]*wb;		
				ints8r(nt,1.0,0.,b,0.0,0.0,nt,time,bb);
				for(it=0;it<nt;it++) aa[it]=wb*aa[it]+wa*bb[it];

				/* write to output array */
				for (it=0; it<nt; it++)
					out_traces[k+1+ixm*(ninterp+1)][it]=
						aa[it];
			}
		}
		save=a;
		a=b;
		b=save;  
	} 
	if(iopt==2) {
		
		/* write to output array */
		for (it=0; it<nt; it++)
			out_traces[nx-1][it]=p[it];
	}else{
		/* write to output array */
		for (it=0; it<nt; it++)
			out_traces[(nx-1)*(ninterp+1)][it]=a[it];
	}


	/* close files, free temporary memory */
	VNDcl(vnda,1);
	VNDcl(vndb,1);
	VNDfree(crt,"ga_xinterpolate: crt");
	VNDfree(ccrt,"ga_xinterpolate: ccrt");
	VNDfree(a,"ga_xinterpolate: a");
	VNDfree(b,"ga_xinterpolate: b");
	VNDfree(p,"ga_xinterpolate: p");
	VNDfree(time,"ga_xinterpolate: time");
	VNDfree(aa,"ga_xinterpolate: aa");
	VNDfree(bb,"ga_xinterpolate: bb");

	return;
}

float rcdot(int n, complex *a, complex *b)
/****************************************************************************** 
return the real part of a complex dot product where
    the first vector is the one complex conjugated
*******************************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
*****************************************************************************/
{
	int j;
	float sum=0.;
	for(j=0;j<n;j++) sum += a[j].r * b[j].r + a[j].i * b[j].i;
	return(sum);
}

void htmul(int n, complex *a, complex *x, complex *y)

/******************************************************************************
   Hermitian Toeplitz matrix multiply

     solve for y = A x   where A is Hermitian Toeplitz

     and defined by the vector a giving the top row of A.
     x is input.  y is output. 
*******************************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
******************************************************************************/
{
	int j,irow;
	complex czero;
	czero.r=czero.i=0.;

	for(irow=0;irow<n;irow++) {
		y[irow]=czero;
		for(j=0;j<irow;j++)
			y[irow] = cadd(cmul(conjg(a[irow-j]),x[j]),y[irow]);
		for(j=irow;j<n;j++)
			y[irow] = cadd(cmul(a[j-irow],x[j]),y[irow]);
	}
}

void runav(int n,int len,float *a,float *b)
/******************************************************************************
compute a boxcar running average filter
*******************************************************************************
int n   	number of samples in a[] and b[]
int len 	length of running average in samples
float a[n]	input array
float b[n]	output array
*******************************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
******************************************************************************/
{
	float sum=0.;
	int j,lenh=len/2;
	if(len<=1) {
		for(j=0;j<n;j++) b[j]=a[j];
		return;
	}
	for(j=0;j<MIN(len,n);j++) sum+=a[j];
	for(j=0;j<MIN(lenh,n);j++) b[j]=sum;
	for(j=lenh;j<n-lenh;j++) {
		sum=sum+a[j+lenh]-a[j-lenh];
		b[j]=sum;
	}
	for(j=MAX(0,n-lenh);j<n;j++) b[j]=sum;
	for(j=0;j<n;j++) b[j]/=len;
	return;
}

void taupmute(int ip,int ipa,int ipb,int nt, int ltap, float *rt)
/******************************************************************************
do simple tau-p mute to elliminate multiples
*******************************************************************************
Function parameters:

int ip		current ray parameter index
int ipa		max ray parameter primary  pick at maximum time
int ipb		max ray parmater primary pick at minimum time
int nt		number of time samples
int ltap	length of mute taper in samples
float rt[nt]	tau-p data for all tau values

*******************************************************************************
Author: John Anderson (visitor to CSM from Mobil) Spring 1993
******************************************************************************/

{
	int j,k;
	float w;
	if(ip<=ipa) return;
	if(ip>=ipb) {
		for(k=0;k<nt;k++) rt[k]=0;
		return;
	}
	w=MAX(ipb-ipa,1);
	w=(ipb-ip)/w;
	j=w*nt;
	for(k=0;k<j;k++) rt[k]=0.;
	for(k=j;k<MIN(nt,j+ltap);k++) rt[k]*=(k-j)/ltap;
}
