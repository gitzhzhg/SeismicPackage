/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


#include "par.h"
#include "Reflect/reflpsvsh.h"
/******************************Self-Documentation*****************************/
/******************************************************************************
SYNTHETICS - Subroutine to compute a synthetic seismogram from the output
				of the reflectivity modeling code

*******************************************************************************
Function Prototypes:
void compute_synthetics (int verbose, int nt, int ntc, int nx, int nor, int nw,
	int nlayers, int lsource, int nf, int flt, int vsp, int win, int wtype, 
	float tlag, float red_vel, float decay, float tsec, float bx, float dx, 
	float w1, float w2, int *fil_phase, int nfilters, int *fil_type, 
	float *dbpo, float *f1, float *f2, int *lobs, float *cl, float *t, 
	complex ***response, float **reflectivity, FILE *outfp);

void red_vel_factor (float x, float red_vel, float tlag, complex wpie, 
	complex wsq, complex *rvfac);

void construct_tx_trace (int nw, int nt, int ntfft, int nfilters,
	int *filters_phase, float tsec, float unexp, float sphrd, complex *refw,
	int *fil_type, float *f1, float *f2, float *dbpo, float *reft);

void compute_Hanning_window (int iwin, int iw, int if1, int if2, int nf, 
	complex *win);

void apply_filters (int *min_phase, int nfilters, int nw, float tsec, float *f1,
	float *f2, float *dbpo, int *filter_type, complex *refw);
*******************************************************************************
compute_synthetics:
Input:
nt			number of time samples in the computed synthetics
tlag		time lag in seconds to be used in computing the synthetics
nf			number of frequencies to be used in computing the synthetics.
			Set to zero if all frequencies are to be used
flt			=1 to apply the earth flattening correction
vsp			=1 to compute synthetic vsp
win			=1 ray parameter windows ???
wtype		=1 for displacement
			=2 for velocity
			=3 for acceleration
w1			frequency to start lo end hanning taper. If w1=0, deafult=0.15*nf
			frequency to start hi end hanning taper. If w2=0, deafult=0.85*nf
red_vel		reducing velocity in km/s. If set to zero, use the highest
			model velocity
nfilters	number of filters to be applied to the synthetics
fil_phase	=1 for minimum phase filters
			=0 for zero phase filters
fil_type	array[nfilters] of filter flags: =1 for high cut, =2 for low cut,
			=3 for notch filter
dbpo		array[nfilters] of slopes in db/octave
f1			frequency 1 in hertz
f2 			frequency 2 in hertz (only in the case of notch filter)
response	array[nw]nx][nor] of computed wavefield amplitudes 

Output:
reflectivity	array[nx][nt] of computed reflectivities	
*******************************************************************************
compute_red_vel_factor
Input:
x			range (offset) of current trace 
red_vel		reducing velocity
tlag		time lag to be applied to the seismogram
wpie
wsq			complex factor

Output
rvfac		computed reducing velocity factor
*******************************************************************************
construct_tx_trace:
Input:
nw			number of frequencies
nt			number of time samples
ix			trace index
nfilt		number of filters to be applied
tsec		length of computed trace
unexp		parameter to undo decay factor	
sphrd		flattening earth correction factor
refw		array[nw+1] of complex trace

Output:
reft		array[nt] current trace in t-x domain
*******************************************************************************
compute_Hanning_window:
Input:
iwin		=1 for Hanning window
if1			lower f=window frequency
if2			higher window frequency
nw			number of frequencies in output data
iw			current frequency

Output:
win			computed Hanning window for given frequency
*******************************************************************************
apply_filters:
Input:
min_phase		=1 for minimum phase filters
				=0 for zero phase filters
nfilters		number of filters to apply
nw				number of frequencies
tsec
f1				array[nfilters] of low frequencies
f2				array[nfilters] of high frequencies
dbpo			array[nfilters] of filter slopes in db/oct
filtype			array[nfilters] of filter types:
				=1 low cut filter
				=2 high cut filter
				=3 notch filter
refw			array[nw+1] of complex samples to filter

Output:
refw			array[nw+1] of complex filtered samples 

Note:
refw contains only the positive frequencies, the negatives are simply their
complex conjugates and can be computed when necessary
*******************************************************************************/
/***********************************End Self_documentation********************/




/******************************************************************************

	Subroutine to generate the synthetic seismograms based on the
		information contained in the files wxp,wxr and wxz

******************************************************************************/
void compute_synthetics (int verbose, int nt, int ntc, int nx, int nor, int nw,
	int nlayers, int lsource, int nf, int flt, int vsp, int win, int wtype, 
	float tlag, float red_vel, float decay, float tsec, float bx, float dx, 
	float w1, float w2, int *fil_phase, int nfilters, int *fil_type, 
	float *dbpo, float *f1, float *f2, int *lobs, float *cl, float *t, 
	complex ***response, float **reflectivity, FILE *outfp)
/****************************************************************************** 
Input:
nt			number of time samples in the computed synthetics
tlag		time lag in seconds to be used in computing the synthetics
nf			number of frequencies to be used in computing the synthetics.
			Set to zero if all frequencies are to be used
flt			=1 to apply the earth flattening correction
vsp			=1 to compute synthetic vsp
win			=1 ray parameter windows ???
wtype		=1 for displacement
			=2 for velocity
			=3 for acceleration
w1			frequency to start lo end hanning taper. If w1=0, deafult=0.15*nf
			frequency to start hi end hanning taper. If w2=0, deafult=0.85*nf
red_vel		reducing velocity in km/s. If set to zero, use the highest
			model velocity
nfilters	number of filters to be applied to the synthetics
fil_phase	=1 for minimum phase filters
			=0 for zero phase filters
fil_type	array[nfilters] of filter flags: =1 for high cut, =2 for low cut,
			=3 for notch filter
dbpo			array[nfilters] of slopes in db/octave
f1			frequency 1 in hertz
f2 			frequency 2 in hertz (only in the case of notch filter)
response	array[nw]nx][nor] of computed wavefield amplitudes 

Output:
reflectivity	array[nx][nt] of computed reflectivities	
******************************************************************************/
{	
	int il,iw,ix,iz=0;	/* loop counters */
	int kz;			/* auxiliary index */
	int ntfft;		/* number of FFT samples */
	int nw1;		/* number of frequencies for FFT */
	float sum=0.0;		/* auxiliary variable */
	float *zr,zs=0.0;	/* depths of source and receivers */
	float sphrd=1.0;	/* factor for flattening earth correction */
	int if1=0,if2=0; 	/* freq indices to start and end Hanning window */
	float unexp;		/* parameter to undo decay factor */
	float x,z;		/* range and depth */
	float w;		/* frequency in rad/sec */
	complex wpie;		/* complex frequency */
	complex wsq;		/* complex frequency squared */
	complex rvfac;		/* reducing velocity factor */
	complex *refw;		/* scratch array for complex wavefield */
	complex window;		/* frequency domain Hanning window */

	/* if requested, check input parameters */
	if (verbose==1||verbose==3) {
		fprintf (stderr,"in compute reflectivity series, checking input ...\n");
		fprintf (stderr,"nt=%d nx=%d nor=%d nw=%d",nt,nx,nor,nw);
		fprintf (stderr," nlayers=%d lsource=%d nf=%d\n",nlayers,lsource,nf);
		fprintf (stderr,"flt=%d vsp=%d win=%d wtype=%d\n",flt,vsp,win,wtype);
		fprintf (stderr,"tlag=%g red_vel=%g decay=%g ",tlag,red_vel,decay);
		fprintf (stderr,"tsec=%g bx=%g dx=%g w1=%g w2=%g\n",tsec,bx,dx,w1,w2);
		fprintf (stderr,"nfilters=%d\n",nfilters);
	}
	if (verbose==2||verbose==3) {
		fprintf (outfp,"in compute reflectivity series, checking input ...\n");
		fprintf (outfp,"nt=%d nx=%d nor=%d nw=%d",nt,nx,nor,nw);
		fprintf (outfp," nlayers=%d lsource=%d nf=%d\n",nlayers,lsource,nf);
		fprintf (outfp,"flt=%d vsp=%d win=%d wtype=%d\n",flt,vsp,win,wtype);
		fprintf (outfp,"tlag=%g red_vel=%g decay=%g ",tlag,red_vel,decay);
		fprintf (outfp,"tsec=%g bx=%g dx=%g w1=%g w2=%g\n",tsec,bx,dx,w1,w2);
		fprintf (outfp,"nfilters=%d\n",nfilters);
	}

	/* compute number of fft samples and frequencies */
	ntfft=npfa(ntc);
	nw1=ntfft/2;
	if (nf>0 && nf<=nw) nw=nf;
	fprintf (stderr,"\n\n nt=%d ntfft=%d nw=%d nw1=%d\n",nt,ntfft,nw,nw1);

	/* allocate working space */
	zr=alloc1float(nor);
	refw=alloc1complex(ntfft);

	/* if not provided, set reducing velocity to highest pwave velocity */
	if (red_vel<0.0) {
		red_vel=1.51;	
		for (il=0; il<nlayers; il++) {
			if (cl[il]>red_vel) red_vel=cl[il];
		}
	}

	/* compute depths of source and receivers */
	iz=0;
	for (il=0; il<nlayers-1; il++) {
		kz=il+1;
		sum +=t[il];
		if (kz==lsource-1) zs=sum;	/* depth of the source */
		if ((iz<nor)&&(kz==lobs[iz]-1)) {
			zr[iz]=sum;		/* depth of the receivers */
			iz++;
		}
	}

	/* if requested, output processing information */
	if (verbose==1||verbose==3) {
		fprintf(stderr,"\nSOURCE AND RECEIVER INFORMATION\n");
		fprintf(stderr,"\nReducing velocity=%g source depth=%g\n",red_vel,zs);
		fprintf(stderr,"\nReceiver Number  Depth   \n");
		for (iz=0; iz<nor; iz++) {
			fprintf (stderr,"%8d%15.3f\n",iz,zr[iz]);
		}
	}
	if (verbose==2||verbose==3) {
		fprintf(outfp,"\nSOURCE AND RECEIVER INFORMATION\n");
		fprintf(outfp,"\nReducing velocity=%g source depth=%g\n",red_vel,zs);
		fprintf(outfp,"\nReceiver Number  Depth   \n");
		for (iz=0; iz<nor; iz++) {
			fprintf (outfp,"%8d%15.3f\n",iz,zr[iz]);
		}
	}
	
	/* compute windowed frequencies */
	if (win==1) {
		if (w1==0) if1=0.15*nf;
		else  if1=w1*tsec;
		if (w2==0) if2=0.85*nf;
		else if2=w2*tsec;
	}
	unexp=decay*tsec;					/* to undo exponential decay */

	/* main loop to compute a vsp or a seismogram */
	if (vsp==1) {
		for (ix=0; ix<nx; ix++) {

			/* compute range and flattening correction factor */
			x=bx+dx*ix;
			if (flt==1) sphrd=sqrt((x/RSO)/ABS(sin(x/RSO)));

			/* loop over the receivers */
			for (iz=0; iz<nor; iz++) {
				z=zr[iz];

				/* loop over frequencies */
				for (iw=0; iw<nw1; iw++) {

					/* initialize output array */
					if (iw<nw) {
						w=(iw+1)*PI*2/tsec;
						refw[iw+1]=response[iw][ix][iz];
					} else {
						w=0.0;
						refw[iw+1]=cmplx(0.0,0.0);
					}

					/* compute complex frequencies */
					wpie=cmplx(w,decay);
					wsq=cneg(cmul(cmplx(0.0,1.0),wpie));

					/* compute reducing velocity factor */
					red_vel_factor (x, red_vel, tlag, wpie,wsq, &rvfac);

					/* compute Hanning window */
					compute_Hanning_window (win, iw, if1, if2, nw, &window);

					/* apply Hanning window */
					refw[iw+1]=cmul(refw[iw+1],cmul(rvfac,window));
				}
			
				/* construct time image */
				construct_tx_trace (nw1, nt, ntc, ntfft, nfilters, fil_phase,
					tsec, unexp, sphrd, refw, fil_type, f1, f2, dbpo,
					reflectivity[ix]);
			}
		}

	} else {		/* compute "normal" seismogram */

		/* loop over recievers */	
		for (iz=0; iz<nor; iz++) {
			z=zr[iz];

			/* loop over ranges (seismogram traces) */
			for (ix=0; ix<nx; ix++) {
				x=bx+dx*ix;

				/* if earth flattening correction requested */
				if (flt==1) sphrd=sqrt((x/RSO)/ABS(sin(x/RSO)));
				
				/* loop over frequencies */
				for (iw=0; iw<nw1; iw++) {

					/* initialize output array */
					if (iw<nw) {
						w=(iw+1)*PI*2/tsec;
						refw[iw+1]=response[iw][ix][iz];
					} else {
						w=0.0;
						refw[iw+1]=cmplx(0.0,0.0);
					}

					/* compute complex frequencies */
					wpie=cmplx(w,decay);
					wsq=cmul(cmplx(0.0,1.0),wpie);
					wsq=cipow(wsq,wtype);

					/* compute reducing velocity factor */
					red_vel_factor (x, red_vel, tlag, wpie, wsq, &rvfac);

					/* compute Hanning window */
					compute_Hanning_window (win, iw, if1, if2, nw, &window);

					/* apply Hanning window and rvfac */
					refw[iw+1]=cmul(refw[iw+1],cmul(rvfac,window));
				}

				/* construct time image */
				construct_tx_trace (nw1, nt, ntc, ntfft,nfilters, fil_phase,
					tsec, unexp, sphrd, refw, fil_type, f1, f2, dbpo,
					reflectivity[ix]);
			}
		}
	}

	/* free working space */
	free1float(zr);
	free1complex(refw);
}
		
/******************************************************************************

			Subroutine to compute the reducing velocity factor

******************************************************************************/
void red_vel_factor (float x, float red_vel, float tlag, complex wpie, 
	complex wsq, complex *rvfac)
/******************************************************************************
Input:
x			range (offset) of current trace 
red_vel		reducing velocity
tlag		time lag to be applied to the seismogram
wpie
wsq			complex factor

Output
rvfac		computed reducing velocity factor
******************************************************************************/
{

	/* compute reducing velocity factor */
	if (red_vel==0.) {
		*rvfac=crmul(cmul(cmplx(0.0,1.0),wpie),tlag);
	} else {
		*rvfac=crmul(cmul(cmplx(0.0,1.0),wpie),tlag-x/red_vel);
	}

	/* multiply by complex factor */
	*rvfac=cmul(cwp_cexp1(*rvfac),wsq);
}


/******************************************************************************	

			Subroutine to construct the trace in t-x domain

******************************************************************************/
void construct_tx_trace (int nw, int nt, int ntc, int ntfft, int nfilters,
	int *filters_phase, float tsec, float unexp, float sphrd, complex *refw,
	int *fil_type, float *f1, float *f2, float *dbpo, float *reft)
/******************************************************************************
Input:
nw				number of frequencies
nt				number of time samples
ix				trace index
nfilt			number of filters to be applied
tsec
unexp
sphrd			flattening earth correction factor
refw			array[nw+1] of complex trace

Output:
reft			array[nt] current trace in t-x domain
******************************************************************************/
{
	int it;			/* loop counter */
	float a,b,c;		/* auxiliary variables */
	
	/* if requested, apply filters */
	if (nfilters !=0)  {
		apply_filters (filters_phase, nfilters, nw, tsec, f1, f2, dbpo, 
			fil_type, refw);
	}

	/* inverse fft to transfer data to t-x domain */
	pfacc (-1, ntfft, refw);
	

	/* construct the t-x image */
	for (it=0; it<nt; it++) {
		a=(float)it/ntc;
		b=unexp*a;
		c=exp(b);
		reft[it]=refw[it].r*c*sphrd;
	}

	/* reinitialize working arrays */
	for (it=0; it<ntfft; it++) {
		refw[it]=cmplx(0.0,0.0);
	}
}
					
					
/******************************************************************************

				Subroutine to compute a Hanning window

******************************************************************************/
void compute_Hanning_window (int iwin, int iw, int if1, int if2, int nf, 
	complex *win)
/******************************************************************************
Input:
iwin		=1 for Hanning window
if1			lower f=window frequency
if2			higher window frequency
nw			number of frequencies in output data
iw			current frequency

Output:
win			computed Hanning window for given frequency
******************************************************************************/
{

	/* compute Hanning window */
	if (iwin==1) {
		if (iw<if1-1) {
			*win=crmul(cmplx(1.0,0.0),0.5*(1.+cos(PI*(if1-iw+1)/if1)));
		} else if((iw>=if1-1)&&(iw<=if2-1)) {
			*win=cmplx(1.0,0.0);
		} else if((iw>if2-1)&&(iw<nf-1)) {
			*win=crmul(cmplx(1.0,0.0),0.5*(1.+cos(PI*(iw-if2+1)/(nf-if2))));
		} else {
			*win=cmplx(0.0,0.0);
		}
	} else {
		if (iw<nf-1) {
			*win=crmul(cmplx(1.0,0.0),0.5*(1.+cos(PI*(iw+1)/nf)));
		} else {
			*win=cmplx(0.0,0.0);
		}
	}
}
			
/******************************************************************************

		Subroutine to apply hi-cut, lo-cut or notch zero or minimum
							phase filters

******************************************************************************/
void apply_filters (int *min_phase, int nfilters, int nw, float tsec, float *f1,
	float *f2, float *dbpo, int *filter_type, complex *refw)
/******************************************************************************
Input:
min_phase		=1 for minimum phase filters
				=0 for zero phase filters
nfilters		number of filters to apply
nw				number of frequencies
tsec
f1				array[nfilters] of low frequencies
f2				array[nfilters] of high frequencies
dbpo				array[nfilters] of filter slopes in db/oct
filtype			array[nfilters] of filter types:
				=1 low cut filter
				=2 high cut filter
				=3 notch filter
refw			array[nw+1] of complex samples to filter

Output:
refw			array[nw+1] of complex filtered samples 
*******************************************************************************
Note:
refw contains only the positive frequencies, the negatives are simply their
complex conjugates and can be computed when necessary
******************************************************************************/
{
	int i,j,iw;			/* loop counters */
	int nw1=nw+1;		/* number of frequencies (positive+zero) */
	int nwt=2*nw;		/* total number of frequencies */
	int index1,index2;	/* auxiliary indices for notch filter */
	float delfq=0.0;
	float poles;		/* filter poles */
	float r,factor;		/* auxiliary variables */
	float maxabs;
	complex *filter;	/* scratch array for filter coefficients */
	complex *filter1;	/* scratch array for filter coefficients */


	/* defensive programming */
	if (nfilters==0) return;

	/* allocate working space */
	filter=alloc1complex(2*nwt);
	filter1=alloc1complex(2*nwt);

	/* initialize filter array */
	for (iw=0; iw<nw1; iw++) filter[iw]=cmplx(1.0,0.0);

	/* main loop over number of requested filters */
	for (i=0; i<nfilters; i++) {
		
		/* smearing range */
		if (min_phase[i]==1) delfq=2*PI/tsec;

		if (filter_type[i]==1) { 	/* if low-pass Butterworth */
			poles=dbpo[i]/6.+1;
			r=1./((f1[i]-delfq)*tsec);

			/* compute filter coefficients */
			for (iw=0; iw<nw1; iw++) {
				factor=1.0/sqrt(1.+pow(iw*r,poles));
				filter[iw]=crmul(filter[iw],factor);
			}

		} else if (filter_type[i]==2) {	/* hi-pass Butterworth */
			poles=dbpo[i]/6.+1;
			r=1./((f1[i]+delfq)*tsec);

			/* compute filter coefficients */
			for (iw=0; iw<nw1; iw++) {
				factor=pow(iw*r,poles);	
				filter[iw]=crmul(filter[iw],sqrt(factor/(1+factor)));
			}

		} else if (filter_type[i]==3) { /* notch filter */
			index1=(f1[i]-delfq)*tsec+1;
			index2=(f2[i]+delfq)*tsec+2;

			if ((index2-index1)<16) {
				warn("in notch filter %d f1 too close to f2, "
				"this filter skiped\n",i);
				continue;
			}

			/* db reduction Neils design */
			r=1.0-pow(10.0,-dbpo[i]/20.);
			for (j=index1; j<=index2; j++) {
				factor=cos(2*PI*(j-index1)/(index2-index1))-1;
				filter[j-1]=crmul(filter[j-1],(1+r*factor)/2.);
			}
		} else {
				warn("unknown filter type for filter number %d,"
				" this filter skiped\n",i);
		}

		/* apply minimum phase correction */
		if (min_phase[i]==1) {
			for (iw=0; iw<nw1; iw++) {
				if (filter[iw].r==0.0) {
					filter[iw]=cmplx(-30.0,0.0);
				} else {

					/* set A[w]=ln(f[w]) */
					filter[iw]=cmplx(log(filter[iw].r),0.0);
				}
			}


			/* take care of negative frequencies needed for FFT */
			for (iw=nw1; iw<nwt; iw++) {
				filter[iw]=conjg(filter[nwt-iw+1]);
			}

			/* take inverse FFT to go to time domain */
			pfacc (-1, nwt, filter);

			for (iw=1; iw<nw1; iw++) {
				r=(1.+cos(PI*iw/nw))/2.;	
				filter[iw]=crmul(filter[iw],r);
				filter[nwt-iw+1]=crmul(filter[nwt-iw+1],r);
			}

			/* construct filter coefficients for all frequencies */
			for (iw=0; iw<nw1; iw++) {
				filter1[iw]=filter[iw];
			}
			for (iw=nw1; iw<nwt; iw++) {
				filter1[iw]=cneg(conjg(filter[iw]));
			}

			/* take forward FFT to get back to frequency domain */
			pfacc (1, nwt, filter);
			pfacc (1, nwt, filter1);
	
			maxabs=0.0;
			for (iw=0; iw<nw1; iw++) {
				filter[iw]=cwp_cexp1(cadd(filter[iw],filter1[iw]));
				maxabs=MAX(rcabs(filter[iw]),maxabs);
			}
	
			/* normalize */
			maxabs=1.0/maxabs;
			for (iw=0; iw<nw1; iw++) {
				filter[iw]=crmul(filter[iw],maxabs);
			}
		}
	}

	/* apply the filter */
	for (iw=0; iw<nw1; iw++) {
		refw[iw]=cmul(refw[iw],filter[iw]);
	}
	for (iw=nw1; iw<nwt; iw++) {
		refw[iw]=conjg(refw[nwt-iw]);
	}

	/* free allocated space */
	free1complex(filter);
	free1complex(filter1);
}
