/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


#include "par.h"
#include "Reflect/reflpsvsh.h"

/******************************************************************************	

		Subroutine to compute psv or sh reflectivities

******************************************************************************/
void compute_reflectivities (int int_type, int verbose, int wtype, int wfield,
	int vsp, int flt, int win, int nx, int nt, int ntc, int nor, int nf,
	int nlayers, int lsource, int layern, int nfilters, int *filters_phase,
 	int nw, int np, float bp, float tlag, float red_vel, float w1,float w2,
 	float fx, float dx, float bx, float fs, float decay, float p2w,
	float tsec, float fw, float wrefp, float wrefs, float epsp, float epss,
	float sigp, float sigs, float pw1, float pw2, float pw3, float pw4,
	float h1, float h2, float m1, float m2, float m3, float fref, int *lobs,
	int *filters_type, float *dbpo, float *f1, float *f2, float *cl,
	float *ct, float *ql, float *qt, float *rho, float *t,
	float **wavefield1, float **wavefield2, float **wavefield3, FILE *outfp)
/******************************************************************************
Input:
int_type	=1 to compute the slowness integration by the trapezoidal rule
		=2 to use a first order Filon scheme
wtype		=1 for psv synthetics. =2 for sh synthetics
wfield		=1 for displacement, =2 for velocity, =3 for acceleration
vsp		=0 for surface synthetic
		=1 for vsp (vertical seismic profiling) synthetic
flt		=1 if earth flattening approximation required
win		=1 if frequency windowing required
nx		number of ranges (traces) in the synthetic seismogram
nt		number of samples per trace in the synthetics
nor		number of receivers
nf		number of frequencies in the synthetics
nlayers		pointer to number of reflecting layers
lsource		layer of top of which the source is located
layern
nfilters	number of filters to apply to the seismograms
filters_phase	=0 for zero phase filters. =1 for minimum phase filters
tlag		time lag to introduce in the seismograms
red_vel		reducing velocity, set to maximum compressional velocity if zero
w1		frequency to start lo-cut taper. =0.15*nw if zero 
w2		frequency to start hi-cut taper. =0.85*nw if zero 
dx		range increment in kms
bx		beginning range in kms
fs		sampling parameter, usually between 0.07 and 0.12.
		sampling is finer as fs increases
decay		decay factor, used to avoid time series wraparoundp
p2w		maximum ray parameter to which the synthetics are
		computed
tsec		length of computed trace in seconds
epsp
epss
sigp
sigs
lobs		array[nor] of layers on top of which the receivers are located
filters_type	array[nfilters] of requested filter types.
		=1 for lo-cut filter
		=2 for hi-cut filter
		=2 for notch filter
dbpo		array[nfilters] of filter slopes in db/octave
f1		array[nfilters] of frequency to start filtering action
f2		array[nfilters] of frequency to end filtering action
cl		array[nlayers] of compressional wave velocities
ql		array[nlayers] of compressional wave Q-values
ct		array[nlayers] of shear wave velocities
qt		array[nlayers] of shear wave Q-values
rho		array[nlayers] of densities
t		array[nlayers] of layer thicknesses

Output:
wavefield1	array[nx][nt] of pressure reflecivities if wtype=1 (PSV)
		or tangential wavefield reflectivities if wtype=2 (SH)
wavefield2	array[nx][nt] of radial wavefield component if wtype=1
		not used if wtype=2
wavefield3	array[nx][nt] of vertical wavefield component if wtype=1
		not used if wtype=2
*******************************************************************************
Notes:
This subroutine uses the reflectivity technique to compute the synthetic
seismic response for a stratified visco-elastic earth and may be used to
model the exploration seismic and earthquake data.  This is specially
suitable for modelling the Po/So as it is capable of handling the frequencies
and the distances involved in such computations.  
For small frequencies (w), 1/Q is proportional to w/eps
For large frequencies (w), 1/Q is proportional to w**(-sigma)
The reference frequency is one hertz
******************************************************************************/
{
	int il;				/* loop counters */
	float xmax;			/* maximum range */
	int *acoustic;			/* flags for receiver type */
	int *flag;			/* ??? */
	complex ***response1=NULL;	/* scratch array for pressure */
	complex ***response2=NULL;	/* scratch array for x-wavefield */
	complex ***response3=NULL;	/* scratch array for z-wavefield */

	/* initialize variables */
	fprintf (stderr,"original decay factor: %g nx=%d\n",decay,nx);
	fx=bx+dx*(nx-1);
	xmax=6*fx;
	if (decay<=0.0) decay=50.0;
	decay=log(decay)/tsec;

	/* allocate working space */
	acoustic=alloc1int(nor);
	flag=alloc1int(nor);
	response1=ealloc3complex(nor,nx,nw);

	if (wtype==1) {
		response2=ealloc3complex(nor,nx,nw);
		response3=ealloc3complex(nor,nx,nw);
	}

	/* determine type of sources and receivers */
	source_receiver_type (nor, nlayers, lsource, lobs, cl, ct, acoustic, 
		flag);

	if (verbose==1||verbose==3) {
		fprintf(stderr,"Number of traces %d\n",nx);
		fprintf(stderr,"Offset of first trace %g\n",bx);
		fprintf(stderr,"Offset of last trace %g\n",fx);
		fprintf(stderr,"Trace to trace offset %g\n",dx);
		fprintf(stderr,"Number of samples in output traces %d\n",nt);
		fprintf(stderr,"Time sampling interval %g\n",tsec/nt);
		fprintf(stderr,"Trace length in seconds %g\n",tsec);
		fprintf(stderr,"Time lag applied to each trace (sec)%g\n",tlag);
		fprintf(stderr,"Total number of layers processed %d\n",nlayers);
		fprintf(stderr,"Total number of frequencies processed %d\n",nw);
		fprintf(stderr,"Decay factor applied=%g\n",decay);
		fprintf(stderr,"Number of frequencies in out traces %d\n",nf);
		fprintf(stderr,"First frequency in out traces %g\n",fref/tsec);
		fprintf(stderr,"Last frequency in output traces %g\n",nw/tsec);
		fprintf(stderr,"\nPROCESSING INFORMATION FOR EACH FREQUENCY\n");
		fprintf(stderr,"  iw   w	bp    fp    np    dp    "
			"pw1	pw2	pw3	pw4   \n");
	} if (verbose==2||verbose==3) {
		fprintf(outfp,"Number of traces %d\n",nx);
		fprintf(outfp,"Offset of first trace %g\n",bx);
		fprintf(outfp,"Offset of last trace %g\n",fx);
		fprintf(outfp,"Trace to trace offset %g\n",dx);
		fprintf(outfp,"Number of samples in output traces %d\n",nt);
		fprintf(outfp,"Time sampling interval %g\n",tsec/nt);
		fprintf(outfp,"Trace length in seconds %g\n",tsec);
		fprintf(outfp,"Time lag applied to each trace in sec%g\n",tlag);
		fprintf(outfp,"Total number of layers processed %d\n",nlayers);
		fprintf(outfp,"Total number of frequencies processed %d\n",nw);
		fprintf(outfp,"Decay factor applied=%g\n",decay);
		fprintf(outfp,"Number of frequencies in out traces %d\n",nf);
		fprintf(outfp,"First frequency in out traces %g\n",fref/tsec);
		fprintf(outfp,"Last frequency in output traces %g\n",nw/tsec);
		fprintf(outfp,"\nPROCESSING INFORMATION FOR EACH FREQUENCY\n");
		fprintf(outfp,"  iw   w	bp    fp   np    dp    "
			"pw1	pw2    pw3	pw4\n");
	}


	/* compute reflectivities */
	if (wtype==1) { 		/* PSV */
		psv_reflectivities (int_type, verbose, wtype, nw, nlayers, nx,
			layern, nor, np, bp, m1, m2, m3, h1, h2, lsource, bx,
			dx, xmax, decay, fref, wrefp, wrefs, tsec, p2w, fs,
			epsp, epss, sigp, sigs, pw1, pw2, pw3, pw4, acoustic,
			flag, lobs, rho, t, cl, ct, ql, qt, response1,
			response2, response3, outfp);

	} else if (wtype==2) {
		sh_reflectivities (int_type, verbose, wtype, nw, nlayers, nx,
			layern, nor, np, bp, m1, m2, m3, h1, h2, lsource, bx,
			dx, xmax, decay, fref, wrefp, wrefs, tsec, p2w, fs,
			epsp, epss, sigp, sigs, pw1, pw2, pw3, pw4, flag, lobs,
			rho, t, cl, ct, ql, qt, response1, outfp);
	}


	/* if requested, output processing information */
	if (verbose==1||verbose==3) {
		fprintf(stderr,"\nMODIFIED INPUT PARAMETERS\n");
		fprintf(stderr,"il   cl    ct	ql	qt	rho	t\n");
		for (il=0; il<nlayers; il++) {
			fprintf (stderr,"%3d%7.3f%7.3f%7.3f%7.3f%7.3f%7.3f\n",
				il,cl[il],ct[il],ql[il],qt[il],rho[il],t[il]);
		}
		fprintf(stderr,"\n");
	} if (verbose==2||verbose==3) {
		fprintf(outfp,"\nMODIFIED INPUT PARAMETERS\n");
		fprintf(outfp,"il    cl	ct	ql	qt	rho    t  \n");
		for (il=0; il<nlayers; il++) {
			fprintf (outfp,"%3d%7.3f%7.3f%7.3f%7.3f%7.3f%7.3f\n",
				il,cl[il],ct[il],ql[il],qt[il],rho[il],t[il]);
		}
		fprintf(outfp,"\n");
	}

	/* compute actual synthetic in t-x domain */
	if (wtype==1) {	

		/* pressure */
		compute_synthetics (verbose, nt, ntc, nx, nor, nw, nlayers, 
			lsource, nf, flt, vsp, win, wfield, tlag, red_vel, 
			decay, tsec, bx, dx, w1, w2, filters_phase, nfilters, 
			filters_type, dbpo, f1, f2, lobs, cl, t, response1, 
			wavefield1,outfp);

		/* radial component */
		compute_synthetics (verbose, nt, ntc, nx, nor, nw, nlayers, 
			lsource, nf, flt, vsp, win, wfield, tlag, red_vel, 
			decay, tsec, bx, dx, w1, w2, filters_phase, nfilters, 
			filters_type, dbpo, f1, f2, lobs, cl, t, response2, 
			wavefield2,outfp);

		/* vertical component */	
		compute_synthetics (verbose, nt, ntc, nx, nor, nw, nlayers, 
			lsource, nf, flt, vsp, win, wfield, tlag, red_vel, 
			decay, tsec, bx, dx, w1, w2, filters_phase, nfilters, 
			filters_type, dbpo, f1, f2, lobs, cl, t, response3, 
			wavefield3,outfp);

		/* free allocated space */
		free3complex(response1);
		free3complex(response2);
		free3complex(response3);

	} else if (wtype==2) {

		/* tangential component */
		compute_synthetics (verbose, nt, nx, ntc, nor, nw, nlayers, 
			lsource, nf, flt, vsp, win, wfield, tlag, red_vel, 
			decay, tsec, bx, dx, w1, w2, filters_phase, nfilters, 
			filters_type, dbpo, f1, f2, lobs, cl, t, response1, 
			wavefield1,outfp);

		/* free allocated space */
		free3complex(response1);
	} else err ("wtype flag has to be one ro two");

	/* free allocated space */
	free1int(acoustic);
	free1int(flag);
}
