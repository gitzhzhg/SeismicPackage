/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


#include "par.h"
#include "Reflect/reflpsvsh.h"
/******************************************************************************

	Subroutine to convolve reflectivity array with a wavelet to 
			produce a seismogram

******************************************************************************/
void convolve_wavelet (int wavelet_type, int nx, int nt, float dt, float fpeak, 
	float **wfield) 
/******************************************************************************
Input:
wavelet_type	=1 for a spike
		=2 for Tong Fei's Ricker wavelet
		=3 for Larner's Ricker wavelet
		=4 for Akima wavelet
nx		number of horizontal samples (traces)
nt		number of vertical samples (samples/trace)
dt		time sampling interval
fpeak		frequency peak for Ricker or Akima wavelets
wfield		array[nx][nt] of reflectivities

Output
wfield		array[nx][nt] of seismic traces 
******************************************************************************/
{
	int ix,it;			/* loop counters */
	float *wavelet;			/* wavelet array */	
	float *temp;			/* scratch array */

	/* allocate working space */
	wavelet=alloc1float(nt);
	temp=alloc1float(nt);

	/* compute wavelet */
	if (wavelet_type==1) {
		spike_wavelet (nt, 1, wavelet);
	} else if (wavelet_type==2) {
		ricker1_wavelet (nt, dt, fpeak, wavelet);
	} else if (wavelet_type==3) {
		ricker2_wavelet (nt/2, dt, 1./fpeak, 1., 0., wavelet);
	} else if (wavelet_type==4) {
		akb_wavelet (nt, dt, fpeak, wavelet);
	} else err ("wavelet_type has to be 1,2,3 or 4");
		
	/* loop over traces to convolve with wavelet */
	for (ix=0; ix<nx; ix++) {

		/* save input reflectivity trace */
		for (it=0; it<nt; it++) temp[it]=wfield[ix][it];
	
		/* convolve with wavelet */
		convolve_cwp (nt, 0, temp, nt, 0, wavelet, nt, 0, wfield[ix]);
	}

	/* free allocated space */
	free1float(temp);
	free1float(wavelet);
}
