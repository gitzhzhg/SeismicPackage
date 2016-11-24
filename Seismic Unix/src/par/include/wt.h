/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/************************************************************************
 *  Author:  Zhaobo Meng, 11/25/95,  Colorado School of Mines           *
 *  Modified: Carlos E. Theodoro, 06/25/97                              *
 *            Included options to:                                      *
 *      	- different level of resolution for each dimension;     *
 *      	- transform back the lower level of resolution, only.   *
 *                                                                      *
 * Reference:                                                           *
 * Daubechies, I., 1988, Orthonormal Bases of Compactly Supported       *
 * Wavelets, Communications on Pure and Applied Mathematics, Vol. XLI,  *
 * 909-996.                                                             *
 ************************************************************************/

#ifndef WT_H
#define WT_H

#include "par.h"

enum ToCorD {ToC,ToD};
typedef struct{
  int order,ioff,joff;
  float *cc,*cr,*phi,*psi,*weight;
} WtFilter;

typedef struct{
        int MaxLevel;   /* maximum level for wavelet function */
        int NDim;       /* number of dimensions */
        int order;      /* order of Daubechies wavelet */
        int *NPointsn;  /* number of samples in each dimension */ 
        int *Mraleveln; /* maximum mra level */
} WtSizes;

/* Function Prototypes */
void wt_cascade(WtFilter *wfilt,WtSizes *wtsizes);
void fhierfromcd (float *f,float *cd,
     WtFilter *wfilt,WtSizes *wtsizes,int justDC);
void wto1d(float *cd,int npoints,enum ToCorD tocord,
     WtFilter *wfilt);
void wto1dset(WtFilter *wfilt,WtSizes *wtsizes);
void wt1(float *cd,enum ToCorD tocord,int npoints,
     WtFilter *wfilt,WtSizes *wtsizes,int idim);
void wtn(float *cd,enum ToCorD tocord,
        WtFilter *wfilt,WtSizes *wtsizes,int justDC);

#endif /* end of WT_H */

