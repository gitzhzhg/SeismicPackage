/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* WTLIB: $Revision: 1.4 $ ; $Date: 2011/10/20 21:08:05 $	*/

/*********************** self documentation **********************/
/***************************************************************************
WTLIB - Functions for wavelet transforms

wt_cascade - generate the mother wavelet and scaling function
fhierfromcd - calculates f(x,z) hierarchically from CD (wavelet coefficients)
wto1d - wavelet transform operator, 1D
wto1dset - setup wavelet operators
wt1 - 1D wavelet transform
wtn - n-D wavelet transform

********************************************************************
Function prototypes:
void wt_cascade(WtFilter *wfilt,WtSizes *wtsizes);
void fhierfromcd(float *f,float *cd,
     WtFilter *wfilt,WtSizes *wtsizes,int justDC);
void wto1d(float *cd,int npoints,enum ToCorD tocord,
     WtFilter *wfilt);
void wto1dset(WtFilter *wfilt,WtSizes *wtsizes);
void wt1(float *cd,enum ToCorD tocord,int npoints,
     WtFilter *wfilt,WtSizes *wtsizes,int idim);
void wtn(float *cd,enum ToCorD tocord,
     	WtFilter *wfilt,WtSizes *wtsizes, int dconly);

********************************************************************
wt_cascade:
Input:
WtFilter *wfilt		pointer to wavelet operator (filter)
WtSizes  *wtsizes	pointer to sizes of the wavelet operator

Returns:
wfilt->psi		pointer to mother wavelet
wfilt->phi		pointer to scaling function

fhierfromcd:
Input:
float *cd		pointer to wavelet coefficients
WtFilter *wfilt		pointer to wavelet operator (filters)
WtSizes *wtsizes	pointer to wavelet sizes
int justDC		flag =1 do DC (zero frequency)

Returns:
float *f		pointer to function f(x,z)

wto1d:
Input:
float *cd		pointer to wavelet coefficients, or f(x)
int npoints		size of the input signal
enum ToCorD tocord	=ToC inverse  or =ToD  forward transform
WtFilter *wfilt		pointer to wavelet operator (filters)

Returns:
float *cd		pointer to f(x) or to wavelet coefficients

wto1dset:
Input:
WtFilter *wfilt		pointer to wavelet filter
WtSizes *wtsizes	pointer to wavelet filter sizes

Return:
wfilt->order		order of Daubechies wavelet
wtsizes->sizes		sizes of the wavelet operator

wt1:
Input:
float *cd		pointer to wavelet coeff. or input f(x)
enum ToCorD tocord	=ToC (invers) =ToD (forward) wavelet transform
int npoints		size of signal f(x) or cd
WtFilter *wfilt		pointer to wavelet operator (filters)
WtSizes *wtsizes	pointer to wavelet sizes
int idim		index for dimension

Returns:
float *cd		pointer to input f(x) or to wavelet coefficients

wtn:
Input:
float *cd		pointer to n-D wavelet coeff, or f(x_1,..,x_n)
enum ToCorD tocord	=ToC (inverse) =ToD (forward) wavelet transform
WtFilter *wfilt		pointer to wavelet operator (filters)
WtSizes *wtsizes	pointer to wavelet sizes 
int dconly		keep and transform back form mra dc component only

Returns:
float *cd		pointer to f(x_1,...,x_n) or n-D wavelet coeffs.
********************************************************************
Notes:
wt_cascade:
Generate data sets phi[0..lengthphi-1] or psi[0..lengthphi-1].
phi or psi[i] = \phi or \psi (i>>MaxLevel).

fhierfromcd:
Once we have the wavelet coefficients cd, we could applied a	     
inverse wavelet transform to reconstruct the function f, this     
is the ordinary way to view the reconstructed f. A good alternative
is to make the "MRA display", which is to show f hierarchically.  
In "MRA display", the DC is put on the upper-left corner, and higher
frequency rectanges are on the lower-right corner.		     

wto1d:
1D wavelet transform operator, used to apply multi-dimensional  
wavelet transform, in which data of each dimension is extracted 
and 1D wavelet transform (forward: tocord==ToD; backward: tocord 
==ToC) is applied to this 1-D data. Before using wto1d, we must 
call wto1dset to set up the wavelet filters.			   
							   
wto1dset:
This subroutine initialize the wavelet filters for wavelet
transform operator wto1d. According to the order you choose
in your main (wtsizes->order=4 up to 20), wto1dset set up
the filter to be the Daubechies wavelet of this order/length. 
This subroutine will be need before calling wt1 or wtn. 

wt1:
1D wavelet transform. cd[1..npoints] as the discrete values of a 1D
function f(x) will be replaced by its wavelet transform if tocord
==ToD, or the wavelet coefficients cd[1..npoints] will be replaced
by the inverse wavelet transform, thus to obtain the reconstructed
f. The size of f npoints MUST be an integer power of 2.

wtn:
Given a n-D discrete values cd[...] of a n-D function f(x_1,...,x_n)
n-D wavelet transform will be applied if tocord==ToD; or given
a n-D wavelet coefficients of an n-D function, inverse wavelet
transform will be applied to reconstruct the n-D function f. Must
call wto1dset before calling wtn.
********************************************************************
References:
Daubechies, I., 1988, Orthonormal Bases of Compactly Supported
Wavelets, Communications on Pure or Applied Mathematics, Vol. XLI,
909-996.

Z. Meng & J. Scales, Multi-resolution Analysis with wavelet 	  
transform: An application to 2-D tomography, CWP-223, 1996    

W. Press et al., Numberical Recipes in C (second edition),     
Cambridge University Press, 1988				   
********************************************************************
Author:
CWP: Zhaobo Meng, Colorado School of Mines, Sept 1996
Modified: Carlos E. Theodoro, Colorado School of Mines, Jun 97
***************************************************************************/

/**************** end self doc ********************************/

#include "par.h"
#include "wt.h"

void wt_cascade(WtFilter *wfilt,WtSizes *wtsizes)
/***********************************************************************
wt_cascade - generate the mother wavelet and scaling function
***********************************************************************
Function Prototype:
void wt_cascade(WtFilter *wfilt,WtSizes *wtsizes);
***********************************************************************
Input:
WtFilter *wfilt		pointer to wavelet operator (filter)
WtSizes  *wtsizes	pointer to sizes of the wavelet operator

Returns:
wfilt->psi		pointer to mother wavelet
wfilt->phi		pointer to scaling function
***********************************************************************
Notes:
Generate data sets phi[0..lengthphi-1] or psi[0..lengthphi-1].
phi or psi[i] = \phi or \psi (i>>MaxLevel).
***********************************************************************
Reference:
Daubechies, I., 1988, Orthonormal Bases of Compactly Supported
Wavelets, Communications on Pure or Applied Mathematics, Vol. XLI,
909-996.
***********************************************************************
Authors:
CWP: Zhaobo Meng, Colorado School of Mines
Modified:
CWP: Carlos E. Theodoro, Colorado School of Mines (06/25/97)
     Included options for:
     - different level of resolution for each dimension;
     - transform back the lower level of resolution, only.
***********************************************************************/
{
        int   sig,k,j,minusn,n,ktemp;
        int   nphi;        /*length of the scaling function phi*/
	int   interval;    /*grid length in the coarsest version*/
	float *phiplus;    /*temp. phi*/
	float coe;         /*square root of 2 thing*/
	float sum=0;       /*summing varible*/
	int   lengthphi;   /*length of phi*/
	int   MaxLevel;    /*finest level phi is generated*/
  
	MaxLevel=wtsizes->MaxLevel;
	lengthphi=wtsizes->order<<MaxLevel;

	wfilt->psi=ealloc1float(lengthphi);
	wfilt->phi=ealloc1float(lengthphi);
	phiplus=ealloc1float(lengthphi);
	interval=1<<MaxLevel;
	nphi=wtsizes->order*interval;
	minusn=interval-nphi/2; /*minusn!*/
	for (k=0;k<wtsizes->order;k++) wfilt->phi[k]=wfilt->cc[k+1];
	for (k=wtsizes->order;k<nphi;k++) wfilt->phi[k]=0;
	coe=1;
	for (k=0;k<MaxLevel;k++) coe=coe*sqrt(2.0);
	/*coe=pow(2.0,MaxLevel/2.0);*/
	for (j=1;j<MaxLevel;j++) { /*the -jth level*/
	        for (n=0;n<nphi;n++) { /*calculate the nth point*/
		        phiplus[n]=0;
			for (k=0;k<nphi;k++){ /*summing up*/
			        if ((n-k-k>=0) && (n-k-k<wtsizes->order))
				        phiplus[n]+=wfilt->cc[n-k-k+1]*wfilt->phi[k];
			}/*next k*/
		}/*next n*/
		for (k=0;k<nphi;k++) wfilt->phi[k]=phiplus[k];
	}/*next j*/
	for (n=0;n<nphi;n++) {wfilt->phi[n]*=coe;sum+=wfilt->phi[n];}
	sum/=interval;
	coe=sqrt(2.0);
	for (k=0;k<nphi;k++){
	        wfilt->psi[k]=0;
		sig=-1;
		for (n=0;n<wtsizes->order;n++){
		        ktemp=2*(k+minusn)+(n-1)*interval;
			if ((ktemp>=0) && (ktemp<nphi))
			        wfilt->psi[k]+=sig*wfilt->cc[n+1]*coe*wfilt->phi[ktemp];
			sig=-sig;
		}/*next n*/
	}/*next k*/
	free1float(phiplus);
}/*end of wt_cascade*/

void fhierfromcd(float *f,float *cd,
     WtFilter *wfilt,WtSizes *wtsizes,int justDC)
/******************************************************************************
fhierfromcd - calculates f(x,z) hierarchically from CD (wavelet coefficients)
*******************************************************************************
Function Prototype:
void fhierfromcd(float *f,float *cd,
     WtFilter *wfilt,WtSizes *wtsizes,int justDC);
*******************************************************************************
Input:
float *cd		pointer to wavelet coefficients
WtFilter *wfilt		pointer to wavelet operator (filters)
WtSizes *wtsizes	pointer to wavelet sizes
int justDC		flag =1 do DC (zero frequency)

Returns:
float *f		pointer to function f(x,z)
*******************************************************************************
Notes:
Once we have the wavelet coefficients cd, we could applied a	     
inverse wavelet transform to reconstruct the function f, this     
is the ordinary way to view the reconstructed f. A good alternative
is to make the "MRA display", which is to show f hierarchically.  
In "MRA display", the DC is put on the upper-left corner, and higher
frequency rectanges are on the lower-right corner.		     
*******************************************************************************
Reference:                                                      
Z. Meng & J. Scales, Multi-resolution Analysis with wavelet 	  
transform: An application to 2-D tomography, CWP-223, 1996    
*******************************************************************************
 Author:   Zhaobo Meng, 11/25/95,  Colorado School of Mines         
 Modified: Carlos E. Theodoro, 06/25/97, Colorado School of Mines
           - modified to allow:
	     - different levels of mra for each dimension; and
	     - transform back only the lower level component, if required.
*******************************************************************************/
{
        int   level;	   /* level of decomposition */
	int   iz,jx,i,j; /* work varibles */
	int   StLevel;   /* starting level for MRA */
	int   StL1;      /* starting level for MRA */
	int   StL2;      /* starting level for MRA */
	float *workf;    /* work varible for signal f */
	float *workcd;   /* work varible for wavelet transform of signal f */
	int   n1;        /* signal size in the fastest direction */
	int   n2;        /* signal size in the slowest direction */
	int   sizez1;    /* signal size in the fastest direction */
	int   sizex2;    /* signal size in the slowest direction */
	int   leap;      /* leap for coarse image */
	float sum,leap2;
	long  int ntot;  /* 1-D length of the multi-D signal */

	n1=wtsizes->NPointsn[1];
	n2=wtsizes->NPointsn[2];
	ntot=n2*n1;
	workf=ealloc1float(ntot+1);
	workcd=ealloc1float(ntot+1);

	StL1=wtsizes->Mraleveln[1];
	StL2=wtsizes->Mraleveln[2];
	if (StL1<=StL2) {
	        StLevel=StL1;
	}
	else {
	        StLevel=StL2;
	}

	for (i=0;i<=ntot;i++) f[i]=0;

	for (level=0;level<StLevel;level++){
    
	  /* fprintf(stderr,"level=%d\n",level); */
    
		sizez1=n1>>level;
		sizex2=n2>>level;
		leap=2<<level;
		leap2=leap*leap;
              
		for (i=0;i<ntot+1;i++) workcd[i]=0;
		for (iz=sizez1/2;iz<sizez1;iz++)
		        for (jx=sizex2/2;jx<sizex2;jx++)
			        workcd[iz+jx*n1]=cd[iz+jx*n1]; 
		wtn(workcd,ToC,wfilt,wtsizes,justDC);
		for (i=0;i<ntot+1;i++) workf[i]=workcd[i];
		for (iz=sizez1/2;iz<sizez1;iz++){
		        for (jx=sizex2/2;jx<sizex2;jx++){
			        sum=0;
				for (i=0;i<leap;i++)
				        for (j=0;j<leap;j++)
					        sum+=workf[iz*leap+i-n1+(jx*leap-n2+j)*n1];
				f[iz+jx*n1]=sum/leap2;
			}
		}
	    
		for (i=0;i<ntot+1;i++) workcd[i]=0;
		for (iz=sizez1/2;iz<sizez1;iz++)
		        for (jx=0;jx<sizex2/2;jx++)
			        workcd[iz+jx*n1]=cd[iz+jx*n1];
		wtn(workcd,ToC,wfilt,wtsizes,justDC);
		for (i=0;i<ntot+1;i++) workf[i]=workcd[i];
		for (iz=sizez1/2;iz<sizez1;iz++){
		        for (jx=0;jx<sizex2/2;jx++){
			        sum=0;
				for (i=0;i<leap;i++)
				        for (j=0;j<leap;j++)
					        sum+=workf[iz*leap+i-n1+(jx*leap+j)*n1];
				f[iz+jx*n1]=sum/leap2;
			}
		}

		for (i=0;i<ntot+1;i++) workcd[i]=0;
		for (iz=0;iz<sizez1/2;iz++)
		        for (jx=sizex2/2;jx<sizex2;jx++)
			        workcd[iz+jx*n1]=cd[iz+jx*n1];
		wtn(workcd,ToC,wfilt,wtsizes,justDC);
		for (i=0;i<ntot+1;i++) workf[i]=workcd[i];
		for (iz=0;iz<sizez1/2;iz++){
		        for (jx=sizex2/2;jx<sizex2;jx++){
			        sum=0;
				for (i=0;i<leap;i++)
				        for (j=0;j<leap;j++)
					        sum+=workf[iz*leap+i+(jx*leap+j-n2)*n1];
				f[iz+jx*n1]=sum/leap2;
			}
		}
	}

	if (StL1==StL2) {
	        level=StLevel-1;

		sizez1=n1>>level;
		sizex2=n2>>level;
		leap=2<<level;
		leap2=leap*leap;
              
		/* fprintf(stderr,"level()=%d\n",level); */

		for (i=0;i<ntot+1;i++) workcd[i]=0;
		for (iz=0;iz<sizez1/2;iz++)
		        for (jx=0;jx<sizex2/2;jx++) 
			        workcd[iz+jx*n1]=cd[iz+jx*n1];
		wtn(workcd,ToC,wfilt,wtsizes,justDC);
		for (i=0;i<ntot+1;i++) workf[i]=workcd[i];
		for (iz=0;iz<sizez1/2;iz++){
		        for (jx=0;jx<sizex2/2;jx++){
			        sum=0;
				for (i=0;i<leap;i++)
				        for (j=0;j<leap;j++)
					        sum+=workf[iz*leap+i+(jx*leap+j)*n1];
				f[iz+jx*n1]=sum/leap2;
			}
		}    
	}

	if (StL1>StL2) {
	        for (level=StLevel;level<StL1;level++){
    
		  /* fprintf(stderr,"level=%d\n",level); */

			sizez1=n1>>level;
			sizex2=n2>>StL2;
			leap=2<<level;
			leap2=leap*leap;
              
			for (i=0;i<ntot+1;i++) workcd[i]=0;
			for (iz=sizez1/2;iz<sizez1;iz++)
			        for (jx=0;jx<sizex2;jx++)
				        workcd[iz+jx*n1]=cd[iz+jx*n1]; 
			wtn(workcd,ToC,wfilt,wtsizes,justDC);
			for (i=0;i<ntot+1;i++) workf[i]=workcd[i];
			for (iz=sizez1/2;iz<sizez1;iz++){
			        for (jx=0;jx<sizex2;jx++){
				        sum=0;
					for (i=0;i<leap;i++)
					        sum+=workf[iz*leap+i-n1+jx*n1];
					f[iz+jx*n1]=sum/leap;
				}
			}

			if (level==(StL1-1)) {

			  /* fprintf(stderr,"level()=%d\n",level); */

				for (i=0;i<ntot+1;i++) workcd[i]=0;
				for (iz=0;iz<sizez1/2;iz++)
			                for (jx=0;jx<sizex2;jx++) 
					        workcd[iz+jx*n1]=cd[iz+jx*n1];
				wtn(workcd,ToC,wfilt,wtsizes,justDC);
				for (i=0;i<ntot+1;i++) workf[i]=workcd[i];
				for (iz=0;iz<sizez1/2;iz++){
				        for (jx=0;jx<sizex2;jx++){
					        sum=0;
						for (i=0;i<leap;i++)
						        sum+=workf[iz*leap+i+jx*n1];
						f[iz+jx*n1]=sum/leap;
					}
				}    
			}
		}
	}

	if (StL2>StL1) {
	        for (level=StLevel;level<StL2;level++){
    
		  /* fprintf(stderr,"level=%d\n",level); */

			sizez1=n1>>StL1;
			sizex2=n2>>level;
			leap=2<<level;
			leap2=leap*leap;
              
			for (i=0;i<ntot+1;i++) workcd[i]=0;
			for (iz=0;iz<sizez1;iz++)
			        for (jx=sizex2/2;jx<sizex2;jx++)
				        workcd[iz+jx*n1]=cd[iz+jx*n1]; 
			wtn(workcd,ToC,wfilt,wtsizes,justDC);
			for (i=0;i<ntot+1;i++) workf[i]=workcd[i];
			for (iz=0;iz<sizez1;iz++){
			        for (jx=sizex2/2;jx<sizex2;jx++){
				        sum=0;
					for (i=0;i<leap;i++)
					        sum+=workf[iz+(jx*leap+i-n2)*n1];
					f[iz+jx*n1]=sum/leap;
				}
			}

			if (level==StL2-1) {

			        /* fprintf(stderr,"level()=%d\n",level); */

				for (i=0;i<ntot+1;i++) workcd[i]=0;
				for (iz=0;iz<sizez1;iz++)
				        for (jx=0;jx<sizex2/2;jx++) 
					        workcd[iz+jx*n1]=cd[iz+jx*n1];
				wtn(workcd,ToC,wfilt,wtsizes,justDC);
				for (i=0;i<ntot+1;i++) workf[i]=workcd[i];
				for (iz=0;iz<sizez1;iz++){
				        for (jx=0;jx<sizex2/2;jx++){
					        sum=0;
						for (i=0;i<leap;i++)
						        sum+=workf[iz+(jx*leap+i)*n1];
						f[iz+jx*n1]=sum/leap;
					}
				}
			}
		}
	}

	free1float(workf);
	free1float(workcd);
} 
		
void wto1d(float *cd,int npoints,enum ToCorD tocord,
            WtFilter *wfilt)
/*********************************************************************
wto1d - wavelet transform operator, 1D
*********************************************************************
Function Prototype:
void wto1d(float *cd,int npoints,enum ToCorD tocord,
            WtFilter *wfilt);
*********************************************************************
Input:
float *cd		pointer to wavelet coefficients, or f(x)
int npoints		size of the input signal
enum ToCorD tocord	=ToC inverse  or =ToD  forward transform
WtFilter *wfilt		pointer to wavelet operator (filters)

Returns:
float *cd		pointer to f(x) or to wavelet coefficients
*********************************************************************
Notes:
1D wavelet transform operator, used to apply multi-dimensional  
wavelet transform, in which data of each dimension is extracted 
and 1D wavelet transform (forward: tocord==ToD; backward: tocord 
==ToC) is applied to this 1-D data. Before using wto1d, we must 
call wto1dset to set up the wavelet filters.			   
							   
*********************************************************************
Reference:                                                    
W. Press et al., Numberical Recipes in C (second edition),     
Cambridge University Press, 1988				   
*********************************************************************
 Author:  Zhaobo Meng, 11/25/95,  Colorado School of Mines 
*********************************************************************/
{
        float ai,ai1,*temp;
	int   i,icd,j,jf,jr,k,n1,ni,nj,nhalf,nmod;

	temp=ealloc1float(npoints+1);
	nmod=wfilt->order*npoints;
	n1=npoints-1;
	nhalf=npoints>>1;
	for (j=1;j<=npoints;j++) temp[j]=0.0;
	if (tocord==ToD){
	        for (icd=1,i=1;i<=npoints;i+=2,icd++){
		        ni=i+nmod+wfilt->ioff;
			nj=i+nmod+wfilt->joff;
			for (k=1;k<=wfilt->order;k++){
			        jf=n1&(ni+k);
				jr=n1&(nj+k);
				temp[icd]+=wfilt->cc[k]*cd[jf+1];
				temp[icd+nhalf]+=wfilt->cr[k]*cd[jr+1];
			}
		}
	}
	else {
	        for (icd=1,i=1;i<=npoints;i+=2,icd++){
		        ai=cd[icd];
			ai1=cd[icd+nhalf];
			ni=i+nmod+wfilt->ioff;
			nj=i+nmod+wfilt->joff;
			for (k=1;k<=wfilt->order;k++){
		                jf=(n1&(ni+k))+1;
				jr=(n1&(nj+k))+1;
				temp[jf]+=wfilt->cc[k]*ai;
				temp[jr]+=wfilt->cr[k]*ai1;
			}
		}
	}
	for (j=1;j<=npoints;j++) cd[j]=temp[j]; 
	free1float(temp);
}

void wto1dset(WtFilter *wfilt,WtSizes *wtsizes)
/*********************************************************************
wto1dset - setup wavelet operators
*********************************************************************
Function Prototype:
void wto1dset(WtFilter *wfilt,WtSizes *wtsizes);
*********************************************************************
Input:
WtFilter *wfilt		pointer to wavelet filter
WtSizes *wtsizes	pointer to wavelet filter sizes

Return:
wfilt->order		order of Daubechies wavelet
wtsizes->sizes		sizes of the wavelet operator
*********************************************************************
Notes:
This subroutine initialize the wavelet filters for wavelet
transform operator wto1d. According to the order you choose
in your main (wtsizes->order=4 up to 20), wto1dset set up
the filter to be the Daubechies wavelet of this order/length. 
This subroutine will be need before calling wt1 or wtn. 

*********************************************************************
Reference: 
W. Press et al., Numberical Recipes in C (second edition), 
Cambridge University Press, 1988
*********************************************************************
Author:  Zhaobo Meng, 11/25/95,  Colorado School of Mines
*********************************************************************/
{
        int   k;
	int   order; /*number of coefficient=order of wavelet*/
	float sig=-1.0;
	static float c4[5]={0.0,0.4829629131445341,0.8365163037378079,
			    0.2241438680420314,-0.1294095225512604};
	static float c6[7]={0.0,0.332670552950,0.806891509311,
			    0.459877502118,-0.135011020010,-0.085441273882,
			    0.035226291882};
	static float c8[9]={0.0,0.230377813309,0.714846570553,0.630880767930,
			    -0.027983769417,-0.187034811719,0.030841381836,
			    0.032883011667,-0.010597401785};
	static float c10[11]={0.0,0.160102397974,0.603829269797,0.724308528438,
			      0.138428145901,-0.242294887066,-0.032244869585,
			      0.077571493840,-0.006241490213,-0.012580751999,
			      0.003335725285};
	static float c12[13]={0.0,0.111540743350,0.494623890398,0.751133908021,
			      0.315250351709,-0.226264693965,-0.129766867567,
			      0.097501605587,0.027522865530,-0.031582039318,
			      0.000553842201,0.004777257511,-0.001077301085};
	static float c14[15]={0.0,0.077852054085,0.396539319482,0.729132090846,
			      0.469782287405,-0.143906003929,-0.224036184994,
			      0.071309219276,0.080612609151,-0.038029936935,
			      -0.016574541631,0.012550998556,0.000429577973,
			      -0.001801640704,0.000353713800};
	static float c16[17]={0.0,0.054415842243,0.312871590914,0.675930736297,
			      0.585354683654,-0.015829105056,-0.284015542962,
			      0.000472484574,0.128747426620,-0.017369301002,
			      -0.044088253931,0.013981027917,0.008746094047,
			      -0.004870352993,-0.000391740373,0.000675449406,
			      -0.000117476784};
	static float c18[19]={0.0,0.038077947364,0.243834674613,0.604823123690,
			      0.657288078051,0.133197385825,-0.293273783279,
			      -0.096840783223,0.148540749338,0.030725681479,
			      -0.067632829061,0.000250947115,0.022361662124,
			      -0.004723204758,-0.004281503682,0.001847646883,
			      0.000230385764,-0.000251963189,0.000039347320};
	static float c20[21]={0.0,0.026670057901,0.188176800078,0.527201188932,
			      0.688459039454,0.281172343661,-0.249846424327,
			      -0.195946274377,0.127369340336,0.093057364604,
			      -0.071394147166,-0.029457536822,0.033212674059,
			      0.003606553567,-0.010733175483,0.001395351747,
			      0.001992405295,-0.000685856695,-0.000116466855,
			      0.000093588670,-0.000013264203};
	static float c4r[5],c6r[7],c8r[9],c10r[11],c12r[13],c14r[15],
	        c16r[17],c18r[19],c20r[21];

	order=wtsizes->order;
	wfilt->order=order;
	switch(order) {
	case 4:
	        wfilt->cc=c4;
		wfilt->cr=c4r;
		break;
	case 6: 
	        wfilt->cc=c6;
		wfilt->cr=c6r;
		break;
	case 8:
	        wfilt->cc=c8;
		wfilt->cr=c8r;
		break;
	case 10:
	        wfilt->cc=c10;
		wfilt->cr=c10r;
		break;
	case 12:
	        wfilt->cc=c12;
		wfilt->cr=c12r;
		break;
	case 14:
	        wfilt->cc=c14;
		wfilt->cr=c14r;
		break;
	case 16:
	        wfilt->cc=c16;
		wfilt->cr=c16r;
		break;
	case 18:
	        wfilt->cc=c18;
		wfilt->cr=c18r;
		break;
	case 20:
	        wfilt->cc=c20;
		wfilt->cr=c20r;
		break;
	default:
	        fprintf(stderr,"unimplemented order in wto1dset");
		exit(0);
	}
	for (k=1;k<=order;k++){
	        wfilt->cr[wfilt->order+1-k]=sig*wfilt->cc[k];
		sig=-sig;
	}

	wfilt->ioff=wfilt->joff=-(order>>1);
  
	wt_cascade(wfilt,wtsizes);
}

void wt1(float *cd,enum ToCorD tocord,int npoints,
     WtFilter *wfilt,WtSizes *wtsizes,int idim)
/***********************************************************************
wt1 - 1D wavelet transform
***********************************************************************
Function Prototype:
void wt1(float *cd,enum ToCorD tocord,int npoints,
     WtFilter *wfilt,WtSizes *wtsizes,int idim);
***********************************************************************
Input:
float *cd		pointer to wavelet coeff. or input f(x)
enum ToCorD tocord	=ToC (invers) =ToD (forward) wavelet transform
int npoints		size of signal f(x) or cd
WtFilter *wfilt		pointer to wavelet operator (filters)
WtSizes *wtsizes	pointer to wavelet sizes
int idim		index for dimension

Returns:
float *cd		pointer to input f(x) or to wavelet coefficients
***********************************************************************
Notes:
1D wavelet transform. cd[1..npoints] as the discrete values of a 1D
function f(x) will be replaced by its wavelet transform if tocord
==ToD, or the wavelet coefficients cd[1..npoints] will be replaced
by the inverse wavelet transform, thus to obtain the reconstructed
f. The size of f npoints MUST be an integer power of 2.

***********************************************************************
Reference:
W. Press et al., Numberical Recipes in C (second edition),
Cambridge University Press, 1988
***********************************************************************
Author:  Zhaobo Meng, 11/25/95,  Colorado School of Mines
***********************************************************************/
{
        int   nthis;    /*length of this version*/
	int   lenlastc; /*length of the last c version*/
  
	lenlastc=npoints>>wtsizes->Mraleveln[idim];
	if (lenlastc<4) {
	        fprintf(stderr,"lenlastc=%d\n",lenlastc);
		fprintf(stderr,"StLevel too large or npoints too small.\n");
		exit(0);
	}

	if (tocord==ToD)
	        for (nthis=npoints;nthis>=2*lenlastc;nthis>>=1)wto1d(cd,nthis,ToD,wfilt);
	else
	        for (nthis=2*lenlastc;nthis<=npoints;nthis<<=1)wto1d(cd,nthis,ToC,wfilt);
}

void wtn(float *cd,enum ToCorD tocord,
     WtFilter *wfilt,WtSizes *wtsizes,int dconly)
/*************************************************************************
wtn - n-D wavelet transform 
**************************************************************************
Function Prototype:
void wtn(float *cd,enum ToCorD tocord,
     WtFilter *wfilt,WtSizes *wtsizes, int dconly) 
*************************************************************************
Input:
float *cd		pointer to n-D wavelet coeff, or f(x_1,..,x_n)
enum ToCorD tocord	=ToC (inverse) =ToD (forward) wavelet transform
WtFilter *wfilt		pointer to wavelet operator (filters)
WtSizes *wtsizes	pointer to wavelet sizes 
int dconly              keep and transform back from mra dc component 
                        only

Returns:
float *cd		pointer to f(x_1,...,x_n) or n-D wavelet coeffs.
*************************************************************************
Notes:
Given a n-D discrete values cd[...] of a n-D function f(x_1,...,x_n)
n-D wavelet transform will be applied if tocord==ToD; or given
a n-D wavelet coefficients of an n-D function, inverse wavelet
transform will be applied to reconstruct the n-D function f. Must
call wto1dset before calling wtn.
*************************************************************************
Reference: 
W. Press et al., Numberical Recipes in C (second edition), 
Cambridge University Press, 1988
*************************************************************************
Author:  Zhaobo Meng, 11/25/95,  Colorado School of Mines
Modified: Carlos E. Theodoro, 06/25/97, Colorado School of Mines
          - modified to transform back only dc component, if required 
*************************************************************************/
{
        long int   i1,i2,i3;
	int   k,n,nt,ij,ji,iz,jx;
	long int   nprev=1;	/* size of the previous version */
	long int   nnew;	/* size of the current version */
	long int   ntot=1;    /* size of the signal length */
	int  lenlastcinidim;	/* length of the last c version in idim-D */
	long int   idim;      /* dimension index */
	float *temp;		/* work space */
	float *temp1;		/* work space */

	for (idim=1;idim<=wtsizes->NDim;idim++) 
	        ntot*=wtsizes->NPointsn[idim];
  
	temp=ealloc1float(ntot+1);
	temp1=ealloc1float(ntot+1);

	for (idim=1;idim<=wtsizes->NDim;idim++) {
	        lenlastcinidim=(wtsizes->NPointsn[idim])>>(wtsizes->Mraleveln[idim]);
		n=wtsizes->NPointsn[idim];
		nnew=n*nprev;
		if (n>lenlastcinidim) {
		        for (i2=0;i2<ntot;i2+=nnew){
			        for (i1=1;i1<=nprev;i1++){
				        for (i3=i1+i2,k=1;k<=n;k++,i3+=nprev) 
					        temp[k]=cd[i3];
					if (tocord==ToC){
					        for (nt=lenlastcinidim;nt<=n;nt<<=1) 
						        wto1d(temp,nt,tocord,wfilt);
					}	
					else{
					        for (nt=n;nt>=lenlastcinidim;nt>>=1) 
						        wto1d(temp,nt,tocord,wfilt);
					}
					for (i3=i1+i2,k=1;k<=n;k++,i3+=nprev) 
					        cd[i3]=temp[k];
				}
			}
		}
		nprev=nnew;
	}
	if (dconly==1 && tocord==ToD) {
	        for (ij=1;ij<=ntot;ij++) temp1[ij]=cd[ij];
		for (ji=1;ji<=ntot;ji++) cd[ji]=0.;
    
		for (iz=1;iz<=((wtsizes->NPointsn[1])>>(wtsizes->Mraleveln[1]));iz++) {
		        for (jx=0;jx<((wtsizes->NPointsn[2])>>(wtsizes->Mraleveln[2]));jx++)
			        cd[iz+(wtsizes->NPointsn[1])*jx]=temp1[iz+(wtsizes->NPointsn[1])*jx];
		}
	}

	free1float(temp);
	free1float(temp1);
}
