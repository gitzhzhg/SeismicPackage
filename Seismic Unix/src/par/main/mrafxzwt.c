/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* MRAFXZWT: $Revision: 1.5 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"
#include "wt.h"

/*********************** self documentation *****************************/
char *sdoc[] = {
" 									",
" MRAFXZWT - Multi-Resolution Analysis of a function F(X,Z) by Wavelet	",
"	 Transform. Modified to perform different levels of resolution  ",
"        analysis for each dimension and also to allow to transform     ",
"        back only the lower level of resolution.  		      	",
" 									",
"    mrafxzwt [parameters] < infile > mrafile 			 	",
"									",
" Required Parameters:							",
" n1=		size of first (fast) dimension				",
" n2=		size of second (slow) dimension 			",
"									",
" Optional Parameters:							",
" p1=		maximum integer such that 2^p1 <= n1			",
" p2=		maximum integer such that 2^p2 <= n2			",
" order=6	order of Daubechies wavelet used (even, 4<=order<=20)	",
" mralevel1=3   maximum multi-resolution analysis level in dimension 1	",
" mralevel2=3   maximum multi-resolution analysis level in dimension 2	",
" trunc=0.0	truncation level (percentage) of the reconstruction	",
" verbose=0	=1 to print some useful information			",
" reconfile=    reconstructed data file to write			",
" reconmrafile= reconstructed data file in MRA domain to write		",
" dfile=	difference between infile and reconfile to write        ",
" dmrafile=	difference between mrafile and reconmrafile to write    ",
" dconly=0      =1 keep only dc	component of MRA			",
" verbose=0     =1 to print some useful information                     ",
" if (n1 or n2 is not integer powers of 2) specify the following:	",
" 	nc1=n1/2 center of trimmed image in the 1st dimension           ",
" 	nc2=n2/2 center of trimmed image in the 2nd dimension           ",
"	trimfile= if given, output the trimmed file			",
"									",
" Notes:								",
" This program performs multi-resolution analysis of an input function	",
" f(x,z) via the wavelet transform method. Daubechies's least asymmetric",
" wavelets are used. The smallest wavelet coefficient retained is given	",
" by trunc times the absolute maximum size coefficient in the MRA.	",
"                                                                       ", 
" The input dimensions of the data must be expressed by (p1,p2) which   ",
" are the log (base 2) of the dimensions (n1,n2).                       ",NULL};

/************************************************************************
 *  Author: Zhaobo Meng, 11/25/95, Colorado School of Mines             *
 *  Modified:  Carlos E. Theodoro, 06/25/97, Colorado School of Mines   *
 *	Included options for:                           	        *
 *	- different level of resolutionf or each dimension;   	        *
 *	- transform back the lower level of resolution, only.		*
 *									*
 * Reference:								*
 * Daubechies, I., 1988, Orthonormal Bases of Compactly Supported	* 
 * Wavelets, Communications on Pure and Applied Mathematics, Vol. XLI,  *
 * 909-996.				 				* 
 ************************************************************************/
/*********************** end self doc ***********************************/

int 
main (int argc, char **argv)
{
        long int lengthphi; /* length of the scaling function */
	long int ntot;      /* signal length in 1-D */
	int p1;             /* power length of the fastest dimension */
	int p2;             /* power length of the slowest dimension */
	int n1;             /* length of the fastest dimension */
	int n2;             /* length of the slowest dimension */
	int mralevel1;      /* number of levels of MRA in the fastest D */
	int mralevel2;      /* number of levels of MRA in the slowest D */
	float trunc;        /* the percentage trunction level */
	float fmax;         /* maximum value of the signal */
	float ratio;
	float *f;           /* the 2-D signal to be processed */
	float *cd;	    /* the 2-D signal in wavelet domain */
	float *dif;         /* difference between signal and reconstruncted */
	float *difmra;      /* difference between them in wavelet domain */
	float *trunccd;     /* wavelet coefficients after truncation */
	int   i,j;          /* working varibles */
	int   nofzeros;     /* number of zeros in the wavelet coefficients */
	int   verbose;      /* print some useful information if nonzero */
	int   dconly;       /* save the DC only, all other information muted */
	char  *reconfile;   /* signal after reconstruction to output */
	char  *reconmrafile;/* reconstructed signal after mra to output */
	char  *dmrafile;    /* difference in signals after mra to output */
	char  *dfile;       /* difference in signals to output */
	char  *trimfile;    /* output truncated file if trim=1 */
 
	WtFilter wfilt;     /* the wavelet filter */
	WtSizes wtsizes;    /* all the size information of the wavelet */
	FILE *fp;           /* file handle */

	long int ntot2p;
	int nt1,nt2,n1st,n2st,n1nd,n2nd;
	float *f0;
	int nc1,nc2,trim=1;
  
	/* hook up getpar to handle the parameters */
	initargs(argc, argv);
	requestdoc(1);

	/* get required parameters */
	if (!getparint("verbose",&verbose)) verbose=0;
	if (!getparint("n1",&n1)) err("must specify n1\n");
	if (!getparint("n2",&n2)) err("must specify n2\n");
	/* get the largest p1 such that 2^p1<=n1 */
	if (!getparint("p1",&p1)) { 
	        /* raising 2 to the power p1 by bit shifting */
	        for (p1=1;(1<<p1)<=n1;p1++) ;
		p1--;
	}	

	/*get the largest p2 such that 2^p2<=n2*/	
	if (!getparint("p2",&p2)) { 
	        /* raising 2 to the power p2 by bit shifting */
	        for (p2=2;(1<<p2)<=n2;p2++) ;
		p2--;
	}

	/* raising 2 to the power p1,p2 by bit shifting */
	nt1=1<<p1; 
	nt2=1<<p2;

	if (nt1==n1 && nt2==n2) trim=0;
    
	if (verbose==1) 
	        warn("n1=%d\nn2=%d\np1=%d\np2=%d\nnt1=%d\nnt2=%d\ntrim=%d",
		        n1,n2,p1,p2,nt1,nt2,trim);

	/* get optional parameters */
	if (!getparint("order",&wtsizes.order)) wtsizes.order = 6;
	if (!getparint("mralevel1",&mralevel1)) mralevel1=3;
	if (!getparint("mralevel2",&mralevel2)) mralevel2=3;
	if (!getparfloat("trunc",&trunc)) trunc=0;
	if (!getparint("dconly",&dconly)) dconly=0;

	if (!getparint("verbose",&verbose)) verbose=0;

	cd=ealloc1float(nt1*nt2+1);	/*the trimmed image*/

	if (trim==1){

	        fprintf(stderr,"trim=1\n");


		/**********************************************
		  Get optional parameters for trimming: 
		  nc1: the center of image in n1 to trim;
		  nc2: the center of image in n2 to trim;
		  **********************************************/
		if (!getparint("nc1",&nc1)) nc1=n1/2;
		if (!getparint("nc2",&nc2)) nc2=n2/2;
	
		/*********************************************
		  The trimmed image region:
		  n1 direction: from n1st to n1nd;
		  n2 direction: from n2st to n2nd.
		  *********************************************/
		n1st=nc1-nt1/2; if (n1st<0) err("use larger nc1 or default");
		n1nd=n1st+nt1;  if (n1nd>n1) err("use smaller nc1 or default");
		n2st=nc2-nt2/2; if (n2st<0) err("use larger nc2 or default");
		n2nd=n2st+nt2;  if (n2nd>n2) err("use smaller nc2 or default");

		ntot=n1*n2;	/* number of samples before trimming*/
		ntot2p=nt1*nt2; /* number of samples after trimming*/ 

		if (verbose) {
		        warn("nt1=%d,nt2=%d\nnc1=%d,nc2=%d",nt1,nt2,nc1,nc2);
			warn("n1st=%d,n1nd=%d\nn2st=%d,n2nd=%d",n1st,n1nd,n2st,n2nd);
		}

		f0=ealloc1float(ntot);	/*the original image*/

		/************************************************
		  Read in the image, then trimmed it, and store it
		  in array cd
		  ************************************************/
		if (fread(f0,sizeof(float),ntot,stdin)!=ntot)
		        err("cannot read %ld f(x,z) ",ntot);

		for (j=n2st;j<n2nd;j++)
		        for (i=n1st;i<n1nd;i++)
			        cd[i-n1st+(j-n2st)*nt1]=f0[i+j*n1];

		free1float(f0);  
		
		/* This is the trimmed image, no WT is done yet */
		if (getparstring("trimfile",&trimfile)) 
		        if (fwrite(cd,sizeof(float),ntot2p,fopen(trimfile,"w"))!=ntot2p)
			        err("cannot write %ld f(x,z) to tem2p.dat",ntot2p);
	}	

	/*****************************************************
	  From now on, the size of image is pow of 2, after the
	  trimming. Now n1 and n2 are the sizes.
	  *****************************************************/
	n1=nt1;
	n2=nt2;
	ntot=n1*n2;

	/****************************************************
	  If the data need not to be trimmed, then read in the
	  data set.
	  ******************************************************/
	if (trim==0) {

	        fprintf(stderr,"trim=0\n");


		if (fread(cd,sizeof(float),ntot,stdin)!=ntot)
		        err("cannot read %ld f(x,z) ",ntot);
	}    

	/***************************************************
	  The order of the wavelet chosen must be even, and 
	  only from the 4th to 20th Daubechies wavelets are
	  defined in this code.
	  ***************************************************/
	wtsizes.order=(wtsizes.order/2)*2;        
	if (wtsizes.order<4 || wtsizes.order>20) {
	        wtsizes.order=6;
		warn("The 3rd. Daubechies is used instead.");
	}

	/**************************************************
	  Set up the wavelet parameters: MaxLevel is the power 
	  of 2 in the Mother wavelet and Scaling functions. As
	  suggested by Daubechies, 7 or 8 is good enough.
	  Next: NDim is the number of dimensions, here only 2D
	  transform is used; Next: NPointsn[1] and NPointsn[2]
	  stores the size in the first and second dimension.
	  Notice that the value of NPointsn[0] does not matter.
	  Next: Mraleveln[1] and Mraleveln[2] are the levels
	  that you want to apply wavelet transforms up to in 
	  the first and second dimensions. Similarly Mraleveln[0]
	  is not used.
	  *****************************************************/
	wtsizes.MaxLevel=7;
	wtsizes.NDim=2;
	wtsizes.NPointsn=ealloc1int(wtsizes.NDim+1);
	wtsizes.Mraleveln=ealloc1int(wtsizes.NDim+1);
	wtsizes.NPointsn[0]=0;
	wtsizes.NPointsn[1]=n1;
	wtsizes.NPointsn[2]=n2;
	wtsizes.Mraleveln[0]=0;
	wtsizes.Mraleveln[1]=mralevel1;
	wtsizes.Mraleveln[2]=mralevel2;

	if (verbose)
	        warn("n1=%d\nn2=%d\nmralevel1=%d\nmralevel2=%d",
		        n1,n2,mralevel1,mralevel2);

	/**************************************************
	  This is the size of scaling function psi 
	  and the mother wavelet psi
	  **************************************************/
	lengthphi=wtsizes.order<<wtsizes.MaxLevel;

	/***************************************************
	  dif:     the difference (or residual) of the original
	           image with the compressed image; 
	  f:       the original image, an extra copy;
	  trunccd: the truncated version of cd (WT image of f);
	  difmra:  the difference in MRA display;
	  ***************************************************/
	dif=ealloc1float(ntot+1);
	f=ealloc1float(ntot+1);
	trunccd=ealloc1float(ntot+1);
	difmra=ealloc1float(ntot+1);
	
	wto1dset(&wfilt,&wtsizes); /*initialize the wavelet filters*/

	if (verbose){
	        if (fwrite(wfilt.phi,sizeof(float),
			lengthphi,fopen("phi.dat","w"))!=lengthphi)
		        err("cannot write %d f(x,z) to file %s",lengthphi,"phi.dat");
		if (fwrite(wfilt.psi,sizeof(float),   
			lengthphi,fopen("psi.dat","w"))!=lengthphi)
		        err("cannot write %d f(x,z) to file %s",lengthphi,"psi.dat");
	}

	/* an extra copy of the original image */
	for (i=0;i<ntot;i++) dif[i]=cd[i];

	/* 2-D forward wavelet transform */
	wtn(cd-1,ToD,&wfilt,&wtsizes,dconly);

	/**************************************************
	  Put the different level at different region to 
	  form the MRA display of the image.
	  **************************************************/
	fhierfromcd(f,cd,&wfilt,&wtsizes,dconly);

	/* an extra copy of the MRA display of the image */
	for (i=0;i<ntot;i++) difmra[i]=f[i];

	/* write to stdout the MRA display */
	if (fwrite(f,sizeof(float),ntot,stdout)!=ntot) 
	        err("cannot write mrafile %d f(x,z) to stdout",ntot);
  
	if (trunc<0 || trunc>1.0) {
	        warn("unsuitable trunc, set to 0");
		trunc=0;
	}

	/********************************************************
	  Truncation level is defined by the maximum
	  value * the percentage
	  ********************************************************/
	fmax=fabs(cd[0]);
	for (i=0;i<ntot;i++) fmax=MAX(fmax,fabs(cd[i]));
	trunc*=fmax;

	/*******************************************************
	  Count number of zeros in the wavelet domain. If the 
	  wavelet coefficient is less than the threshhold, it will
	  be muted to zero.
	  *******************************************************/
	nofzeros=0;
	for (i=0;i<ntot;i++) {
	        if (fabs(cd[i])<trunc) {
		        cd[i]=0;
			nofzeros++;
		}
	}

	/******************************************************
	  This is the compression ratio. Not very precise. Because
	  we might do quantization and coding for further 
	  compression. 
	  ******************************************************/
	ratio=(float)nofzeros/(float)(ntot-nofzeros);
	warn("The Compression Ratio is %f to 1",ratio);

	for (i=0;i<ntot;i++) trunccd[i]=cd[i];

	/******************************************************
	  If reconstructed image is required, then apply the
	  inverse wavelet transform to the compressed wavelet
	  coefficients in trunccd. Then write it to reconfile.
	  dfile is the difference between the original image
	  and the compressed image.
	  ******************************************************/
	if (getparstring("reconfile",&reconfile)){
	        wtn(cd-1,ToC,&wfilt,&wtsizes,dconly);
		for (i=0;i<ntot;i++) f[i]=cd[i];
		for (i=0;i<ntot;i++) dif[i]-=f[i];
		if ((fp=fopen(reconfile,"w"))==NULL) 
		        err("Can not open reconfile");
		if (fwrite(f,sizeof(float),ntot,fp)!=ntot)
		        err("cannot write %d f(x,z) to file %s",ntot,reconfile);
		fclose(fp);
    
		if (getparstring("dfile",&dfile)) {
		        if ((fp=fopen(dfile,"w"))==NULL)
			        err("Can not open dfile");
			if (fwrite(dif,sizeof(float),ntot,fp)!=ntot)
			        err("cannot write %d f(x,z) to file %s",ntot,dfile);
			fclose(fp);
		}	
	}
      
	/******************************************************
	  For the compressed image, do the MRA display, and write
	  it to file reconmrafile as suggested
	  ******************************************************/	
	fhierfromcd(f,trunccd,&wfilt,&wtsizes,dconly);
	if (getparstring("reconmrafile",&reconmrafile)) {
	        if ((fp=fopen(reconmrafile,"w"))==NULL)
		        err("Can not open reconmrafile");
		if (fwrite(f,sizeof(float),ntot,fp)!=ntot)
		        err("cannot write %d f(x,z) to file %s",ntot,reconmrafile);
		fclose(fp);
	}

	/***************************************************
	  The difference between the true MRA display and the
	  compressed MRA display.
	  ***************************************************/
	for (i=0;i<ntot;i++) difmra[i]-=f[i];
	if (getparstring("dmrafile",&dmrafile)){
	        if ((fp=fopen(dmrafile,"w"))==NULL) err("Can not open dmrafile");  
		if (fwrite(difmra,sizeof(float),ntot,fp)!=ntot)
		        err("cannot write %d f(x,z) to file %s",ntot,dmrafile);
		fclose(fp);
	}

        checkpars();

	free1float(f);
	free1float(cd);

	return(CWP_Exit());
}
