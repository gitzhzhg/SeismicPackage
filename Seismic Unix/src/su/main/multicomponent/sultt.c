/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SULTT: $Revision: 1.4 $ ; $Date: 2015/08/07 22:21:18 $	*/


#include "su.h"
#include "segy.h" 
#include "header.h"
/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" SULTT - trace by trace, sample by sample, rotation of shear wave data ",
"	  volumes using the Linear Transform Technique of Li & Crampin  ",
"	  (1993)							",
" 									",
" sultt inS11=file1 inS22=file2 inS12=file3 inS21=file4 [optional       ",
" parameters]                                                           ",
" 									",
" optional parameters:							",
" 									",
" mode		determines what the linear transform will compute 	",
"			mode=1, computes asymmetry indexes		",
" 			mode=2, computes Polarization and main       	",
"				reflectivity series.			",
"			mode=3, same as above, but using eigenvalues    ",
" 									",
" 		mode=3 is more robust estimation for Polarization angle ",
"		than mode=2. In both cases the reflectivity series is   ",
"		computed in the same way. mode=1 outputs two other data ",
"		volumes, each containing the asymmetry parameters theta,",
"		and gamma. The other two modes only output an extra da- ",
"		ta volume, the instant polarization alpha		",
"									",
" outSij	defines the names of the output seismic data files,     ",
"		i and j equal either 1 or 2				",
"									",
" alpha, gamma	name for optional output volumes: instanteneous polarity",
" theta		, alpha; theta, for angle misalignment between source   ",
"		and receiver; gamma, the medium asymmetric response     ",
"		coefficient						",
" 									",
" wl		running window acting on traces (in samples)		",
" 									",
" ntraces	number of traces to be average for each location	",
" 									",
" CAVEAT								",
" 									",
" Naming convention for off-diagonal volumes:				",
" S12 - Inline source, Xline receiver					",
" S21 - Xline source, Inline receiver					",
" 									",
" the running will always have an odd number of samples, despite the    ",
" input length.								",
" 									",
NULL};

/* Credits:
 *	CWP/RCP: Rodrigo Felicio Fuck
 *      Code based on algorithms presented in Li & Crampin (1993) and 
 *	Li & MacBeth (1997)
 *
 *
 *	Li, X.Y., and Crampin, S., 1993, Linear-transform techniques for 
 *		processing shear-wave anisotropy in four-component
 *		seismic data, Geophysics, 58, 240-256.
 *	Li, X.Y., and MacBeth, C., 1997, Data-Matrix asymmetry and polar-
 *		ization changes from multicomponent surface seismics 
 */
/**************** end self doc *******************************************/



/* Global variables */
segy in_S11_tr, out_S11_tr;
segy in_S12_tr, out_S12_tr;
segy in_S21_tr, out_S21_tr;
segy in_S22_tr, out_S22_tr;
segy alpha_tr, theta_tr;
segy gamma_tr;

/* function prototypes */

int FindPeak(float *y, int nt, float *val);

float Find_Angle(float *_S11, float *_S12, float *_S21, float *_S22, 
		 float _ang_inc, int _ns, float _dt, int nt, int imin,
		 int imax, float *quality, float *inputquality, float *lag,
		 float *xcorr, int taper);

float QuadFit (float *y, float *max);

float Quality_Factor(float *_S11, float *_S12, float *_S21, float *_S22,
		     int nt, int mode);

float RMS_amp(float *_tr, int nt);

float Total_Quality(float *S11, float *S22, float qmin, float qmax,
		    int nt);

void Az_swap(float *_S1,float *_D1,float *_D2,float *_S2, int _ns);

void TypeConv(int tempval, cwp_String type, Value *val);

void Final_Rot(float *S1, float *S12, float *S21, float *S22,
	       float theta, int ns, int itmin, int taper);

void nxcor(int nx, int x1, float *x, int ny, int y1, float *y,
	   int nz, int z1, float *z);

void ROT4TR(float *_S11, float *_S12, float *_S21, float *_S22, 
	    float *_S1, float *_D1, float *_D2, float *_S2, 
	    float _theta, int itmin, int itmax, int taper);


void interp(float *S1, float *S2, int nt_in, int dt_in,
	    float *S1_res, float *S2_res, int nt, float dt);

FILE *open_vol(cwp_String vol_name, int *ns, float *dt, segy *tr);
	    
FILE *check_output(cwp_String filename);


/* function prototypes */


void LTT(float *S11, float *S12,float *S21, float *S22, int ns, 
	 float dt, float *Alpha, float *Theta, float *Gamma, 
	 int wls, int flag);

float AngleAv(float angle1, float angle2);

float TimeSeries(float x,float y);

void eig_2x2(float a11, float a12, float a22, float *phi);

void sliding_window(int ns, float *vector, int nsfilter, float *filter);

void Asym_analysis(float a11, float a12, float a22, float* phi, 
		   float *ratio);

void InstQuality(float *S11, float *S12,float *S21, float *S22, int ns, 
	 float dt, float *Quality, int wls);



int main(int argc, char **argv)
{
	/*********************************************************
	 	BEGIN variables section
	*********************************************************/

	/* matrices and vectors to hold data samples */
	/* (as pointers)			     */
		
		/* seismic */
		float **S11=NULL,**S12=NULL;   
		float **S21=NULL,**S22=NULL;
		
		/* trace mix */
		float *mixS11=NULL, *mixS22=NULL;
		float *mixS12=NULL, *mixS21=NULL; 
		
		/* output attribute traces */
		float *Alpha=NULL,  *Gamma=NULL;
		float *Theta=NULL;

	/* Volume names */
		/* input */
		cwp_String inS11="";
		cwp_String inS12="";
		cwp_String inS21="";
		cwp_String inS22="";
		
		/*output */
		cwp_String outS11="";
		cwp_String outS12="";
		cwp_String outS21="";
		cwp_String outS22="";
		cwp_String alpha ="";
		cwp_String theta ="";
		cwp_String gamma ="";
		
		
	
	/* File pointers - seismic data */
	FILE *iS11fp;		/* input S11  file pointer 		*/
	FILE *iS12;		/* input S12  file pointer 		*/
	FILE *iS21;		/* input S21 file pointer 		*/
	FILE *iS22;		/* input S22 file pointer 		*/

	
	/* attribute volumes  File pointers */

	FILE *oalpha=NULL;	/* output alpha angle file pointer	*/
	FILE *otheta=NULL;	/* output theta angle file pointer	*/
	FILE *ogamma=NULL;	/* output gamma (asymetry index)	*/
	
	/* Trace header variables for input seismic volumes */

	float dtS11, dtS12, dtS21, dtS22; /* sampling on input volumes */
	int nS11, nS22, nS12, nS21;  	  /* samples per trace	    */
	

	
	/* Input optional parameters variables */

	int   Total;            /* total number of traces to be processed */
	int   ntraces;		/* number of traces to enter in the 
				   computation of the angle of rotation	*/
	int   wl;		/* running window length in samples */
	int mode;		/* modes of operation of the program */


	/* miscellaneous variables */
	
	size_t tracebytes, databytes;
	float f_counter;
	float *mix;		/* array of mix values 			*/
	int i, j;
	int counter = 0;
	
	/****************************************************************
		END of Variable section
	****************************************************************/		
	/* Initialize */

	initargs(argc, argv);
	requestdoc(1);

	/****************************************************************/
	/*		Checking and Reading the data   		*/
	/****************************************************************/

	/*get optional parameter specifying how many traces to 
	 	   be processed at a time. Default is five      	*/

	if(!getparint("ntraces", &ntraces)) ntraces=5;


	/* getting the names of the input and output files */
	/* input files */
	if (!getparstring("inS11",&inS11))
		err("no file specified for S11 volume. Aborting...");	
	if (!getparstring("inS12",&inS12))
		err("no file specified for S12 volume. Aborting...");
	if (!getparstring("inS21",&inS21))
		err("no file specified for S21 volume. Aborting...");
	if(!getparstring("inS22",&inS22))
		err("no file specified for S22 volume. Aborting...");
	
	/* output files */
	if(!getparstring("outS11",&outS11))
		outS11="S11_out";	
	if(!getparstring("outS12",&outS12))
		outS12="S12_out";	
	if (!getparstring("outS21",&outS21))
		outS21="S21_out";
	if (!getparstring("outS22",&outS22))
		outS22="S22_out";
	if (!getparstring("alpha", &alpha))
		alpha="alpha";
	if (!getparstring("theta", &theta))
		theta="theta";
	if (!getparstring("gamma", &gamma))
		gamma="gamma";
	
	/*   open the volumes of input data 	*/
	
	iS11fp = open_vol(inS11, &nS11, &dtS11, &in_S11_tr);
	iS12   = open_vol(inS12, &nS12, &dtS12, &in_S12_tr);
	iS21   = open_vol(inS21, &nS21, &dtS21, &in_S21_tr);
	iS22   = open_vol(inS22, &nS22, &dtS22, &in_S22_tr);
		
	/* verifying whether all the volumes have same number of samples */

	if(nS11 != nS22 || nS11 != nS12 || nS11 != nS21){
	  err("trace length for S11 is not equal to other traces \n");
	}
	if(nS22 != nS12 || nS22 != nS21){
	    err("trace length for S22 is not equal to other traces\n");
	}
	if(nS12 != nS21){
	  err("trace length for off diagonals is not the same\n");
	}

	/* compute databytes per trace and bytes in the mixing panel */
	tracebytes = FSIZE*nS11;
	databytes  = tracebytes * ntraces;
	
	/* Allocating memory and initializing the data */
	    S11 = ealloc2float(nS11,ntraces);
	    S12 = ealloc2float(nS12,ntraces);
	    S21 = ealloc2float(nS21,ntraces);
	    S22 = ealloc2float(nS22,ntraces);

	    memset((void *) S11[0], (int) '\0', databytes);
	    memset((void *) S12[0], (int) '\0', databytes);
	    memset((void *) S21[0], (int) '\0', databytes);
	    memset((void *) S22[0], (int) '\0', databytes);
	
	/* copying first trace to S11[][] and so on */
	
	    memcpy((void *) S11[0], (const void *) in_S11_tr.data, tracebytes);
	    memcpy((void *) S12[0], (const void *) in_S12_tr.data, tracebytes);
	    memcpy((void *) S21[0], (const void *) in_S21_tr.data, tracebytes);
	    memcpy((void *) S22[0], (const void *) in_S22_tr.data, tracebytes);

			   
	
	/****************************************************************/
	/*              Reading optional parameters			*/
	/****************************************************************/


	/* get optional parameter for total number of traces to be processed */

	if(!getparint("Total", &Total)) Total = 0;

	/* get optional parameter for maximum lag between the 
	   fast and slow shear waves (in ms) */
	

	/* optional parameter for runing window length (in ms) to be used
	   to compute the "instantaneous" angle alpha		*/
	if(!getparint("wl", &wl)) wl=3;
	if(!getparint("mode", &mode)) mode=2;
        checkpars();
	
	
	/********************************************************************	
				 Checking output volumes
	********************************************************************/
   

		/*oS11fp=check_output(outS11);
		oS12  =check_output(outS12);
		oS21  =check_output(outS21);
		oS22  =check_output(outS22);*/
	if (mode == 1){
		otheta = check_output(theta);
		ogamma = check_output(gamma);
	}
	else{
		oalpha=check_output(alpha);
	}


	/* Initializing the array mix that will be used to produce a 
	   running average mix of the input traces to the Alford 
	   rotation 							*/
	
	mix=ealloc1float(ntraces);
	for (i=0; i < ntraces; i++){
		mix[i] = 1/((float) ntraces);
	}		


	/* Allocate temporary space	*/


	mixS11 = ealloc1float(nS11);
	mixS12 = ealloc1float(nS12);
	mixS21 = ealloc1float(nS21);
	mixS22 = ealloc1float(nS22);

	/* Allocate space for attributes */
	Alpha = ealloc1float(nS11);
	Theta = ealloc1float(nS11);
	Gamma = ealloc1float(nS11);

	
	/*******************************************************************
         * looping through traces                                             *
	 *********************************************************************/
	
	do{
	    	if (counter !=0)
	     	{/* reads the next trace in S22 input volume */
	      		 memcpy((void *) S22[0], (const void *) in_S22_tr.data,
				 nS22*FSIZE);
	     	}
		
		/* initialize attribute files */
		
		memset((void *) Alpha, (int) '\0', nS11*FSIZE);
		memset((void *) Theta, (int) '\0', nS11*FSIZE);
		memset((void *) Gamma, (int) '\0', nS11*FSIZE);
	
		/* initialize mix files */

		memset((void *) mixS11, (int) '\0', nS11*FSIZE);
		memset((void *) mixS12, (int) '\0', nS12*FSIZE);
		memset((void *) mixS21, (int) '\0', nS21*FSIZE);
		memset((void *) mixS22, (int) '\0', nS22*FSIZE);
		
		/* form mix files */

	  		/* loop over time samples */
			for (i=0; i < nS11; ++i){
				/* loop over the input traces   */
				for (j=0; j < ntraces; ++j){
					mixS11[i]+=S11[j][i]*mix[j];
					mixS12[i]+=S12[j][i]*mix[j];
					mixS21[i]+=S21[j][i]*mix[j];
					mixS22[i]+=S22[j][i]*mix[j];
				}
			}
		/* Now that the input data was prepared find the 
		   desired attributes as chosen by mode of operation */
	

			LTT(mixS11, mixS12,mixS21, mixS22, nS11, dtS11,
			     Alpha, Theta, Gamma, wl, mode);



	/*****************************************************************
		Outputting the results 
	*******************************************************************/
	
	  /* copying the modified traces to output traces */
	
	 /* memcpy((void *)out_S11_tr.data, (const void *)mixS11, nS11*FSIZE);
	  memcpy((void *)out_S12_tr.data, (const void *)mixS12, nS12*FSIZE);
	  memcpy((void *)out_S21_tr.data, (const void *)mixS21, nS21*FSIZE);
	  memcpy((void *)out_S22_tr.data, (const void *)mixS22, nS22*FSIZE);
	*/
	  if(mode == 1){
	    memcpy((void *)  gamma_tr.data, (const void *)Gamma,  nS11*FSIZE);
	    memcpy((void *)  theta_tr.data, (const void *)Theta,  nS11*FSIZE);
	  
	   }
	   else {
	    memcpy((void *)  alpha_tr.data, (const void *)Alpha,  nS11*FSIZE);
	   }

	  /* copying headers */
	  /*memcpy( (void *) &out_S11_tr, (const void *) &in_S11_tr, HDRBYTES);
	  memcpy( (void *) &out_S12_tr, (const void *) &in_S12_tr, HDRBYTES);
	  memcpy( (void *) &out_S21_tr, (const void *) &in_S21_tr, HDRBYTES);
	  memcpy( (void *) &out_S22_tr, (const void *) &in_S22_tr, HDRBYTES);*/

	  if (mode == 1){
	    memcpy( (void *) &gamma_tr  , (const void *) &in_S11_tr, HDRBYTES);
	    memcpy( (void *) &theta_tr  , (const void *) &in_S11_tr, HDRBYTES);
	  }
	   else{
	    memcpy( (void *) &alpha_tr  , (const void *) &in_S11_tr, HDRBYTES);
	  }
	  /* writing out the traces to the output files*/
	  /*  fputtr(oS11fp, &out_S11_tr);
	    fputtr(oS12, &out_S12_tr);
	    fputtr(oS21, &out_S21_tr);
	    fputtr(oS22, &out_S22_tr);*/
	   
		if(mode == 1){
	    		fputtr(otheta, &theta_tr);
	    		fputtr(ogamma, &gamma_tr);
	  	}
		else{
			fputtr(oalpha, &alpha_tr);
		}	
	/* Bump columns of the data matrices to make space for 
	   data from next trace (cf. sumix.c)			*/
		for (i=ntraces-1; 0<i; --i){
			for(j=0; j<nS11; ++j){
				S11[i][j]=S11[i-1][j];
				S12[i][j]=S12[i-1][j];
				S21[i][j]=S21[i-1][j];
				S22[i][j]=S22[i-1][j];
			}
	    	}	

	    /* to prevent error after the last sample in volume S22 
		has been read on the previous pass	     */
	    if(!fgettr(iS11fp, &in_S11_tr)) break;
	    if(!fgettr(iS12, &in_S12_tr)) break;
	    if(!fgettr(iS21, &in_S21_tr)) break;
	  
	    /* reading the next batch of data */
			
	    memcpy((void *) S11[0], (const void *) in_S11_tr.data, nS11*FSIZE);
	    memcpy((void *) S12[0], (const void *) in_S12_tr.data, nS12*FSIZE);
	    memcpy((void *) S21[0], (const void *) in_S21_tr.data, nS21*FSIZE);
	    counter++;
	    if(Total!=0)
	      {
		f_counter=(float)counter;
		printf("%2.f %% done\xD",100*(f_counter/Total));
	      }
	  
	}while(fgettr(iS22, &in_S22_tr));


	return(CWP_Exit());
}


void LTT(float *S11, float *S12,float *S21, float *S22, int ns, 
	 float dt, float *Alpha, float *Theta, float *Gamma, 
	 int wls, int flag)
/***********************************************************************
 LTT - computes the linear transform and then outputs the associated
	attributes Alpha (polarization angle), Delay (time delay),
	Gamma (asymmetry index) and Theta (angle measuring misalign-
	ment of sources and receivers
**********************************************************************

input:
	Sij 	     - the traces for the seismic volumes 
			(source i, receiver j)
	ns  	     - number of samples in the Sij traces
	dt           - sampling rate on tracs
	Alpha        - output trace for polarization angle
	Theta        - misalignment angle
	wls          - number of samples in the sliding window
	flag	     - a flag determining which way to compute instan-	
			tenous Polarization (alpha) angle
				=1 - Computes asymetry indexes
				=2 - computes polarization
				=3  - computes polarization 
					more robustly (Eigenvalues)
output:
	none

modified input parameters:
	Sij, Alpha, Theta, Delay, quality, inputquality
*************************************************************************
NOTES

This function is based on the works of Li & Crampin (1993) and 
Li and MacBeth (1997)

************************************************************************/
{
	
	/* variables */
	float *ZETA=NULL, *CHI=NULL;
	float *XI=NULL, *ETA=NULL;
	float *XI2=NULL, *ETA2=NULL;
	float *ZETA2=NULL, *CHI2=NULL;
	float *ZETACHI=NULL, *XIETA=NULL;
	
 	float *w;
	float sumalphas=0.0, diffalphas=0.0;   /* (in degrees)*/
	int i;

	/* define the weight-vector */
	
	w=ealloc1float(wls);
	for (i=0; i < wls; ++i){
		w[i]= 1; /* just accumulate sum */
	/*	w[i]= 1/((float) wls);		*/
	}

	/* form matrices ETA and ZETA */

	if (flag == 1){
		ZETA = ealloc1float(ns);
		CHI  = ealloc1float(ns);
		ZETA2 = ealloc1float(ns);
		CHI2  = ealloc1float(ns);
		ZETACHI = ealloc1float(ns);
	}
	else if (flag >1 ){
		XI   = ealloc1float(ns);
		ETA  = ealloc1float(ns);
		/*V1   = ealloc1float(ns);
		V2   = ealloc1float(ns);*/
	}

	if (flag == 3){	
		XI2   = ealloc1float(ns);
		ETA2  = ealloc1float(ns);
		XIETA   = ealloc1float(ns);
	}


	/* loop over input samples to form necessary vectors   */
	for (i=0; i < ns; ++i){
		if (flag == 1){
			ZETA[i] = S11[i] + S22[i];
			CHI[i]  = S21[i] - S12[i]; 
			/* chi here is the same as in Li & Crampin(93)
			but since my convention for Sij is the opposi				   te, then CHI is S21-S12 and not S12-S21,
		  as in the referred work */
			ZETA2[i] = ZETA[i]*ZETA[i];
			CHI2[i]  = CHI[i]*CHI[i];
			ZETACHI[i] = ZETA[i]*CHI[i];
		}
		if (flag > 1){
			XI[i]  = S11[i] - S22[i];
			ETA[i] = S12[i] + S21[i];
		}
		if (flag == 3){
			XI2[i] 	 = XI[i]*XI[i];
			ETA2[i]  = ETA[i]*ETA[i];
			XIETA[i] = XI[i]*ETA[i];
		}
	}
	/*warn("done computations of vectors");*/
	
	/* use sliding_window to compute moving averages of 
	   necessary quantities to enter computations (whole trace)*/
	if (flag == 1){	
		sliding_window(ns,ZETA,wls,w);
		sliding_window(ns,CHI,wls,w);
		sliding_window(ns,ZETA2,wls,w);
		sliding_window(ns,CHI2,wls,w);
		sliding_window(ns,ZETACHI,wls,w);
	}
	else if(flag > 1){
		sliding_window(ns,XI,wls,w);
		sliding_window(ns,ETA,wls,w);
	}

	
	if(flag == 3){
		sliding_window(ns,XI2,wls,w);
		sliding_window(ns,ETA2,wls,w);
		sliding_window(ns,XIETA,wls,w);
	 }
	/*warn("done convolutions");*/
		
	/* sample by sample computation of attributes 
	   taken over the averaged results 		*/
	for (i=0; i< ns; i++){
		if (flag == 1){
			/* computing the asymmetry attributes */
			Asym_analysis(ZETA2[i],ZETACHI[i],CHI2[i],
					&diffalphas, &Gamma[i]);
			Theta[i] = diffalphas;
		}
		else if (flag == 2){
			/* compute angle alpha simply as arctan */
			sumalphas  = atan(ETA[i]/XI[i])*180/PI;
			/*diffalphas = atan(CHI[i]/ZETA[i])*180/PI;*/
		}
		else  if (flag == 3){
			/* computes angle Alpha from Eigenvectors */
			eig_2x2(XI2[i],XIETA[i],ETA2[i],&sumalphas);
			/*eig_2x2(ZETA2[i],ZETACHI[i],CHI2[i],&diffalphas);*/
		}
		/* return Alpha[i] as the average of angles found
		   in the previous computations */	
		if (flag >1){
			Alpha[i]=0.5*sumalphas;
			/*alpha = (sumalphas + diffalphas)/2;
			beta  = (sumalphas-diffalphas)/2;
			Alpha[i] = AngleAv(alpha,beta);*/
		}	
			
		/* reusing some variables */
	/*	alpha = Alpha[i]*PI/180;
		beta = Alpha[i]*PI/90; 
		diffalphas=0.5*(S22[i]-S11[i])*sin(beta);*/
		/* the new off-diagonals *
		D1 = diffalphas + S12[i]*cos(alpha)*cos(alpha) - 
			S21[i]*sin(alpha)*sin(alpha);
		D2 = diffalphas + S21[i]*cos(alpha)*cos(alpha) -
			S12[i]*sin(alpha)*sin(alpha);
		
		S12[i] = D1;
		S21[i] = D2;*/

		/* The principal reflection series *
		V2[i] = TimeSeries(XI[i],ETA[i]);
		V1[i] = TimeSeries(ZETA[i],CHI[i]);
		
		S11[i] = (V1[i] + V2[i])/2;
		S22[i] = (V1[i] - V2[i])/2;*/

	}
	/* freeing whatever memory was used */
 	if (flag ==1 ){
		free1float(ZETA);
		free1float(CHI); 
		free1float(ZETACHI);
		free1float(ZETA2);
		free1float(CHI2);
	}
	else if (flag >1){
		free1float(XI);
		free1float(ETA);
		/*free1float(V1);
		free1float(V2);
		*/
	}
	if (flag == 3){
		free1float(ETA2);
		free1float(XIETA);
		free1float(XI2);
	}
}

float AngleAv(float angle1, float angle2)
/*********************************************************************
	AngleAv is a arithmetic average of angles, but considering
	the angles to azimuths, which should be restricted only to
	0 to pi interval. Hence angles are mapped by PI/90, i.e., 
	the vector components are computed and average for double
	angles, to get the averaging right.
**********************************************************************
input:
	anglei	the 2 angles that we wish to average
output:
	the average
*********************************************************************/
{
	float x1,x2,y1,y2,xav,yav;
	float average;	
	x1=cos(angle1*PI/90);
	y1=sin(angle1*PI/90);
	x2=cos(angle2*PI/90);
 	y2=sin(angle2*PI/90);
	
	xav = (x1+x2)/2;
	yav = (y1+y2)/2;
	
	average=atan(yav/xav)*90/PI;
	
	return average;
}
	
	

float TimeSeries(float x,float y)
/**********************************************************************
 TimeSeries is used to compute the linear particle motion given
 the circular motion components x and y. See Li & Crampin(1993)
***********************************************************************
input:
x,y	components of the circular motion A where A is
		A= x*cos(t) + y*sin(t)
output:
A	time series
***********************************************************************
CAVEAT:
the sign of A is decided from sign of x component
**********************************************************************/
{
   float A, signx;
	
	if (x !=0.0){
		signx = x/fabs(x);
		A = signx * sqrt(x*x + y*y);
	}
	else{
		A=0;
	}
	
	return A;	
}
void Asym_analysis(float a11, float a12, float a22, float* phi, 
		   float *ratio)
/*************************************************************************
	Asym_analysis is exactly the same as eig_2x2 put it also outputs
	the ratio between the minor and major eigenvalues
************************************************************************
input:

aij 	- 3 elements of the input matrix a
phi	- variable to store the main eigenvector angle in degrees
ratio   - the ratio between the minor/major eigenvalues of 
	  matrix a (optional argument)

output
none

modified parameters:
phi, ratio

*************************************************************************/
{
	float minor,major,angle;
	float D;  /* workhorse */
	float threshold = 1.0E-10;
 	
	/* Eigenvalues */
	D     = sqrt((a11-a22)*(a11-a22)-4*a12*a12);
	major = 0.5*(a11 + a22 + D);
	minor = 0.5*(a11 + a22 - D);
	if ( major <= threshold ){
		D = 0;
	}
	else{		
		D = minor/major;
	}
	/* eigenvector of biggest eigenvalue */
		angle = 0.5*atan2((2*a12),(a11 - a22));
	
	*phi = angle*180/PI;
	*ratio = D;
}  	


void eig_2x2(float a11, float a12, float a22, float *phi)
/***********************************************************************
	eig_2x2 is an anlytic solution for 2 x 2 eigen problems for
	symmetric matrices
************************************************************************
input:

aij 	- 3 elements of the input matrix a
phi	- variable to store the main eigenvector angle in degrees
ratio   - the ratio between the minor/major eigenvalues of 
	  matrix a (optional argument)

output
none

modified parameters:
phi, ratio

*************************************************************************/
{
	float minor,major,angle;
	float D;  /* workhorse */

	 	
	/* Eigenvalues */
	D     = sqrt((a11-a22)*(a11-a22)-4*a12*a12);
	major = 0.5*(a11 + a22 + D);
	minor = 0.5*(a11 + a22 - D);
	

	/* eigenvector of the biggest eigenvalue */
	
		angle = 0.5*atan2((2*a12),(a11 - a22));
	/*	angle = atan2(-a12,(a22-major));*/

	*phi = angle*180/PI;
  	
}

void sliding_window(int ns, float *vector, int nsfilter, float *filter)
/*********************************************************************
 sliding_window - computes the convolution between input vector and 
		  filter, overwritting input vector, retaining the 
		  original length of vector 
**********************************************************************
input: 

vector	- array of values
ns	- number of samples in array
filter  - filter array
nsfilter - number of samples in filter array

output:
none

modified parameters:
vector

**********************************************************************/
{
	float *temp;
	int ntemp;        /* number of samples of temp pointer */
	int i, bound;
	
	ntemp = ns + nsfilter -1;
	temp  = ealloc1float(ntemp);
	bound = nsfilter/2;

	/* convolve vector with the filter vector to 
	   produce the sliding window results		*/
	
  	convolve_cwp(ns,0,vector,nsfilter,-bound,filter,ntemp,0,temp);

	/* getting rid of the extra samples and returning vector */
	
	for (i=bound; i<(ntemp-bound); i++){
		vector[i-bound] = temp[i];
	}
}



FILE * open_vol(cwp_String vol_name, int *ns, float *dt, segy *tr) 
/****************************************************************
 
	open_vol	a function to read the volumes of 
			input data. Outputs the file pointer fp
*****************************************************************
INPUT

	vol_name	string containing the name of the volume
			to be opened

	ns		integer containg the number of samples
			in the input trace

	dt		float for the sampling rate of the 
			input data
	
	tr 		trace structure that will contain the 
			trace data

OUTPUT
	fp, ns, dt and tr

*****************************************************************/
{
	int nt;
	segy tr2;
 	FILE *fp;	
	/* verifying the existence of input file */  
	    if ((fp = fopen(vol_name,"r")) == NULL)
	      {
		err("cannot open volume %s\n",vol_name);
	      }
	/* getting first trace from input file */
	    if (!fgettr(fp,&tr2))
	      {
		err("cannot read first trace in %s volume\n",vol_name);
	      }
	/* getting number of samples and sampling rate in sec */
	    nt = (int)tr2.ns;
	   *dt = ((double)tr2.dt)/1000000.0;
	   *ns=nt; 
	    if (dt == 0) err("dt not set in header for %s volume", vol_name);
	   *tr=tr2;
	return fp;   

}
	


 FILE *check_output(cwp_String filename)

/****************************************************************
	check_output 		just checks whether the output
				file can be written and returns
				the file pointer fp, that will 
				be used later 
*****************************************************************
INPUT
	filename		
OUTPUT
	fp

****************************************************************/
{
	FILE *fp;

    if ((fp = fopen(filename,"w")) == NULL)
      {
	err("cannot open output volume %s file \n",filename);
      }
	return fp;
}
	




