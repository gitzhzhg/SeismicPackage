/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUALFORD: $Revision: 1.6 $ ; $Date: 2011/11/16 22:58:31 $	*/

#include "su.h"
#include "segy.h" 
#include "header.h"
/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" SUALFORD - trace by trace Alford Rotation of shear wave data volumes  ",
" 									",
" sualford inS11=file1 inS22=file2 inS12=file3 inS21=file4		",
" outS11=file5 outS22=file6 outS12=file7 outS21=file8 [optional         ",
" parameters]                                                           ",
"                                                                       ",
" Required Parameters:                                                  ",
" inS11=	input data volume for the 11 component			",
" inS12=	input data volume for the 12 component			",
" inS21=	input data volume for the 21 component			",
" inS22=	input data volume for the 22 component			",
" outS11=	output data volume for the 11 component			",
" outS12=	output data volume for the 11 component			",
" outS21=	output data volume for the 11 component			",
" outS22=	output data volume for the 11 component			",
"                                                                       ",
" Optional parameters:                                                  ",
" angle_inc=               sets the increment to the angle by which	",
"                         the data sets are rotated. The minimum is     ",
"                         set to be 1 degree and default is 5.          ",
" Az_key=                  to set the header field storing the azimuths	",
"                         for the fast shear wave on the output volumes ",
" Q_key=                   to set the header field storing the quality	",
"                         factors of performed optimum rotations        ",
" lag_key=                 to set the header field storing the lag in	",
"                         miliseconds the fast and slow shear components",
" xcorr_key=               to set the header field containing the maxi-	",
"                         mum normalized cross-correlation between the	",
"                         and slow shear waves.                         ",
" taper=		  2*taper+1 is the length of the sample overlap ",
"			  between the unrotated data with the rotated   ",
"			  data on the traces. The boundary between them ",
" 			  is defined by time windowning.                ",
"				taper = -1, for no-overlap		",
"				taper = 0, for overlap of one sample	",
"				taper =>1, for use of cosine scale to   ",
"					   to interpolate between the 	",
"					   unrotated and rotated parts	",
"					   of the traces		",
"									",
" taperwin=               another taper used to taper the data within   ",
"			  the window of analysis to diminish the effect ",
"                         of data near the window edges.In this way one ",
"                         can focus on a given reflector. Also given in ",
"                         number of samples                             ",
"									",
" maxlag=		  maximum limit in ms for the lag between fast  ",
" 			  and slow shear waves. If this threshold is 	",
"			  attained or surpassed, the quality factor for	",
"			  the rotation is zeroed as well as all the     ",
"			  parameters found for that certain rotation 	",
"			 						",
"									",
" ntraces=		  number of traces to be used per computation   ",
"			  ntraces=3 will use three adjacent traces to   ",
"		          compute the angle of rotation                 "
"                                                                       ",
" Notes:                                                                ",
"                                                                       ",
" The Alford Rotation is a method to rotate the four components         ",
" of a shear wave survey into its natural coordinate system, where      ",
" the fast and slow shear correspond to the inline to inline shear (S11)",
" and xline to xline (S22) volumes, respectively.                       ",
"                                                                       ",
" This Alford Rotation code tries to maximize the energy in the         ",
" diagonal volumes, i.e., S11 and S22, while minimizing the energy      ",
" in the off-diagonals, i.e., in volumes S12 and S21, in a trace by     ",
" trace manner. It then returns the new rotated volumes, saving the     ",
" the quality factor for the rotation and azimuth angle of the fast     ",
" shear wave direction for each trace headers of the new rotated S11    ",
" volume.                                                               ",
"                                                                       ",
" The fields in the header containing the Azimuth and Quality factor    ",
" and the sample lag between fast and slow shear are otrav, grnolf and  ",
" grnofr, respectively, by default. The values are multiplied by ten in ",
" the case of the angles and by a thousand for quality factors. To      ",
" change this defaults use the optional parameters Az_key, Q_key and    ",
" lag_key                                                             	",
"                                                                       ",
"			   						", 
" modified header fields:                                               ",
" the ones specified by Az_key, Q_key, lag_key and xcorr_key. By default",
" these are otrav, grnlof, tstat and grnors, respectively.            	",
NULL};

/* Credits:
 *	CWP: Rodrigo Felicio Fuck
 *      Code translated and adapted from original version in Fortran
 *      by Ted Schuck (1993)
 *
 *
 * Schuck, E. L. , 1993, Multicomponent, three dimensional seismic 
 * characterization of a fractured coalbed methane reservoir, 
 * Cedar Hill Field, San Juan County, New Mexico, Ph.D. Thesis,
 * Colorado School of Mines
 *
 */
/**************** end self doc *******************************************/



/* Global variables */
segy in_S11_tr, out_S11_tr;
segy in_S12_tr, out_S12_tr;
segy in_S21_tr, out_S21_tr;
segy in_S22_tr, out_S22_tr;


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




int main(int argc, char **argv)
{

	int nS11, nS22, nS12, nS21;  /* number of points on trace for volumes */
	float **S11=NULL,**S12=NULL;  /* the data for respective volumes */
	float **S21=NULL,**S22=NULL;
	float *mixS11=NULL, *mixS22=NULL; 
	float *mixS12=NULL, *mixS21=NULL; /* compute the angle          */
	cwp_String inS11="";	/* input S11 volume file name		*/
	cwp_String inS12="";	/* input S12 volume file name		*/
	cwp_String inS21="";	/* input S21 volume file name		*/
	cwp_String inS22="";	/* input S22 volume file name		*/
	cwp_String outS11="";	/* desired output S11 volume file name  */
	cwp_String outS12="";	/* desired output S12 volume file name	*/
	cwp_String outS21="";	/* desired output S21 volume file name	*/
	cwp_String outS22="";	/* desired output S22 volume file name	*/
	FILE *iS11fp=NULL;	/* input S11  file pointer 		*/
	FILE *oS11fp=NULL;	/* desired S11 file pointer		*/
	FILE *iS12=NULL;	/* input S12  file pointer 		*/
	FILE *oS12=NULL;	/* desired S12 file pointer		*/
	FILE *iS21=NULL;	/* input S21 file pointer 		*/
	FILE *oS21=NULL;	/* desired S21 file pointer		*/
	FILE *iS22=NULL;	/* input S22 file pointer 		*/
	FILE *oS22=NULL;	/* desired S22 file pointer		*/
	float dtS11, dtS12, dtS21, dtS22;
	cwp_String Az_key;      /* keys used to store the obtained azimuth */
	cwp_String Q_key;       /* and quality factor in the trace header */
	                        /* of the output S11 volume               */
	cwp_String lag_key;
	cwp_String xcorr_key;
	cwp_String INPUTQ_key;

	cwp_String xcorr_type;
	cwp_String lag_type;
	cwp_String Az_type;
	cwp_String Q_type;
	cwp_String INPUTQ_type;

	Value Az_value;
	Value Q_value;
	Value lag_value;
	Value xcorr_value;
	Value INPUTQ_value;

	int index_lag;
	int index_Az;
	int index_Q;
	int index_xcorr; 
	int index_INPUTQ;
	int counter = 0;

 	float angle_inc;        /*optional parameter to set angle increment */
	float tmin,tmax;        /* optional parameters specifying a time 
				 * window where the best alford rotation angle
				 * should be estimated */
	int itmin,itmax,nt;     /*first sample, last sample and number of sam-
			         * ples in the window of analysis          */
	int taper, taper2;
	float angle;              
	float quality;
	float inputquality;
	float lag;
	float maxlag;
	float xcorr;
	int tempval;
	int Total;              /*  total number of traces to be processed */

	float f_counter;
	int   ntraces;		/* number of traces to enter in the 
				   computation of the angle of rotation	*/
	float *mix;		/* array of mix values 			*/
	int i, j;
	size_t tracebytes, databytes;
		
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
	if (!getparstring("inS11",&inS11))
		err("no file specified for S11 volume. Aborting...");	
	if (!getparstring("inS12",&inS12))
		err("no file specified for S12 volume. Aborting...");
	if (!getparstring("inS21",&inS21))
		err("no file specified for S21 volume. Aborting...");
	if(!getparstring("inS22",&inS22))
		err("no file specified for S22 volume. Aborting...");
	if(!getparstring("outS11",&outS11))
		outS11="S11_out";	
	if(!getparstring("outS12",&outS12))
		outS12="S12_out";	
	if (!getparstring("outS21",&outS21))
		outS21="S21_out";
	if (!getparstring("outS22",&outS22))
		outS22="S22_out";

	/*      open the volumes of input data 	*/
	
	iS11fp = open_vol(inS11, &nS11, &dtS11, &in_S11_tr);
	iS12   = open_vol(inS12, &nS12, &dtS12, &in_S12_tr);
	iS21   = open_vol(inS21, &nS21, &dtS21, &in_S21_tr);
	iS22   = open_vol(inS22, &nS22, &dtS22, &in_S22_tr);
		
	/* compute databytes per trace and bytes in the mixing panel */
		tracebytes = FSIZE*nS11;
		databytes  = tracebytes * ntraces;
	
	/* Allocating memory and initializing the data */
	    S11 = ealloc2float(nS11,ntraces);
	    S12 = ealloc2float(nS12,ntraces);
	    S21 = ealloc2float(nS21,ntraces);
	    S22 = ealloc2float(nS22,ntraces);

	    memset((void *) S11[0], 0, databytes);
	    memset((void *) S12[0], 0, databytes);
	    memset((void *) S21[0], 0, databytes);
	    memset((void *) S22[0], 0, databytes);
	
	/* copying first trace to S11[][] and so on */
	
	    memcpy((void *) S11[0], (const void *) in_S11_tr.data, tracebytes);
	    memcpy((void *) S12[0], (const void *) in_S12_tr.data, tracebytes);
	    memcpy((void *) S21[0], (const void *) in_S21_tr.data, tracebytes);
	    memcpy((void *) S22[0], (const void *) in_S22_tr.data, tracebytes);

			   
	
	/****************************************************************/
	/*              Reading optional parameters			*/
	/****************************************************************/


	/*get keys for storing values in trace headers */

	if (!getparstring("Az_key",&Az_key)) 		Az_key = "otrav";
	if (!getparstring("Q_key",&Q_key)) 		Q_key = "grnlof";
	if (!getparstring("lag_key",&lag_key))          lag_key = "tstat";
	if (!getparstring("xcorr_key", &xcorr_key))     xcorr_key = "grnors";
	if (!getparstring("INPUTQ_key", &INPUTQ_key))   INPUTQ_key= "gaps";

	Az_type      = hdtype(Az_key);
	Q_type       = hdtype(Q_key);
	INPUTQ_type  = hdtype(INPUTQ_key);
	lag_type     = hdtype(lag_key);
	xcorr_type   = hdtype(xcorr_key);
	index_Az     = getindex(Az_key);
	index_Q      = getindex(Q_key);
	index_INPUTQ = getindex(INPUTQ_key);
	index_lag    = getindex(lag_key);
	index_xcorr  = getindex(xcorr_key);

	/* get optional parameter for taper to be used
	   on the top of the window to be rotated */
	if(!getparint("taper", &taper)) taper = 3;

	/* get optional parameter for taper2 to be used
	   on tapering the window edgesi within the Find_Angle function */
	if(!getparint("taperwin", &taper2)) taper2 = 10;
	
	/* get optional parameter for angle increment */
           
	if (!getparfloat("angle_inc",&angle_inc)) angle_inc = 5.0;

	/* get optional parameter for total number of traces to be processed */

	if(!getparint("Total", &Total)) Total = 0;

	/* get optional parameter for maximum lag between the 
	   fast and slow shear waves (in ms) */
	
	if(!getparfloat("maxlag", &maxlag)) maxlag=40; 

	/******************************************************************
		get optional parameters for window of analysis 
			(code stolen from suwind)                        
	******************************************************************/

	if (getparfloat("tmin", &tmin))
		itmin = NINT((tmin - in_S11_tr.delrt/1000.0)/dtS11);
	else {
		itmin = 0;
		tmin = in_S11_tr.delrt/1000.0;
	}
		
	if (getparfloat("tmax", &tmax)){
    			itmax = NINT((tmax - in_S11_tr.delrt/1000.0)/dtS11);
    			nt    = itmax - itmin + 1;
	  	}

	else {
		itmax = nS11 - 1;
		tmax  = itmax*dtS11 + in_S11_tr.delrt/1000.0;
		nt    = itmax - itmin +1;
	}
        checkpars();
		/* checking time gating values */
		if (itmin <0)
			err("itmin = %d should be positive", itmin);
		if (nt > SU_NFLTS)
			err(" nt = %d exceeds SU_NFLTS=%d", nt, SU_NFLTS);
		if (itmin > itmax)
			err ("itmin=%d, itmax = %d conflict", itmin, itmax);

	/*********************************************************************
			 End of borrowed code from suwind 
	**********************************************************************/

	
	/********************************************************************	
				 Checking output volumes
	********************************************************************/
   

		oS11fp=check_output(outS11);
		oS12  =check_output(outS12);
		oS21  =check_output(outS21);
		oS22  =check_output(outS22);


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


	/* Initializing the array mix that will be used to produce a 
	   running average mix of the input traces to the Alford 
	   rotatoin 							*/
	
	mix=ealloc1float(ntraces);
	for (i=0; i < ntraces; i++){
		mix[i] = 1/((float) ntraces);
	}		


	/* Allocate temporary space	*/


	mixS11 = ealloc1float(nS11);
	mixS12 = ealloc1float(nS12);
	mixS21 = ealloc1float(nS21);
	mixS22 = ealloc1float(nS22);
	/*******************************************************************
         * looping through traces                                             *
	 *********************************************************************/
	
	do{
	    	if (counter !=0)
	     	{/* reads the next trace in S22 input volume */
	      		 memcpy((void *) S22[0], (const void *) in_S22_tr.data,
				 nS22*FSIZE);
	     	}
		/* initialize mix files */

		memset((void *) mixS11, 0, nS11*FSIZE);
		memset((void *) mixS12, 0, nS12*FSIZE);
		memset((void *) mixS21, 0, nS21*FSIZE);
		memset((void *) mixS22, 0, nS22*FSIZE);
	

	  	/* loop over time samples */
		for (i=itmin; i <= itmax; ++i){
			/* loop over the input traces   */
			for (j=0; j < ntraces; ++j){
				mixS11[i]+=S11[j][i]*mix[j];
				mixS12[i]+=S12[j][i]*mix[j];
				mixS21[i]+=S21[j][i]*mix[j];
				mixS22[i]+=S22[j][i]*mix[j];
			}
		}
		/* Now that the input data was prepared find the best
		   angle, and also output the found quality factor,
		   lag and xcorrelation coefficient between fast and slow
		   traces of the shear waves				*/
	
		angle=Find_Angle(mixS11, mixS12,mixS21, mixS22, angle_inc,
			     nS11, dtS11, nt, itmin, itmax, &quality, 
			     &inputquality, &lag, &xcorr,taper2);


		/* if the found lag is too big then the rotation is 
		not performed  					*/
		if(lag*1000 > maxlag || quality<=0 ){
			quality = 0.0;
			lag = 0.0;
			angle=0.0;
		}

		/* if there was a window of anlysis then perform the final 
		   rotation, which will rotate the whole trace from the window
			beginning till the end of the trace 		*/
	  		
		 if (nt != nS11)
		   {
		    /* mix files will function as a tempfile to store
			the traces contained in S11[0][0-1499] and so on */
	   		memcpy((void *) mixS11,(const void *)S11[0],tracebytes);
	   		memcpy((void *) mixS12,(const void *)S12[0],tracebytes);
	   		memcpy((void *) mixS21,(const void *)S21[0],tracebytes);
	    		memcpy((void *) mixS22,(const void *)S22[0],tracebytes);
			
		     Final_Rot(mixS11, mixS12, mixS21, mixS22, 
				angle, nS11, itmin, taper);
		   }

	/*****************************************************************
		Outputting the results 
	*******************************************************************/
	
	  /* copying the modified traces to output traces */
	
	  memcpy((void *)out_S11_tr.data, (const void *)mixS11, nS11*FSIZE);
	  memcpy((void *)out_S12_tr.data, (const void *)mixS12, nS12*FSIZE);
	  memcpy((void *)out_S21_tr.data, (const void *)mixS21, nS21*FSIZE);
	  memcpy((void *)out_S22_tr.data, (const void *)mixS22, nS22*FSIZE);

	  /* copying headers */
	  memcpy( (void *) &out_S11_tr, (const void *) &in_S11_tr, HDRBYTES);
	  memcpy( (void *) &out_S12_tr, (const void *) &in_S12_tr, HDRBYTES);
	  memcpy( (void *) &out_S21_tr, (const void *) &in_S21_tr, HDRBYTES);
	  memcpy( (void *) &out_S22_tr, (const void *) &in_S22_tr, HDRBYTES);
	 
    
	  /****************************************************************
		 storing Azimuth of the fast shear wave, quality of the 
		rotation, the lag between the fast and slow shear in samples 
		and maximum of the normalized cross correlation between the 
		traces of the fast and slow shear in each trace header for 
		the four output volumes 
	   *****************************************************************/

	  tempval = NINT(angle*10);
	  TypeConv(tempval, Az_type, &Az_value);
	  puthval(&out_S11_tr, index_Az, &Az_value); /* Fast shear az angle */
	  puthval(&out_S12_tr, index_Az, &Az_value);
	  puthval(&out_S21_tr, index_Az, &Az_value);
	  puthval(&out_S22_tr, index_Az, &Az_value);
	 
	  tempval = NINT(quality*1000);
	  TypeConv(tempval, Q_type, &Q_value);
	  puthval(&out_S11_tr, index_Q, &Q_value); /* quality factor*/
	  puthval(&out_S12_tr, index_Q, &Q_value);
	  puthval(&out_S21_tr, index_Q, &Q_value);
	  puthval(&out_S22_tr, index_Q, &Q_value);
	 
	  tempval = NINT(inputquality*1000);
          TypeConv(tempval, INPUTQ_type, &INPUTQ_value);
          puthval(&out_S11_tr, index_INPUTQ, &INPUTQ_value); /* inputq factor*/
          puthval(&out_S12_tr, index_INPUTQ, &INPUTQ_value);
          puthval(&out_S21_tr, index_INPUTQ, &INPUTQ_value);
          puthval(&out_S22_tr, index_INPUTQ, &INPUTQ_value);


	  tempval=NINT(lag*1000); 
	  TypeConv(tempval, lag_type, &lag_value);
	  puthval(&out_S11_tr, index_lag, &lag_value); /* lag of maximum */
	  puthval(&out_S22_tr, index_lag, &lag_value);

	  tempval = NINT(lag*500);
	  TypeConv(tempval, lag_type, &lag_value);
	  puthval(&out_S12_tr, index_lag, &lag_value);/* off-diagonals get */
	  puthval(&out_S21_tr, index_lag, &lag_value);/* only half of it  */	

	  tempval = NINT(xcorr*100); /* maximum of cross-correlation*/
	  TypeConv(tempval, xcorr_type, &xcorr_value);
	  puthval(&out_S11_tr, index_xcorr, &xcorr_value);
	  puthval(&out_S12_tr, index_xcorr, &xcorr_value);
	  puthval(&out_S21_tr, index_xcorr, &xcorr_value);
	  puthval(&out_S22_tr, index_xcorr, &xcorr_value);
	  /* writing out the traces to the output files*/
	    fputtr(oS11fp, &out_S11_tr);
	    fputtr(oS12, &out_S12_tr);
	    fputtr(oS21, &out_S21_tr);
	    fputtr(oS22, &out_S22_tr);
	  
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


float Quality_Factor(float *_S11, float *_S12, float *_S21, float *_S22,
		     int nt, int mode)

/************************************************************************
Quality_Factor - computes the quality factor for a given rotation

*************************************************************************

input:
_S11         trace from S11 volume
_S12         trace from S12 volume
_S21         trace from S21 volume
_S22         trace from S22 volume

output:

Q            quality factor (varying from 0 to 1.0)

*************************************************************************
Notes:
The quality factor is a measure of the energy in the diagonals traces (S11
and S22 vs. that of the off-diagonal traces (S12 and S21) as is defined 
by Schuck (1993)                                                        

*************************************************************************/
{

  float amp_S11, amp_S12, amp_S21, amp_S22;
  float Q, min, max, num, den;
  float noise = 1.0E-30;

  amp_S11 = RMS_amp(_S11,nt) + noise;
  amp_S12 = RMS_amp(_S12,nt) + noise;
  amp_S21 = RMS_amp(_S21,nt) + noise;
  amp_S22 = RMS_amp(_S22,nt) + noise;

  if(amp_S11 >= amp_S22)
    {
      max = amp_S11;
      min = amp_S22;
    }
  else
    {
      max = amp_S22;
      min = amp_S11;
    }
  num = amp_S12 + amp_S21;
  den = amp_S11 + amp_S22;

  if (mode==0)
  {
    Q = (1.0-(num/den));
  }
 else
   {
     Q = (min/max)*(1.0 - (num/den));
   }
  return Q;

}



void ROT4TR(float *_S11, float *_S12, float *_S21, float *_S22, 
	    float *_S1,float *_D1, float *_D2, float *_S2, 
	    float _theta, int itmin, int itmax, int taper)
/***************************************************************************
ROT4TR - rotate traces by a given angle _theta

Input:

_S11          original S11 volume
_S12          original S12 trace
_S21          original S21 trace
_S22          original S22 trace
_S1           rotated S11 trace (reference parameter)
_D1           rotated S12 trace (reference parameter)
_D2           rotated S21 trace (reference parameter)
_S2           rotated S22 trace (reference parameter)
_theta        angle of rotation
itmin         first sample of the window on orginal datasets 
itmax         last sample of the window on original datasets
taper	      the taper length in samples; used to taper the 
		traces on the window of analysis. the tapering 
		is a cosine one

output:

none

modified referenced parameters:

_S1, _D1, _D2, _S2
**************************************************************************
NOTES:
This code is an adaptation of a FORTRAN code done by Schuck (1993)

***************************************************************************/
{

     float rad, cos2, sin2, sincos,temp, env=0.0;
     float f;
     int i=0, nt;


     rad    = _theta*(PI/180); /* PI is defined in su.h */
     cos2   = pow(cos(rad),2);
     sin2   = pow(sin(rad),2);
     sincos = cos(rad)*sin(rad);
     nt     = itmax - itmin +1;

     /* loop over trace samples */
     for (i=itmin;i <= itmax;i++)
       {
	 temp = sincos*(_S12[i]+ _S21[i]);
	 _S1[i-itmin] = _S11[i]*cos2 + _S22[i]*sin2 + temp;
	 _S2[i-itmin] = _S11[i]*sin2 + _S22[i]*cos2 - temp;
	 temp         = sincos*(_S22[i]- _S11[i]);
	 _D1[i-itmin] =  _S12[i]*cos2 - _S21[i]*sin2 + temp;
	 _D2[i-itmin] = -_S12[i]*sin2 + _S21[i]*cos2 + temp;
       }

	/* tapering the data  - code adapted from sutaper */

	
	if (taper > 1){
		for (i=0; i<taper; i++) {
			f = (float)i/(float)taper;
			env = 0.5*(1.0-cos(PI*f));

			/* upper part */
			_S1[i] *= env;
			_S2[i] *= env;
			_D1[i] *= env;
			_D2[i] *= env;
			
			/* lower part */
			
			_S1[nt - i] *= env;
			_S2[nt - i] *= env;
			_D1[nt - i] *= env;
			_D2[nt - i] *= env;
		}
	}
		
			
	

}

void Final_Rot(float *_S11, float *_S12, float *_S21, float *_S22,
	       float _theta, int nt, int itmin, int taper)
/******************************************************************
Final_Rot - executes a rotation of 4 traces by a given angle

*******************************************************************
Input:
_S11         input S11 trace
_S12         input S12 trace
_S21         input S21 trace
_S22         input S22 trace
_theta       angle of rotation
nt           number of samples in input traces
itmin        sample number of the first sample on 
	     top of the window chosen to perform
	     the rotation
taper        2*taper+1 is the overlap between the 
	     unrotated and the rotated parts of
	     the traces

Output:
none

Modified reference parameters
_S11, _S12, _S21, _S22
******************************************************************
NOTES: This is exactly the same function as ROT4TR except that
it does modify the input reference parameters, i.e., the input 
traces and operates of the entire input traces. Also it provides
for interpolation between the unrotated and rotated parts of the
input traces. The unrotated part of the trace is located immediately
above the window of analysis

******************************************************************/
{
  float rad, cos2, sin2, sincos,temp, c1, c2;
  int i=0;
  float *_S1, *_D1, *_D2, *_S2;
  int overlap;
  int n=0;

  /* allocating memory  for temporary variables*/
     _S1 = ealloc1float(nt);
     _S2 = ealloc1float(nt);
     _D1 = ealloc1float(nt);
     _D2 = ealloc1float(nt);


     rad = _theta*(PI/180); /* PI is defined in su.h */
     cos2 = pow(cos(rad),2);
     sin2 = pow(sin(rad),2);
     sincos = cos(rad)*sin(rad);

	/* upper part of the output traces */
	if (taper == -1) {n=1;} /* no overlaping */

	for (i = 0; i <= itmin-taper-1-n; i++){
		_S1[i] = _S11[i];
		_S2[i] = _S22[i];
		_D1[i] = _S12[i];
		_D2[i] = _S21[i];}

	/* overlap and lower part of the output traces */

     for (i = itmin-taper-n; i <= nt-1; i++)
       {
	 temp = sincos*(_S12[i] + _S21[i]);
	 _S1[i] = _S11[i]*cos2 + _S22[i]*sin2 + temp;
	 _S2[i] = _S11[i]*sin2 + _S22[i]*cos2 - temp;
	 temp = sincos*(_S22[i] - _S11[i]);
	 _D1[i] =  _S12[i]*cos2 - _S21[i]*sin2 + temp;
	 _D2[i] = -_S12[i]*sin2 + _S21[i]*cos2 + temp;
       }

	/* joining the top and bottom of traces using 
	   a cosine scale (code borrowed and adapted 
	   from suvcat) */
	overlap = 2*taper+1; /* length of overlap */
	if (overlap == 1){ 
		/* simple average for one sample overlap */
		temp = 0.5*(_S1[itmin] + _S11[itmin]);
		_S1[itmin] = temp;
		temp = 0.5*(_D1[itmin] + _S12[itmin]);
		_D1[itmin] = temp;
		temp = 0.5*(_D2[itmin] + _S21[itmin]);
		_D2[itmin] = temp;
		temp = 0.5*(_S2[itmin] + _S22[itmin]);
		_S2[itmin] = temp;
	}
	if (overlap > 1){ 
		/* cosine scale for overlaps bigger than 1 */
		for (i=0; i < overlap; i++){
			c1 = cos((PI/2)*(i/(overlap-1)));
			c2 = 1-c1;
		
			temp = c1*_S11[itmin-taper+i] + 
				c2*_S1[itmin-taper+i];
			_S1[itmin-taper+i] = temp;

			temp = c1*_S12[itmin-taper+i] + 
				c2*_D1[itmin-taper+i];
			_D1[itmin-taper+i] = temp;

			temp = c1*_S21[itmin-taper+i] + 
				c2*_D2[itmin-taper+i];
			_D2[itmin-taper+i] = temp;

			temp = c1*_S22[itmin-taper+i] + 
				c2*_S2[itmin-taper+i];
			_S2[itmin-taper+i] = temp;
		}
	}

     /* saving the results in the on the input traces */

     memcpy((void *) _S11, (const void *) _S1, nt*FSIZE);
     memcpy((void *) _S12, (const void *) _D1, nt*FSIZE);
     memcpy((void *) _S21, (const void *) _D2, nt*FSIZE);
     memcpy((void *) _S22, (const void *) _S2, nt*FSIZE);
}



float Find_Angle(float *_S11, float *_S12, float *_S21, float *_S22,
		 float _ang_inc,int _nS11, float _dtS11,int nt, int _itmin, 
		 int _itmax,float *_quality, float *_inputquality,
		 float *lag, float *xcorr, int taper)
/****************************************************************
Find_Angle - computes the optimum angle for the alford rotation

*****************************************************************
Input:
_S11             input S11 trace

_S12             input S12 trace

_S21             input S21 trace

_S22             input S22 trace

_ang_inc         angle increment to be used on the discrete 
                 rotations of input traces

_nS11            number of samples in input traces

_itmin           index of first sample within window of analysis

_itmax           index of last sample within window of analysis

nt               number of samples in window of analysis

_quality         quality factor for best rotation

_inputquality	the "quality factor for the input traces"

lag              number of samples by which the slow shear wave 
                 is delayed in relation to the fast for the 
                 best rotation. Computed by cross-correlation

xcorr            Value of the maximum of cross-correlation
                 between the traces of the fast and slow shear
                 waves

taper		 taper length to be used to taper the traces
		 on the window of analysis. This is the same
		 as the taper length used in Final Rot function
		 but there it serves another purpose
		
Output:
_angles[imax]    angle corresponding to optimum angle of rotation

Modified referenced parameters:
_quality, _inputquality, lag, xcorr

_S11, _S12, _S21, _S22 if _nS11 equals nt

*******************************************************************
NOTES:This function is an adaptation to original FORTRAN code 
written by Schuck (1993)                                             
*******************************************************************/

{
  float *_S1, *_S2, *_D1, *_D2;
  float *S1res, *S2res;
  int 	ntres;
  float dtres;
  int i=0;
  float *_angles;
  float *Q;     /* Array of quality factors for rotations */
  float qmax, qmin;
  int imax=0, imin;
  float y[3];
  float *temp;
  int _ns_angle;
  float angle_frac;
  int peak_index,  ntemp, zero_lag;
  float AMPS1, AMPS2; 		/* variables to store the rms amplitudes 
				   of the S1 and S2 traces after the best
				   angle has been found */


  /* allocating memory  for temporary variables*/
     _S1 = ealloc1float(nt);
     _S2 = ealloc1float(nt);
     _D1 = ealloc1float(nt);
     _D2 = ealloc1float(nt);


  if (_ang_inc < 1.0)
    {
      _ang_inc = 5.0;
    }


  /* allocating memory to variables dependent on
   * the number of angles              */

     _ns_angle = 1+ floor(90/_ang_inc);
     _angles = ealloc1float(_ns_angle);
     Q = ealloc1float(_ns_angle);

/* building table of angles to be used on rotations */
	_angles[0]=0;

     for (i = 1; i<=_ns_angle-1; i++){
	 _angles[i] = _angles[i-1]+_ang_inc;
	}

  /*  rotate data for each one of the angles,
   *  and compute respective quality factors
   */

  for (i=0; i<=_ns_angle-1; i++)
    {
      ROT4TR(_S11, _S12, _S21,_S22,_S1,_D1,_D2,_S2,
		_angles[i],_itmin, _itmax, taper);
      Q[i] = Quality_Factor(_S1,_D1,_D2,_S2, nt,0);
     
    }

	*_inputquality = Q[0];

  /* searching for max value in Q[i] */

  qmax = -1.0E30;
  qmin = 1.0E30;

  for (i=0; i<= _ns_angle-1; i++)
    if (Q[i] > qmax)
      {
	qmax = Q[i];
	imax = i;
      }
  if (Q[i] <= qmin)
    {
      qmin = Q[i];
      imin = i;
    }
  /* selecting Qs for interpolation */
  

   if (imax!=0 && imax != _ns_angle-1)
    {
      for (i=0; i<=2;i++)
	{
	  y[i] = Q[imax-1+i];
	}
    }
    else
      {
	/* if the maximum is located on the first or last
	* element of the array of Q's, make use of the
	* symmetry of the problem that allows that the Q for
	* 95 degrees to be equal to that of 5 or of -5 degrees
	* be equal to 85 degrees
	*/

	y[1] = Q[imax];
	if(imax == 0)
	  {

	    y[0]=Q[_ns_angle-2]; /* the sample before last*/
	    y[2] = Q[imax+1];
	  }
	else
	  {
	    y[0] = Q[imax -1];
	    y[2] = Q[1]; /* the sample after the first */
	  }
      }

/* interpolating the data around the max found
   * above by quadratic to interpolate the angle
   */
  

  angle_frac = QuadFit(y,&qmax);
  _angles[imax] += _ang_inc*angle_frac;

  /* Rotating the data on window  to the angle determined above */

  ROT4TR(_S11, _S12, _S21,_S22,_S1,_D1,_D2,_S2,
	 _angles[imax],_itmin,_itmax, taper);


  /* compute crosscorrelation between output S1 and S2 traces
   * and pick the sample with maximum amplitude
   */

   	/* First interpolate the S1 and S2 datasets 
	   for better resolution of the delay 		*/
	


	ntres=4*nt;
	dtres=_dtS11/4;

	S1res = ealloc1float(ntres);
	S2res = ealloc1float(ntres);

	interp(_S1,_S2,nt,_dtS11, S1res, S2res, ntres, dtres);
  
	ntemp = 2*ntres-1;
	temp = ealloc1float(ntemp);
	


	/* crosscorrelated the interpolated data sets	*/
	
  	nxcor(ntres,0,S1res,ntres,0,S2res,ntemp,-ntres+1,temp);
 	peak_index = FindPeak(temp, ntemp, xcorr);

  	zero_lag = ntres-1;
  	*lag = dtres*(peak_index - zero_lag);
/* if the peak is before the zero_lag position
  * we have to delay the S2 in relation
  * to S1. This means that S2 is actually the fast
  * shear, and the angle should then be adjusted accordingly
  */
  if (peak_index < zero_lag)
    {
      _angles[imax] += 90;
     

      if (_angles[imax] >=180)
	{
	  _angles[imax] -= 180;
	}
      *lag *= -1; 
    }

	/* if there is no lag between S1 and S2 but the amplitudes 
	of S2 are bigger, the right angle is also 90 from the found angle 
	(this assumes that the shear-velocity contrast is bigger for the 
	fast shear wave than the slow-shear wave)*/
       		AMPS1 = RMS_amp(_S1,nt);
		AMPS2 = RMS_amp(_S2,nt);
	if(peak_index == zero_lag && AMPS2 > AMPS1){
		_angles[imax] +=90;
		if (_angles[imax] >=180){
			_angles[imax] -= 180;
		}
	}	
		

  *_quality = Quality_Factor(_S1, _D1, _D2, _S2, nt,0);
 
  

  /* copying  the result to respective traces */
 /* if the window encompasses the whole of the trace*/
  if(nt == _nS11){

    Az_swap(_S1, _D1,_D2,_S2, nt);

    memcpy((void *)_S11, (const void *)_S1, nt*FSIZE);
    memcpy((void *)_S22, (const void *)_S2, nt*FSIZE);
    memcpy((void *)_S12, (const void *)_D1, nt*FSIZE);
    memcpy((void *)_S21, (const void *)_D2, nt*FSIZE);
	}
  /* returning the angle and quality factor for the trace*/
  
  return _angles[imax];
  
  }




float Total_Quality(float *S11, float *S22, float qmin, float qmax, int nt)
/**************************************************************************
Total_Quality - computes the final quality of related to the best rotation

**************************************************************************
Input:
S11            input trace from window on S11 data
S22            input trace from window on S22 data
qmin           minimum quality factor from discrete 
               rotations
qmax           quality factor associated with optimum
               angle 
nt             number of samples in window of data

Output:
Q              final quality factor

*****************************************************************************
NOTES: If Q is high then the best rotation was indeed better than the worst 
one. Also, the ratio between the diagonal components S11 and S22 ensures that
amplitudes should not be signficantly different for this volumes.

This code was adapted from FORTRAN original written by Schuck (1993)

****************************************************************************/
{
 float amp_S11, amp_S22;
 float Q, min, max;
 float noise = 1.0E-30;
  amp_S11 = RMS_amp(S11, nt)+noise;
  amp_S22 = RMS_amp(S22, nt)+noise;

  if(amp_S11 >= amp_S22)
    {
      max = amp_S11;
      min = amp_S22;
    }
  else 
    {
      max = amp_S22;
      min = amp_S11;
    }
  
  Q = (min/max)*(1.0 - (qmin/qmax));

  return Q;
}


void Az_swap(float *_S1,float *_D1,float *_D2,float *_S2,int _ns)
/****************************************************************
	Az_swap - swap the data from S1 to S2, and turn D1 into
		  -D2
*****************************************************************
Inputs:
	_S1  	fast shear wave 
	_S2	slow shear wave data
	_D1	the first-off diagonal, S12
	_D2	the second off-diagonal, S21
	_ns 	number of samples
Output:
	_S1,_S2,_D1 and _D2
****************************************************************/
{
	  float *temp;
  	int i=0;

 	 temp = ealloc1float(_ns);
	  /* copying S1 into S2 and vice versa */
	  memcpy((void *)temp, (const void *)_S1, _ns*FSIZE);
	  memcpy((void *)_S1, (const void *)_S2, _ns*FSIZE);
	  memcpy((void *) _S2, (const void *) temp, _ns*FSIZE);
	 /* copying -D2 into D1 and vice versa */
	 for (i=0;i<=_ns-1; i++)
	   {
     		_D1[i] *= -1;
     		_D2[i] *= -1;
   	}
 	memcpy((void *)temp, (const void *)_D1, _ns*FSIZE); 
	 memcpy((void *) _D1, (const void *)_D2, _ns*FSIZE);
	 memcpy((void *) _D2, (const void *) temp, _ns*FSIZE);

}

float RMS_amp(float *_tr, int nt)
/*********************************************************************
 * RMS_amp - computes the RMS amplitude of an input trace 
 *
 **********************************************************************
 Input:
  *_tr                    input trace
  nt                      trace length    
      
 output:
  _amp                    Rms amplitude
  *********************************************************************/
{
    float _amp = 0;
    int _counter = 0;
    int i=0;

  for (i=0; i<= nt-1; ++i)
    {
      _amp += _tr[i]*_tr[i];
      _counter++;
    }
	 _amp =sqrt(_amp/_counter);

  return _amp;
}


int FindPeak(float *y, int nt, float *val)
/**************************************************************
FindPeak - finds the maximum of an input vector

***************************************************************
Input:
y            input vector
nt           number of samples of input vector
val          reference parameter to store the maximum
             value found in the input vector

Output:
index        index of the sample with maximum
             value      

modified parameters:
val                        
****************************************************************/

{
  float max = -1E30;
  int i, index=0;
  for (i=0; i <= nt-1; i++)
    {
      if (y[i]> max)
	{
	  max = y[i];
	  index = i;
	}
    }
  *val= max;
  return index;
}

float QuadFit (float *y, float *max)
/*****************************************************************
QuadFit - quadratic interpolation of three given values, assuming
          the second is the biggest of them. Function used to 
          estimate the maximum around the second value.
******************************************************************
Input:
y                input vector of three values
max              reference parameter used to 
                 store the interpolated maximum value found

Output:
x                ordinate of the interpolated maximum

Modified Reference Parameter:
max               

******************************************************************
NOTE: function written by Schuck (1993) in FORTRAN, with only 
necessary changes 
 Three equally spaced quality measures
 are input, assuming that the maximum is the middle one.
 function returns both the fractional distance away from the
 middle point and the amplitude value of 
 the computed quality maxima as max

*****************************************************************/
{
  float a, b, c, x;
  /* computing quadratic coefficients */
 
  c = y[1];
  b = (y[2]-y[0])*0.5;
  a = y[0]+b-c;

  /* the point of maxima */

  x = -b/(2.0*a);
  /* new maximum value */

  *max = c + x*(b+x*a);

  return x;
}



void TypeConv(int dval_out, cwp_String type_in, Value *val_out)
/****************************************************************
	TypeConv is a function used to convert between two types
	of variables. Used to allow the user to set a certain
	variable to a desired trace header field.
*****************************************************************
NOTE
	fucntion TypeConv is actually a copy of part of the  
	code written by John Stockwell for suazimuth
*****************************************************************/
{
/* Convert output to appropriate type */
	switch (*type_in) {
	case 's':
		err("can't change char header word");
	break;
	case 'h':
		val_out->h = (short) dval_out;
	break;
	case 'u':
		val_out->u = (unsigned short) dval_out;
	break;
	case 'l':
		val_out->l = (long) dval_out;
	break;
	case 'v':
		val_out->v = (unsigned long) dval_out;
	break;
	case 'i':
		val_out->i = (int) dval_out;
	break;
	case 'p':
		val_out->p = (unsigned int) dval_out;
	break;
	case 'f':
		val_out->f = (float) dval_out;
	break;
	case 'd':
		val_out->d = (double) dval_out;
	break;
	default:
		err("unknown type %s", type_in);
	break;
	}
}

void nxcor(int nx, int x1, float *x, int ny, int y1, float *y,
	    int nz, int z1, float *z)
/*****************************************************************
  nxcor - computes the normalized cross-correlation of x and y

******************************************************************
Input:

nx, ny, nz         lengths of vector x, y and z
x1, y1, z1         first samples of vectors x, y and z
x, y               input vectors to be correlated
z                  output vector storing correlation

Output:

none

Modified reference paramenter:

z

********************************************************************
NOTES: this is just a modified version of function xcor provided
with SU. The only change is that the output of xcor is normalized

*********************************************************************/
{
  float sumX = 0;
  float sumY = 0;
  float N;
  int i=0;

  /* calculating the cross-correlation */
  xcor(nx, x1, x, ny, y1, y, nz, z1, z);


  /* doing the normalization */
  if (ny==nx)
	{
	  for (i=0; i<=nx-1; i++)
   		 {
		   sumX += x[i]*x[i];
		   sumY += y[i]*y[i];
   		 }
	}
   else
	{
	 for (i=0; i<=nx-1; i++)
		{
		  sumX += x[i]*x[i];
		}
	 for (i=0; i <= ny-1; i++)
		{
		  sumY += y[i]*y[i];
		}
	}
  sumX = sqrt(sumX);
  sumY = sqrt(sumY);
  N = sumX * sumY;

  for(i=0; i<=nz-1; i++)
    {
      z[i]=z[i]/N;
    }
}



void interp(float *S1, float *S2, int nt_in, int dt_in,
	    float *S1_res, float *S2_res, int nt, float dt)
/************************************************************
	interp  - interpolates S1 and S2 in preparation for 
		crosscorrelation to measure the time lag 
		between fast and slow shear traces

**************************************************************
Input

	S1  		fast shear volume
	S2		slow shear wave volume
	nt_in		number of samples on trace
	dt_in		sampling spacing
	S1_res		resampled S1 trace
	S2_res		resampled S2 trace
	nt		number of samples on output interpolated
			traces
	dt		sampling spacing of output interpolated
			traces
				
Output
	none

Reference variables modified

	S1_res, S2_res, nt and dt
************************************************************
NOTES: this is code draws on the SURESAMP

************************************************************/

{

	float *t;	/* array of output times		*/
	
	
	
	/* Allocate vector of output times */
	t = ealloc1float(nt);

			
	/* Compute output times */
	{ register int itime;
	  register float tvalue;
	  for (itime=0,tvalue=0; itime<nt; itime++,tvalue+=0.25)
		t[itime] = tvalue;

	}
	
		
	/* sinc interpolate new data */
	
	ints8r(nt_in, 1, 0, S1,0.0, 0.0, nt, t, S1_res);
	ints8r(nt_in, 1, 0, S2,0.0, 0.0, nt, t, S2_res);
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
	
