/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUWELLRF: $Revision: 1.14 $ ; $Date: 2011/11/16 23:59:55 $	*/

#include "par.h"
#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUWELLRF - convert WELL log depth, velocity, density data into a	",
"	uniformly sampled normal incidence Reflectivity Function of time",
" 									",
" suwellrf [required parameters] [optional parameters] > [stdout]	",
" 									",
" Required Parameters:							",
" dvrfile=	file containing depth, velocity, and density values	",
" ...or...								",
" dvfile=	file containing depth and velocity values		",
" drfile=	file containing depth and density values		",
" ...or...								",
" dfile=	file containing depth values				",
" vfile=	file containing velocity log values			",
" rhofile=	file containing density log values			",
" nval= 	number of triplets of d,v,r values if dvrfile is set,	",
" 		number of pairs of d,v and d,r values dvfile and drfile	",
"		are set, or number of values if dfile, vfile, and rhofile",
"		are set.						",
" 									",
" Optional Parameters:							",
" dtout=.004	desired time sampling interval (sec) in output		",
" ntr=1         number of traces to output 				",
" 									",
" Notes:								",
" The format of the input file(s) is C-style binary float. These files	",
" may be constructed from ascii file via:   				",
" 									",
"       a2b n1=3 < dvrfile.ascii > dvrfile.bin				",
" ...or...								",
"       a2b n1=2 < dvfile.ascii > dvfile.bin				",
"       a2b n1=2 < drfile.ascii > drfile.bin				",
" ...or...								",
"       a2b n1=1 < dfile.ascii > dfile.bin				",
"       a2b n1=1 < vfile.ascii > dfile.bin				",
"       a2b n1=1 < rhofile.ascii > rhofile.bin				",
" 									",
" A raw normal-incidence impedence reflectivity as a function of time is",
" is generated using the smallest two-way traveltime implied by the	",
" input velocities as the time sampling interval. This raw reflectivity	",
" trace is then resampled to the desired output time sampling interval	",
" via 8 point sinc interpolation. If the number of samples on the output",
" exceeds SU_NFLTS the output trace will be truncated to that value.	",
"  									",
" Caveat: 								",
" This program is really only a first rough attempt at creating a well	",
" log utility. User input and modifications are welcome.		",
" 									",
" See also:  suresamp 							",
" 									",
NULL};

/*
 * Author:  CWP: John Stockwell, Summer 2001, updated Summer 2002.
 * inspired by a project by GP grad student Leo Brown
 */
/**************** end self doc ********************************/


segy tr;	/* output trace of reflectivity spikes */

int
main(int argc, char **argv)
{

	int i=0;		/* counter				*/
	int ntr=1;		/* number of output traces		*/
	int nval;		/* number of values			*/
	int ntmax;		/* number of samples on maxtrace trace	*/
	int ntout;		/* number of samples on output trace	*/
	int ndvr;		/* number of samples in dvrfile		*/
	int dvrbytes;		/* size of dvrinput in bytes		*/
	int itempt=0;		/* integerized traveltime		*/

	float dtmin=FLT_MAX;	/* minimum delta t			*/
	float dtout=0.0;	/* time sampling interval on output	*/
	float tmax=0.0;		/* maximum traveltime on output		*/
	float ttotal=0.0;	/* maximum traveltime on output		*/
	float tempdt=0.0;	/* temporary dt				*/

	float *t=NULL;		/* array of output times for interp	*/
	float *tempd=NULL;	/* temporary data vector		*/
	float *tempv=NULL;	/* temporary data vector		*/
	float *tempr=NULL;	/* temporary data vector		*/
	float *maxtrace=NULL;	/* maxtrace data trace 			*/
	float **dvrinput=NULL;	/* array of depths,velocities,densities	*/

	/* Input file names */
	char *dvrfile="";	/* depth, velocity, density file name	*/
	char *dvfile="";	/* depth, velocity file name		*/
	char *drfile="";	/* depth, density file  name		*/
	char *dfile="";		/* depth file name			*/
	char *vfile="";		/* velocity file  name			*/
	char *rhofile="";	/* density file name			*/

	/* Input file pointers */
	FILE *dfp=NULL;		/* pointer to depth file		*/
	FILE *vfp=NULL;		/* pointer to velocity file		*/
	FILE *rfp=NULL;		/* pointer to density file		*/
	FILE *dvfp=NULL;	/* pointer to depth, velocity file	*/
	FILE *drfp=NULL;	/* pointer to depth, density file	*/
	FILE *dvrfp=NULL;	/* pointer to depth, velocity, density file  */	

	/* Hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);
	
	/* Get required parameters */
	MUSTGETPARINT("nval",&nval);
	getparstring("dfile",&dfile);
	getparstring("vfile",&vfile);
	getparstring("rhofile",&rhofile);
	getparstring("dvfile",&dvfile);
	getparstring("drfile",&drfile);
	getparstring("dvrfile",&dvrfile);

	if (!getparint("ntr",&ntr)) ntr=1;;

	/* Allocate space for d,v,r values */
	dvrinput = ealloc2float(3,nval);
	ndvr = 3*nval;
	dvrbytes = ndvr*sizeof(float);

	/* Zero out  d,v,r array */
	memset((void *) dvrinput[0],0,dvrbytes);
	
	/* Open input files and read values */
	if ( (*dvrfile!='\0')) {

		/* Open  file */
		if((dvrfp=efopen(dvrfile,"r"))==NULL)
                        err("cannot open dvrfile=%s",dvrfile); 

		/* Read d,v,r values */
		efread(dvrinput[0],sizeof(float),ndvr,dvrfp);
		
	} else if ( (*dvfile!='\0') && (*dvfile!='\0') ) {

		float **temp1=NULL;
		float **temp2=NULL;
		float nvtemp=2*nval;

		/* Allocate space for temporary arrays */
		temp1 = ealloc2float(2,nval);
		temp2 = ealloc2float(2,nval);
	
		/* Open files */
		if((dvfp=efopen(dvfile,"r"))==NULL)
                        err("cannot open dvfile=%s\n",dvfile); 

		if((drfp=efopen(drfile,"r"))==NULL)
                        err("cannot open drfile=%s\n",drfile); 

		/* Read d,v,r values */
		efread(temp1[0],sizeof(float),nvtemp,dvfp);
		efread(temp2[0],sizeof(float),nvtemp,drfp);

		/* Read values from temp1 and temp2 into dvrinput */
		for (i=0; i<nval; ++i) {
			dvrinput[i][0] = temp1[i][0];
			dvrinput[i][1] = temp1[i][1];
			dvrinput[i][2] = temp2[i][1];
		}
		free2float(temp1);
		free2float(temp2);
			
		
	} else if ( (*dfile!='\0') && (*vfile!='\0') && (*rhofile!='\0') ) {
		float *temp1=NULL;
		float *temp2=NULL;
		float *temp3=NULL;

		temp1 = ealloc1float(nval);
		temp2 = ealloc1float(nval);
		temp3 = ealloc1float(nval);

		/* open files */
		if((dfp=efopen(dfile,"r"))==NULL)
                        err("cannot open dfile=%s\n",dfile); 

		if((vfp=efopen(vfile,"r"))==NULL)
                        err("cannot open vfile=%s\n",vfile); 

		if((rfp=efopen(rhofile,"r"))==NULL)
                        err("cannot open rhofile=%s\n",rhofile); 

		/* Read d,v,r values */
		efread(temp1,sizeof(float),nval,dfp);
		efread(temp2,sizeof(float),nval,vfp);
		efread(temp3,sizeof(float),nval,rfp);

		/* Read values from temp1, temp2, and temp3 into dvrinput */
		for (i=0; i<nval; ++i) {
			dvrinput[i][0] = temp1[i];
			dvrinput[i][1] = temp2[i];
			dvrinput[i][2] = temp3[i];
		}
		free1float(temp1);
		free1float(temp2);
		free1float(temp3);
	}

	/* Store values in a temporary arrays */
	tempd=ealloc1float(nval);
	tempv=ealloc1float(nval);
	tempr=ealloc1float(nval);

	for (i=0; i<nval; ++i) {

		tempd[i] = dvrinput[i][0];
		tempv[i] = dvrinput[i][1];
		tempr[i] = dvrinput[i][2];
		
		/* compute min(dt) */
		if (i>0) {
			if ((tempd[i] - tempd[i-1]) < 0)
				err("depth not monotonically increasing!");

			tempdt = 2.0*(tempd[i]-tempd[i-1])/tempv[i-1];
			dtmin = MIN(dtmin,tempdt);
			tmax+=tempdt;
		}
	}

	/* Set output dt, ns */
	if (!getparfloat("dtout", &dtout))            dtout = dtmin/2.0;
	tr.dt = NINT(dtout*1000000.0);

        checkpars();

	/* maxtrace trace */
	ntmax = NINT(tmax/dtmin);
	maxtrace = ealloc1float(ntmax);
	memset((void *) maxtrace,0,ntmax*FSIZE);

	/* set number of samples on output trace */
	ntout = NINT(tmax/dtout);
	if (ntout>SU_NFLTS) {
		warn("ntout=%d too big! Setting ntout to SU_NFLTS=%d",ntout,SU_NFLTS);
		ntout=SU_NFLTS;
	}
	tr.ns = ntout;


	/* Convert depth values to traveltimes and reflectivities */
	for (i=1; i<nval; ++i) {

		/* Nearest neighbor */
		/* Compute integerized traveltime and set output data to reflectivity */
		ttotal += 2.0*(tempd[i]-tempd[i-1])/tempv[i-1];
		itempt=NINT(ntmax*ttotal/tmax);

		/* Compute impedence reflection coefficient */
		maxtrace[itempt] = ( tempv[i]*tempr[i] - tempv[i-1]*tempr[i-1])/
					( tempv[i]*tempr[i] + tempv[i-1]*tempr[i-1]);

	}

	/* Compute output times */
	t = ealloc1float(ntout);	
	memset((void *) t,0,ntout*FSIZE);

	{ register int itime;
 	  register float tvalue;
          for (itime=0,tvalue=0.0; itime<ntout; itime++,tvalue+=dtout)
                        t[itime] = tvalue;
         }

	/* Resample maxtrace via 8 point sinc interpolation to desired */
	/* output dt */
	ints8r(ntmax, dtmin, 0, maxtrace, 
			0.0, 0.0, ntout, t, tr.data);

	/* Free space */
	free1float(maxtrace);
	free1float(t);

	/* Output traces */
	for(i=0; i<ntr; ++i) puttr(&tr);

	return(CWP_Exit());
	
}

