/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUMIXGATHER:  $Date: June 2000  */

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUMIXGATHERS - mix two gathers					",
" 									",
" sumixgathers file1 file2 > stdout [optional parameters]		",
" 									",
" Required Parameters:							",
" ntr=tr.ntr	if ntr header field is not set, then ntr is mandatory	",
"									",
" Optional Parameters: none						",
"									",
" Notes: Both files have to be sorted by offset				",
" Mixes two gathers keeping only the traces of the first file		",
" if the offset is the same. The purpose is to substitute only		",
" traces non existing in file1 by traces interpolated store in file2. 	", 
"									",
" Example. If file1 is original data file and file 2 is obtained by	",
" resampling with Radon transform, then the output contains original  	",
" traces with gaps filled						",
" 									",
"									",
 NULL};

/* Credits:
 *	Daniel Trad. UBC
 * Trace header fields accessed: ns, dt, ntr
 * Copyright (c) University of British Columbia, 1999.
 * All rights reserved.
 */

/**************** end self doc ***********************************/

segy tr,tr2; 
int main(int argc, char **argv)
{
	int j,ih;
	register int it;
	int nt;
	int ntr;
	int nh;
	int flag;
	float *h;
	float scale=1;
	int scaling;

	FILE *fp1=NULL;		/* file pointer for first file		*/
	FILE *fp2=NULL;		/* file pointer for second file		*/
	FILE *tracefp=NULL;	/* fp for trace storage file		*/
	FILE *headerfp=NULL;	/* fp for header storage file		*/

	/* Initialize */

	initargs(argc, argv);
	requestdoc(1);

	/* Open two files given as arguments for reading */
	fp1 = efopen(argv[1], "r");
	fp2 = efopen(argv[2], "r");  
	tracefp = etmpfile();
	headerfp = etmpfile();
	if (!getparint("scaling",&scaling)) scaling=0;  
	if (!fgettr(fp1,&tr)) err("can't read first trace");

	nt = (int) tr.ns;  
	if (!getparint("ntr",&ntr)) ntr=tr.ntr;  
        checkpars();
	if (!ntr)
		err("ntr neither set in header nor getparred!");
	h=ealloc1float(ntr);

	j=0;
	do{	/* Loop over traces */	
		efwrite(&tr,HDRBYTES,1,headerfp);
		efwrite(tr.data,FSIZE, nt, tracefp);  		
		h[j]=tr.offset;
		j++;
	} while(fgettr(fp1, &tr));

	erewind(tracefp);
	erewind(headerfp);
	nh=j;

	warn("nh=%d",nh);

	scale=1;

	while(fgettr(fp2, &tr2)){
		flag=1;
		for (ih=0;ih<nh;ih++){
			if (h[ih]>=0) 
			if ((tr2.offset>0.999*h[ih])&&(tr2.offset<1.001*h[ih])) flag=0;
			if (h[ih]<0) 
			if ((tr2.offset<0.99*h[ih])&&(tr2.offset>1.01*h[ih])) flag=0;
		}
		if (flag==1){
			if (scaling && fabs(tr2.offset) > 0){
		  	  scale=(1+0.03*(fabs(tr2.offset)/1000.));
			  fprintf(stderr,"tr2.offset=%d,scale=%f\n",tr2.offset,scale);
			  for (it=0;it<nt;it++) tr2.data[it]*=scale;
			}
		puttr(&tr2);
		}
	}
	
	for (ih=0;ih<nh;ih++){
		efread(&tr,HDRBYTES,1,headerfp);
		efread(tr.data,FSIZE, nt, tracefp);
		puttr(&tr);
	}
	

	efclose(fp1);
	efclose(fp2);
	efclose(tracefp);
	efclose(headerfp);
	free1float(h);

	return(CWP_Exit());
}
