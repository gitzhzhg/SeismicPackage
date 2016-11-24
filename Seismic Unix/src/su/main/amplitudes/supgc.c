/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "su.h"
#include "segy.h" 

/*********************** self documentation **********************/
char *sdoc[] = { 
" SUPGC   -   Programmed Gain Control--apply agc like function	",
"              but the same function to all traces preserving	",
"              relative amplitudes spatially.			",
" Required parameter:						",
" file=             name of input file				",
"								",
" Optional parameters:						",
" ntrscan=200       number of traces to scan for gain function	",
" lwindow=1.0       length of time window in seconds		",
"								",
NULL};

/*
 * Author: John Anderson (visitor to CWP from Mobil)
 *
 * Trace header fields accessed: ns, dt
 */

/**************** end self doc ********************************/

segy tr;	/* Input and output trace data of length nt */
int main (int argc, char **argv)
{
	FILE *fp;
	char *file;
	int ntrscan;
	int icount,j,k,kk,nt,lw;
	float lwindow,dt,sum;
	float *gain,*g;

	/* hook up getpars */
	initargs(argc,argv);
	requestdoc(1);

	/* get parameters */
	if (!getparint("ntrscan",&ntrscan)) ntrscan = 200;
        if(!getparstring("file",&file)) file=NULL;
	if(!getparfloat("lwindow",&lwindow)) lwindow=1.0;

        checkpars();

	/* get info from first trace */
        if( (fp = fopen(file,"r"))==NULL) 
		err("could not open input file");
        if(!fgettr(fp,&tr)) err("Can't get first trace");
	nt=tr.ns;
	dt = ((double) tr.dt)/1000000.0;
	lw=NINT(0.5*lwindow/dt);
	warn("window length= %d samples",lw); 

	gain=ealloc1float(nt);
	g=ealloc1float(nt);

	for(j=0;j<nt;j++) gain[j]=0.;	
	icount=0;
	do{
		for(j=0;j<nt;j++)  gain[j]+=fabs(tr.data[j]);
		icount++;
		
	} while(fgettr(fp,&tr) && icount<ntrscan);

	rewind(fp);

	for(j=0;j<nt;j++) {
		sum=0.;
		kk=0;
		for(k=MAX(j-lw,0);k<MIN(j+lw,nt);k++){
			kk++;
			sum+=gain[k];
		}
		if(sum==0.) sum=1.;
		g[j]=kk*icount/sum;
	}
	
	warn("scan covered %d traces",icount);		

	if(!fgettr(fp,&tr))err("failure after rewind");
	do {
		for(j=0;j<nt;j++) tr.data[j]*=g[j];

		puttr(&tr);

	} while(fgettr(fp,&tr));


	fclose(fp);
	return(CWP_Exit());
}
