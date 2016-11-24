/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* FX domain filter corelation coefficient filter*/


#include "su.h"
#include "segy.h"


#define PFA_MAX 720720  /* Largest allowed nfft           */
#define PIP2 1.570796327
#define AMPSP(c) rcabs(c)
#define PHSSP(c) atan2(c.i,c.r)


/*********************** self documentation **********************/
char *sdoc[] = {
" SUCCFILT -  FX domain Correlation Coefficient FILTER			",
"                       						",
"   sucff < stdin > stdout [optional parameters]			",
"                       						",
" Optional parameters:							",
" cch=1.0		Correlation coefficient high pass value		",
" ccl=0.3		Correlation coefficient low pass value		",
" key=ep		ensemble identifier				",
" padd=25		FFT padding in percentage			",
"                       						",
" Notes:                       						",
" This program uses \"get_gather\" and \"put_gather\" so requires that	",
" the  data be sorted into ensembles designated by \"key\", with the ntr",
" field set to the number of traces in each respective ensemble.  	",
"                       						",
" Example:                     						",
" susort ep offset < data.su > datasorted.su				",
" suputgthr dir=Data verbose=1 < datasorted.su				",
" sugetgthr dir=Data verbose=1 > dataupdated.su				",
" succfilt  < dataupdated.su > ccfiltdata.su				",
"                       						",
" (Work in progress, editing required)                 			",
NULL};

/* 
 * Credits:
 *  Potash Corporation: Balazs Nemeth, Saskatoon Canada. c. 2008
 */


/**************** end self doc ********************************/

segy tr;
segy tr2;

int main( int argc, char *argv[] )
{
	cwp_String key;		/* header key word from segy.h		*/
	cwp_String type;	/* ... its type				*/
	Value val;

	segy **rec_o=NULL;	/* trace header+data matrix */

	int first=0;		/* true when we passed the first gather */
	int ng=0;		/* counter of gathers */
	float dt;
	int nt;
	int ntr;
	
	int nfft=0;		/* lenghth of padded array */
	float snfft;		/* scale factor for inverse fft */
	int nf=0;		/* number of frequencies */
	float *rt=NULL;		/* real trace */
	complex **fd=NULL;	/* frequency domain data */
	float **cc=NULL;	/* correlation coefficinet matrix */
	
	float padd;
	float cch;
	float ccl;

	int verbose=0;
		
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);
	
	if (!getparstring("key", &key))	key = "ep";
	if (!getparfloat("padd", &padd)) padd = 25.0;
	padd = 1.0+padd/100.0;
	
	if (!getparfloat("cch", &cch)) cch = 1.0;
	if (!getparfloat("ccl", &ccl)) ccl = 0.3;
	if (!getparint("verbose",&verbose)) verbose = 0;
	
	/* get the first record */
	rec_o = get_gather(&key,&type,&val,&nt,&ntr,&dt,&first);
	if(ntr==0) err("Can't get first record\n");
	
	/* set up the fft */
	nfft = npfar(nt*padd);
        if (nfft >= SU_NFLTS || nfft >= PFA_MAX)
               	err("Padded nt=%d--too big", nfft);
        nf = nfft/2 + 1;
        snfft=1.0/nfft;
	
	rt = ealloc1float(nfft);
	
	do {
		ng++;

		fd = ealloc2complex(nf,ntr); 
       		cc = ealloc2float(nf,ntr);

		/* transform the data into FX domain */
		{ unsigned int itr;
			for(itr=0;itr<ntr;itr++) {
				memcpy( (void *) rt, (const void *) (*rec_o[itr]).data,nt*FSIZE);
                		memset( (void *) &rt[nt],0, (nfft - nt)*FSIZE);
				
				pfarc(1, nfft, rt, fd[itr]);
			}
		}
		
		/* Compute correlation coefficients */
		{ unsigned int itr,ifr;
			for(itr=0;itr<ntr-1;itr++) {
				for(ifr=0;ifr<nf-1;ifr++) { 
					cc[itr][ifr] = cos(PHSSP(fd[itr][ifr])-PHSSP(fd[itr+1][ifr])); 
				}			
			}
		
		}
		
		/* Filter */
		{ unsigned int itr,ifr;
			for(itr=0;itr<ntr-1;itr++) {
				for(ifr=0;ifr<nf-1;ifr++) { 
					if(cc[itr][ifr]> cch || cc[itr][ifr]<ccl) {
						fd[itr][ifr].r = 0.0; 
						fd[itr][ifr].i = 0.0;
					} 
				}			
			}
		
		}
		
		{ unsigned int itr,it;
			for(itr=0;itr<ntr;itr++) {
				
				pfacr(-1, nfft, fd[itr], rt);
				
				for(it=0;it<nt;it++) 		
                			(*rec_o[itr]).data[it]=rt[it]*snfft;
			}
		}
			
		free2complex(fd);
		free2float(cc);

	    	rec_o = put_gather(rec_o,&nt,&ntr);
	    	rec_o = get_gather(&key,&type,&val,&nt,&ntr,&dt,&first);
		
		if (verbose) warn(" %d %d\n",ng,ntr);
		
	} while(ntr);
	
	free1float(rt);

	warn("Number of gathers %10d\n",ng);
	 
	return EXIT_SUCCESS;
}

