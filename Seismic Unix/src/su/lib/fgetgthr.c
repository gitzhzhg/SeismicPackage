/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* FGETGTHR: $Revision: 1.3 $; $Date: 2011/11/11 23:57:38 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"


/*********************** self documentation **********************/
/********************************************************************
FGETGTHR - get gathers from SU datafiles

fget_gather - get a gather from a file
get_gather - get a gather from stdin


******************************************************************
Function Prototypes:
segy **fget_gather(FILE *fp, cwp_String *key,cwp_String *type,Value *n_val,
			int *nt,int *ntr, float *dt,int *first);
segy **get_gather(cwp_String *key,cwp_String *type,Value *n_val,
			int *nt,int *ntr, float *dt,int *first)

******************************************************************
fget_gather - get a gather from a file
************************************************************************
Input:
fp		file pointer of input file
key		header key of ensemble
type		header value type
value		value of ensemble key word
nt		number of time samples
ntr		number of traces in ensemble
dt		time sampling interval
first		flag is this the first ensemble being read?
************************************************************************
Notes: 
The input seismic dataset must be sorted into ensembles defined by key
************************************************************************
Author: Potash Corporation Sascatchewan, Balasz Nemeth
given to CWP in 2008.

******************************************************************
get_gather - get a gather from stdin
************************************************************************
Input:
key		header key of ensemble
type		header value type
value		value of ensemble key word
nt		number of time samples
ntr		number of traces in ensemble
dt		time sampling interval
first		flag is this the first ensemble being read?
************************************************************************
Notes: 
The input seismic dataset must be sorted into ensembles defined by key
************************************************************************
Author: Potash Corporation Sascatchewan, Balasz Nemeth
given to CWP in 2008.
***********************************************************************/

segy tr;

segy **fget_gather(FILE *fp, cwp_String *key,cwp_String *type,Value *n_val,
			int *nt,int *ntr, float *dt,int *first)
/***********************************************************************
fget_gather - get a gather from a file
************************************************************************
Input:
fp		file pointer of input file
key		header key of ensemble
type		header value type
value		value of ensemble key word
nt		number of time samples
ntr		number of traces in ensemble
dt		time sampling interval
first		flag is this the first ensemble being read?
************************************************************************
Notes: 
The input seismic dataset must be sorted into ensembles defined by key
************************************************************************
Author: Potash Corporation Sascatchewan, Balasz Nemeth
given to CWP in 2008.
***********************************************************************/
{
	int nsegy;
	FILE *tracefp=NULL;
	FILE *headerfp=NULL;
	static FILE *ntracefp=NULL; /* first different trace pointer */
	static FILE *nheaderfp=NULL;
	segy **rec=NULL;
	int ntrr=0;
	int indx=0;
	static Value val;
	
	
	*type = hdtype(*key);
	indx = getindex(*key);
        *ntr = 0;
        	
	
	if(*first==0) {
	       	/* get info from first trace */
		nsegy = fvgettr(fp,&tr);
		if (nsegy==0)  err("can't get first trace");
		*nt = tr.ns;
        	*dt = (float) tr.dt/1000000.0;
        	++ntrr;
		gethval(&tr, indx, n_val);
		ntracefp = etmpfile();
		nheaderfp = etmpfile();
		*first=1;
	} else {
		/* This is the first trace of the nex gather */
		erewind(nheaderfp);
        	erewind(ntracefp);
                fread (&tr,HDRBYTES, 1, nheaderfp);
                fread (tr.data,FSIZE, *nt, ntracefp);
		gethval(&tr, indx, n_val);
	}
		
	

        /* Store traces in tmpfile while getting a count */
	tracefp = etmpfile();
	headerfp = etmpfile();
        do {
                efwrite(&tr, 1, HDRBYTES, headerfp);
                efwrite(tr.data, FSIZE, *nt, tracefp);
		
		/* read the next trace */
		*ntr+=1;
		val=*n_val;
		nsegy = fvgettr(fp,&tr);
		if(nsegy) ntrr++;
		gethval(&tr, indx, n_val);
	} while (nsegy && !valcmp(*type,val,*n_val));

	
	/* If there are no more traces then return */
	if(nsegy==0 && ntrr==0 ) {
		 *ntr=0;
		 efclose(nheaderfp);
		 efclose(ntracefp);
		 return(rec=NULL);
	} else {
		/* store the first trace of the next gather */
		erewind(nheaderfp);
        	erewind(ntracefp);
                efwrite(&tr, 1, HDRBYTES, nheaderfp);
                efwrite(tr.data, FSIZE, *nt, ntracefp);
	}

	/* allocate memory for the record */
	{ register int i;
		rec = ealloc1(*ntr,sizeof(segy *));
		for(i=0;i<*ntr;i++)
			rec[i] = (segy *)ealloc1((*nt*FSIZE+HDRBYTES),sizeof(char));
	}
	
	
	/* load traces into an array and close temp file */
	erewind(headerfp);
        erewind(tracefp);
	{ register int ix;
        	for (ix=0; ix<*ntr; ix++)
                	fread (rec[ix],HDRBYTES, 1, headerfp);
        	efclose (headerfp);
		for(ix=0; ix<*ntr; ix++)
                	fread ((*rec[ix]).data,FSIZE, *nt, tracefp);
        	efclose (tracefp);
	}
	
	return(rec);	
}

segy tr;

segy **get_gather(cwp_String *key,cwp_String *type,Value *n_val,
			int *nt,int *ntr, float *dt,int *first)
/***********************************************************************
get_gather - get a gather from stdin
************************************************************************
Input:
key		header key of ensemble
type		header value type
value		value of ensemble key word
nt		number of time samples
ntr		number of traces in ensemble
dt		time sampling interval
first		flag is this the first ensemble being read?
************************************************************************
Notes: 
The input seismic dataset must be sorted into ensembles defined by key
************************************************************************
Author: Potash Corporation Sascatchewan, Balasz Nemeth
given to CWP in 2008.
***********************************************************************/
{
	int nsegy;
	FILE *tracefp=NULL;
	FILE *headerfp=NULL;
	static FILE *ntracefp=NULL; /* first different trace pointer */
	static FILE *nheaderfp=NULL;
	segy **rec=NULL;
	int ntrr=0;
	int indx=0;
	static Value val;
	
	
	*type = hdtype(*key);
	indx = getindex(*key);
        *ntr = 0;
        	
	
	if(*first==0) {
	       	/* get info from first trace */
		nsegy = vgettr(&tr);
		if (nsegy==0)  err("can't get first trace");
		*nt = tr.ns;
        	*dt = (float) tr.dt/1000000.0;
        	++ntrr;
		gethval(&tr, indx, n_val);
		ntracefp = etmpfile();
		nheaderfp = etmpfile();
		*first=1;
	} else {
		/* This is the first trace of the nex gather */
		erewind(nheaderfp);
        	erewind(ntracefp);
                fread (&tr,HDRBYTES, 1, nheaderfp);
                fread (tr.data,FSIZE, *nt, ntracefp);
		gethval(&tr, indx, n_val);
	}
		
	

        /* Store traces in tmpfile while getting a count */
	tracefp = etmpfile();
	headerfp = etmpfile();
        do {
                efwrite(&tr, 1, HDRBYTES, headerfp);
                efwrite(tr.data, FSIZE, *nt, tracefp);
		
		/* read the next trace */
		*ntr+=1;
		val=*n_val;
		nsegy = vgettr(&tr);
		if(nsegy) ntrr++;
		gethval(&tr, indx, n_val);
	} while (nsegy && !valcmp(*type,val,*n_val));

	
	/* If there are no more traces then return */
	if(nsegy==0 && ntrr==0 ) {
		 *ntr=0;
		 efclose(nheaderfp);
		 efclose(ntracefp);
		 return(rec=NULL);
	} else {
		/* store the first trace of the next gather */
		erewind(nheaderfp);
        	erewind(ntracefp);
                efwrite(&tr, 1, HDRBYTES, nheaderfp);
                efwrite(tr.data, FSIZE, *nt, ntracefp);
	}

	/* allocate memory for the record */
	{ register int i;
		rec = ealloc1(*ntr,sizeof(segy *));
		for(i=0;i<*ntr;i++)
			rec[i] = (segy *)ealloc1((*nt*FSIZE+HDRBYTES),sizeof(char));
	}
	
	
	/* load traces into an array and close temp file */
	erewind(headerfp);
        erewind(tracefp);
	{ register int ix;
        	for (ix=0; ix<*ntr; ix++)
                	fread (rec[ix],HDRBYTES, 1, headerfp);
        	efclose (headerfp);
		for(ix=0; ix<*ntr; ix++)
                	fread ((*rec[ix]).data,FSIZE, *nt, tracefp);
        	efclose (tracefp);
	}
	
	return(rec);	
}
