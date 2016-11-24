/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* FPUTGTHR: $Revision: 1.3 $; $Date: 2011/11/11 23:57:38 $	*/



/*********************** self documentation **********************/

/********************************************************************
FPUTGTHR - put gathers to a file

fput_gather - put a gather to a file
put_gather - put a gather to stdout

************************************************************************
Function Prototypes:
segy **fput_gather(FILE *fp, segy **rec,int *nt, int *ntr);
segy **put_gather(segy **rec,int *nt, int *ntr)

************************************************************************
fput_gather - put a gather to a file
************************************************************************
Input:
fp		pointer to output file
rec		array of segy traces
nt		number of time samples per trace
ntr		number of traces in ensemble
************************************************************************
Author: Potash Corporation Sascatchewan, Balasz Nemeth
given to CWP in 2008.

put_gather - put a gather to stdout
************************************************************************
Input:
rec		array of segy traces
nt		number of time samples per trace
ntr		number of traces in ensemble
************************************************************************
Author: Potash Corporation Sascatchewan, Balasz Nemeth
given to CWP in 2008.
********************************************************************/

#include "su.h"
#include "segy.h"
#include "header.h"


segy tr;

segy **fput_gather(FILE *fp, segy **rec,int *nt, int *ntr)
/***********************************************************************
fput_gather - put a gather to a file
************************************************************************
Input:
fp		pointer to output file
rec		array of segy traces
nt		number of time samples per trace
ntr		number of traces in ensemble
************************************************************************
Author: Potash Corporation Sascatchewan, Balasz Nemeth
given to CWP in 2008.
***********************************************************************/
{


	segy tr;
	
	{ register int i;
		for(i=0;i<*ntr;i++) {
			memcpy( (void *) &tr, (const void *) rec[i],
					*nt*FSIZE+HDRBYTES);
			fvputtr(fp,&tr);
			free1((void *)rec[i]);
		}
	}
	return(rec=NULL);
}


segy **put_gather(segy **rec,int *nt, int *ntr)
/***********************************************************************
put_gather - put a gather to stdout
************************************************************************
Input:
rec		array of segy traces
nt		number of time samples per trace
ntr		number of traces in ensemble
************************************************************************
Author: Potash Corporation Sascatchewan, Balasz Nemeth
given to CWP in 2008.
***********************************************************************/
{
	segy tr;
	
	{ register int i;
		for(i=0;i<*ntr;i++) {
			memcpy( (void *) &tr, (const void *) rec[i],
					*nt*FSIZE+HDRBYTES);
			vputtr(&tr);
			free1((void *)rec[i]);
		}
	}
	return(rec=NULL);
}
