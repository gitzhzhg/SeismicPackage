/* Copyright (c) Colorado School of Mines, 2013.*/
/* All rights reserved.											 */

/* SEGYSCAN: $Revision: 1.3 $ ; $Date: 2015/08/07 21:59:39 $	 */


#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <float.h>

#include "su.h"
#include "segy.h"


/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SEGYSCAN -- SCANs SEGY file trace headers for min-max in  several	",
"     possible formats.							",
"									",
"   segyscan < segyfile							",
"									",
" Notes:								",
" The SEGY file trace headers are scanned assuming short, ushort, int,  ",
" uint, float, and double and the results are printed as tables.	",
" 									",
"									",
NULL};

/*********************************************************************
 Credits: Stew Levin, June 2013 

*********************************************************************/
/**************** end self doc ********************************/

int main(int argc, char **argv)
{
	int i,j;		/* counters */
	int trlen;	
	ssize_t nread;

	char *inhdr;
	short *s2hd;
	short s2min[120];
	short s2max[120];

	unsigned short *u2hd;
	unsigned short u2min[120];
	unsigned short u2max[120];

	int *s4hd;
	int s4min[60];
	int s4max[60];

	unsigned int *u4hd;
	unsigned int u4min[60];
	unsigned int u4max[60];

	float *f4hd;
	float f4min[60];
	float f4max[60];

	double *f8hd;
	double f8min[30];
	double f8max[30];

	/* auto endian detection */
	union {
		int i;
		short s[2];
	} echeck;

#define IMLITTLEENDIAN echeck.s != 0

/*
	if(isatty(fileno(stdin))) {
		fprintf(stderr,"syntax: %s < segyfile\n", argv[0]);
		return (EXIT_FAILURE);
	}

*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);




	echeck.i = 1;

	inhdr = (char *) malloc(400);
	if(inhdr == ((char *) NULL)) {
		perror(argv[0]);
		fprintf(stderr,"Unable to allocate trace header array\n");
		return (EXIT_FAILURE);
	}
	s2hd = (short *) inhdr;
	u2hd = (unsigned short *) inhdr;
	s4hd = (int *) inhdr;
	u4hd = (unsigned int *) inhdr;
	f4hd = (float *) inhdr;
	f8hd = (double *) inhdr;

	for( i=0; i<120; ++i) {
		s2min[i] = SHRT_MAX;
		s2max[i] = SHRT_MIN; 
		u2min[i] = USHRT_MAX;
		u2max[i] = 0;
	}
	
	for(i=0; i<60; ++i) {
		s4min[i] = INT_MAX;
		s4max[i] = INT_MIN;
		u4min[i] = UINT_MAX;
		u4max[i] = 0;
		f4min[i] = FLT_MAX;
		f4max[i] = -FLT_MAX;
	}

	for(i=0; i<30; ++i) {
		f8min[i] = DBL_MAX;
		f8max[i] = -DBL_MAX;
	}

	/* done initializing */

	/* read and toss card image */ 
	for(i=0; i<3200; ++i) (void) getc(stdin);

	/* extract trace length from binary reel header */ 
	nread = fread(inhdr,1,400,stdin);
	swab(u2hd,u2hd,26); /* convert to little endian */

	trlen = u2hd[10];
	switch (u2hd[12]) {

	case 1:
	case 2:
	case 4:
	case 5:
			trlen *= 4;
			break;
	case 3:
			trlen *= 2;
			break;
	}

	/* now process headers, toss trace data */
	do {
		 nread = fread(inhdr,1,240,stdin);
		 for(i=0; i<trlen; ++i) (void) getc(stdin);
		 if(nread < 240) break;
		 if(IMLITTLEENDIAN)swab(inhdr, inhdr, 240);
	 
		 for(i=0; i<120; ++i) {
				if(s2min[i] > s2hd[i]) s2min[i] = s2hd[i];
				if(s2max[i] < s2hd[i]) s2max[i] = s2hd[i];
				if(u2min[i] > u2hd[i]) u2min[i] = u2hd[i];
				if(u2max[i] < u2hd[i]) u2max[i] = u2hd[i];
		 }

		 if(IMLITTLEENDIAN)
		 for(i=0; i<120; i += 2){
				j = s2hd[i]; 
				s2hd[i] = s2hd[i+1];
				s2hd[i+1] = j;
		 }

		 for(i=0; i<60; ++i) {
				if(s4min[i] > s4hd[i]) s4min[i] = s4hd[i];
				if(s4max[i] < s4hd[i]) s4max[i] = s4hd[i];
				if(u4min[i] > u4hd[i]) u4min[i] = u4hd[i];
				if(u4max[i] < u4hd[i]) u4max[i] = u4hd[i];
				if(f4min[i] > f4hd[i]) f4min[i] = f4hd[i];
				if(f4max[i] < f4hd[i]) f4max[i] = f4hd[i];
		 }

		 if(IMLITTLEENDIAN)
		 for(i=0; i<60; i += 2 ) {
				j = s4hd[i];
				s4hd[i] = s4hd[i+1];
				s4hd[i+1] = j;
		 }
	 
		 for(i = 0; i<30; ++i) {
				if(f8min[i] > f8hd[i]) f8min[i] = f8hd[i];
				if(f8max[i] < f8hd[i]) f8max[i] = f8hd[i];
		 }

	} while(nread > 0);

	/* now print out ranges that have been found in the trace headers */
	fprintf(stdout, "\nbyte#|     short      |     ushort     \n");
	fprintf(stdout,   "---------------------------------------\n");
	for(i=0; i<120; ++i) {
		fprintf(stdout,"%4d | %6hd - %6hd | %6hu - %6hu\n", 1+i*2, s2min[i], s2max[i], u2min[i], u2max[i]);
	}

	fprintf(stdout, "\nbyte#|           int            |           uint           |         float    \n");
	fprintf(stdout,   "-------------------------------------------------------------------------------------\n");

	for(i=0; i<60; ++i) {
		fprintf(stdout,"%4d | %11d - %11d | %11u - %11u | %g - %g\n", 1+i*4, s4min[i], s4max[i], u4min[i], u4max[i],f4min[i], f4max[i]);
	}

	fprintf(stdout, "\nbyte#|              double \n");
	fprintf(stdout,   "------------------------------------------\n");

	for(i=0; i<30; ++i) {
		fprintf(stdout,"%4d | %g - %g\n", 1+i*8, f8min[i], f8max[i]);
	}

	return(CWP_Exit());

}
