/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUVCAT: $Revision: 1.18 $ ; $Date: 2011/11/16 23:09:52 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUVCAT -  append one data set to another, with or without an  ", 
"           overlapping	region.  Data in the overlap may be     ",
"           determined by one of several methods.               ",
" 								",
" suvcat data1 data2 >stdout					",
" 								",
" Required parameters:						",
"        none							",
"                                                               ",
" Optional parameters for overlapping sections:			",
"                                                               ",
"  taplen=0    Length of overlap in integer number of           ",
"                  samples.(Default is 0.)                      ",
"                                                               ",
"  taptype=0    Type of taper or combination method in the	",
"                  overlap region.  0 - average                 ",
"                                   1 - maximum magnitude       ",
"                                   2 - cosine scaled           ",
"                                   3 - summation               ",
"                                                               ",
" Computational Notes:						",
"                                                               ", 
" This program vertically concatenates traces from data2 onto   ",
" the end of the corresponding traces in data1, with a region   ",
" of overlap, defined by taplen.  Data in the overlapping       ", 
" region is combined by the method specified by taptype. The    ",
" currently available methods are:                              ",
"                                                               ",
"     taptype=0    output is assigned the unweighted average of ",
"                  each point in the overlap                    ",
"     taptype=1    output is assigned the value of the maximum  ",
"                  absolute value of each point in the overlap  ",
"     taptype=2    output is assigned the weighted average of   ",
"                  each point in the overlap, where the output  ",
"                  is the sum of cos(x) times the values on the ",
"                  first section, and 1-cos(x) times the values ",
"                  on the second section, where x is factor that",
"                  goes from 0 to pi/2 across the overlap. This ",
"                  favors the upper section in the upper part of",
"                  the overlap, and favors the lower section in ",
"                  the lower part of the overlap.               ",
"     taptype=3    output is assigned the sum of the amplitudes ",
"                  at each sample in the overlap                ",
" 								",
NULL};

/* Credits:
 *	CWP: Jack K. Cohen, Michel Dietrich (Original SUVCAT)
 *	     Steven D. Sheaffer (modifed to include overlap) 
 * IfG Kiel: Thies Beilecke (added taptype=3)
 *
 * Trace header fields accessed:  ns
 * Trace header fields modified:  ns
 */
/**************** end self doc ***********************************/

segy intrace1, intrace2;

int
main(int argc, char **argv)
{
	FILE *fp1;	/* file pointer for first file		*/
	FILE *fp2;	/* file pointer for second file		*/
	int data2flag=0;/* return from gettr on data set #2	*/
	int itr = 0;	/* number of trace being processed	*/
	int taplen;     /* length of overlap in samples         */
	int taptype;    /* flag for type of averaging           */
	float hold;     /* temporary variable                   */
	float *outtrace;/* temporary space for output trace     */
	int i;          /* counter                              */
        float s1,s2;    /* scale factors                        */

	/* Initialize */
	initargs(argc, argv);
	requestdoc(2); /* two file args required */

	/* Get parameters  */
	if(!getparint("taplen", &taplen))   taplen=0 ;
	if(!getparint("taptype", &taptype))   taptype=0 ;

        checkpars();
	if(taptype > 3) err("taptype must be 0, 1, 2, or 3!\n");

	/* Open two files given as arguments for reading */
	fp1 = efopen(argv[1], "r");
	fp2 = efopen(argv[2], "r");

	/* Loop over the traces */
	while (fgettr(fp1, &intrace1) &&
				(data2flag = fgettr(fp2, &intrace2))) {
	        int nt1 = intrace1.ns;
		int nt2 = intrace2.ns;
		int nt  = nt1+nt2-taplen;
		outtrace = alloc1float(nt);

		if (nt > SU_NFLTS)
			err("nt=%d exceeds SU_NFLTS=%d", nt, SU_NFLTS);

	/* Copy data from 1 to output trace up to the start of overlap */

		for(i=0; i<nt1-taplen; i++){
		    outtrace[i] = intrace1.data[i];
		} 

	/* Combine data in the overlapping region by the average */

	        if(taptype==0){

  		for(i=0; i<taplen; i++){                                   
  		    hold=intrace1.data[nt1-taplen+i]+intrace2.data[i];     
  		    outtrace[nt1-taplen+i]=hold*0.5;                       
  		}                                                          
		}

	/* Combine data in the overlapping region by using the maximum */

		if(taptype==1){

	  	for(i=0; i<taplen; i++){
	    	  if(abs(intrace1.data[nt1-taplen+i])>
					abs(intrace2.data[i])){
			hold = intrace1.data[nt1-taplen+i];
	    	  }else{
		        hold = intrace2.data[i];
	    	} 
	    	outtrace[nt1-taplen+i] = hold; 
          	}
		}

	/* Combine data in overlap using a cosine scale */

		if(taptype==2){

                for(i=0; i<taplen; i++){                                   
		  s1 = cos((PI/2)*(i/(taplen-1)));
		  s2 = 1-s1;
                  hold = s1*intrace1.data[nt1-taplen+i] + s2*intrace2.data[i];     
                  outtrace[nt1-taplen+i]=hold;                       
                }                                                          
                }

	/* Combine data in overlap using a simple summation */

		if(taptype==3){

                for(i=0; i<taplen; i++){
                  hold = intrace1.data[nt1-taplen+i] + intrace2.data[i];     
                  outtrace[nt1-taplen+i]=hold;                       
                }                                                          
                }

	/* Copy data from 2 to fill up remaining output trace  */

		for(i=0; i<nt2-taplen; i++){
		    outtrace[nt1+i]=intrace2.data[i+taplen];
		};

	/* modify header and put outtrace into old data1  */

		intrace1.ns = nt;

		for(i=0; i<nt; i++){
		  intrace1.data[i] = outtrace[i];
		};
		puttr(&intrace1); 
		++itr;
	}

	/* See if both files exhausted; notice if fd1 exhausted, then
	   we don't do an fgettr on fd2 on the final pass above */
	if (!data2flag) {
		warn("%s still had traces when %s was exhausted",
						argv[1], argv[2]);
		warn("processed %d pairs of traces before EOF", itr);
	} else if (fgettr(fp2, &intrace2)) {
		warn("%s still had traces when %s was exhausted",
						argv[2], argv[1]);
		warn("processed %d pairs of traces before EOF", itr);
	}


	return(CWP_Exit());
}
