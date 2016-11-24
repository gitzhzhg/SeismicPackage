/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* H2B: $Revision: 1.11 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" H2B - convert 8 bit hexidecimal floats to binary		",
" 								",
" h2b <stdin >stdout outpar=/dev/tty 				",
" 								",
" Required parameters:						",
" 	none							",
" 								",
" Optional parameters:						",
" 	outpar=/dev/tty output parameter file, contains the	",
"			number of lines (n=)			",
" 			other choices for outpar are: /dev/tty,	",
" 			/dev/stderr, or a name of a disk file	",
" 								",
" Note: this code may be used to recover binary data from PostScript",
" bitmaps. To do this, strip away all parts of the PSfile that	",
" are not the actual hexidecimal bitmap and run through h2b.	",
" 								",
" Note: that the binary file may need to be transposed using	",
" \"transp\" to appear to be the same as input data.		",
" 								",
" Note:	output will be floats with the values 0-255		",
NULL};
/**************** end self doc ***********************************/

/* Credits:
 *	CWP: Craig Artley , with small changes by John Stockwell  July 1993
 */

int
main(int argc, char **argv)
{
	float fz[BUFSIZ];
	int iz=0,jz,nz=0;
	unsigned int z[BUFSIZ];
	char line[BUFSIZ],*lp, *outpar;
	FILE *infp=stdin,*outfp=stdout, *outparfp;

	unsigned int *uz = &(z[0]);

	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* Prevent floats from dumping on screen */
	switch(filestat(STDOUT)) {
	case BADFILETYPE:
		warn("stdout is illegal filetype");
		pagedoc();
	break;
	case TTY:
		warn("stdout can't be tty");
		pagedoc();
	break;
	default: /* rest are OK */
	break;
	}


	/* Get parameters and do set up */
	if (!getparstring("outpar", &outpar))  outpar = "/dev/tty" ;
	outparfp = efopen(outpar, "w");
        checkpars();


	while (fgets(line,BUFSIZ,infp)!=NULL) {

		/* set pointer to beginning of line */
		lp = line;

		/* read hex digits from input line */
		for(iz=0;sscanf(lp,"%2x",&uz[iz])==1;iz++,nz++,lp+=2);

		/* convert to floats */
		for(jz=0;jz<iz;jz++)
			fz[jz] = 255-z[jz];

		/* write floats */
		fwrite(fz,sizeof(float),iz,outfp);
	}

	/* Make par file */
	fprintf(outparfp, "total number of values=%d\n",nz);

	return(CWP_Exit());
}
