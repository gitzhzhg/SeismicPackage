/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* WPC1UNCOMP2: $Revision: 1.5 $ ; $Date: 2011/11/21 16:21:37 $	*/

#include "wpc1.h"
#include "par.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"                                                                       ",
" WPC1UNCOMP2 --- UNCOMPRESS a 2D seismic section, which has been	",
"  			compressed using Wavelet Packets		",
"                                                                       ",
" wpc1uncomp2 < stdin > stdout               				",
"                                                                       ",
" Required Parameters:                                                  ",
" none                                                                  ",
"                                                                       ",
" Optional Parameters:                                                  ",
" none                                                                  ",
"                                                                       ",
" Notes:                                                                ",
"  No parameter is required for this program. All the information for	",
"  uncompression has been encoded in the header of the compressed data.	",
"									",
" Caveats:								",
"  For the current implementation, the compressed data themselves are	",
"  NOT portable, i.e., the data compressed on one platform might not be	",
"  recognizable on another.						",
"                                                                       ",
NULL};
/**************** end self doc *******************************************/

/*
 *       Credits: Tong Chen
 */


int
main(int argc, char **argv)
{
	float *f;
	int n1;
	void *wpc1;
	int i;

	initargs(argc,argv);
	requestdoc(1);

	wpc1 = wpc1Read(stdin);

	n1 = wpc1Numsmp(wpc1);

	if(n1< 0) fprintf(stderr, "Inconsistent data, n=%d\n", n1);

	f = alloc1float(n1);
	
	i = 0;
	while(i >= 0 /*TRUE*/){
	    if(!wpc1Uncompress(wpc1, f)){
	    	fprintf(stderr, "Inconsistent data, trace # %d\n", i);
		break;
	    }

	    i++;
	    fwrite(f, sizeof(float), n1, stdout);
	    if((wpc1 = wpc1Read(stdin)) == NULL) break; 
	}
	return EXIT_SUCCESS;
}
