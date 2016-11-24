/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* WPC2COMP2: $Revision: 1.5 $ ; $Date: 2011/11/21 16:22:41 $	*/

#include "wpc1.h"
#include "par.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"                                                                       ",
" WPC1COMP2 --- COMPress a 2D seismic section trace-by-trace using 	",
"		Wavelet Packets						",
"                                                                       ",
" wpc1comp2 < stdin n1= [optional parameters ] > stdout              	",
"                                                                       ",
" Required Parameters:                                                  ",
" n1=                    number of samples in each trace		",
"                                                                       ",
" Optional Parameters:                                                  ",
" error=0.01              relative RMS allowed in compress		", 
"                                                                       ",
" Notes:                                                                ",
"  This program is used to compress a 2D section using 1D method.	",
"  It is not a true 2D compression algorithm, since the later diretion	",
"  is not compressed at all. It can be used in situations where random	",
"  accessing of each trace is desirable, or when there are very few	",
"  traces and normal 2D compression algorithm will not compress.	",
"                                                                       ",
"  The parameter error is used control the allowable compression error,	",
"  and thus the compression ratio. The larger the error, the more 	",
"  the more compression you can get. The amount of error depends on 	",
"  the type of data and the application of the compression. From my 	",
"  experience, error=0.01 is a safe choice even when the compressed data 	",
"  are used for further processing. For some other applications, it 	",
"  may be set higher to achieve larger compression.			", 
"                                                                       ",
" Caveats:								",
"  For the current implementation, the compressed data themselves are	",
"  NOT portable, i.e., the data compressed on one platform might not be	",
"  recognizable on another.						",
"                                                                       ",
NULL};
/**************** end self doc *******************************************/

/*
 *       Credits: CWP: Tong Chen
 */


int
main(int argc, char **argv)
{
	float error, *f;
	int n1, nbytes;
	void *wpc1;

	initargs(argc, argv);
	requestdoc(1);

	getparint("n1", &n1);
	if(!getparfloat("error", &error)) error=.01;

        checkpars();
	f = alloc1float(n1);
	
	while(fread(f, sizeof(float), n1, stdin) == n1){
	    wpc1 = wpc1Compress(f, n1, error, &nbytes);
	    wpc1Write(wpc1, stdout);
	}

	return EXIT_SUCCESS;
}

