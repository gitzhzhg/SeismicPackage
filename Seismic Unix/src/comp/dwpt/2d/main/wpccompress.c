/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* WPCCOMPRESS: $Revision: 1.5 $ ; $Date: 2011/11/21 16:25:37 $	*/

#include "par.h"
#include "wpc.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"                                                                       ",
" WPCCOMPRESS --- COMPRESS a 2D section using Wavelet Packets		",
"                                                                       ",
" wpccompress < stdin n1= n2= [optional parameters ] > stdout          	",
"                                                                       ",
" Required Parameters:                                                  ",
" n1=                    number of samples in the 1st dimension		",
" n2=                    number of samples in the 2nd dimenstion	",
"                                                                       ",
" Optional Parameters:                                                  ",
" error=0.01              relative RMS allowed in compress		", 
"                                                                       ",
" Notes:                                                                ",
"  This program is used to compress a 2D section. It compresses in both	",
"  directions, vertically and horizontally.				",
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
	float error, **f;
	int n1, n2, nbytes;
	void *wpc;
	float time_used=0.0;

	/* hook up getpars */
	initargs(argc, argv);
	requestdoc(0);

	if(!getparint("n1", &n1)) err("Must specify n1");
	if(!getparint("n2", &n2)) err("Must specify n2");

	if(!getparfloat("error", &error)) error=.01;

        checkpars();
	f = alloc2float(n1, n2);
	
	efread(f[0], n1*n2, FSIZE, stdin);

	wpc = wpcCompress(f[0], n1, n2, error, &nbytes);

	wpcWrite(wpc, stdout);

	fprintf(stderr,"# of bytes after compression = %d\n", nbytes);

	fprintf(stderr, "time used in compression is %f sec\n", time_used);

	return EXIT_SUCCESS;
}
