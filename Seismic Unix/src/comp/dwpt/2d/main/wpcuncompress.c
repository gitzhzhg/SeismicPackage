/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* WPCUNCOMPRESS: $Revision: 1.4 $ ; $Date: 2011/11/21 16:25:02 $	*/

#include "par.h"
#include "wpc.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"                                                                       ",
" WPCUNCOMPRESS --- UNCOMPRESS a 2D section 				",
"                                                                       ",
" wpc1uncompress < stdin > stdout            				",
"                                                                       ",
" Required Parameters:                                                  ",
"                                                                       ",
" Optional Parameters:                                                  ",
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
 *       Credits: CWP: Tong Chen
 */

int
main(int argc, char **argv)
{
	float **f;
	int n1, n2;
	void *wpc;
	float time_used=0.0;

	initargs(argc, argv);
	requestdoc(1);

	wpc = wpcRead(stdin);

	n1 = wpcN1(wpc);
	n2 = wpcN2(wpc);

	if(n1< 0 || n2 < 0) 
	fprintf(stderr, "Inconsistent data\n");

	f = alloc2float(n1, n2);
	
/*
	time_used = wallsec();
*/

	if(!wpcUncompress(wpc, f[0])) 
	fprintf(stderr, "Inconsistent data\n");

/*
	time_used = wallsec() - time_used;
*/

	fwrite(f[0], sizeof(float), n1*n2, stdout);

	fprintf(stderr,"time used in uncompression is %f sec \n", time_used);

	return EXIT_SUCCESS;
}
