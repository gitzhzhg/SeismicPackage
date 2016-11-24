/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* MKPARFILE: $Revision: 1.7 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" MKPARFILE - convert ascii to par file format 				",
" 									",
" mkparfile <stdin >stdout 						",
" 									",
" Optional parameters:							",
" 	string1=\"par1\"	first par string			",
" 	string2=\"par2\"	second par string			",
" 									",
" This is a tool to convert values written line by line to parameter 	",
" vectors in the form expected by getpar.  For example, if the input	",
" file looks like:							",
" 	t0 v0								",
" 	t1 v1								",
"	...								",
" then									",
"	mkparfile <input >output string1=tnmo string2=vnmo		",
" yields:								",
"	tnmo=t0,t1,...							",
"	vnmo=v0,v1,...							",
" 									",
NULL};
/**************** end self doc *******************************************/

/* Credits:
 *	CWP: Jack K. Cohen
 */


/* Caveat: A more general tool allowing n1 strings would be desirable. */

int
main(int argc, char **argv)
{
	int i2, n2 = 0;
	float x1, x2;
	char *string1;
	char *string2;
	char buf[BUFSIZ];
	FILE *datafp;


	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);


	/* Get parameters and set up tmpfile */
	if (!getparstring("string1", &string1))	string1 = "par1";
	if (!getparstring("string2", &string2))	string2 = "par2";
	datafp = etmpfile();


        checkpars();

	/* Extract x1's from data and save data for later pass over x2's */
	if (!fgets(buf,sizeof(buf),stdin))  err("no data found");
	if (2 == sscanf(buf, "%f %f", &x1, &x2)) { /* no comma first time */
		printf("%s=%g", string1, x1);
		efwrite(&x2, FSIZE, 1, datafp);
	} else  err("line #%d: scan failed:\n%s", n2+1, buf);
	++n2;

	while (fgets(buf,sizeof(buf),stdin)) {
		if (2 == sscanf(buf, "%f %f", &x1, &x2)) {
			printf(",%g", x1);
			efwrite(&x2, FSIZE, 1, datafp);
		} else  err("line #%d: scan failed:\n%s", n2+1, buf);
		++n2;
	}
	putchar('\n');

	/* Rewind and get the x2's */
	rewind(datafp);

	efread(&x2, FSIZE, 1, datafp);
	printf("%s=%g", string2, x2);

	for (i2 = 1; i2 < n2; ++i2) {
		efread(&x2, FSIZE, 1, datafp);
		printf(",%g", x2);
	}
	putchar('\n');

	return(CWP_Exit());
}
