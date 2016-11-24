/* cshotplot - convert CSHOT data to plotdata and parfile for CWP graphers */

#include "par.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
"	 								",
" CSHOTPLOT - convert CSHOT data to files for CWP graphers		",
" 									",
" cshotplot <cshot1plot [optional parameter file]			",
" 									",
" Required parameters:							",
" 	none 								",
" 									",
" Optional parameter:							",
" 	outpar=/dev/tty		output parameter file, contains:	",
"					number of plots (n2=)		",
"					points in each plot (n1=)	",
"					colors for plots (linecolor=)	",
" 									",
NULL};
/**************** end self doc *******************************************/

/* Credits:
 *	CWP: Jack K. Cohen
 *
 *	Caveat: Should do dynamic allocation for n1, color arrays
 *		instead of arbitrarily defining BIGBUFSIZ below.
 *		But since xgraph, psgraph are hard-wired anyway,
 *		there was nothing to gain.
 */

#define BIGBUFSIZ	10240	/* max number of plots, see above Caveat */

int
main(int argc, char **argv)
{
	char *outpar;		/* name of file holding output parfile	*/
	FILE *outparfp;		/* ... its file pointer			*/
	int n1[BIGBUFSIZ];	/* array of points in each plot		*/
	int color[BIGBUFSIZ];	/* array of color for each plot		*/
	int n2 = 0;		/* number of plots		 	*/
	char buf[BUFSIZ];	/* buffer for the ascii data		*/


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
		warn("stdout can't be terminal");
		pagedoc();
        default: /* Others OK */
	break;
	}


	/* Get and open parameter file */
	if (!getparstring("outpar", &outpar))  outpar = "/dev/tty" ;
	outparfp = efopen(outpar, "w");
	
	
	/* Loop over CSHOT raydata file */
	while (fgets(buf,sizeof(buf),stdin)) { /* get n1cur */
		int n1cur;	/* current value of n1		*/
		int colorcur;   /* current color		*/
		float x[2];	/* z,x pairs defining rays	*/
		register int i1;
		
		if (2 != sscanf(buf, "%d %d", &n1cur, &colorcur))
			err("scan failed on header line #%d:\n%s",
				 n2, buf);
		color[n2] = colorcur;
		n1[n2++] = n1cur;

		/* get lines giving points */
		for (i1 = 0; i1 < n1cur; ++i1) {
			fgets(buf,sizeof(buf),stdin);
			 
			/* CSHOT puts (x,z), we want (z,x), switch in scanf */
			if (2 != sscanf(buf, "%f %f", x+1, x))
				err("group #%d: scan failed on line #%d:\n%s",
					n2+1, i1+1, buf);
			efwrite(x, FSIZE, 2, stdout);
		}
	}


	/* Make par file */
	fprintf(outparfp, "n2=%d\n", n2);
	fprintf(outparfp, "n1=%d", n1[0]);
	{ register int i2;
	  for (i2 = 1; i2 < n2; ++i2)  fprintf(outparfp, ",%d", n1[i2]);
	}
	fprintf(outparfp, "\n");
	
	fprintf(outparfp, "linecolor=%d", color[0]);
	{ register int i2;
	  for (i2 = 1; i2 < n2; ++i2)  fprintf(outparfp, ",%d", color[i2]);
	}
	fprintf(outparfp, "\n");
	
	
	/* Clean up */
	efclose(outparfp);
	
	return EXIT_SUCCESS;
}
