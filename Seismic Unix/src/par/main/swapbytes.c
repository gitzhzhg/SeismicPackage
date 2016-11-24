/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SWAPBYTES: $Revision: 1.12 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SWAPBYTES - SWAP the BYTES of various  data types			",
" 									",
" swapbytes <stdin [optional parameters]  >stdout 			",
" 									",
" Required parameters:							",
" 	none								",
" 									",
" Optional parameters:							",
" in=float	input type	(float)					",
" 		=double		(double)				",
" 		=short		(short)					",
" 		=ushort		(unsigned short)			",
" 		=long		(long)					",
" 		=ulong		(unsigned long)				",
" 		=int		(int)					",
" 									",
" outpar=/dev/tty		output parameter file, contains the	",
"				number of values (n1=)			",
" 			other choices for outpar are: /dev/tty,		",
" 			/dev/stderr, or a name of a disk file		",
" 									",
" Notes:								",
" The byte order of the mantissa of binary data values on PC's and DEC's",
" is the reverse of so called \"big endian\" machines (IBM RS6000, SUN,etc.)",
" hence the need for byte-swapping capability. The subroutines in this code",
" have been tested for swapping between PCs and	big endian machines, but",
" have not been tested for DEC products.				",
" 									",
" Caveat:								",
" 2 byte short, 4 byte long, 4 byte float, 4 byte int,			",
" and 8 bit double assumed.						",
" 									",
NULL};

/* Credits:
 *	CWP: John Stockwell (Jan 1994)
 * Institut fur Geophysik, Hamburg: Jens Hartmann supplied byte swapping
 *					subroutines
 */


/**************** end self doc ***********************************/

int
main(int argc, char **argv)
{
	char *in;		/* input data type			*/
	char *outpar;		/* name of file holding output parfile	*/
	FILE *outparfp;		/* ... its file pointer			*/
	int n = 0;		/* number of input points	 	*/

	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* Get parameters and do set up */
	if (!getparstring("outpar", &outpar))	outpar = "/dev/tty" ;
	outparfp = efopen(outpar, "w");

	/* Get input and output data types */
	if (!getparstring("in", &in))		in = "float" ;
        checkpars();
	
	/* Check in and out to see if types supported */
	if ( !(	STREQ(in,"float") 
		|| STREQ(in,"double") 
		|| STREQ(in,"short") 
		|| STREQ(in,"ushort") 
		|| STREQ(in,"long")
		|| STREQ(in,"ulong")
		|| STREQ(in,"int")
		) 
	) err("%s is an unsupported type",in);
	
	/* Read floats, swap bytes */
	if (STREQ(in,"float"))  {
		float xf;

		/* Loop over data swapping bytes  */
		while (efread(&xf,FSIZE, 1, stdin)) {
			++n;
			swap_float_4(&xf);
			efwrite(&xf, FSIZE, 1, stdout);
		}

	/* Read doubles, swap bytes */
	} else if (STREQ(in,"double")) {
		double xd;

		/* Loop over data swapping bytes */
		while (efread(&xd, DSIZE, 1, stdin)) {
			++n;
			swap_double_8(&xd);
			efwrite(&xd, DSIZE, 1, stdout);
		}

	/* Read shorts, swap bytes */
	} else if (STREQ(in,"short")) {
		short xs;

		/* Loop over data converting integer to float */
		while (efread(&xs, sizeof(short), 1, stdin)) {
			++n;
			swap_short_2(&xs);
			efwrite(&xs, sizeof(short), 1, stdout);
		}

	/* Read unsigned shorts, swap bytes */
	} else if (STREQ(in,"ushort")) {
		unsigned short xus;

		/* Loop over data converting integer to float */
		while (efread(&xus, sizeof(unsigned short), 1, stdin)) {
			++n;
			swap_u_short_2(&xus);
			efwrite(&xus, sizeof(unsigned short), 1, stdout);
		}

	/* read longs; swap bytes */
	} else if (STREQ(in,"long")) {
		long xl;

		/* Loop over data converting integer to float */
		while (efread(&xl, sizeof(long), 1, stdin)) {
			++n;
			swap_long_4(&xl);
			efwrite(&xl, sizeof(long), 1, stdout);
		}
	
	/* read unsigned longs; swap bytes */
	} else if (STREQ(in,"ulong")) {
		unsigned long xul;

		/* Loop over data converting integer to float */
		while (efread(&xul, sizeof(unsigned long), 1, stdin)) {
			++n;
			swap_u_long_4(&xul);
			efwrite(&xul, sizeof(unsigned long), 1, stdout);
		}

	} else if (STREQ(in,"int")) {
		int xi;

		/* Loop over data converting integer to float */
		while (efread(&xi, sizeof(int), 1, stdin)) {
			++n;
			swap_int_4(&xi);
			efwrite(&xi, sizeof(int), 1, stdout);
		}
	
	}

	/* Make par file */
	fprintf(outparfp, "n=%d\n", n);

	return(CWP_Exit());
}
