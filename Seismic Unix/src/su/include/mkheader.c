/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* mkheader - makes header.h file from output of mkprehdr.sh and mkoffs.sh
 *
 * Credits:
 *	CWP: Jack, Shuki
 *
 * This program belongs to the Center for Wave Phenomena
 * Colorado School of Mines
 *
 * $Author: john $
 * $Source: /usr/local/cwp/src/su/include/RCS/mkheader.c,v $
 * $Revision: 1.5 $ ; $Date: 1997/10/15 15:18:17 $
*/


#include "su.h"
#include "segy.h"
#include "prehdr.h"

segy tr;

int
main()
{
	int SU_NKEYS = DIM(hdr);
	int MAXSEGY = sizeof(segy);
	int HDRBYTES = (int) (MAXSEGY - SU_NFLTS*sizeof(float));


	/* Print header.h on stdout */
	printf("/*\n * header.h - include file for segy sizes\n");
	printf(" * THIS HEADER FILE IS GENERATED AUTOMATICALLY - \n");
	printf(" * see the Makefile in this directory\n */\n\n");
	printf("#ifndef HEADER_H\n");
	printf("#define HEADER_H\n\n");
	printf("#define SU_NKEYS\t%d\t/* Number of key header words */\n",
			SU_NKEYS);
	printf("#define HDRBYTES\t%d\t/* Bytes in the trace header */\n",
			HDRBYTES);
	printf("#define	MAXSEGY\t\t%d\n\n", MAXSEGY);
	printf("#endif\n");

	return EXIT_SUCCESS;
}
