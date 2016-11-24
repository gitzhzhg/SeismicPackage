/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/* mkhdr - makes hdr.h file from output of mkprehdr.sh and mkoffs.sh
 *
 * Credits:
 *	CWP: Jack, Shuki
 *
 * This program belongs to the Center for Wave Phenomena
 * Colorado School of Mines
 *
 * $Author: john $
 * $Source: /usr/local/cwp/src/su/include/RCS/mkhdr.c,v $
 * $Revision: 1.5 $ ; $Date: 1996/09/09 17:08:23 $
*/

#include "su.h"
#include "segy.h"
#include "prehdr.h"

segy tr;

int
main()
{
	int i;
	char buf[16];	/* buf is used to align the output fields */
	int SU_NKEYS = DIM(hdr);

	/* Don't need following if stddefs has a good offsetof macro */
	/* static STRICT_ALIGN _aligned = 0; */  /* for offset macro */


	/* hdr[].offs has been initialized to zero by the prehdr shell */

#include "offsets.h"

	/* Print hdr.h on stdout */
	printf("/*\n * hdr.h - include file for segy offset array\n");
	printf(" * THIS HEADER FILE IS GENERATED AUTOMATICALLY - \n");
	printf(" * see the Makefile in this directory\n */\n\n");
	printf("#ifndef HDR_H\n");
	printf("#define HDR_H\n\n");
	printf("static struct {\n");
	printf("\tchar *key;\tchar *type;\tint offs;\n");
	printf("} hdr[] = {\n");
	buf[0] = '"';
	for (i = 0; i < SU_NKEYS; i++) {
		strcpy(buf+1, hdr[i].key);
		strcat(buf,"\"");
		printf("\t{%10s,\t\t\"%s\",\t\t%d},\n",
			buf, hdr[i].type, hdr[i].offs);
	}
	printf("};\n");
	printf("#endif\n");

	return EXIT_SUCCESS;
}
