/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* PSBBOX: $Revision: 1.7 $ ; $Date: 2011/11/17 00:10:53 $	*/

#include "par.h"
#include "psplot.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"								",
" PSBBOX - change BoundingBOX of existing PostScript file	",
"								",
" psbbox < PostScriptfile [optional parameters] > PostScriptfile",
"								",
" Optional Parameters:						",
" llx=		new llx						",
" lly=		new lly						",
" urx=		new urx						",
" ury=		new ury						",
" verbose=1	=1 for info printed on stderr (0 for no info)	",
"								",
NULL};
/**************** end self doc ********************************/

/*
 * AUTHOR: CWP:  Craig Artley, Colorado School of Mines, 08/30/91
 */

#define MAXLINE 2048

int main (int argc, char **argv)
{
	int llx,lly,urx,ury,verbose,BBoxFound=0;
	float fllx,flly,furx,fury;
	char line[MAXLINE];
	FILE *infp=stdin,*outfp=stdout;

	/* initialize getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* get verbose parameter if present */
	if (!getparint("verbose",&verbose)) verbose = 1;

	/* loop over lines in file */
	while (fgets(line,MAXLINE,infp)!=NULL) {

		/* if comment and still looking for BoundingBox */
		if (line[0]=='%' && !BBoxFound) {

			/* if bounding box specification */
			if (strstr(line,"%%BoundingBox:")!=NULL) {

				/* if no bbox values, skip it */
				if (strstr(line,"atend")!=NULL)
					continue;

				/* read bounding box */
				if (sscanf(line,"%*s %d %d %d %d",
					&llx,&lly,&urx,&ury)==4) {
				} else if (sscanf(line,"%*s %f %f %f %f",
					&fllx,&flly,&furx,&fury)==4) {
					/* BoundingBox given as floats */
					llx = NINT(fllx);
					lly = NINT(flly);
					urx = NINT(furx);
					ury = NINT(fury);
				} else {
					err("Error reading BoundingBox!\n");
				}
				
				/* echo original BoundingBox */
				if (verbose) {
					fprintf(stderr,"Original:  ");
					fprintf(stderr,"%s",line);
				}

				/* update bounding box */
				getparint("llx",&llx);
				getparint("lly",&lly);
				getparint("urx",&urx);
				getparint("ury",&ury);
				sprintf(line,"%s %d %d %d %d\n",
					"%%BoundingBox:",
					llx,lly,urx,ury);
				BBoxFound=1;

				/* echo updated BoundingBox */
				if (verbose) {
					fprintf(stderr,"Updated:   ");
					fprintf(stderr,"%s",line);
				}
			}
		}

		/* write output line */
		fputs(line,outfp);
	}

	return 0;
}
