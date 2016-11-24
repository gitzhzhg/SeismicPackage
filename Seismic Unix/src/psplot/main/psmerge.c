/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* PSMERGE: $Revision: 1.10 $ ; $Date: 2011/11/17 00:10:53 $	*/

#include "par.h"
#include "psplot.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" PSMERGE - MERGE PostScript files					",
" 									",
" psmerge in= [optional parameters] >postscriptfile			",
" 									",
" Required Parameters:							",
" in=                    postscript file to merge			",
" 									",
" Optional Parameters:							",
" origin=0.0,0.0         x,y origin in inches				",
" scale=1.0,1.0          x,y scale factors				",
" rotate=0.0             rotation angle in degrees			",
" translate=0.0,0.0      x,y translation in inches			",
" 									",
" Notes:								",
" More than one set of in, origin, scale, rotate, and translate		",
" parameters may be specified.  Output x and y coordinates are		",
" determined by:							",
"          x = tx + (x-ox)*sx*cos(d) - (y-oy)*sy*sin(d)			",
"          y = ty + (x-ox)*sx*sin(d) + (y-oy)*sy*cos(d)			",
" where tx,ty are translate coordinates, ox,oy are origin coordinates,	",
" sx,sy are scale factors, and d is the rotation angle.  Note that the	",
" order of operations is shift (origin), scale, rotate, and translate.	",
" 									",
" If the number of occurrences of a given parameter is less than the number",
" of input files, then the last occurrence of that parameter will apply to",
" all subsequent files.							",
" 									",
NULL};
/**************** end self doc ********************************/

/*
 * AUTHOR:  Dave Hale, Colorado School of Mines, 11/10/90
 * MODIFIED:  Dave Hale, Colorado School of Mines, 04/16/91
 *           added rotate parameter
 * MODIFIED:  Craig Artley, Colorado School of Mines, 08/30/91
 *           BoundingBox moved to top of PostScript output
 *        Fixed bug in calculating BoundingBox for rotated output
 */

#define MAXLINE 2048

/* prototype for function defined below */
static void updatebbox (float sx, float sy, float tx, float ty,
	float ox, float oy, float d,
	int *llx, int *lly, int *urx, int *ury);

int main (int argc, char **argv)
{
	int nin,iin,llx,lly,urx,ury,llxm,llym,urxm,urym;
	float fllx,flly,furx,fury,*sx,*sy,*tx,*ty,*ox,*oy,*d,fval[100];
	char **in,line[MAXLINE];
	FILE *infp,*outfp=stdout;

	/* initialize getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* determine number of input files */
	nin = countparname("in");
	if (nin<=0) err("must specify at least one input file");

	/* allocate space for parameter vectors */
	in = (char**)ealloc1(nin,sizeof(char*));
	sx = ealloc1float(nin);
	sy = ealloc1float(nin);
	tx = ealloc1float(nin);
	ty = ealloc1float(nin);
	ox = ealloc1float(nin);
	oy = ealloc1float(nin);
	d = ealloc1float(nin);

	/* initialize bounding box limits */
	llxm = llym = INT_MAX;
	urxm = urym = INT_MIN;

	/* loop over input files to find bounding box limits */
	for (iin=0; iin<nin; ++iin) {

		/* open file */
		getnparstring(iin+1,"in",&in[iin]);
		infp = efopen(in[iin],"r");

		/* transform coordinates */
		if (getnparfloat(iin+1,"rotate",fval)==1) {
			d[iin] = fval[0];
		} else if (iin>0) {
			d[iin] = d[iin-1];
		 } else {
			d[iin] = 0.0;
		}
		if (getnparfloat(iin+1,"scale",fval)==2) {
			sx[iin] = fval[0];
			sy[iin] = fval[1];
		} else if (iin>0) {
			sx[iin] = sx[iin-1];
			sy[iin] = sy[iin-1];
		} else {
			sx[iin] = 1.0;
			sy[iin] = 1.0;
		}
		if (getnparfloat(iin+1,"translate",fval)==2) {
			tx[iin] = fval[0]*72.0;
			ty[iin] = fval[1]*72.0;
		} else if (iin>0) {
			tx[iin] = tx[iin-1];
			ty[iin] = ty[iin-1];
		}
		else {
			tx[iin] = 0.0;
			ty[iin] = 0.0;
		}
		if (getnparfloat(iin+1,"origin",fval)==2) {
			ox[iin] = fval[0]*72.0;
			oy[iin] = fval[1]*72.0;
		} else if(iin>0) {
			ox[iin] = ox[iin-1];
			oy[iin] = oy[iin-1];
		} else {
			ox[iin] = 0.0;
			oy[iin] = 0.0;
		}

		/* loop over lines in file, looking for bounding box */
		while (fgets(line,MAXLINE,infp)!=NULL) {

			/* if comment */
			if (line[0]=='%') {

				/* if bounding box specification */
				if (strstr(line,"%%BoundingBox:")!=NULL) {

					/* if no bbox values, skip it */
					if (strstr(line,"atend")!=NULL)
						continue;

					/* update bounding box */
					if (sscanf(line,"%*s %d %d %d %d",
						&llx,&lly,&urx,&ury)==4) {
						/* read integers */
					} else if (sscanf(line,"%*s %f %f %f %f",
						&fllx,&flly,&furx,&fury)==4) {
						/* read floats and convert */
						llx = NINT(fllx);
						lly = NINT(flly);
						urx = NINT(furx);
						ury = NINT(fury);
					} else {
						err("Error reading BoundingBox!\n");
					}
					updatebbox(sx[iin],sy[iin],
						tx[iin],ty[iin],
						ox[iin],oy[iin],
						d[iin],
						&llx,&lly,&urx,&ury);
					llxm = MIN(llxm,llx);
					llym = MIN(llym,lly);
					urxm = MAX(urxm,urx);
					urym = MAX(urym,ury);

					/* found bbox, quit looking */
					break;
				}

			}

		}

		/* close file */
		fclose(infp);
	}

	/* set bounding box */
	boundingbox(llxm,llym,urxm,urym);

	/* begin PostScript */
	begineps();

	/* loop over input files */
	for (iin=0; iin<nin; ++iin) {

		/* open file */
		infp = efopen(in[iin],"r");

		/* save graphics state */
		gsave();

		translate(tx[iin],ty[iin]);
		rotate(d[iin]);
		scale(sx[iin],sy[iin]);
		translate(-ox[iin],-oy[iin]);

		/* loop over lines in file */
		while (fgets(line,MAXLINE,infp)!=NULL) {

			/* discard comments */
			if (line[0]=='%') continue;

			/* discard showpage if at beginning of line */
			if (strstr(line,"showpage")==&line[0]) continue;

			/* write output line */
			fputs(line,outfp);
		}

		/* restore graphics state */
		grestore();

		/* close file */
		fclose(infp);
	}

	/* end PostScript */
	showpage();
	endeps();

	/* free space */
	free1(in);
	free1float(sx);
	free1float(sy);
	free1float(tx);
	free1float(ty);
	free1float(ox);
	free1float(oy);
	free1float(d);

	return 0;
}

static void updatebbox (float sx, float sy, float tx, float ty,
	float ox, float oy, float d,
	int *llx, int *lly, int *urx, int *ury)
/**************************************************************************
UPDATE BoundingBOX based on translation, scaling, and rotation parameters
***************************************************************************
Input:
sx,sy			x and y scaling factors
tx,ty			x and y translations
ox,oy			x and y origin shifts
d			rotation angle in degrees
llx,lly,urx,ury		original BoundingBox

Output:
llx,lly,urx,ury		updated BoundingBox
***************************************************************************
Author: Craig Artley, Colorado School of Mines, 08/30/91
***************************************************************************/
{
	float s,c,x1,x2,x3,x4,y1,y2,y3,y4;

	/* compute sine and cosine of rotation angle */
	s = sin(PI*d/180.0);
	c = cos(PI*d/180.0);

	/* compute new coordinates of the original BoundingBox vertices */ 
	x1 = c*(*llx-ox)*sx - s*(*lly-oy)*sy + tx;
	y1 = s*(*llx-ox)*sx + c*(*lly-oy)*sy + ty;

	x2 = c*(*urx-ox)*sx - s*(*lly-oy)*sy + tx;
	y2 = s*(*urx-ox)*sx + c*(*lly-oy)*sy + ty;

	x3 = c*(*urx-ox)*sx - s*(*ury-oy)*sy + tx;
	y3 = s*(*urx-ox)*sx + c*(*ury-oy)*sy + ty;

	x4 = c*(*llx-ox)*sx - s*(*ury-oy)*sy + tx;
	y4 = s*(*llx-ox)*sx + c*(*ury-oy)*sy + ty;

	/* set the new BoundingBox */	
	*llx = MIN(x1,MIN(x2,MIN(x3,x4)));
	*lly = MIN(y1,MIN(y2,MIN(y3,y4)));
	*urx = MAX(x1,MAX(x2,MAX(x3,x4)));
	*ury = MAX(y1,MAX(y2,MAX(y3,y4)));
}
