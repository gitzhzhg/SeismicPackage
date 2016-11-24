
#include "par.h"

char *sdoc[] = {
"									",
" TRIVIEW - View 3-d coordinates in 2-d					",
"									",
" triview <(3-d ascii coords) [optional parameters] >(2-d ascii coords)	",
"									",
" Optional Parameters:							",
" theta=   viewing angle (elevation)					",
" phi=     viewing angle (azimuth)					",
" xs0=     x translation of 2-d coords					",
" ys0=     y translation of 2-d coords					",
"									",
NULL};

/*
 * AUTHOR:  Phil Anno, Colorado School of Mines, 10/26/91
 *
 */


#define  DEG2RAD (PI/180.)

int
main (int argc, char **argv)
{
	int      itrios,ntrios,icolor;
	float    theta,phi,x,y,z,xs,ys,xs0,ys0;

	/* hook up getpar */
        initargs(argc,argv);
        requestdoc(1);

        /* get command line parameters, if present */
        if (!getparfloat("theta",&theta)) theta=0.;
        if (!getparfloat("phi",&phi))     phi=0.;
        if (!getparfloat("xs0",&xs0))     xs0=0.;
        if (!getparfloat("ys0",&ys0))     ys0=0.;

	theta = DEG2RAD*theta;
	phi   = DEG2RAD*phi;

	/* loop over coord list groups */
	while (fscanf(stdin,"%d %d\n",&ntrios,&icolor) != EOF) {

		/* valid ntrios & icolor, so */
		fprintf(stdout,"%d %d\n",ntrios,icolor);

		/* loop over coord list */
		for (itrios=1; itrios<=ntrios; ++itrios) {
			
			if (fscanf(stdin,"%f %f %f\n",&x,&y,&z) == 3) {
				/* valid coord, so */
				xs = cos(phi)*x + sin(phi)*y + xs0;
				ys = cos(phi)*sin(theta)*y -
				     sin(phi)*sin(theta)*x + z + ys0;
				fprintf (stdout,"%f %f\n",xs,ys);
			} else {
				err("invalid coordinate trio\n");
			}
		}
	}
	return EXIT_SUCCESS;
}
