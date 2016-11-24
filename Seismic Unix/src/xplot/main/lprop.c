/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* LPROP: $Revision: 1.6 $ ; $Date: 2011/11/21 17:03:15 $	*/

/*********************** self documentation **********************/
/*
 * LPROP - List PROPerties of root window of default screen of display 
 *
 * Usage:  lprop
 *
 */
/**************** end self doc ********************************/

#include "par.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
	
int
main(int argc, char **argv)
{
	Display *dpy;
	Atom *atom;
	int i,natoms;

	/* connect to X server */
	if ((dpy=XOpenDisplay(NULL))==NULL) {
		fprintf(stderr,"Cannot connect to display %s\n",
			XDisplayName(NULL));
		exit(-1);
	}

	/* list properties of root window */
	atom = XListProperties(dpy,DefaultRootWindow(dpy),&natoms);
	printf("Number of properties = %d\n",natoms);
	for (i=0; i<natoms; i++)
		printf("property[%d] = %s\n",i,XGetAtomName(dpy,atom[i]));

	/* close connection to X server */
	XCloseDisplay(dpy);

	return EXIT_SUCCESS;
}
