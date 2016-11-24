/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/* 
 * PAUSE - prompt and wait for user signal to continue
 *
 * Usage:   pause [optional arguments]
 *
 * Note:
 * Default prompt is "press return key to continue" which is *evoked 
 * by calling pause with no arguments.  The word,
 * "continue", is replaced by any optional arguments handed to pause.
 * Thus, the command "pause do plot" will evoke the prompt,
 * "press return key to do plot".
 *
 */
/**************** end self doc ********************************/

/* Credits:
 *	CWP: Shuki
 *
 * This program belongs to the Center for Wave Phenomena
 * Colorado School of Mines
*/

#include "cwp.h"

int
main(int argc, char **argv)
{
	char do_what[BUFSIZ];
	int i;

	if (argc == 1) {
		strcpy(do_what, "continue");
	} else {
		strcpy(do_what, argv[1]);
		for (i = 2; i < argc; i++) {
			strcat(do_what, " ");
			strcat(do_what, argv[i]);
		}
	}
	printf("\npress return key to %s\n", do_what);
	getchar();
	return EXIT_SUCCESS;
}
