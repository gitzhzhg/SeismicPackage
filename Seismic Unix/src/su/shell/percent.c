/* percent - find 100 * ratio of arguments
 * Usage: percent x y
 *
 * This triviality is called by the rmaxdiff shell.  Surely, there
 * is a better way, but ...
 *
 * $Author: john $
 * $Source: /usr/local/cwp/src/su/shell/RCS/percent.c,v $
 * $Revision: 1.1 $ ; $Date: 2011/11/22 18:03:33 $
 */

#include <stdio.h>
#include <math.h>

int main( int argc, char* argv[])
{
	double x, y;

	x = atof(argv[1]);
	y = atof(argv[2]);
	printf("%g\n", y == 0.0 ? 0.0 : 100.0*x/y);
}
