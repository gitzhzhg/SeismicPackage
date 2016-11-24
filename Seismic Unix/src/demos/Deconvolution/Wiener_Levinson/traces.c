/* Make traces with 2 way reverberations--suggested by Ken */

#include "cwp.h"

int
main()
{
	float x;	/* trace value       */
	int ns=512;	/* number of samples */
	int ntr=8;	/* number of traces  */
	int dtime=16;	/* one way time in samples through "ocean" layer */
	float rbot=0.8;	/* reflection strength of "ocean" bottom         */
	int h=100;	/* location in samples of two way reverb train 	 */
	float amp=0.2;	/* strength of reflector */
	int loc=170;	/* location of reflector on trace 1 in time samples */
	int dip=12;	/* dip of reflector in time samples */

	{ int j;
	for (j = 0; j < ntr; j++) {
		int i;
		for (i = 0; i < ns; i++) {
			x = 0.0;
			if (i >= h && ((i-h) % dtime == 0)) {
				int k = (i-h)/dtime;
				int sgn = (ISODD(k) ? -1 : 1);
				x = sgn * (k+1) * pow(rbot, k);
			}
			if (i == loc + j*dip) x += amp;
			fwrite(&x, FSIZE, 1, stdout);
		}
	} }
	return EXIT_SUCCESS;
}
