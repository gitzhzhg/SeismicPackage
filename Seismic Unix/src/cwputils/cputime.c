/*********************** self documentation **********************/
/*****************************************************************************
CPUTIME - return cpu time (UNIX user time) in seconds using ANSI C built-ins

cputime return cpu time (UNIX user time) in seconds using ANSI C built-ins

******************************************************************************
Input:

Returned:	cpu time in seconds

******************************************************************************
Function Prototype:
float cputime (void);

******************************************************************************
NOTES:
Relies on CLK_TCK being defined in time.h per ANSI C.
See also: cpusec() in this library.  There is some variability in
the definition that should be used for CLOCK_UNIT.

On Solaris, use the folowing #ifdef ... #endif block:
#if (_IBMR2||__hp9000s800)
#define CLOCK_UNIT	1000000
#else
#define CLOCK_UNIT	CLK_TCK
#endif

******************************************************************************
Author:		Jack K. Cohen, Colorado School of Mines, 07/27/90
******************************************************************************/
/**************** end self doc ********************************/

#include <time.h>

#ifndef CLK_TCK
#define CLK_TCK	64
#endif

/* ANSI C book says clock() units are CLK_TCK, but IBM docs say millisecs */
/* under solaris change CLK_TCK to _SC_CLK_TCK */
#if (_IBMR2||__hp9000s800)
#define CLOCK_UNIT	1000000
#else
#define CLOCK_UNIT	CLK_TCK
#endif

float
cputime()
/*****************************************************************************
return cpu time (UNIX user time) in seconds using ANSI C built-ins
******************************************************************************
Input:

Returned:	cpu time in seconds
******************************************************************************
NOTES:
	Relies on CLK_TCK being defined in time.h per ANSI C.
	See also: cpusec() in this library.
******************************************************************************
Author:		Jack K. Cohen, Colorado School of Mines, 07/27/90
*****************************************************************************/
{
	return clock() / (float) CLOCK_UNIT;
}



#ifdef TEST

main()
{
	int i, n = 1000000;
	float cpu_used, a = 0.0, b = 1.0;

	cpu_used = cputime();
	for (i = 0; i < n; ++i)  a += b;
	cpu_used = cputime() - cpu_used;
	printf("a = %f  cpu time = %f\n", a, cpu_used);
}
#endif
