/*********************** self documentation **********************/
/*****************************************************************************
CPUSEC - return cpu time (UNIX user time) in seconds

cpusec		return cpu time in seconds

******************************************************************************
Input:

Returned:	cpu time in seconds

******************************************************************************
Function Prototype:
float cpusec (void);

******************************************************************************
Notes:
	Uses system call and include file.
	See also: cputime() in this library.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 04/29/89
*****************************************************************************/
/**************** end self doc ********************************/

#include <sys/time.h>
#include <sys/resource.h>

float 
cpusec()
/*****************************************************************************
return cpu time (UNIX user time) in seconds
******************************************************************************
Input:

Returned:	cpu time in seconds
******************************************************************************
Notes:
	Uses system call and include file.
	See also: cputime() in this library.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 04/29/89
*****************************************************************************/
{
	struct rusage rusage;
	getrusage(RUSAGE_SELF,&rusage);
	return ((float)((double)(rusage.ru_utime.tv_sec)+
		1.0e-6*(double)(rusage.ru_utime.tv_usec)));
}



#ifdef TEST

main()
{
	int i, n = 1000000;
	float cpu_used, a = 0.0, b = 1.0;

	cpu_used = cpusec();
	for (i = 0; i < n; ++i)  a += b;
	cpu_used = cpusec() - cpu_used;
	printf("a = %f  cpu time = %f\n", a, cpu_used);
}
#endif
