#include	"vthdr.h"

/*	Deleting a thread
**
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int vtkill(Vthread_t* vt)
#else
int vtkill(vt)
Vthread_t*	vt;
#endif
{
#if !vt_threaded
	return -1;
#else
	int	rv;

	if(!vt)
		return -1;

	if(!(vt->state&VT_RUNNING) )
	{	vt->error = 0;
		return -1;
	}

#if _WIN32
	if((rv = TerminateThread(vt->self, 0)) == FALSE)
	{	vt->error = GetLastError();
		rv = -1;
	}
	else	rv = 0;
#else
	if((rv = pthread_cancel(vt->self)) != 0)
	{	vt->error = rv;
		rv = -1;
	}
#endif

	if(rv == 0)
		vt->state &= ~VT_RUNNING;

	return rv;

#endif /*!vt_threaded*/
}
