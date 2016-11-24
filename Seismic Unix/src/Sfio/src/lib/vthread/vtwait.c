#include	"vthdr.h"

/*	Wait for a thread to end
**
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int vtwait(Vthread_t* vt)
#else
int vtwait(vt)
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
	if((rv = WaitForSingleObject(vt->self, INFINITE)) == 0xffffffff)
		vt->error = GetLastError();
	else
	{	vt->exit = (Void_t*)rv;
		rv = 0;
		CloseHandle(vt->self);
	}
#else
	if((rv = pthread_join(vt->self, &vt->exit)) != 0)
		vt->error = rv;
#endif

	if(rv == 0)
		vt->state = (vt->state & ~VT_RUNNING) | VT_WAITED;

	return rv ? -1 : 0;

#endif /*vt_threaded*/
}
