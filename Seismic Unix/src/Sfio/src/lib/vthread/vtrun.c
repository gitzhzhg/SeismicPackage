#include	"vthdr.h"

/*	Run a thread
**
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int vtrun(Vthread_t* vt, Void_t* (*startf)(Void_t*), Void_t* arg)
#else
int vtrun(vt, startf, arg)
Vthread_t*	vt;
Void_t*		(*startf)();
Void_t*		arg;
#endif
{
#if !vt_threaded
	return -1;
#else

#if _WIN32
	vt->self = CreateThread(0, (DWORD)vt->stack,
				(LPTHREAD_START_ROUTINE)startf, (LPVOID)arg,
				0, &vt->id);
	vt->error = vt->self ? 0 : EPERM;
#else
	vt->error = pthread_create(&vt->self, ATTR(vt->attrs), startf, arg);
	vt->id = vt->self;
#endif

	if(!vt->error)
		vt->state |= VT_RUNNING;

	return vt->error ? -1 : 0;

#endif /*vt_threaded*/
}
