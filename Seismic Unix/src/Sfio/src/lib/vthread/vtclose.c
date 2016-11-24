#include	"vthdr.h"

/*	Deleting a thread
**
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int vtclose(Vthread_t* vt)
#else
int vtclose(vt)
Vthread_t*	vt;
#endif
{
#if !vt_threaded
	return -1;
#else
	int	s;

	if(!vt )
		return -1;

	vtmtxlock(_Vtmutex);

	for(s = 0; s < _Vtnlist; ++s)
		if(_Vtlist[s] == vt)
			break;

	if(s == _Vtnlist)
	{	vt->error = EINVAL;
		vtmtxunlock(_Vtmutex);
		return -1;
	}
	else if((vt->state&VT_RUNNING) && vtwait(vt) < 0)
	{	vtmtxunlock(_Vtmutex);
		return -1;
	}

	_Vtlist[s] = NIL(Vthread_t*);

#if !_WIN32
	pthread_attr_destroy(&vt->attrs);
#endif

	if(vt->state & VT_FREE)
		free(vt);

	vtmtxunlock(_Vtmutex);

	return 0;

#endif /*vt_threaded*/
}
