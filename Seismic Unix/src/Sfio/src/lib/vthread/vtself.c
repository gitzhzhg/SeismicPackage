#include	"vthdr.h"

/*	Identify self
**
**	Written by Kiem-Phong Vo
*/

Vthread_t* vtself()
{
#if !vt_threaded
	return NIL(Vthread_t*);
#else
	Vthread_t*	vt;
	_vtid_t		id;
	int		t;

#if _WIN32
#define IDEQUAL(id1,id2)	(id1 == id2)
	id = GetCurrentThreadId();
#else
#define IDEQUAL(id1,id2)	pthread_equal(id1,id2)
	id = pthread_self();
#endif

	for(t = 0; t < _Vtnlist; ++t)
		if((vt = _Vtlist[t]) && IDEQUAL(id, vt->id) )
			return vt;

	return NIL(Vthread_t*);

#endif /*vt_threaded*/
}
