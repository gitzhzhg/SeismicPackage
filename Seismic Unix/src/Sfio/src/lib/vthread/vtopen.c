/* Copyright (c) Colorado School of Mines, 2007.*/
/* All rights reserved.                       */

#include	"vthdr.h"
#include	<string.h>

static char*    Version = "\n@(#)vthread (AT&T Labs - kpv) 2000-12-01\0\n";

/*	Open a thread handle
**
**	Written by Kiem-Phong Vo
*/

#if __STD_C
Vthread_t* vtopen(Vthread_t* vt, int flags)
#else
Vthread_t* vtopen(vt, flags)
Vthread_t*	vt;
int		flags;
#endif
{
#if !vt_threaded
	return NIL(Vthread_t*);
#else
	Vthread_t*	v;
	int		s, slot, myvt;

	VTONCE();

	v = (Vthread_t*)Version; /* shut compiler warning */

	vtmtxlock(_Vtmutex);

	myvt = 0;

	if(vt)
	{	/* if there is a running thread, wait for its termination */
		for(slot = 0; slot < _Vtnlist; ++slot)
			if(_Vtlist[slot] == vt)
				break;

		if(slot < _Vtnlist && (vt->state&VT_RUNNING) )
		{	vtmtxunlock(_Vtmutex);
			if(vtwait(vt) < 0)
				return NIL(Vthread_t*);
			vtmtxlock(_Vtmutex);
		}

		if(slot == _Vtnlist)
			goto find_slot;
	}
	else
	{ find_slot:
		for(slot = 0; slot < _Vtnlist; ++slot)
			if(_Vtlist[slot] == NIL(Vthread_t*))
				break;
	}

	/* create a slot for this new thread if necessary */
	if(slot >= _Vtnlist)
	{	Vthread_t** list;
#define INCR	8
		if(!(list = (Vthread_t**)malloc((slot+INCR)*sizeof(Vthread_t*))) )
		{	vtmtxunlock(_Vtmutex);
			return NIL(Vthread_t*);
		}

		if(_Vtnlist > 0)
/* vtopen.c:68: warning: incompatible implicit declaration of built-in function 'memcpy' */
		{	memcpy(list,_Vtlist,_Vtnlist*sizeof(Vthread_t*));
			free(_Vtlist);
		}

		_Vtlist = list;
		_Vtnlist += INCR;

		for(s = slot; s < _Vtnlist; ++s)
			list[s] = NIL(Vthread_t*);
	}

	if(!(v = vt) )
	{	if(!(v = (Vthread_t*)malloc(sizeof(Vthread_t))) )
		{	vtmtxunlock(_Vtmutex);
			return NIL(Vthread_t*);
		}
		flags |= VT_INIT|VT_FREE;
		myvt = 1;
	}

	if(flags&VT_INIT)
	{	v->state = (flags&VT_FREE);
		v->stack = 0;
		v->error = 0;
		v->exit  = NIL(Void_t*);
#if !_WIN32
		if(pthread_attr_init(&v->attrs) != 0)
		{	if(myvt)
				free(v);
			vtmtxunlock(_Vtmutex);
			return NIL(Vthread_t*);
		}
#endif
	}

	_Vtlist[slot] = v;

	vtmtxunlock(_Vtmutex);

	return v;

#endif /*vt_threaded*/
}
