#include	"vthdr.h"


/*	Try to lock a mutex.
**
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
int vtmtxtrylock(Vtmutex_t* mtx)
#else
int vtmtxtrylock(mtx)
Vtmutex_t*	mtx;
#endif
{
#if !vt_threaded
	return -1;
#else
	int	rv;

	if(!mtx)
		return -1;

#if _WIN32
#define _did_trylock	1
	if(_Vttrylockf && (*_Vttrylockf)(&mtx->lock) )
	{	if(mtx->count <= 0)
			mtx->count = 1;
		else	mtx->count += 1;
		return rv=0;
	}
	else
	{	mtx->error = EBUSY;
		return -1;
	}
#endif /*_WIN32*/

#if !_did_trylock && _mtx_recursive
#define _did_trylock	1
	if((rv = pthread_mutex_trylock(&mtx->lock)) == MTXTRYLOCK_OK)
	{	if(mtx->count <= 0)
			mtx->count = 1;
		else	mtx->count += 1;
		return 0;
	}
	else
	{	mtx->error = rv;
		return -1;
	}
#endif

#if !_did_trylock
	if((rv = pthread_mutex_trylock(&mtx->lock)) == MTXTRYLOCK_OK)
	{	mtx->count = 1;
		mtx->owner = pthread_self();
		return 0;
	}
	else if(MTXBUSY(rv) && pthread_equal(pthread_self(), mtx->owner) )
	{	mtx->count += 1;
		return 0;
	}
	else
	{	mtx->error = rv;
		return -1;
	}
#endif

#endif /*vt_threaded*/
}
