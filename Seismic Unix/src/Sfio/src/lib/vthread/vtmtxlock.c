#include	"vthdr.h"


/*	Lock a mutex.
**
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
int vtmtxlock(Vtmutex_t* mtx)
#else
int vtmtxlock(mtx)
Vtmutex_t*	mtx;
#endif
{
#if !vt_threaded
	return -1;
#else
	int	rv;

	if(!mtx )
		return -1;

#if _WIN32
#define _did_lock	1
	EnterCriticalSection(&mtx->lock);
	if(mtx->count <= 0)
		mtx->count = 1;
	else	mtx->count += 1;
	return rv=0;
#endif

#if !_did_lock && _mtx_recursive
#define _did_lock	1
	if((rv = pthread_mutex_lock(&mtx->lock)) == MTXLOCK_OK)
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

#if !_did_lock && _mtx_errorcheck && !_hpux_pthread
#define _did_lock	1
	if((rv = pthread_mutex_lock(&mtx->lock)) == MTXLOCK_OK)
	{	mtx->count = 1;
		mtx->owner = pthread_self();
		return 0;
	}
	else if(rv == EDEADLK && pthread_equal(pthread_self(), mtx->owner))
	{	mtx->count += 1;
		return 0;
	}
	else
	{	mtx->error = rv;
		return -1;
	}
#endif

#if !_did_lock	/* hpux,solaris,bsd */
	if((rv = pthread_mutex_trylock(&mtx->lock)) == MTXTRYLOCK_OK)
	{	mtx->count = 1;
		mtx->owner = pthread_self();
		return 0;
	}
	else if(MTXBUSY(rv) && pthread_equal(pthread_self(), mtx->owner) )
	{	mtx->count += 1;
		return 0;
	}
	else if((rv = pthread_mutex_lock(&mtx->lock)) == MTXLOCK_OK)
	{	mtx->count = 1;
		mtx->owner = pthread_self();
		return 0;
	}
	else
	{	mtx->error = rv;
		return -1;
	}
#endif

#endif /*vt_threaded*/
}

